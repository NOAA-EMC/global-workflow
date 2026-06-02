"""Property-based tests for DAG Filter correctness properties.

Tests the following correctness properties from the design document:

- Property 1: DAG Filter Soundness (no false exclusions)
- Property 2: DAG Filter Completeness (no false inclusions)
- Property 11: Unconditional Config Inclusion
- Property 12: JAAAAA Naming Enforcement

Uses Hypothesis to generate random workflow YAMLs, J-Job sets, and filenames
to verify these properties hold across all valid inputs.
"""

from __future__ import annotations

import os
import re
import sys
import tempfile
from pathlib import Path

from hypothesis import given, settings, HealthCheck
from hypothesis import strategies as st

sys.path.insert(0, os.path.join(os.path.dirname(__file__), ".."))

from deployment.dag_filter import (
    DAGFilter,
    DAGReachabilitySet,
    _UNCONDITIONAL_CONFIGS,
)


# ---------------------------------------------------------------------------
# JAAAAA naming pattern (from Requirements 1.4, 10.2)
# All uppercase, starts with J, at least one uppercase letter after J,
# underscores allowed as separators but not leading/trailing after J.
# No extension (no dots), no digits.
# ---------------------------------------------------------------------------

_JJOB_NAME_PATTERN = re.compile(r"^J[A-Z][A-Z_]*$")


def is_valid_jjob_name(name: str) -> bool:
    """Check if a filename follows the JAAAAA naming convention.

    Valid: starts with J, followed by at least one uppercase letter,
    then any combination of uppercase letters and underscores. No dots,
    no digits, no lowercase.
    Examples: JGLOBAL_FORECAST, JGFS_ATMOS_POST, JA
    Invalid: jglobal_forecast, JGLOBAL.sh, forecast, J123, J_, J
    """
    return bool(_JJOB_NAME_PATTERN.match(name))


# ---------------------------------------------------------------------------
# Hypothesis Strategies
# ---------------------------------------------------------------------------


@st.composite
def _valid_jjob_names(draw, min_size=1, max_size=8):
    """Generate a set of valid JAAAAA names (min_size to max_size names)."""
    num = draw(st.integers(min_value=min_size, max_value=max_size))
    # Generate unique J-Job names: J + uppercase letter + 1-14 uppercase/underscores
    names = set()
    for _ in range(num):
        # First char after J must be uppercase letter
        first = draw(st.sampled_from("ABCDEFGHIJKLMNOPQRSTUVWXYZ"))
        # Rest can be uppercase letters and underscores
        rest_len = draw(st.integers(min_value=1, max_value=14))
        rest = draw(
            st.text(
                alphabet=st.sampled_from("ABCDEFGHIJKLMNOPQRSTUVWXYZ_"),
                min_size=rest_len,
                max_size=rest_len,
            ).filter(lambda s: not s.endswith("_"))
        )
        names.add(f"J{first}{rest}")
    return names


@st.composite
def _workflow_yaml_with_jjobs(draw, jjob_names: set[str] | None = None):
    """Generate a random workflow YAML dict referencing the given J-Job names.

    If jjob_names is None, generates a random set of valid J-Job names.
    Distributes jjobs across 1-4 families with 1-5 tasks each.
    """
    if jjob_names is None:
        jjob_names = draw(_valid_jjob_names())

    jjob_list = sorted(jjob_names)
    if not jjob_list:
        return {"families": []}

    # Distribute jjobs across random number of families
    num_families = draw(st.integers(min_value=1, max_value=min(4, len(jjob_list))))
    families = []

    # Assign jjobs round-robin to families
    for i in range(num_families):
        families.append({"name": f"family_{i}", "tasks": []})

    for idx, jjob in enumerate(jjob_list):
        family_idx = idx % num_families
        families[family_idx]["tasks"].append(
            {"name": f"task_{idx}", "jjob": jjob}
        )

    return {"families": families}


@st.composite
def _superset_jjob_names(draw, referenced: set[str]):
    """Generate a set of available J-Jobs that is a strict superset of referenced.

    Adds 1-10 extra unreferenced J-Job names.
    """
    extra_count = draw(st.integers(min_value=1, max_value=10))
    extra = set()
    for _ in range(extra_count):
        first = draw(st.sampled_from("ABCDEFGHIJKLMNOPQRSTUVWXYZ"))
        rest = draw(
            st.text(
                alphabet=st.sampled_from("ABCDEFGHIJKLMNOPQRSTUVWXYZ_"),
                min_size=3,
                max_size=12,
            ).filter(lambda s: not s.endswith("_"))
        )
        name = f"J{first}{rest}"
        # Ensure extra names don't collide with referenced
        if name not in referenced:
            extra.add(name)
    return referenced | extra


# ---------------------------------------------------------------------------
# Filesystem Setup Helpers
# ---------------------------------------------------------------------------


def _create_dev_structure(base_dir: Path, jjob_names: set[str]) -> Path:
    """Create minimal dev/ directory with the given J-Job files.

    Creates:
    - jobs/<JJOB> for each jjob name
    - scripts/ (empty)
    - ush/ (empty)
    - parm/config/gfs/ with unconditional config files
    """
    jobs_dir = base_dir / "jobs"
    jobs_dir.mkdir(parents=True, exist_ok=True)
    scripts_dir = base_dir / "scripts"
    scripts_dir.mkdir(parents=True, exist_ok=True)
    ush_dir = base_dir / "ush"
    ush_dir.mkdir(parents=True, exist_ok=True)
    config_dir = base_dir / "parm" / "config" / "gfs"
    config_dir.mkdir(parents=True, exist_ok=True)

    # Create J-Job files (minimal content — no ex-script references)
    for jjob in jjob_names:
        (jobs_dir / jjob).write_text(f"#!/bin/bash\necho {jjob}\n")

    # Create unconditional config files
    for cfg in _UNCONDITIONAL_CONFIGS:
        (config_dir / cfg).write_text(f"# {cfg}\n")

    # Create base resources config
    (config_dir / "config.resources").write_text("# resources\n")

    return base_dir


# ---------------------------------------------------------------------------
# Property 1: DAG Filter Soundness (no false exclusions)
# ---------------------------------------------------------------------------


@given(data=st.data())
@settings(
    max_examples=100,
    deadline=None,
    suppress_health_check=[HealthCheck.too_slow, HealthCheck.function_scoped_fixture],
)
def test_dag_filter_soundness(data):
    """Property 1: DAG Filter Soundness (no false exclusions).

    For any valid Workflow_YAML and corresponding dev/ source tree,
    every J-Job referenced by a task in the Workflow_YAML SHALL appear
    in the DAG_Reachability_Set's jjobs field.

    **Validates: Requirements 1.1, 1.3, 2.1, 2.3**
    """
    # Generate random valid J-Job names
    jjob_names = data.draw(_valid_jjob_names(min_size=1, max_size=8))

    # Create workflow YAML referencing all these J-Jobs
    workflow_yaml = data.draw(_workflow_yaml_with_jjobs(jjob_names))

    # Create filesystem with all referenced J-Jobs present
    with tempfile.TemporaryDirectory() as tmp_dir:
        dev_root = _create_dev_structure(Path(tmp_dir), jjob_names)

        # Run DAGFilter Layer 1
        dag = DAGFilter(dev_root, workflow_yaml, "hera")
        extracted_jjobs = dag.extract_jjobs_from_yaml()

        # Property: every referenced J-Job MUST appear in the extracted set
        for jjob in jjob_names:
            assert jjob in extracted_jjobs, (
                f"Soundness violation: J-Job '{jjob}' is referenced in the "
                f"Workflow_YAML but was NOT extracted by the DAGFilter.\n"
                f"Referenced jjobs: {jjob_names}\n"
                f"Extracted jjobs: {extracted_jjobs}\n"
                f"Workflow YAML: {workflow_yaml}"
            )


# ---------------------------------------------------------------------------
# Property 2: DAG Filter Completeness (no false inclusions)
# ---------------------------------------------------------------------------


@given(data=st.data())
@settings(
    max_examples=100,
    deadline=None,
    suppress_health_check=[HealthCheck.too_slow, HealthCheck.function_scoped_fixture],
)
def test_dag_filter_completeness(data):
    """Property 2: DAG Filter Completeness (no false inclusions).

    For any valid Workflow_YAML and corresponding dev/ source tree,
    the DAG_Reachability_Set SHALL contain NO J-Job that is not
    referenced by any task in the Workflow_YAML.

    **Validates: Requirements 1.2, 2.2**
    """
    # Generate a set of referenced J-Jobs
    referenced_jjobs = data.draw(_valid_jjob_names(min_size=1, max_size=5))

    # Generate a superset of available J-Jobs (includes unreferenced ones)
    available_jjobs = data.draw(_superset_jjob_names(referenced_jjobs))
    unreferenced_jjobs = available_jjobs - referenced_jjobs

    # Create workflow YAML referencing only the referenced set
    workflow_yaml = data.draw(_workflow_yaml_with_jjobs(referenced_jjobs))

    # Create filesystem with ALL available J-Jobs present
    with tempfile.TemporaryDirectory() as tmp_dir:
        dev_root = _create_dev_structure(Path(tmp_dir), available_jjobs)

        # Run DAGFilter Layer 1
        dag = DAGFilter(dev_root, workflow_yaml, "hera")
        extracted_jjobs = dag.extract_jjobs_from_yaml()

        # Property: NO unreferenced J-Job should appear in the extracted set
        for jjob in unreferenced_jjobs:
            assert jjob not in extracted_jjobs, (
                f"Completeness violation: J-Job '{jjob}' is NOT referenced "
                f"in the Workflow_YAML but WAS included in the extracted set.\n"
                f"Referenced jjobs: {referenced_jjobs}\n"
                f"Unreferenced jjobs: {unreferenced_jjobs}\n"
                f"Extracted jjobs: {extracted_jjobs}\n"
                f"Workflow YAML: {workflow_yaml}"
            )

        # Additional: extracted set should be exactly the referenced set
        assert extracted_jjobs == referenced_jjobs, (
            f"Extracted set does not match referenced set exactly.\n"
            f"Expected: {referenced_jjobs}\n"
            f"Got: {extracted_jjobs}"
        )


# ---------------------------------------------------------------------------
# Property 11: Unconditional Config Inclusion
# ---------------------------------------------------------------------------


@given(data=st.data())
@settings(
    max_examples=100,
    deadline=None,
    suppress_health_check=[HealthCheck.too_slow, HealthCheck.function_scoped_fixture],
)
def test_unconditional_config_inclusion(data):
    """Property 11: Unconditional Config Inclusion.

    For any valid Workflow_YAML, the DAG_Filter SHALL always include
    config.base.j2, config.base, and config.com in the extracted
    config file set, regardless of which tasks are in the DAG.

    **Validates: Requirements 4.4**
    """
    # Generate random J-Job names (may be empty set for edge case)
    jjob_names = data.draw(_valid_jjob_names(min_size=0, max_size=6))

    # Create workflow YAML (may reference jjobs or be empty)
    workflow_yaml = data.draw(_workflow_yaml_with_jjobs(jjob_names))

    # Create filesystem with the J-Jobs and unconditional configs
    with tempfile.TemporaryDirectory() as tmp_dir:
        dev_root = _create_dev_structure(Path(tmp_dir), jjob_names)

        # Run DAGFilter Layer 4 with whatever jjobs we have
        dag = DAGFilter(dev_root, workflow_yaml, "hera")
        config_files = dag.extract_config_files(jjob_names)

        # Property: unconditional configs MUST always be present
        for cfg in _UNCONDITIONAL_CONFIGS:
            assert cfg in config_files, (
                f"Unconditional config '{cfg}' is MISSING from extracted "
                f"configs.\n"
                f"_UNCONDITIONAL_CONFIGS: {_UNCONDITIONAL_CONFIGS}\n"
                f"Extracted configs: {config_files}\n"
                f"J-Jobs: {jjob_names}\n"
                f"Workflow YAML: {workflow_yaml}"
            )


# ---------------------------------------------------------------------------
# Property 12: JAAAAA Naming Enforcement
# ---------------------------------------------------------------------------


# Strategy: generate valid JAAAAA names — J followed by uppercase letters/underscores
# Must have at least one uppercase letter after J
_st_valid_jjob = st.from_regex(r"J[A-Z][A-Z_]{0,14}", fullmatch=True).filter(
    lambda s: not s.endswith("_")
)

# Strategy: generate invalid names (various violations)
_st_invalid_jjob = st.one_of(
    # Lowercase letters after J
    st.from_regex(r"J[a-z][a-z_]+", fullmatch=True),
    # Doesn't start with J (starts with other uppercase)
    st.from_regex(r"[A-IK-Z][A-Z_]{2,10}", fullmatch=True),
    # Has file extension (dot)
    st.from_regex(r"J[A-Z][A-Z_]{1,8}\.[a-z]{1,3}", fullmatch=True),
    # Contains digits
    st.from_regex(r"J[A-Z][A-Z_]*[0-9]+[A-Z_]*", fullmatch=True),
    # Just "J" (too short, no letter after J)
    st.just("J"),
    # Empty string
    st.just(""),
    # Starts with lowercase j
    st.from_regex(r"j[A-Z][A-Z_]+", fullmatch=True),
    # Only underscores after J (no uppercase letter)
    st.from_regex(r"J_+", fullmatch=True),
)


@given(name=_st_valid_jjob)
@settings(
    max_examples=100,
    deadline=None,
    suppress_health_check=[HealthCheck.too_slow],
)
def test_jaaaaa_naming_accepts_valid(name):
    """Property 12a: JAAAAA Naming Enforcement — accepts valid names.

    For any filename matching ^J[A-Z][A-Z_]*$ (starts with J, followed
    by at least one uppercase letter, then uppercase letters/underscores),
    the naming validator SHALL accept it.

    **Validates: Requirements 1.4, 10.2**
    """
    assert is_valid_jjob_name(name), (
        f"Valid JAAAAA name '{name}' was rejected by the validator.\n"
        f"Pattern: ^J[A-Z][A-Z_]*$"
    )


@given(name=_st_invalid_jjob)
@settings(
    max_examples=100,
    deadline=None,
    suppress_health_check=[HealthCheck.too_slow],
)
def test_jaaaaa_naming_rejects_invalid(name):
    """Property 12b: JAAAAA Naming Enforcement — rejects invalid names.

    For any filename NOT matching ^J[A-Z][A-Z_]*$ (contains lowercase,
    digits, dots, doesn't start with J, or has no uppercase letter after J),
    the naming validator SHALL reject it.

    **Validates: Requirements 1.4, 10.2**
    """
    assert not is_valid_jjob_name(name), (
        f"Invalid JAAAAA name '{name}' was ACCEPTED by the validator.\n"
        f"Pattern: ^J[A-Z][A-Z_]*$\n"
        f"This name should have been rejected."
    )
