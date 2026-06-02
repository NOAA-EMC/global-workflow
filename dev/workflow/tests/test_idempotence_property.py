"""Property-based test: Deployment Idempotence (Property 10).

Tests that deploying the same configuration twice produces byte-identical
results from individual pipeline components. Focuses on determinism of:

1. DAGFilter: compute_reachability() returns identical sets and warnings
   regardless of call count or filesystem enumeration order.
2. ConfigConditioner: condition_file() returns identical output bytes for
   the same input content and deploy-time variable set.
3. Combined: Running the same operation N times produces identical outputs.

**Validates: Requirements 12.1, 12.2, 12.3, 12.4**

Traces to: Design Document - Correctness Property 10
  "Deploying the same Workflow_YAML, platform, and git commit twice
   SHALL produce byte-identical results."
"""

from __future__ import annotations

import os
import sys
import tempfile
from pathlib import Path

from hypothesis import given, settings, HealthCheck, assume
from hypothesis import strategies as st

sys.path.insert(0, os.path.join(os.path.dirname(__file__), ".."))

from deployment.dag_filter import (
    DAGFilter,
    DAGReachabilitySet,
    _UNCONDITIONAL_CONFIGS,
)
from deployment.config_conditioner import ConfigConditioner, ConditionerResult


# ---------------------------------------------------------------------------
# Hypothesis Strategies
# ---------------------------------------------------------------------------

# Deploy-time variable names from the registry (Req 11.3)
_DEPLOY_TIME_VAR_NAMES = [
    "RUN", "NET", "CASE", "CASE_ENS", "MACHINE", "CDUMP",
    "NMEM_ENS", "APP", "CCPP_SUITE", "DO_COUPLED", "DO_WAVE",
    "DO_OCN", "DO_ICE", "DO_AERO", "REPLAY_ICS",
]

# Values that deploy-time variables might take
_RUN_VALUES = ["gfs", "gdas", "gefs", "sfs", "enkfgdas"]
_NET_VALUES = ["gfs", "gefs", "sfs"]
_CASE_VALUES = ["C48", "C96", "C192", "C384", "C768"]
_MACHINE_VALUES = ["HERA", "WCOSS2", "ORION", "HERCULES", "GAEAC6"]
_BOOL_VALUES = ["YES", "NO"]


@st.composite
def _deploy_time_vars(draw):
    """Generate a random but valid deploy-time variable dict."""
    return {
        "RUN": draw(st.sampled_from(_RUN_VALUES)),
        "NET": draw(st.sampled_from(_NET_VALUES)),
        "CASE": draw(st.sampled_from(_CASE_VALUES)),
        "CASE_ENS": draw(st.sampled_from(_CASE_VALUES)),
        "MACHINE": draw(st.sampled_from(_MACHINE_VALUES)),
        "CDUMP": draw(st.sampled_from(_RUN_VALUES)),
        "NMEM_ENS": draw(st.sampled_from(["0", "20", "30", "80"])),
        "APP": draw(st.sampled_from(["ATM", "ATMA", "S2S", "S2SW", "S2SWA"])),
        "CCPP_SUITE": draw(st.sampled_from([
            "FV3_GFS_v17_p8", "FV3_GFS_v17_p8_ugwpv1",
        ])),
        "DO_COUPLED": draw(st.sampled_from(_BOOL_VALUES)),
        "DO_WAVE": draw(st.sampled_from(_BOOL_VALUES)),
        "DO_OCN": draw(st.sampled_from(_BOOL_VALUES)),
        "DO_ICE": draw(st.sampled_from(_BOOL_VALUES)),
        "DO_AERO": draw(st.sampled_from(_BOOL_VALUES)),
        "REPLAY_ICS": draw(st.sampled_from(_BOOL_VALUES)),
    }


@st.composite
def _valid_jjob_names(draw, min_size=1, max_size=6):
    """Generate a set of valid JAAAAA names."""
    num = draw(st.integers(min_value=min_size, max_value=max_size))
    names = set()
    for _ in range(num):
        suffix = draw(
            st.text(
                alphabet=st.sampled_from("ABCDEFGHIJKLMNOPQRSTUVWXYZ_"),
                min_size=2,
                max_size=12,
            ).filter(lambda s: not s.startswith("_") and not s.endswith("_"))
        )
        names.add(f"J{suffix}")
    return names


@st.composite
def _workflow_yaml_with_jjobs(draw, jjob_names):
    """Generate a workflow YAML dict referencing the given J-Job names."""
    jjob_list = sorted(jjob_names)
    if not jjob_list:
        return {"families": []}

    num_families = draw(st.integers(min_value=1, max_value=min(3, len(jjob_list))))
    families = []
    for i in range(num_families):
        families.append({"name": f"family_{i}", "tasks": []})

    for idx, jjob in enumerate(jjob_list):
        family_idx = idx % num_families
        families[family_idx]["tasks"].append(
            {"name": f"task_{idx}", "jjob": jjob}
        )

    return {"families": families}


@st.composite
def _config_content_with_deploy_conditionals(draw, deploy_vars):
    """Generate config file content with deploy-time conditionals.

    Produces valid shell content with case/if blocks testing
    deploy-time variables that the ConfigConditioner can evaluate.
    """
    lines = ["#!/bin/bash", "# Auto-generated config for idempotence test"]

    # Add some unconditional variable assignments
    num_assignments = draw(st.integers(min_value=1, max_value=4))
    for i in range(num_assignments):
        varname = draw(st.sampled_from([
            "FHOUT", "FHMAX", "FHOUT_HF", "FHMAX_HF", "DELTIM",
            "layout_x", "layout_y", "WRTTASK_PER_GROUP",
        ]))
        value = draw(st.sampled_from(["3", "6", "12", "24", "120", "384", "450"]))
        lines.append(f"export {varname}={value}")

    # Add a case block on a deploy-time variable
    if draw(st.booleans()):
        var = draw(st.sampled_from(["RUN", "NET", "MACHINE"]))
        actual_value = deploy_vars.get(var, "gfs")
        other_values = [v for v in _RUN_VALUES if v != actual_value][:2]

        lines.append(f'case ${{{var}}} in')
        # Matching branch
        lines.append(f'  {actual_value})')
        lines.append(f'    export MATCHED_BRANCH="yes"')
        lines.append(f'    ;;')
        # Non-matching branch(es)
        for ov in other_values:
            lines.append(f'  {ov})')
            lines.append(f'    export MATCHED_BRANCH="no"')
            lines.append(f'    ;;')
        lines.append(f'  *)')
        lines.append(f'    export MATCHED_BRANCH="default"')
        lines.append(f'    ;;')
        lines.append('esac')

    # Add an if block on a deploy-time variable
    if draw(st.booleans()):
        var = draw(st.sampled_from(["DO_WAVE", "DO_OCN", "DO_ICE", "DO_AERO"]))
        actual_value = deploy_vars.get(var, "NO")
        lines.append(f'if [[ "${{{var}}}" == "YES" ]]; then')
        lines.append(f'  export {var}_ACTIVE=1')
        lines.append('else')
        lines.append(f'  export {var}_ACTIVE=0')
        lines.append('fi')

    # Add a runtime conditional (should be preserved unchanged)
    if draw(st.booleans()):
        lines.append('if [[ "${PDY}" != "" ]]; then')
        lines.append('  export RUNTIME_SET="yes"')
        lines.append('fi')

    lines.append("")  # trailing newline
    return "\n".join(lines)


# ---------------------------------------------------------------------------
# Filesystem Setup Helpers
# ---------------------------------------------------------------------------


def _create_dev_structure(tmp_path: Path, jjob_names: set[str]) -> Path:
    """Create minimal dev/ directory with the given J-Job files."""
    jobs_dir = tmp_path / "jobs"
    jobs_dir.mkdir(parents=True, exist_ok=True)
    scripts_dir = tmp_path / "scripts"
    scripts_dir.mkdir(parents=True, exist_ok=True)
    ush_dir = tmp_path / "ush"
    ush_dir.mkdir(parents=True, exist_ok=True)
    config_dir = tmp_path / "parm" / "config" / "gfs"
    config_dir.mkdir(parents=True, exist_ok=True)

    # Create J-Job files (minimal content)
    for jjob in jjob_names:
        (jobs_dir / jjob).write_text(f"#!/bin/bash\necho {jjob}\n")

    # Create unconditional config files
    for cfg in _UNCONDITIONAL_CONFIGS:
        (config_dir / cfg).write_text(f"# {cfg}\n")

    # Create base resources config
    (config_dir / "config.resources").write_text("# resources\n")

    return tmp_path


# ---------------------------------------------------------------------------
# Property 10a: DAG Filter Determinism
# ---------------------------------------------------------------------------


@given(data=st.data())
@settings(
    max_examples=50,
    deadline=None,
    suppress_health_check=[HealthCheck.too_slow],
)
def test_dag_filter_determinism(data):
    """Property 10a: DAG Filter produces deterministic results.

    Given the same workflow YAML and filesystem structure,
    compute_reachability() returns identical sets, warnings, and
    statistics every time it is called.

    **Validates: Requirements 12.1, 12.2**
    """
    # Generate random valid J-Job names
    jjob_names = data.draw(_valid_jjob_names(min_size=1, max_size=6))

    # Create workflow YAML referencing all these J-Jobs
    workflow_yaml = data.draw(_workflow_yaml_with_jjobs(jjob_names))

    platform = data.draw(st.sampled_from(["HERA", "WCOSS2", "ORION"]))

    # Create filesystem with all referenced J-Jobs present
    with tempfile.TemporaryDirectory() as tmpdir:
        dev_root = _create_dev_structure(Path(tmpdir), jjob_names)

        # Run DAGFilter twice with identical inputs
        dag1 = DAGFilter(dev_root, workflow_yaml, platform)
        result1 = dag1.compute_reachability()

        dag2 = DAGFilter(dev_root, workflow_yaml, platform)
        result2 = dag2.compute_reachability()

    # Property: results MUST be identical between runs
    assert result1.jjobs == result2.jjobs, (
        f"DAG Filter jjobs differ between runs.\n"
        f"Run 1: {result1.jjobs}\n"
        f"Run 2: {result2.jjobs}"
    )
    assert result1.ex_scripts == result2.ex_scripts, (
        f"DAG Filter ex_scripts differ between runs.\n"
        f"Run 1: {result1.ex_scripts}\n"
        f"Run 2: {result2.ex_scripts}"
    )
    assert result1.ush_scripts == result2.ush_scripts, (
        f"DAG Filter ush_scripts differ between runs.\n"
        f"Run 1: {result1.ush_scripts}\n"
        f"Run 2: {result2.ush_scripts}"
    )
    assert result1.config_files == result2.config_files, (
        f"DAG Filter config_files differ between runs.\n"
        f"Run 1: {result1.config_files}\n"
        f"Run 2: {result2.config_files}"
    )

    # Stronger check: warnings must be in the same order
    assert result1.warnings == result2.warnings, (
        f"DAG Filter warnings differ between runs.\n"
        f"Run 1: {result1.warnings}\n"
        f"Run 2: {result2.warnings}"
    )

    # Statistics fields must also match
    assert result1.total_available_jjobs == result2.total_available_jjobs
    assert result1.total_available_ex_scripts == result2.total_available_ex_scripts
    assert result1.total_available_ush_scripts == result2.total_available_ush_scripts
    assert result1.total_available_configs == result2.total_available_configs


# ---------------------------------------------------------------------------
# Property 10b: Config Conditioner Determinism
# ---------------------------------------------------------------------------


@given(data=st.data())
@settings(
    max_examples=50,
    deadline=None,
    suppress_health_check=[HealthCheck.too_slow],
)
def test_config_conditioner_determinism(data):
    """Property 10b: Config Conditioner produces deterministic output.

    Given the same input content and deploy-time variables,
    condition_file() returns byte-identical output every time.

    **Validates: Requirements 12.3**
    """
    # Generate random deploy-time variable set
    deploy_vars = data.draw(_deploy_time_vars())

    # Generate config content with deploy-time conditionals
    content = data.draw(_config_content_with_deploy_conditionals(deploy_vars))

    # Run conditioner twice with identical inputs
    conditioner1 = ConfigConditioner(deploy_vars)
    result1 = conditioner1.condition_file(content)

    conditioner2 = ConfigConditioner(deploy_vars)
    result2 = conditioner2.condition_file(content)

    # Property: output MUST be byte-identical between runs
    assert result1.output == result2.output, (
        f"Config Conditioner output differs between runs.\n"
        f"Input content:\n{content}\n\n"
        f"Deploy vars: {deploy_vars}\n\n"
        f"Run 1 output:\n{result1.output}\n\n"
        f"Run 2 output:\n{result2.output}"
    )

    # Statistics must also match
    assert result1.eliminated_branches == result2.eliminated_branches, (
        f"Eliminated branches differ: {result1.eliminated_branches} vs "
        f"{result2.eliminated_branches}"
    )
    assert result1.preserved_conditionals == result2.preserved_conditionals, (
        f"Preserved conditionals differ: {result1.preserved_conditionals} vs "
        f"{result2.preserved_conditionals}"
    )


# ---------------------------------------------------------------------------
# Property 10c: Combined Determinism (N repetitions)
# ---------------------------------------------------------------------------


@given(data=st.data())
@settings(
    max_examples=50,
    deadline=None,
    suppress_health_check=[HealthCheck.too_slow],
)
def test_combined_determinism_n_repetitions(data):
    """Property 10c: Running the same operation N times yields identical results.

    Runs both DAGFilter and ConfigConditioner multiple times (3 iterations)
    and verifies all outputs are identical. This is a stronger test than
    pairwise comparison as it catches any state leakage between runs.

    **Validates: Requirements 12.1, 12.2, 12.3, 12.4**
    """
    N = 3  # Number of repetitions

    # --- Setup: generate inputs ---
    jjob_names = data.draw(_valid_jjob_names(min_size=1, max_size=4))
    workflow_yaml = data.draw(_workflow_yaml_with_jjobs(jjob_names))
    platform = data.draw(st.sampled_from(["HERA", "WCOSS2"]))

    deploy_vars = data.draw(_deploy_time_vars())
    content = data.draw(_config_content_with_deploy_conditionals(deploy_vars))

    with tempfile.TemporaryDirectory() as tmpdir:
        dev_root = _create_dev_structure(Path(tmpdir), jjob_names)

        # --- Run DAGFilter N times ---
        dag_results: list[DAGReachabilitySet] = []
        for _ in range(N):
            dag = DAGFilter(dev_root, workflow_yaml, platform)
            dag_results.append(dag.compute_reachability())

    # All DAG results must be identical to the first
    for i in range(1, N):
        assert dag_results[0].jjobs == dag_results[i].jjobs, (
            f"DAG jjobs differ on iteration {i+1}/{N}"
        )
        assert dag_results[0].ex_scripts == dag_results[i].ex_scripts, (
            f"DAG ex_scripts differ on iteration {i+1}/{N}"
        )
        assert dag_results[0].ush_scripts == dag_results[i].ush_scripts, (
            f"DAG ush_scripts differ on iteration {i+1}/{N}"
        )
        assert dag_results[0].config_files == dag_results[i].config_files, (
            f"DAG config_files differ on iteration {i+1}/{N}"
        )
        assert dag_results[0].warnings == dag_results[i].warnings, (
            f"DAG warnings differ on iteration {i+1}/{N}"
        )

    # --- Run ConfigConditioner N times ---
    cond_results: list[ConditionerResult] = []
    for _ in range(N):
        conditioner = ConfigConditioner(deploy_vars)
        cond_results.append(conditioner.condition_file(content))

    # All conditioner results must be byte-identical to the first
    for i in range(1, N):
        assert cond_results[0].output == cond_results[i].output, (
            f"Config Conditioner output differs on iteration {i+1}/{N}.\n"
            f"First output:\n{cond_results[0].output}\n\n"
            f"Iteration {i+1} output:\n{cond_results[i].output}"
        )
        assert cond_results[0].eliminated_branches == cond_results[i].eliminated_branches
        assert cond_results[0].preserved_conditionals == cond_results[i].preserved_conditionals
