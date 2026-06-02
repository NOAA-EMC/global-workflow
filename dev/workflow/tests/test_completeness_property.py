"""Property-based test: Completeness Verifier (Property 9).

Generates random EXPDIRs with known sets of J-Jobs, ex-scripts, ush scripts,
and config files. J-Job files contain patterns referencing specific ex-scripts
and configs. Ex-script files contain source patterns referencing specific ush
scripts. Files are intentionally omitted to create "gaps", and the verifier
is checked to detect ALL intentionally missing dependencies.

**Validates: Requirements 8.1, 8.2, 8.3**

Traces to: Design Document - Correctness Property 9
  "For any staged EXPDIR with intentional gaps in cross-references,
   the Completeness_Verifier SHALL detect all missing dependencies."
"""

from __future__ import annotations

import os
import sys
import tempfile
from pathlib import Path

from hypothesis import given, settings, HealthCheck, assume
from hypothesis import strategies as st

sys.path.insert(0, os.path.join(os.path.dirname(__file__), ".."))

from deployment.completeness_verifier import CompletenessVerifier, CompletenessResult
from deployment.pipeline import PipelineError


# ---------------------------------------------------------------------------
# Hypothesis Strategies for generating random EXPDIRs
# ---------------------------------------------------------------------------

# Valid ex-script names (must match _EX_SCRIPT_PATTERNS: ex[a-z_]+\.sh)
_EX_SCRIPT_NAMES = st.lists(
    st.from_regex(r"ex[a-z]{3,8}_[a-z]{3,8}\.sh", fullmatch=True),
    min_size=1,
    max_size=6,
    unique=True,
)

# Valid ush script names (must match _USH_SOURCE_PATTERNS: [a-z_][a-z0-9_.]+)
_USH_SCRIPT_NAMES = st.lists(
    st.from_regex(r"[a-z]{3,8}_[a-z]{3,8}\.sh", fullmatch=True),
    min_size=1,
    max_size=6,
    unique=True,
)

# Valid config basenames (used in jjob_header -c patterns)
_CONFIG_BASENAMES = st.lists(
    st.from_regex(r"[a-z]{3,8}", fullmatch=True),
    min_size=1,
    max_size=5,
    unique=True,
)


@st.composite
def _random_expdir_with_gaps(draw):
    """Generate a random EXPDIR structure with intentional dependency gaps.

    Returns a tuple of:
    - jjob_names: list of J-Job names to create
    - ex_script_refs: dict mapping jjob -> list of ex-scripts it references
    - ush_script_refs: dict mapping ex-script -> list of ush scripts it sources
    - config_refs: dict mapping jjob -> list of config basenames it requires
    - missing_ex_scripts: set of ex-scripts intentionally omitted from scripts/
    - missing_ush_scripts: set of ush scripts intentionally omitted from ush/
    - missing_configs: set of config basenames intentionally omitted from parm/config/
    """
    # Generate J-Job names (uppercase, J-prefixed)
    num_jjobs = draw(st.integers(min_value=1, max_value=4))
    jjob_names = [f"JGLOBAL_TASK_{i}" for i in range(num_jjobs)]

    # Generate ex-script names
    ex_scripts = draw(_EX_SCRIPT_NAMES)

    # Generate ush script names
    ush_scripts = draw(_USH_SCRIPT_NAMES)

    # Generate config basenames
    config_bases = draw(_CONFIG_BASENAMES)

    # Assign ex-script references to J-Jobs (each jjob refs 1-2 ex-scripts)
    ex_script_refs: dict[str, list[str]] = {}
    for jjob in jjob_names:
        num_refs = draw(st.integers(min_value=1, max_value=min(2, len(ex_scripts))))
        indices = draw(
            st.lists(
                st.integers(min_value=0, max_value=len(ex_scripts) - 1),
                min_size=num_refs,
                max_size=num_refs,
                unique=True,
            )
        )
        ex_script_refs[jjob] = [ex_scripts[i] for i in indices]

    # Assign ush script references to ex-scripts (each ex-script sources 1-3 ush)
    ush_script_refs: dict[str, list[str]] = {}
    for ex in ex_scripts:
        num_refs = draw(st.integers(min_value=1, max_value=min(3, len(ush_scripts))))
        indices = draw(
            st.lists(
                st.integers(min_value=0, max_value=len(ush_scripts) - 1),
                min_size=num_refs,
                max_size=num_refs,
                unique=True,
            )
        )
        ush_script_refs[ex] = [ush_scripts[i] for i in indices]

    # Assign config references to J-Jobs (each jjob refs 1-3 configs)
    config_refs: dict[str, list[str]] = {}
    for jjob in jjob_names:
        num_refs = draw(st.integers(min_value=1, max_value=min(3, len(config_bases))))
        indices = draw(
            st.lists(
                st.integers(min_value=0, max_value=len(config_bases) - 1),
                min_size=num_refs,
                max_size=num_refs,
                unique=True,
            )
        )
        config_refs[jjob] = [config_bases[i] for i in indices]

    # Determine which dependencies to intentionally omit (create gaps)
    # Strategy: choose a mode - either gaps exist or all deps are present
    has_gaps = draw(st.booleans())

    missing_ex: set[str] = set()
    missing_ush: set[str] = set()
    missing_cfg: set[str] = set()

    if has_gaps:
        # Collect all referenced ex-scripts, ush scripts, configs
        all_referenced_ex = set()
        for refs in ex_script_refs.values():
            all_referenced_ex.update(refs)

        all_referenced_ush = set()
        for refs in ush_script_refs.values():
            all_referenced_ush.update(refs)

        all_referenced_cfg = set()
        for refs in config_refs.values():
            all_referenced_cfg.update(refs)

        # Pick some to omit (at least 1 gap must exist)
        if all_referenced_ex:
            omit_ex = draw(
                st.lists(
                    st.sampled_from(sorted(all_referenced_ex)),
                    min_size=0,
                    max_size=min(2, len(all_referenced_ex)),
                    unique=True,
                )
            )
            missing_ex = set(omit_ex)

        if all_referenced_ush:
            omit_ush = draw(
                st.lists(
                    st.sampled_from(sorted(all_referenced_ush)),
                    min_size=0,
                    max_size=min(2, len(all_referenced_ush)),
                    unique=True,
                )
            )
            missing_ush = set(omit_ush)

        if all_referenced_cfg:
            omit_cfg = draw(
                st.lists(
                    st.sampled_from(sorted(all_referenced_cfg)),
                    min_size=0,
                    max_size=min(2, len(all_referenced_cfg)),
                    unique=True,
                )
            )
            missing_cfg = set(omit_cfg)

        # Ensure at least one gap exists when has_gaps is True
        assume(missing_ex or missing_ush or missing_cfg)

    return (
        jjob_names,
        ex_script_refs,
        ush_script_refs,
        config_refs,
        missing_ex,
        missing_ush,
        missing_cfg,
    )


def _create_expdir(
    tmp_dir: Path,
    jjob_names: list[str],
    ex_script_refs: dict[str, list[str]],
    ush_script_refs: dict[str, list[str]],
    config_refs: dict[str, list[str]],
    missing_ex: set[str],
    missing_ush: set[str],
    missing_cfg: set[str],
) -> Path:
    """Create a temporary EXPDIR structure with the specified references and gaps.

    Creates:
    - <expdir>/jobs/          J-Job files with ex-script + config references
    - <expdir>/scripts/       Ex-script files (minus missing_ex)
    - <expdir>/ush/           Ush script files (minus missing_ush)
    - <expdir>/parm/config/   Config files (minus missing_cfg)
    """
    expdir = tmp_dir / "expdir"
    jobs_dir = expdir / "jobs"
    scripts_dir = expdir / "scripts"
    ush_dir = expdir / "ush"
    config_dir = expdir / "parm" / "config"

    jobs_dir.mkdir(parents=True)
    scripts_dir.mkdir(parents=True)
    ush_dir.mkdir(parents=True)
    config_dir.mkdir(parents=True)

    # Write J-Job files with ex-script references and config references
    for jjob in jjob_names:
        lines = ["#!/bin/bash\n"]

        # Add jjob_header with config references
        configs_str = " ".join(config_refs.get(jjob, []))
        if configs_str:
            lines.append(
                f'source "${{HOMEglobal}}/ush/jjob_header.sh" -e "task" -c "{configs_str}"\n'
            )

        # Add ex-script references using the assignment pattern
        for ex_script in ex_script_refs.get(jjob, []):
            lines.append(
                f': "${{TASKSH:=${{SCRglobal}}/{ex_script}}}"\n'
            )

        lines.append('"${TASKSH}" && true\n')
        (jobs_dir / jjob).write_text("".join(lines))

    # Write ex-script files (except those intentionally missing)
    all_referenced_ex = set()
    for refs in ex_script_refs.values():
        all_referenced_ex.update(refs)

    for ex_script in all_referenced_ex:
        if ex_script in missing_ex:
            continue
        lines = ["#!/bin/bash\n"]
        for ush_script in ush_script_refs.get(ex_script, []):
            lines.append(f'source "${{USHglobal}}/{ush_script}"\n')
        (scripts_dir / ex_script).write_text("".join(lines))

    # Write ush script files (except those intentionally missing)
    all_referenced_ush = set()
    for refs in ush_script_refs.values():
        all_referenced_ush.update(refs)

    for ush_script in all_referenced_ush:
        if ush_script in missing_ush:
            continue
        (ush_dir / ush_script).write_text("#!/bin/bash\n# ush utility\n")

    # Write config files (except those intentionally missing)
    all_referenced_cfg = set()
    for refs in config_refs.values():
        all_referenced_cfg.update(refs)

    for cfg_base in all_referenced_cfg:
        if cfg_base in missing_cfg:
            continue
        # Write both config.<base>.j2 variant
        (config_dir / f"config.{cfg_base}.j2").write_text(
            f"#!/bin/bash\n# config for {cfg_base}\n"
        )

    return expdir


def _compute_expected_missing(
    jjob_names: list[str],
    ex_script_refs: dict[str, list[str]],
    ush_script_refs: dict[str, list[str]],
    config_refs: dict[str, list[str]],
    missing_ex: set[str],
    missing_ush: set[str],
    missing_cfg: set[str],
) -> tuple[set[tuple[str, str]], set[tuple[str, str]], set[tuple[str, str]]]:
    """Independently compute what the verifier should detect as missing.

    Returns:
        (expected_missing_ex, expected_missing_ush, expected_missing_cfg)
        Each is a set of (referencing_file, missing_file) tuples.
    """
    expected_missing_ex: set[tuple[str, str]] = set()
    expected_missing_ush: set[tuple[str, str]] = set()
    expected_missing_cfg: set[tuple[str, str]] = set()

    # Missing ex-scripts: jjobs reference ex-scripts that don't exist in scripts/
    for jjob in jjob_names:
        for ex in ex_script_refs.get(jjob, []):
            if ex in missing_ex:
                expected_missing_ex.add((jjob, ex))

    # Missing ush scripts: ex-scripts (that ARE present) reference ush scripts
    # that don't exist in ush/
    for ex in set().union(*ex_script_refs.values()) if ex_script_refs else set():
        if ex in missing_ex:
            # If the ex-script itself is missing, the verifier won't parse it
            continue
        for ush in ush_script_refs.get(ex, []):
            if ush in missing_ush:
                expected_missing_ush.add((ex, ush))

    # Missing configs: jjobs reference config basenames that don't exist
    for jjob in jjob_names:
        for cfg_base in config_refs.get(jjob, []):
            if cfg_base in missing_cfg:
                expected_missing_cfg.add((jjob, f"config.{cfg_base}"))

    return expected_missing_ex, expected_missing_ush, expected_missing_cfg


# ---------------------------------------------------------------------------
# Property Test: Completeness Verifier Detects All Missing Dependencies
# ---------------------------------------------------------------------------


@given(expdir_spec=_random_expdir_with_gaps())
@settings(
    max_examples=100,
    deadline=None,
    suppress_health_check=[HealthCheck.too_slow],
)
def test_completeness_verifier_detects_all_gaps(expdir_spec):
    """Property 9: Completeness Verifier detects ALL missing dependencies.

    **Validates: Requirements 8.1, 8.2, 8.3**

    For any random EXPDIR with intentional gaps:
    - If gaps exist, verify() must raise PipelineError
    - Every intentionally omitted ex-script must appear in the error
    - Every intentionally omitted ush script must appear in the error
    - Every intentionally omitted config must appear in the error
    """
    (
        jjob_names,
        ex_script_refs,
        ush_script_refs,
        config_refs,
        missing_ex,
        missing_ush,
        missing_cfg,
    ) = expdir_spec

    # Only test cases where gaps exist
    assume(missing_ex or missing_ush or missing_cfg)

    with tempfile.TemporaryDirectory() as tmp_str:
        tmp_dir = Path(tmp_str)
        expdir = _create_expdir(
            tmp_dir,
            jjob_names,
            ex_script_refs,
            ush_script_refs,
            config_refs,
            missing_ex,
            missing_ush,
            missing_cfg,
        )

        # Compute expected missing deps independently
        expected_missing_ex, expected_missing_ush, expected_missing_cfg = (
            _compute_expected_missing(
                jjob_names,
                ex_script_refs,
                ush_script_refs,
                config_refs,
                missing_ex,
                missing_ush,
                missing_cfg,
            )
        )

        # The verifier should raise PipelineError when gaps exist
        verifier = CompletenessVerifier(expdir)
        try:
            result = verifier.verify()
            # If no exception was raised but we expected gaps, something is wrong
            # (This can happen if the regex patterns didn't match our generated content)
            # Check if at least our expected gaps were truly detectable
            if expected_missing_ex or expected_missing_ush or expected_missing_cfg:
                assert False, (
                    f"verify() did not raise PipelineError despite gaps.\n"
                    f"  Expected missing ex-scripts: {expected_missing_ex}\n"
                    f"  Expected missing ush scripts: {expected_missing_ush}\n"
                    f"  Expected missing configs: {expected_missing_cfg}\n"
                    f"  Result: {result}"
                )
        except PipelineError:
            # Expected — gaps were detected.
            # Now verify completeness: run checks individually to get the result
            missing_ex_found = verifier._check_jjob_ex_script_refs()
            missing_ush_found = verifier._check_ex_script_ush_refs()
            missing_cfg_found = verifier._check_config_refs()

            # Convert to sets of tuples for comparison
            actual_missing_ex = set(missing_ex_found)
            actual_missing_ush = set(missing_ush_found)
            actual_missing_cfg = set(missing_cfg_found)

            # Property: ALL expected missing deps must be detected
            for jjob, ex in expected_missing_ex:
                assert (jjob, ex) in actual_missing_ex, (
                    f"Verifier missed ex-script gap: J-Job '{jjob}' -> '{ex}'\n"
                    f"  Actual detected: {actual_missing_ex}\n"
                    f"  Expected: {expected_missing_ex}"
                )

            for ex, ush in expected_missing_ush:
                assert (ex, ush) in actual_missing_ush, (
                    f"Verifier missed ush-script gap: ex-script '{ex}' -> '{ush}'\n"
                    f"  Actual detected: {actual_missing_ush}\n"
                    f"  Expected: {expected_missing_ush}"
                )

            for jjob, cfg in expected_missing_cfg:
                assert (jjob, cfg) in actual_missing_cfg, (
                    f"Verifier missed config gap: J-Job '{jjob}' -> '{cfg}'\n"
                    f"  Actual detected: {actual_missing_cfg}\n"
                    f"  Expected: {expected_missing_cfg}"
                )


@given(expdir_spec=_random_expdir_with_gaps())
@settings(
    max_examples=100,
    deadline=None,
    suppress_health_check=[HealthCheck.too_slow],
)
def test_completeness_verifier_passes_when_complete(expdir_spec):
    """Property 9 (complement): When all deps are present, verify() passes.

    **Validates: Requirements 8.1, 8.2, 8.3**

    For any random EXPDIR with NO intentional gaps:
    - verify() must NOT raise PipelineError
    - The returned CompletenessResult must have passed=True
    - All missing_* lists must be empty
    """
    (
        jjob_names,
        ex_script_refs,
        ush_script_refs,
        config_refs,
        missing_ex,
        missing_ush,
        missing_cfg,
    ) = expdir_spec

    # Only test cases where NO gaps exist (all deps present)
    assume(not missing_ex and not missing_ush and not missing_cfg)

    with tempfile.TemporaryDirectory() as tmp_str:
        tmp_dir = Path(tmp_str)
        expdir = _create_expdir(
            tmp_dir,
            jjob_names,
            ex_script_refs,
            ush_script_refs,
            config_refs,
            missing_ex,
            missing_ush,
            missing_cfg,
        )

        verifier = CompletenessVerifier(expdir)
        result = verifier.verify()

        # Property: complete EXPDIR must pass verification
        assert result.passed is True, (
            f"verify() failed on a complete EXPDIR.\n"
            f"  missing_ex_scripts: {result.missing_ex_scripts}\n"
            f"  missing_ush_scripts: {result.missing_ush_scripts}\n"
            f"  missing_configs: {result.missing_configs}"
        )
        assert result.missing_ex_scripts == [], (
            f"Expected no missing ex-scripts but got: {result.missing_ex_scripts}"
        )
        assert result.missing_ush_scripts == [], (
            f"Expected no missing ush-scripts but got: {result.missing_ush_scripts}"
        )
        assert result.missing_configs == [], (
            f"Expected no missing configs but got: {result.missing_configs}"
        )
