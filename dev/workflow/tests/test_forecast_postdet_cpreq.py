"""Shell-level verification tests for the forecast runtime sealed-copy path.

Asserts that ``ush/forecast_postdet.sh`` has been remediated to consume
pre-rendered coupled-model configs from the sealed EXPDIR via ``cpreq``
instead of generating them at run time by sourcing the
``parsing_namelists_{WW3,MOM6,CICE,GOCART}.sh`` scripts.

**Validates: Requirements 7.1, 7.2, 7.3, 7.4, 7.5**

Traces to parent: EE2 essential-file pattern (cpreq + FATAL ERROR pre-flight).
"""

from __future__ import annotations

import re
from pathlib import Path

import pytest

# Repo root: tests -> workflow -> dev -> <repo root>
REPO_ROOT = Path(__file__).resolve().parents[3]
FORECAST_POSTDET = REPO_ROOT / "ush" / "forecast_postdet.sh"

# Components whose runtime namelist generation must be eliminated.
COMPONENTS = ("WW3", "MOM6", "CICE", "GOCART")

# All pre-rendered EXPDIR sources expected to appear in the script (Req 7.1).
# These are the files copied via cpreq from ${EXPDIR}/parm/ufs/<component>/ to ${DATA}/
EXPECTED_CPREQ_SOURCES = (
    '${EXPDIR}/parm/ufs/fv3/input.nml',
    '${EXPDIR}/parm/ufs/fv3/model_configure',
    '${EXPDIR}/parm/ufs/fv3/diag_table',
    '${EXPDIR}/parm/ufs/fv3/field_table',
    '${EXPDIR}/parm/ufs/wave/ww3_shel.nml',
    '${EXPDIR}/parm/ufs/ocean/MOM_input',
    '${EXPDIR}/parm/ufs/ocean/MOM6_data_table',
    '${EXPDIR}/parm/ufs/ice/ice_in',
    '${EXPDIR}/parm/ufs/gocart',
)

# Sources copied via a direct `cpreq "<source>" "<dest>"` (not via loop).
DIRECT_CPREQ_SOURCES = (
    '${EXPDIR}/parm/ufs/fv3/input.nml',
    '${EXPDIR}/parm/ufs/fv3/model_configure',
    '${EXPDIR}/parm/ufs/fv3/diag_table',
    '${EXPDIR}/parm/ufs/fv3/field_table',
    '${EXPDIR}/parm/ufs/wave/ww3_shel.nml',
    '${EXPDIR}/parm/ufs/ocean/MOM_input',
    '${EXPDIR}/parm/ufs/ocean/MOM6_data_table',
    '${EXPDIR}/parm/ufs/ice/ice_in',
)


@pytest.fixture(scope="module")
def script_text() -> str:
    assert FORECAST_POSTDET.is_file(), f"Missing script: {FORECAST_POSTDET}"
    return FORECAST_POSTDET.read_text(encoding="utf-8")


def _non_comment_lines(text: str) -> list[str]:
    """Return script lines with shell comments stripped.

    A line is treated as a comment when its first non-whitespace character
    is ``#``. This avoids false positives from the explanatory comments that
    reference the now-removed parsing scripts by name.
    """
    lines = []
    for line in text.splitlines():
        if line.lstrip().startswith("#"):
            continue
        lines.append(line)
    return lines


# ============================================================================
# Requirement 7.1: SHALL use cpreq to copy pre-rendered files from
# ${EXPDIR}/parm/ufs/<component>/ to ${DATA}/
# ============================================================================


@pytest.mark.parametrize("source_path", EXPECTED_CPREQ_SOURCES)
def test_cpreq_from_expdir_present(script_text: str, source_path: str):
    """Req 7.1: each component is staged via cpreq from the sealed EXPDIR.

    Verifies that the script references the pre-rendered source path from
    ${EXPDIR}/parm/ufs/<component>/ in a non-comment line.
    """
    non_comment = "\n".join(_non_comment_lines(script_text))
    assert source_path in non_comment, (
        f"Expected a cpreq staging the pre-rendered config from "
        f"'{source_path}' but it was not found in forecast_postdet.sh."
    )


@pytest.mark.parametrize("source_path", DIRECT_CPREQ_SOURCES)
def test_cpreq_copies_to_data_directory(script_text: str, source_path: str):
    """Req 7.1/7.5: cpreq destinations reference ${DATA}/ directory.

    The staged model inputs must be copied to ${DATA}/ (or ${DATA}/INPUT/),
    using the variable established by the J-Job.
    """
    # Find the cpreq line for this source and verify destination uses ${DATA}
    cpreq_pattern = re.compile(
        r'cpreq\s+"' + re.escape(source_path) + r'"\s+"(\$\{DATA\}[^"]*)"'
    )
    match = cpreq_pattern.search(script_text)
    assert match is not None, (
        f"Expected `cpreq \"{source_path}\" \"${{DATA}}/...\"` but no matching "
        f"cpreq line was found with a ${{DATA}} destination."
    )


def test_gocart_staged_via_cpreq_loop(script_text: str):
    """Req 7.1/7.4: GOCART .rc + ExtData files are staged via a cpreq loop.

    The GOCART block iterates over `${EXPDIR}/parm/ufs/gocart/*.rc` and
    ExtData, copying each with cpreq to ${DATA}/.
    """
    non_comment = "\n".join(_non_comment_lines(script_text))
    # The loop iterates over the gocart .rc files (and ExtData).
    assert re.search(
        r'for\s+\w+\s+in\s+"\$\{EXPDIR\}/parm/ufs/gocart"/\*\.rc', non_comment
    ), "Expected a `for ... in \"${EXPDIR}/parm/ufs/gocart\"/*.rc` loop for GOCART."
    assert 'ExtData' in non_comment, (
        "Expected the GOCART staging loop to include the ExtData file."
    )
    # The loop body copies each resolved file with cpreq.
    assert re.search(r'cpreq\s+"\$\{rc_file\}"', non_comment), (
        "Expected `cpreq \"${rc_file}\" ...` inside the GOCART staging loop."
    )


# ============================================================================
# Requirement 7.2: SHALL NOT invoke any parsing_namelists_*.sh scripts or
# runtime template rendering for model inputs
# ============================================================================


def test_no_runtime_parsing_namelists_source(script_text: str):
    """Req 7.2: no `source ... parsing_namelists_{WW3,MOM6,CICE,GOCART}.sh`.

    The script must not source any of the superseded runtime parsing scripts.
    Comment lines that merely mention the script names for documentation
    are not violations.
    """
    pattern = re.compile(
        r"source\s+.*parsing_namelists_(?:WW3|MOM6|CICE|GOCART)\.sh"
    )
    violations = [
        line.strip()
        for line in _non_comment_lines(script_text)
        if pattern.search(line)
    ]
    assert violations == [], (
        "forecast_postdet.sh still sources runtime parsing_namelists scripts:\n"
        + "\n".join(f"  {v}" for v in violations)
    )


def test_no_namelist_function_invocations(script_text: str):
    """Req 7.2: the `*_namelists` generator functions are no longer called.

    Runtime namelist generation functions (WW3_namelists, MOM6_namelists,
    CICE_namelists, GOCART_namelists) must not be invoked.
    """
    non_comment = "\n".join(_non_comment_lines(script_text))
    for component in COMPONENTS:
        invocation = f"{component}_namelists"
        assert invocation not in non_comment, (
            f"forecast_postdet.sh still invokes {invocation}; runtime "
            f"namelist generation for {component} must be removed."
        )


def test_no_parsing_namelists_fv3_source(script_text: str):
    """Req 7.2: no `source ... parsing_namelists_FV3.sh` for FV3 model inputs.

    FV3 namelists are pre-rendered; the legacy FV3 parsing script must not
    be sourced.
    """
    pattern = re.compile(
        r"source\s+.*parsing_namelists_FV3\.sh"
    )
    violations = [
        line.strip()
        for line in _non_comment_lines(script_text)
        if pattern.search(line)
    ]
    assert violations == [], (
        "forecast_postdet.sh still sources parsing_namelists_FV3.sh:\n"
        + "\n".join(f"  {v}" for v in violations)
    )


# ============================================================================
# Requirement 7.3: WHEN a pre-rendered Model_Input file is missing, SHALL
# emit a FATAL ERROR with a descriptive message naming the missing file path
# ============================================================================


@pytest.mark.parametrize(
    "expected_path",
    (
        '${EXPDIR}/parm/ufs/fv3/input.nml',
        '${EXPDIR}/parm/ufs/fv3/model_configure',
        '${EXPDIR}/parm/ufs/fv3/diag_table',
        '${EXPDIR}/parm/ufs/fv3/field_table',
        '${EXPDIR}/parm/ufs/wave/ww3_shel.nml',
        '${EXPDIR}/parm/ufs/ocean/MOM_input',
        '${EXPDIR}/parm/ufs/ocean/MOM6_data_table',
        '${EXPDIR}/parm/ufs/ice/ice_in',
        '${EXPDIR}/parm/ufs/gocart',
    ),
)
def test_preflight_existence_check_present(script_text: str, expected_path: str):
    """Req 7.3: a pre-flight existence check guards each pre-rendered file.

    Each pre-rendered model input must be guarded with `[[ ! -f ... ]]` or
    `[[ ! -d ... ]]` before the cpreq copy to ensure a descriptive error is
    emitted if missing.
    """
    guard_pattern = re.compile(
        r"\[\[\s*!\s*-[fd]\s+\"" + re.escape(expected_path) + r"[^\"]*\"\s*\]\]"
    )
    assert guard_pattern.search(script_text), (
        f"Missing existence pre-flight check for '{expected_path}' in "
        f"forecast_postdet.sh."
    )


@pytest.mark.parametrize(
    "expected_path",
    (
        '${EXPDIR}/parm/ufs/fv3/input.nml',
        '${EXPDIR}/parm/ufs/fv3/model_configure',
        '${EXPDIR}/parm/ufs/fv3/diag_table',
        '${EXPDIR}/parm/ufs/fv3/field_table',
        '${EXPDIR}/parm/ufs/wave/ww3_shel.nml',
        '${EXPDIR}/parm/ufs/ocean/MOM_input',
        '${EXPDIR}/parm/ufs/ocean/MOM6_data_table',
        '${EXPDIR}/parm/ufs/ice/ice_in',
        '${EXPDIR}/parm/ufs/gocart',
    ),
)
def test_fatal_error_names_missing_file_path(script_text: str, expected_path: str):
    """Req 7.3: FATAL ERROR message names the missing file path.

    When a pre-rendered file is missing, the FATAL ERROR message must include
    the expected file path so operators can identify what is missing.
    """
    # Extract the basename from the expected path for searching in error messages
    # For directory paths like ${EXPDIR}/parm/ufs/gocart, use the last segment
    path_basename = expected_path.rsplit("/", 1)[-1]

    # Find FATAL ERROR lines that reference this specific file/directory
    fatal_pattern = re.compile(
        r'echo\s+"FATAL ERROR:.*' + re.escape(path_basename) + r'.*"'
    )
    assert fatal_pattern.search(script_text), (
        f"Expected a 'FATAL ERROR:' message referencing '{path_basename}' "
        f"but none was found. The error message must name the missing file path."
    )


def test_fatal_error_messages_cover_all_components(script_text: str):
    """Req 7.3: all component areas have FATAL ERROR messages.

    At minimum, the four remediated components (WW3, MOM6, CICE, GOCART)
    plus FV3 must each have a FATAL ERROR guard message referencing the
    pre-rendered file.
    """
    fatal_lines = [
        line.strip()
        for line in script_text.splitlines()
        if "FATAL ERROR:" in line and "Pre-rendered" in line
    ]
    joined = "\n".join(fatal_lines)
    required_tokens = (
        "input.nml",
        "model_configure",
        "diag_table",
        "field_table",
        "ww3_shel.nml",
        "MOM_input",
        "MOM6_data_table",
        "ice_in",
        "GOCART",
    )
    for token in required_tokens:
        assert token in joined, (
            f"Expected a 'FATAL ERROR: Pre-rendered ...' message referencing "
            f"'{token}' but none was found."
        )


# ============================================================================
# Requirement 7.4: SHALL use cpreq (not cp or cpfs) for essential model input
# files, per EE2 standards requiring abort-on-failure
# ============================================================================


@pytest.mark.parametrize("source_path", DIRECT_CPREQ_SOURCES)
def test_uses_cpreq_not_cp_or_cpfs(script_text: str, source_path: str):
    """Req 7.4: staging uses `cpreq` (not `cp` or `cpfs`) for model inputs.

    Per EE2 standards, essential model input files MUST use cpreq which
    aborts on copy failure. Plain `cp` or `cpfs` would not provide the
    required abort-on-failure semantics.
    """
    # Verify cpreq is used
    cpreq_pattern = re.compile(
        r'cpreq\s+"' + re.escape(source_path)
    )
    assert cpreq_pattern.search(script_text), (
        f"Expected `cpreq \"{source_path}\"...` but cpreq usage not found; "
        f"staging must use cpreq per EE2 essential-file pattern."
    )

    # Verify plain cp or cpfs are NOT used for this specific source
    cp_pattern = re.compile(
        r'(?:^|\s)cp\s+"' + re.escape(source_path)
    )
    cpfs_pattern = re.compile(
        r'cpfs\s+"' + re.escape(source_path)
    )
    non_comment = "\n".join(_non_comment_lines(script_text))
    assert not cp_pattern.search(non_comment), (
        f"Found `cp \"{source_path}\"...` — must use cpreq, not cp, "
        f"for essential model input files (EE2 abort-on-failure)."
    )
    assert not cpfs_pattern.search(non_comment), (
        f"Found `cpfs \"{source_path}\"...` — must use cpreq, not cpfs, "
        f"for essential model input files from EXPDIR."
    )


# ============================================================================
# Requirement 7.5: SHALL stage model inputs from the EXPDIR using variables
# established in the J-Job (${EXPDIR}, ${DATA}) and SHALL NOT alter them
# ============================================================================


def test_uses_expdir_variable_for_source(script_text: str):
    """Req 7.5: model input sources use ${EXPDIR} variable from J-Job.

    The pre-rendered files are sourced from ${EXPDIR}/parm/ufs/<component>/
    using the EXPDIR variable set by the J-Job, not a hard-coded path.
    """
    non_comment = "\n".join(_non_comment_lines(script_text))
    # Find all cpreq lines that copy from parm/ufs/ (our pre-rendered model inputs)
    cpreq_model_input_pattern = re.compile(
        r'cpreq\s+"([^"]+parm/ufs/[^"]+)"'
    )
    matches = cpreq_model_input_pattern.findall(non_comment)
    assert len(matches) > 0, (
        "Expected cpreq calls with parm/ufs/ sources but found none."
    )
    for source in matches:
        assert source.startswith("${EXPDIR}") or source.startswith("$EXPDIR"), (
            f"Model input source '{source}' does not use ${{EXPDIR}} variable. "
            f"All pre-rendered inputs must be sourced via ${{EXPDIR}} from J-Job."
        )


def test_uses_data_variable_for_destination(script_text: str):
    """Req 7.5: model input destinations use ${DATA} variable from J-Job.

    The cpreq destinations for pre-rendered model inputs must use ${DATA}
    (or ${DATA}/INPUT/) — the working directory variable established by
    the J-Job.
    """
    non_comment = "\n".join(_non_comment_lines(script_text))
    # Find cpreq lines copying from EXPDIR/parm/ufs/ and check destination
    cpreq_full_pattern = re.compile(
        r'cpreq\s+"(\$\{EXPDIR\}/parm/ufs/[^"]+)"\s+"([^"]+)"'
    )
    matches = cpreq_full_pattern.findall(non_comment)
    assert len(matches) > 0, (
        "Expected cpreq calls with ${EXPDIR}/parm/ufs/ sources but found none."
    )
    for source, dest in matches:
        assert "${DATA}" in dest or "$DATA" in dest, (
            f"cpreq destination '{dest}' for source '{source}' does not use "
            f"${{DATA}} variable. Model inputs must be staged to ${{DATA}}/."
        )


def test_does_not_reassign_expdir(script_text: str):
    """Req 7.5: the script SHALL NOT alter the ${EXPDIR} variable.

    EXPDIR is established by the J-Job and must not be reassigned or
    modified in the forecast ush script.
    """
    non_comment = _non_comment_lines(script_text)
    reassign_pattern = re.compile(
        r'^\s*(?:export\s+)?EXPDIR\s*='
    )
    violations = [
        line.strip()
        for line in non_comment
        if reassign_pattern.search(line)
    ]
    assert violations == [], (
        "forecast_postdet.sh reassigns EXPDIR, which must remain as set "
        "by the J-Job:\n" + "\n".join(f"  {v}" for v in violations)
    )


def test_does_not_reassign_data(script_text: str):
    """Req 7.5: the script SHALL NOT alter the ${DATA} variable.

    DATA is established by the J-Job and must not be reassigned in the
    forecast ush script.
    """
    non_comment = _non_comment_lines(script_text)
    reassign_pattern = re.compile(
        r'^\s*(?:export\s+)?DATA\s*='
    )
    violations = [
        line.strip()
        for line in non_comment
        if reassign_pattern.search(line)
    ]
    assert violations == [], (
        "forecast_postdet.sh reassigns DATA, which must remain as set "
        "by the J-Job:\n" + "\n".join(f"  {v}" for v in violations)
    )


# ============================================================================
# EE2 compliance: no set -e for error handling
# ============================================================================


def test_no_set_e_added_for_error_handling(script_text: str):
    """EE2: no `set -e`/`set -eu` introduced solely for error handling.

    Per EE2 Phase 2 SME-corrected patterns, error handling uses
    err_chk/err_exit, not set -e.
    """
    non_comment = _non_comment_lines(script_text)
    set_e_pattern = re.compile(r"^\s*set\s+-(?:e|eu|eux|ex)\b")
    offenders = [line.strip() for line in non_comment if set_e_pattern.search(line)]
    assert offenders == [], (
        "forecast_postdet.sh must not add `set -e`/`set -eu` for error "
        "handling (EE2 Phase 2 SME correction); found:\n"
        + "\n".join(f"  {o}" for o in offenders)
    )
