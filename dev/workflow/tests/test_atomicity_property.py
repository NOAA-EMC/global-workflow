"""Property-based test: Atomicity (Property 5).

Simulate partial failure during the atomic_publish staging phase and verify
that no partial files appear in COMOUT. After a failure during staging or
verification, COMOUT must remain unchanged.

**Validates: Requirements 7.6**

Traces to: Design Document - Correctness Property 5
  "A partial failure during product staging leaves ${COMOUT} unchanged
   for that deliverable set."
"""

from __future__ import annotations

import os
import subprocess
import sys
from pathlib import Path

from hypothesis import given, settings, HealthCheck, assume
from hypothesis import strategies as st

sys.path.insert(0, os.path.join(os.path.dirname(__file__), ".."))

# Path to the atomic_publish.sh script
SCRIPT_PATH = Path(__file__).parents[2] / "ush" / "atomic_publish.sh"


# ---------------------------------------------------------------------------
# Hypothesis Strategies
# ---------------------------------------------------------------------------


@st.composite
def _deliverable_set(draw):
    """Generate a random deliverable set of files to publish.

    Returns a list of (filename, content_bytes) tuples representing
    files that should be atomically published to COMOUT.
    """
    num_files = draw(st.integers(min_value=2, max_value=6))

    extensions = [".grib2", ".nc", ".idx", ".bufr", ".txt", ".bin"]
    files = []
    used_names = set()

    for i in range(num_files):
        ext = draw(st.sampled_from(extensions))
        name = f"product_{i}{ext}"
        if name in used_names:
            continue
        used_names.add(name)

        # Generate non-empty content (1 to 200 bytes)
        content = draw(st.binary(min_size=1, max_size=200))
        files.append((name, content))

    # Ensure at least 2 files for meaningful atomicity test
    if len(files) < 2:
        files.append(("product_extra.grib2", draw(st.binary(min_size=1, max_size=100))))

    return files


@st.composite
def _failure_index(draw, max_files):
    """Generate an index at which to inject a failure during staging.

    The failure occurs after staging some files but before all are staged,
    simulating a partial failure.
    """
    # Fail after staging at least 1 file but before all files are staged
    return draw(st.integers(min_value=1, max_value=max_files - 1))


# ---------------------------------------------------------------------------
# Helper Functions
# ---------------------------------------------------------------------------


def _create_test_env(tmp_path: Path) -> dict:
    """Create a minimal test environment with mock EE2 utilities."""
    comout = tmp_path / "comout"
    comout.mkdir()
    data_dir = tmp_path / "data"
    data_dir.mkdir()

    # Create mock utilities
    mock_utils = tmp_path / "mock_utils.sh"
    mock_utils.write_text(
        '#!/usr/bin/env bash\n'
        'err_exit() { echo "ERR_EXIT: $*" >&2; exit 1; }\n'
        'cpfs() { cp "$1" "$2"; }\n'
        'export -f err_exit cpfs\n'
    )

    env = os.environ.copy()
    env["COMOUT"] = str(comout)
    env["jobid"] = "atomicity_test_job"
    env["SENDDBN"] = "NO"
    env["MOCK_UTILS"] = str(mock_utils)

    return env


def _run_script(tmp_path: Path, script_content: str, env: dict) -> subprocess.CompletedProcess:
    """Run a bash script with the given environment."""
    wrapper = tmp_path / "run_test.sh"
    wrapper.write_text(script_content)
    wrapper.chmod(0o755)

    result = subprocess.run(
        ["bash", str(wrapper)],
        env=env,
        capture_output=True,
        text=True,
        timeout=30,
    )
    return result


# ---------------------------------------------------------------------------
# Property Test: Atomicity (Property 5)
# ---------------------------------------------------------------------------


@given(deliverable_set=_deliverable_set())
@settings(
    max_examples=50,
    deadline=None,
    suppress_health_check=[HealthCheck.too_slow],
)
def test_atomicity_partial_staging_failure_leaves_comout_unchanged(
    deliverable_set, tmp_path_factory
):
    """Property 5: Partial failure during staging leaves COMOUT unchanged.

    **Validates: Requirements 7.6**

    For any random deliverable set of files:
    - Record the initial state of COMOUT (empty)
    - Simulate a failure during staging by making cpfs fail on the Nth file
    - Assert that COMOUT contains none of the deliverable set files
    - The staging directory should not leave partial files in COMOUT
    """
    tmp_path = tmp_path_factory.mktemp("atomicity")

    # Need at least 2 files for a meaningful partial failure
    assume(len(deliverable_set) >= 2)

    env = _create_test_env(tmp_path)
    comout = Path(env["COMOUT"])
    data_dir = tmp_path / "data"

    # Create source files
    source_files = []
    for filename, content in deliverable_set:
        src = data_dir / filename
        src.write_bytes(content)
        source_files.append(str(src))

    # Pick a failure point: cpfs will fail on the Nth file (1-indexed)
    # This means some files get staged but not all
    fail_at = len(deliverable_set) // 2 + 1  # Fail roughly in the middle

    # Create a mock cpfs that fails on the Nth invocation
    mock_utils = tmp_path / "mock_utils.sh"
    mock_utils.write_text(
        '#!/usr/bin/env bash\n'
        'err_exit() { echo "ERR_EXIT: $*" >&2; exit 1; }\n'
        f'CPFS_CALL_COUNT=0\n'
        f'CPFS_FAIL_AT={fail_at}\n'
        'cpfs() {\n'
        '    CPFS_CALL_COUNT=$((CPFS_CALL_COUNT + 1))\n'
        '    if [[ $CPFS_CALL_COUNT -ge $CPFS_FAIL_AT ]]; then\n'
        '        return 1\n'
        '    fi\n'
        '    cp "$1" "$2"\n'
        '}\n'
        'export -f err_exit cpfs\n'
        'export CPFS_CALL_COUNT CPFS_FAIL_AT\n'
    )
    env["MOCK_UTILS"] = str(mock_utils)

    # Record initial COMOUT state
    initial_files = set(f.name for f in comout.iterdir() if f.is_file())

    # Build the script that sources mock utils and runs atomic_publish
    file_args = " ".join(f'"{f}"' for f in source_files)
    script_content = (
        f'#!/usr/bin/env bash\n'
        f'source "{mock_utils}"\n'
        f'source "{SCRIPT_PATH}" {file_args}\n'
    )

    result = _run_script(tmp_path, script_content, env)

    # The script should have failed (non-zero exit)
    assert result.returncode != 0, (
        f"Expected failure during staging but script succeeded.\n"
        f"stdout: {result.stdout}\n"
        f"stderr: {result.stderr}"
    )

    # COMOUT should remain unchanged — no deliverable files at final location
    final_files = set(f.name for f in comout.iterdir() if f.is_file())
    deliverable_names = {name for name, _ in deliverable_set}

    # None of the deliverable set files should be in COMOUT
    leaked_files = final_files & deliverable_names
    assert leaked_files == set(), (
        f"Partial files leaked into COMOUT after staging failure!\n"
        f"Leaked files: {leaked_files}\n"
        f"COMOUT should remain unchanged but found: {final_files}\n"
        f"Initial COMOUT: {initial_files}"
    )

    # COMOUT should be exactly as it was before
    assert final_files == initial_files, (
        f"COMOUT state changed after staging failure!\n"
        f"Before: {initial_files}\n"
        f"After: {final_files}"
    )


@given(deliverable_set=_deliverable_set())
@settings(
    max_examples=50,
    deadline=None,
    suppress_health_check=[HealthCheck.too_slow],
)
def test_atomicity_verification_failure_leaves_comout_unchanged(
    deliverable_set, tmp_path_factory
):
    """Property 5: Verification failure leaves COMOUT unchanged.

    **Validates: Requirements 7.6**

    For any random deliverable set of files:
    - Stage all files successfully (cpfs works)
    - But make one staged file empty (simulating corruption during staging)
    - Assert that COMOUT contains none of the deliverable set files
    - The verification step should catch the empty file and abort
    """
    tmp_path = tmp_path_factory.mktemp("atomicity_verify")

    assume(len(deliverable_set) >= 2)

    env = _create_test_env(tmp_path)
    comout = Path(env["COMOUT"])
    data_dir = tmp_path / "data"

    # Create source files
    source_files = []
    for filename, content in deliverable_set:
        src = data_dir / filename
        src.write_bytes(content)
        source_files.append(str(src))

    # Pick a file to corrupt during staging (make it empty)
    corrupt_index = len(deliverable_set) // 2
    corrupt_filename = deliverable_set[corrupt_index][0]

    # Create a mock cpfs that copies normally but truncates one specific file
    mock_utils = tmp_path / "mock_utils.sh"
    mock_utils.write_text(
        '#!/usr/bin/env bash\n'
        'err_exit() { echo "ERR_EXIT: $*" >&2; exit 1; }\n'
        'cpfs() {\n'
        f'    local basename=$(basename "$2")\n'
        f'    if [[ "$basename" == "{corrupt_filename}" ]]; then\n'
        f'        : > "$2"\n'  # Truncate to empty
        '    else\n'
        '        cp "$1" "$2"\n'
        '    fi\n'
        '}\n'
        'export -f err_exit cpfs\n'
    )
    env["MOCK_UTILS"] = str(mock_utils)

    # Record initial COMOUT state
    initial_files = set(f.name for f in comout.iterdir() if f.is_file())

    # Build the script
    file_args = " ".join(f'"{f}"' for f in source_files)
    script_content = (
        f'#!/usr/bin/env bash\n'
        f'source "{mock_utils}"\n'
        f'source "{SCRIPT_PATH}" {file_args}\n'
    )

    result = _run_script(tmp_path, script_content, env)

    # The script should have failed due to empty file verification
    assert result.returncode != 0, (
        f"Expected failure during verification but script succeeded.\n"
        f"stdout: {result.stdout}\n"
        f"stderr: {result.stderr}"
    )

    # COMOUT should remain unchanged — no deliverable files at final location
    final_files = set(f.name for f in comout.iterdir() if f.is_file())
    deliverable_names = {name for name, _ in deliverable_set}

    # None of the deliverable set files should be in COMOUT
    leaked_files = final_files & deliverable_names
    assert leaked_files == set(), (
        f"Partial files leaked into COMOUT after verification failure!\n"
        f"Leaked files: {leaked_files}\n"
        f"Corrupted file: {corrupt_filename}\n"
        f"COMOUT should remain unchanged but found: {final_files}"
    )

    # COMOUT should be exactly as it was before
    assert final_files == initial_files, (
        f"COMOUT state changed after verification failure!\n"
        f"Before: {initial_files}\n"
        f"After: {final_files}"
    )


@given(deliverable_set=_deliverable_set())
@settings(
    max_examples=50,
    deadline=None,
    suppress_health_check=[HealthCheck.too_slow],
)
def test_atomicity_all_or_nothing_on_success(
    deliverable_set, tmp_path_factory
):
    """Property 5: On success, ALL files in the deliverable set are in COMOUT.

    **Validates: Requirements 7.6**

    For any random deliverable set of files:
    - Run atomic_publish with all valid files
    - Assert that either ALL files are present in COMOUT (success)
      or NONE are present (failure) — never a partial set
    """
    tmp_path = tmp_path_factory.mktemp("atomicity_success")

    env = _create_test_env(tmp_path)
    comout = Path(env["COMOUT"])
    data_dir = tmp_path / "data"

    # Create source files with non-empty content
    source_files = []
    for filename, content in deliverable_set:
        src = data_dir / filename
        src.write_bytes(content)
        source_files.append(str(src))

    # Use normal cpfs (just cp)
    mock_utils = tmp_path / "mock_utils.sh"
    mock_utils.write_text(
        '#!/usr/bin/env bash\n'
        'err_exit() { echo "ERR_EXIT: $*" >&2; exit 1; }\n'
        'cpfs() { cp "$1" "$2"; }\n'
        'export -f err_exit cpfs\n'
    )
    env["MOCK_UTILS"] = str(mock_utils)

    # Build the script
    file_args = " ".join(f'"{f}"' for f in source_files)
    script_content = (
        f'#!/usr/bin/env bash\n'
        f'source "{mock_utils}"\n'
        f'source "{SCRIPT_PATH}" {file_args}\n'
    )

    result = _run_script(tmp_path, script_content, env)

    # Check the all-or-nothing property
    deliverable_names = {name for name, _ in deliverable_set}
    final_files = set(f.name for f in comout.iterdir() if f.is_file())

    present_deliverables = final_files & deliverable_names

    # Either ALL deliverables are present (success) or NONE are (failure)
    assert present_deliverables == deliverable_names or present_deliverables == set(), (
        f"Atomicity violation! Partial deliverable set in COMOUT.\n"
        f"Expected all of: {deliverable_names}\n"
        f"Found only: {present_deliverables}\n"
        f"Missing: {deliverable_names - present_deliverables}\n"
        f"Return code: {result.returncode}\n"
        f"stderr: {result.stderr}"
    )

    # If the script succeeded, all files should be present
    if result.returncode == 0:
        assert present_deliverables == deliverable_names, (
            f"Script succeeded but not all deliverables are in COMOUT.\n"
            f"Expected: {deliverable_names}\n"
            f"Found: {present_deliverables}"
        )
