"""Property-based test: Atomicity (Property 5).

Tests the atomic_publish.sh script to verify that partial failures during
staging leave ${COMOUT} unchanged — no partial files are ever visible to
downstream consumers.

**Validates: Requirements 7.6**

Traces to: Design Document - Correctness Property 5
  "A partial failure during product staging leaves ${COMOUT} unchanged
   for that deliverable set."

Test approach:
  1. Set up a mock COMOUT directory and staging area
  2. Stage files to ${COMOUT}/.staging/${jobid}/
  3. Simulate a partial failure (e.g., one file fails verification)
  4. Verify that COMOUT remains unchanged (no partial files)
  5. Test the success path: all files verified → atomic mv to final location

Since this tests a bash script, uses subprocess-based testing with temporary
directories.
"""

from __future__ import annotations

import os
import subprocess
from pathlib import Path

import pytest
from hypothesis import given, settings, HealthCheck, assume
from hypothesis import strategies as st

# Path to the atomic_publish.sh script
SCRIPT_PATH = Path(__file__).parents[2] / "ush" / "atomic_publish.sh"


# ---------------------------------------------------------------------------
# Hypothesis Strategies
# ---------------------------------------------------------------------------

# Valid filename characters (safe for bash and filesystem)
_SAFE_CHARS = st.sampled_from(
    "abcdefghijklmnopqrstuvwxyz0123456789_"
)

_EXTENSIONS = st.sampled_from([".grib2", ".nc", ".idx", ".bufr", ".txt", ".bin"])


@st.composite
def deliverable_fileset(draw):
    """Generate a random set of deliverable files for atomic publish.

    Returns a list of (filename, content_bytes) tuples. Each file has a
    unique name and non-empty content, representing a deliverable set that
    should be atomically published to COMOUT.
    """
    num_files = draw(st.integers(min_value=2, max_value=8))

    files = []
    used_names = set()

    for i in range(num_files):
        # Generate a unique filename with a safe prefix and extension
        prefix = draw(st.text(_SAFE_CHARS, min_size=3, max_size=10))
        ext = draw(_EXTENSIONS)
        name = f"{prefix}_{i}{ext}"

        if name in used_names:
            name = f"file_{i}{ext}"
        used_names.add(name)

        # Non-empty content (1 to 256 bytes)
        content = draw(st.binary(min_size=1, max_size=256))
        files.append((name, content))

    return files


# ---------------------------------------------------------------------------
# Helper Functions
# ---------------------------------------------------------------------------


def _setup_environment(tmp_path: Path) -> dict:
    """Create a test environment with mock COMOUT and EE2 utilities.

    Sets up:
    - A clean COMOUT directory
    - A data directory for source files
    - Mock err_exit and cpfs functions
    """
    comout = tmp_path / "comout"
    comout.mkdir()
    data_dir = tmp_path / "data"
    data_dir.mkdir()

    # Mock EE2 utilities: err_exit aborts, cpfs copies
    mock_utils = tmp_path / "mock_utils.sh"
    mock_utils.write_text(
        '#!/usr/bin/env bash\n'
        'err_exit() { echo "ERR_EXIT: $*" >&2; exit 1; }\n'
        'cpfs() { cp "$1" "$2"; }\n'
        'export -f err_exit cpfs\n'
    )

    env = os.environ.copy()
    env["COMOUT"] = str(comout)
    env["jobid"] = "prop_test_atomicity"
    env["SENDDBN"] = "NO"
    env["MOCK_UTILS"] = str(mock_utils)

    return env


def _create_source_files(
    data_dir: Path, deliverable_set: list[tuple[str, bytes]]
) -> list[str]:
    """Write deliverable files to the data directory and return their paths."""
    source_files = []
    for filename, content in deliverable_set:
        src = data_dir / filename
        src.write_bytes(content)
        source_files.append(str(src))
    return source_files


def _run_atomic_publish(
    tmp_path: Path, source_files: list[str], env: dict
) -> subprocess.CompletedProcess:
    """Execute atomic_publish.sh via a wrapper script."""
    file_args = " ".join(f'"{f}"' for f in source_files)
    wrapper = tmp_path / "run_test.sh"
    wrapper.write_text(
        f'#!/usr/bin/env bash\n'
        f'source "{env["MOCK_UTILS"]}"\n'
        f'source "{SCRIPT_PATH}" {file_args}\n'
    )
    wrapper.chmod(0o755)

    return subprocess.run(
        ["bash", str(wrapper)],
        env=env,
        capture_output=True,
        text=True,
        timeout=30,
    )


def _get_comout_files(comout: Path) -> set[str]:
    """Return the set of regular file names directly in COMOUT (not in subdirs)."""
    return {f.name for f in comout.iterdir() if f.is_file()}


# ---------------------------------------------------------------------------
# Property Tests
# ---------------------------------------------------------------------------


class TestAtomicityProperty:
    """Property 5: Atomicity — partial failure leaves COMOUT unchanged.

    **Validates: Requirements 7.6**
    """

    @given(deliverable_set=deliverable_fileset())
    @settings(
        max_examples=50,
        deadline=None,
        suppress_health_check=[HealthCheck.too_slow],
    )
    def test_partial_staging_failure_no_files_in_comout(
        self, deliverable_set, tmp_path_factory
    ):
        """When cpfs fails mid-staging, COMOUT must contain no partial files.

        **Validates: Requirements 7.6**

        Strategy:
        - Generate a random deliverable set (2+ files)
        - Inject a cpfs failure at a random point during staging
        - Assert COMOUT remains empty (no partial deliverables)
        """
        tmp_path = tmp_path_factory.mktemp("atomicity_staging")
        assume(len(deliverable_set) >= 2)

        env = _setup_environment(tmp_path)
        comout = Path(env["COMOUT"])
        data_dir = tmp_path / "data"

        source_files = _create_source_files(data_dir, deliverable_set)

        # Inject failure: cpfs fails on the 2nd file (after 1 succeeds)
        # This simulates a partial staging failure
        fail_at = max(2, len(deliverable_set) // 2)
        mock_utils = tmp_path / "mock_utils.sh"
        mock_utils.write_text(
            '#!/usr/bin/env bash\n'
            'err_exit() { echo "ERR_EXIT: $*" >&2; exit 1; }\n'
            f'_CPFS_COUNT=0\n'
            'cpfs() {\n'
            '    _CPFS_COUNT=$((_CPFS_COUNT + 1))\n'
            f'    if [[ $_CPFS_COUNT -ge {fail_at} ]]; then\n'
            '        return 1\n'
            '    fi\n'
            '    cp "$1" "$2"\n'
            '}\n'
            'export -f err_exit cpfs\n'
        )
        env["MOCK_UTILS"] = str(mock_utils)

        # Record initial state
        initial_files = _get_comout_files(comout)

        result = _run_atomic_publish(tmp_path, source_files, env)

        # Script must fail
        assert result.returncode != 0, (
            f"Expected staging failure but script succeeded.\n"
            f"stderr: {result.stderr}"
        )

        # COMOUT must remain unchanged — no deliverable files leaked
        final_files = _get_comout_files(comout)
        deliverable_names = {name for name, _ in deliverable_set}
        leaked = final_files & deliverable_names

        assert leaked == set(), (
            f"ATOMICITY VIOLATION: Partial files leaked into COMOUT!\n"
            f"Leaked: {leaked}\n"
            f"Deliverable set: {deliverable_names}\n"
            f"COMOUT before: {initial_files}\n"
            f"COMOUT after: {final_files}"
        )
        assert final_files == initial_files

    @given(deliverable_set=deliverable_fileset())
    @settings(
        max_examples=50,
        deadline=None,
        suppress_health_check=[HealthCheck.too_slow],
    )
    def test_verification_failure_no_files_in_comout(
        self, deliverable_set, tmp_path_factory
    ):
        """When a staged file fails verification (empty), COMOUT stays unchanged.

        **Validates: Requirements 7.6**

        Strategy:
        - Generate a random deliverable set (2+ files)
        - Stage all files, but corrupt one (make it empty) during cpfs
        - The verification step should detect the empty file
        - Assert COMOUT remains empty (no partial deliverables)
        """
        tmp_path = tmp_path_factory.mktemp("atomicity_verify")
        assume(len(deliverable_set) >= 2)

        env = _setup_environment(tmp_path)
        comout = Path(env["COMOUT"])
        data_dir = tmp_path / "data"

        source_files = _create_source_files(data_dir, deliverable_set)

        # Pick a file to corrupt (truncate to empty during cpfs)
        corrupt_idx = len(deliverable_set) // 2
        corrupt_name = deliverable_set[corrupt_idx][0]

        mock_utils = tmp_path / "mock_utils.sh"
        mock_utils.write_text(
            '#!/usr/bin/env bash\n'
            'err_exit() { echo "ERR_EXIT: $*" >&2; exit 1; }\n'
            'cpfs() {\n'
            '    local bname\n'
            '    bname=$(basename "$2")\n'
            f'    if [[ "$bname" == "{corrupt_name}" ]]; then\n'
            '        : > "$2"\n'
            '    else\n'
            '        cp "$1" "$2"\n'
            '    fi\n'
            '}\n'
            'export -f err_exit cpfs\n'
        )
        env["MOCK_UTILS"] = str(mock_utils)

        # Record initial state
        initial_files = _get_comout_files(comout)

        result = _run_atomic_publish(tmp_path, source_files, env)

        # Script must fail due to empty file verification
        assert result.returncode != 0, (
            f"Expected verification failure but script succeeded.\n"
            f"Corrupted file: {corrupt_name}\n"
            f"stderr: {result.stderr}"
        )

        # COMOUT must remain unchanged
        final_files = _get_comout_files(comout)
        deliverable_names = {name for name, _ in deliverable_set}
        leaked = final_files & deliverable_names

        assert leaked == set(), (
            f"ATOMICITY VIOLATION: Files leaked into COMOUT after verification failure!\n"
            f"Leaked: {leaked}\n"
            f"Corrupted file: {corrupt_name}\n"
            f"COMOUT before: {initial_files}\n"
            f"COMOUT after: {final_files}"
        )
        assert final_files == initial_files

    @given(deliverable_set=deliverable_fileset())
    @settings(
        max_examples=50,
        deadline=None,
        suppress_health_check=[HealthCheck.too_slow],
    )
    def test_success_path_all_files_in_comout(
        self, deliverable_set, tmp_path_factory
    ):
        """On success, ALL files in the deliverable set appear in COMOUT.

        **Validates: Requirements 7.6**

        Strategy:
        - Generate a random deliverable set
        - Run atomic_publish with all valid, non-empty files
        - Assert ALL files are present in COMOUT (complete set)
        - Verify content integrity (bytes match source)
        """
        tmp_path = tmp_path_factory.mktemp("atomicity_success")

        env = _setup_environment(tmp_path)
        comout = Path(env["COMOUT"])
        data_dir = tmp_path / "data"

        source_files = _create_source_files(data_dir, deliverable_set)

        result = _run_atomic_publish(tmp_path, source_files, env)

        # Script must succeed
        assert result.returncode == 0, (
            f"Expected success but script failed.\n"
            f"stderr: {result.stderr}"
        )

        # ALL deliverable files must be in COMOUT
        final_files = _get_comout_files(comout)
        deliverable_names = {name for name, _ in deliverable_set}

        assert deliverable_names.issubset(final_files), (
            f"Not all deliverables published to COMOUT!\n"
            f"Expected: {deliverable_names}\n"
            f"Found: {final_files}\n"
            f"Missing: {deliverable_names - final_files}"
        )

        # Verify content integrity
        for filename, content in deliverable_set:
            final_file = comout / filename
            assert final_file.exists(), f"Missing: {filename}"
            assert final_file.read_bytes() == content, (
                f"Content mismatch for {filename}"
            )

    @given(deliverable_set=deliverable_fileset())
    @settings(
        max_examples=50,
        deadline=None,
        suppress_health_check=[HealthCheck.too_slow],
    )
    def test_all_or_nothing_invariant(
        self, deliverable_set, tmp_path_factory
    ):
        """The all-or-nothing invariant: either ALL or NONE of the set is in COMOUT.

        **Validates: Requirements 7.6**

        Regardless of success or failure, the deliverable set in COMOUT must
        be complete (all files) or absent (no files). A partial set is never
        acceptable.
        """
        tmp_path = tmp_path_factory.mktemp("atomicity_invariant")

        env = _setup_environment(tmp_path)
        comout = Path(env["COMOUT"])
        data_dir = tmp_path / "data"

        source_files = _create_source_files(data_dir, deliverable_set)

        result = _run_atomic_publish(tmp_path, source_files, env)

        # Check the all-or-nothing property
        final_files = _get_comout_files(comout)
        deliverable_names = {name for name, _ in deliverable_set}
        present = final_files & deliverable_names

        # Must be ALL or NONE — never partial
        assert present == deliverable_names or present == set(), (
            f"ATOMICITY VIOLATION: Partial deliverable set in COMOUT!\n"
            f"Expected all of {deliverable_names} or none.\n"
            f"Found: {present}\n"
            f"Missing: {deliverable_names - present}\n"
            f"Return code: {result.returncode}"
        )
