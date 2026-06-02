"""Tests for dev/ush/atomic_publish.sh

Validates the atomic publish pattern:
- Files are staged to ${COMOUT}/.staging/${jobid}/
- All staged files are verified non-empty
- Atomic mv to final ${COMOUT} location
- On verification failure, err_exit leaves COMOUT unchanged
- dbn_alert only after file is at final location and SENDDBN=YES
- Uses cpfs for inter-filesystem copies per EE2

Traces to: Requirements 7.1, 7.2, 7.3, 7.4, 7.5
"""

import os
import subprocess
import tempfile
from pathlib import Path

import pytest

# Path to the atomic_publish.sh script
SCRIPT_PATH = Path(__file__).parents[2] / "ush" / "atomic_publish.sh"


def _make_test_env(tmp_path: Path) -> dict:
    """Create a minimal test environment with mock EE2 utilities."""
    comout = tmp_path / "comout"
    comout.mkdir()
    data_dir = tmp_path / "data"
    data_dir.mkdir()

    # Create a mock err_exit function and cpfs function
    mock_utils = tmp_path / "mock_utils.sh"
    mock_utils.write_text(
        '#!/usr/bin/env bash\n'
        'err_exit() { echo "ERR_EXIT: $*" >&2; exit 1; }\n'
        'cpfs() { cp "$1" "$2"; }\n'
        'export -f err_exit cpfs\n'
    )

    env = os.environ.copy()
    env["COMOUT"] = str(comout)
    env["jobid"] = "test_job_12345"
    env["SENDDBN"] = "NO"
    env["MOCK_UTILS"] = str(mock_utils)

    return env


def _run_atomic_publish(tmp_path: Path, files: list, env: dict,
                        extra_env: dict = None) -> subprocess.CompletedProcess:
    """Run atomic_publish.sh with the given files and environment."""
    if extra_env:
        env.update(extra_env)

    # Build a wrapper script that sources mock utilities then sources atomic_publish
    wrapper = tmp_path / "run_test.sh"
    file_args = " ".join(f'"{f}"' for f in files)
    wrapper.write_text(
        f'#!/usr/bin/env bash\n'
        f'source "{env["MOCK_UTILS"]}"\n'
        f'source "{SCRIPT_PATH}" {file_args}\n'
    )
    wrapper.chmod(0o755)

    result = subprocess.run(
        ["bash", str(wrapper)],
        env=env,
        capture_output=True,
        text=True,
        timeout=30,
    )
    return result


class TestAtomicPublishSuccess:
    """Tests for successful atomic publish operations."""

    def test_single_file_published_to_comout(self, tmp_path: Path):
        """A single file should end up in COMOUT after atomic publish."""
        env = _make_test_env(tmp_path)
        comout = Path(env["COMOUT"])

        # Create a source file
        src_file = tmp_path / "data" / "output.grib2"
        src_file.write_text("grib2 data content")

        result = _run_atomic_publish(tmp_path, [str(src_file)], env)
        assert result.returncode == 0, f"Failed: {result.stderr}"

        # File should be at final location
        final_file = comout / "output.grib2"
        assert final_file.exists()
        assert final_file.read_text() == "grib2 data content"

    def test_multiple_files_published(self, tmp_path: Path):
        """Multiple files should all end up in COMOUT."""
        env = _make_test_env(tmp_path)
        comout = Path(env["COMOUT"])

        # Create source files
        files = []
        for name in ["file1.nc", "file2.grib2", "file3.idx"]:
            f = tmp_path / "data" / name
            f.write_text(f"content of {name}")
            files.append(str(f))

        result = _run_atomic_publish(tmp_path, files, env)
        assert result.returncode == 0, f"Failed: {result.stderr}"

        for name in ["file1.nc", "file2.grib2", "file3.idx"]:
            final_file = comout / name
            assert final_file.exists()
            assert final_file.read_text() == f"content of {name}"

    def test_staging_dir_cleaned_up(self, tmp_path: Path):
        """The .staging/${jobid}/ directory should be removed after success."""
        env = _make_test_env(tmp_path)
        comout = Path(env["COMOUT"])

        src_file = tmp_path / "data" / "product.nc"
        src_file.write_text("netcdf data")

        result = _run_atomic_publish(tmp_path, [str(src_file)], env)
        assert result.returncode == 0, f"Failed: {result.stderr}"

        staging_dir = comout / ".staging" / env["jobid"]
        assert not staging_dir.exists()


class TestAtomicPublishVerification:
    """Tests for verification failure handling."""

    def test_empty_file_causes_failure(self, tmp_path: Path):
        """An empty staged file should trigger err_exit."""
        env = _make_test_env(tmp_path)
        comout = Path(env["COMOUT"])

        # Create an empty source file
        src_file = tmp_path / "data" / "empty.grib2"
        src_file.write_text("")

        # We need cpfs to copy the empty file, then verification should fail
        # Override cpfs to copy even empty files
        result = _run_atomic_publish(tmp_path, [str(src_file)], env)
        assert result.returncode != 0

        # COMOUT should remain unchanged (no file at final location)
        final_file = comout / "empty.grib2"
        assert not final_file.exists()

    def test_missing_source_file_causes_failure(self, tmp_path: Path):
        """A non-existent source file should trigger err_exit."""
        env = _make_test_env(tmp_path)
        comout = Path(env["COMOUT"])

        result = _run_atomic_publish(
            tmp_path, ["/nonexistent/file.grib2"], env
        )
        assert result.returncode != 0
        assert "ERR_EXIT" in result.stderr

    def test_hash_check_mismatch_causes_failure(self, tmp_path: Path):
        """Hash mismatch should trigger err_exit when hash check is enabled."""
        env = _make_test_env(tmp_path)
        comout = Path(env["COMOUT"])

        # Create source file
        src_file = tmp_path / "data" / "product.nc"
        src_file.write_text("original content")

        # Override cpfs to corrupt the staged file
        mock_utils = tmp_path / "mock_utils.sh"
        mock_utils.write_text(
            '#!/usr/bin/env bash\n'
            'err_exit() { echo "ERR_EXIT: $*" >&2; exit 1; }\n'
            'cpfs() { echo "corrupted" > "$2"; }\n'
            'export -f err_exit cpfs\n'
        )

        result = _run_atomic_publish(
            tmp_path, [str(src_file)], env,
            extra_env={"ATOMIC_PUBLISH_HASH_CHECK": "YES"}
        )
        assert result.returncode != 0
        assert "ERR_EXIT" in result.stderr

        # COMOUT should remain unchanged
        final_file = comout / "product.nc"
        assert not final_file.exists()


class TestAtomicPublishDBNAlert:
    """Tests for dbn_alert behavior."""

    def test_no_alert_when_senddbn_not_yes(self, tmp_path: Path):
        """dbn_alert should NOT be called when SENDDBN != YES."""
        env = _make_test_env(tmp_path)

        src_file = tmp_path / "data" / "product.nc"
        src_file.write_text("data content")

        # Create a mock dbn_alert that records calls
        alert_log = tmp_path / "alert.log"
        dbn_root = tmp_path / "dbn" / "bin"
        dbn_root.mkdir(parents=True)
        dbn_alert = dbn_root / "dbn_alert"
        dbn_alert.write_text(
            f'#!/usr/bin/env bash\necho "$@" >> "{alert_log}"\n'
        )
        dbn_alert.chmod(0o755)

        env["SENDDBN"] = "NO"
        env["DBNROOT"] = str(tmp_path / "dbn")
        env["DBN_ALERT_TYPE"] = "MODEL_TEST"
        env["job"] = "test_job"

        result = _run_atomic_publish(tmp_path, [str(src_file)], env)
        assert result.returncode == 0, f"Failed: {result.stderr}"

        # No alert should have been sent
        assert not alert_log.exists()

    def test_alert_sent_when_senddbn_yes(self, tmp_path: Path):
        """dbn_alert should be called when SENDDBN=YES and file is at final location."""
        env = _make_test_env(tmp_path)

        src_file = tmp_path / "data" / "product.nc"
        src_file.write_text("data content")

        # Create a mock dbn_alert that records calls
        alert_log = tmp_path / "alert.log"
        dbn_root = tmp_path / "dbn" / "bin"
        dbn_root.mkdir(parents=True)
        dbn_alert = dbn_root / "dbn_alert"
        dbn_alert.write_text(
            f'#!/usr/bin/env bash\necho "$@" >> "{alert_log}"\n'
        )
        dbn_alert.chmod(0o755)

        env["SENDDBN"] = "YES"
        env["DBNROOT"] = str(tmp_path / "dbn")
        env["DBN_ALERT_TYPE"] = "MODEL_TEST"
        env["job"] = "test_job"

        result = _run_atomic_publish(tmp_path, [str(src_file)], env)
        assert result.returncode == 0, f"Failed: {result.stderr}"

        # Alert should have been sent
        assert alert_log.exists()
        alert_content = alert_log.read_text()
        assert "MODEL_TEST" in alert_content
        assert "product.nc" in alert_content


class TestAtomicPublishEnvironment:
    """Tests for environment variable validation."""

    def test_missing_comout_causes_failure(self, tmp_path: Path):
        """Missing COMOUT should trigger err_exit."""
        env = _make_test_env(tmp_path)
        del env["COMOUT"]

        src_file = tmp_path / "data" / "product.nc"
        src_file.write_text("data")

        result = _run_atomic_publish(tmp_path, [str(src_file)], env)
        assert result.returncode != 0

    def test_missing_jobid_causes_failure(self, tmp_path: Path):
        """Missing jobid should trigger err_exit."""
        env = _make_test_env(tmp_path)
        del env["jobid"]

        src_file = tmp_path / "data" / "product.nc"
        src_file.write_text("data")

        result = _run_atomic_publish(tmp_path, [str(src_file)], env)
        assert result.returncode != 0

    def test_no_files_argument_causes_failure(self, tmp_path: Path):
        """Calling with no file arguments should trigger err_exit."""
        env = _make_test_env(tmp_path)

        # Run with no file arguments
        wrapper = tmp_path / "run_test.sh"
        wrapper.write_text(
            f'#!/usr/bin/env bash\n'
            f'source "{env["MOCK_UTILS"]}"\n'
            f'source "{SCRIPT_PATH}"\n'
        )
        wrapper.chmod(0o755)

        result = subprocess.run(
            ["bash", str(wrapper)],
            env=env,
            capture_output=True,
            text=True,
            timeout=30,
        )
        assert result.returncode != 0
