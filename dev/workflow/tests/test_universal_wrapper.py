"""Tests for dev/ush/universal_wrapper.sh.j2

Validates the Universal_Wrapper behavior:
- Ephemeral directory creation and cleanup (Req 5.1, 5.2)
- Platform environment sourcing (Req 6.2)
- Error handling on JJob failure (Req 6.4)
- Structured JSON lifecycle logging (Req 6.6)

Traces to: Requirements 5.1, 5.2, 6.2, 6.4, 6.6
"""

import json
import os
import stat
import subprocess
import tempfile
from pathlib import Path

import jinja2
import pytest

# Path to the universal_wrapper Jinja2 template
TEMPLATE_PATH = Path(__file__).parents[2] / "ush" / "universal_wrapper.sh.j2"


def _render_wrapper(expdir: str) -> str:
    """Render the universal_wrapper.sh.j2 template with the given EXPDIR."""
    template_content = TEMPLATE_PATH.read_text()
    env = jinja2.Environment(
        loader=jinja2.BaseLoader(),
        undefined=jinja2.StrictUndefined,
    )
    template = env.from_string(template_content)
    return template.render(EXPDIR=expdir)


def _setup_expdir(tmp_path: Path, machine: str = "hera") -> Path:
    """Create a minimal EXPDIR structure for testing."""
    expdir = tmp_path / "expdir"
    expdir.mkdir()

    # Create env directory with platform env file
    env_dir = expdir / "env"
    env_dir.mkdir()
    env_file = env_dir / f"{machine}.env"
    env_file.write_text(
        '#!/bin/bash\n'
        '# Minimal platform env for testing\n'
        'export ENV_SOURCED="yes"\n'
    )

    # Create ush directory with rendered wrapper
    ush_dir = expdir / "ush"
    ush_dir.mkdir()
    wrapper_path = ush_dir / "universal_wrapper.sh"
    wrapper_content = _render_wrapper(str(expdir))
    wrapper_path.write_text(wrapper_content)
    wrapper_path.chmod(0o755)

    # Create jobs directory
    jobs_dir = expdir / "jobs"
    jobs_dir.mkdir()

    return expdir


def _create_jjob(expdir: Path, name: str, script_content: str) -> Path:
    """Create a JJob script in the EXPDIR/jobs/ directory."""
    jjob_path = expdir / "jobs" / name
    jjob_path.write_text(script_content)
    jjob_path.chmod(0o755)
    return jjob_path


def _run_wrapper(expdir: Path, jjob_name: str, env_overrides: dict = None,
                 timeout: int = 30) -> subprocess.CompletedProcess:
    """Run the universal_wrapper.sh with the given JJob name."""
    wrapper_path = expdir / "ush" / "universal_wrapper.sh"

    env = {
        "PATH": os.environ.get("PATH", "/usr/bin:/bin"),
        "HOME": os.environ.get("HOME", "/tmp"),
        "MACHINE": "hera",
        "DATAROOT": str(expdir / "dataroot"),
        "PDY": "20250115",
        "cyc": "06",
        "SNAPSHOT_ID": "v17.0.0+test123456",
    }
    if env_overrides:
        env.update(env_overrides)

    # Ensure DATAROOT exists
    dataroot = Path(env["DATAROOT"])
    dataroot.mkdir(parents=True, exist_ok=True)

    result = subprocess.run(
        ["bash", str(wrapper_path), jjob_name],
        env=env,
        capture_output=True,
        text=True,
        timeout=timeout,
    )
    return result


class TestEphemeralDirectoryCreation:
    """Tests for ephemeral working directory creation (Req 5.1)."""

    def test_data_directory_created(self, tmp_path: Path):
        """Universal_Wrapper creates ${DATAROOT}/${jobid} as DATA."""
        expdir = _setup_expdir(tmp_path)

        # Create a JJob that verifies DATA exists and is a directory
        _create_jjob(expdir, "JTEST_DATA_CHECK", (
            '#!/bin/bash\n'
            'if [[ -d "${DATA}" ]]; then\n'
            '    echo "DATA_EXISTS=yes"\n'
            '    echo "DATA_PATH=${DATA}"\n'
            'else\n'
            '    echo "DATA_EXISTS=no" >&2\n'
            '    exit 1\n'
            'fi\n'
        ))

        result = _run_wrapper(expdir, "JTEST_DATA_CHECK")
        assert result.returncode == 0, f"Failed: {result.stderr}"
        assert "DATA_EXISTS=yes" in result.stdout

    def test_data_directory_uses_jobid(self, tmp_path: Path):
        """DATA directory path includes the jobid."""
        expdir = _setup_expdir(tmp_path)

        _create_jjob(expdir, "JTEST_JOBID", (
            '#!/bin/bash\n'
            'echo "DATA=${DATA}"\n'
            'echo "JOBID=${jobid}"\n'
        ))

        result = _run_wrapper(expdir, "JTEST_JOBID")
        assert result.returncode == 0, f"Failed: {result.stderr}"
        # The jobid should be part of the DATA path
        assert "JTEST_JOBID" in result.stdout

    def test_stale_data_directory_removed(self, tmp_path: Path):
        """If DATA directory already exists, it is removed before re-creation."""
        expdir = _setup_expdir(tmp_path)
        dataroot = expdir / "dataroot"
        dataroot.mkdir(parents=True, exist_ok=True)

        # Pre-create a stale DATA directory with a marker file
        stale_dir = dataroot / "JTEST_STALE.$$"
        # We can't predict the exact jobid, so create a JJob that checks
        _create_jjob(expdir, "JTEST_STALE", (
            '#!/bin/bash\n'
            '# Check that no stale marker file exists\n'
            'if [[ -f "${DATA}/stale_marker" ]]; then\n'
            '    echo "STALE_FOUND=yes" >&2\n'
            '    exit 1\n'
            'fi\n'
            'echo "STALE_FOUND=no"\n'
        ))

        # Pre-create the expected DATA path with a stale marker
        # The jobid will be JTEST_STALE.<PID>, but we can set jobid explicitly
        env_overrides = {"jobid": "JTEST_STALE.12345"}
        stale_dir = dataroot / "JTEST_STALE.12345"
        stale_dir.mkdir(parents=True, exist_ok=True)
        (stale_dir / "stale_marker").write_text("stale")

        result = _run_wrapper(expdir, "JTEST_STALE", env_overrides=env_overrides)
        assert result.returncode == 0, f"Failed: {result.stderr}"
        assert "STALE_FOUND=no" in result.stdout

    def test_pgmout_variable_set(self, tmp_path: Path):
        """pgmout is set to OUTPUT.$$ per EE2 conventions (Req 5.7)."""
        expdir = _setup_expdir(tmp_path)

        _create_jjob(expdir, "JTEST_PGMOUT", (
            '#!/bin/bash\n'
            'echo "PGMOUT=${pgmout}"\n'
        ))

        result = _run_wrapper(expdir, "JTEST_PGMOUT")
        assert result.returncode == 0, f"Failed: {result.stderr}"
        assert "PGMOUT=OUTPUT." in result.stdout


class TestEphemeralDirectoryCleanup:
    """Tests for ephemeral working directory cleanup (Req 5.2)."""

    def test_data_directory_cleaned_on_success(self, tmp_path: Path):
        """DATA directory is removed after successful JJob execution."""
        expdir = _setup_expdir(tmp_path)
        dataroot = expdir / "dataroot"
        dataroot.mkdir(parents=True, exist_ok=True)

        _create_jjob(expdir, "JTEST_CLEANUP", (
            '#!/bin/bash\n'
            '# Write a marker to DATA so we can check it was cleaned\n'
            'echo "marker" > "${DATA}/cleanup_test"\n'
            'echo "DATA=${DATA}"\n'
        ))

        env_overrides = {"jobid": "JTEST_CLEANUP.99999"}
        result = _run_wrapper(expdir, "JTEST_CLEANUP", env_overrides=env_overrides)
        assert result.returncode == 0, f"Failed: {result.stderr}"

        # DATA directory should be cleaned up
        data_dir = dataroot / "JTEST_CLEANUP.99999"
        assert not data_dir.exists(), "DATA directory was not cleaned up"

    def test_data_directory_kept_when_keepdata_yes(self, tmp_path: Path):
        """DATA directory is preserved when KEEPDATA=YES."""
        expdir = _setup_expdir(tmp_path)
        dataroot = expdir / "dataroot"
        dataroot.mkdir(parents=True, exist_ok=True)

        _create_jjob(expdir, "JTEST_KEEP", (
            '#!/bin/bash\n'
            'echo "marker" > "${DATA}/keep_test"\n'
        ))

        env_overrides = {
            "jobid": "JTEST_KEEP.88888",
            "KEEPDATA": "YES",
        }
        result = _run_wrapper(expdir, "JTEST_KEEP", env_overrides=env_overrides)
        assert result.returncode == 0, f"Failed: {result.stderr}"

        # DATA directory should still exist
        data_dir = dataroot / "JTEST_KEEP.88888"
        assert data_dir.exists(), "DATA directory was removed despite KEEPDATA=YES"
        assert (data_dir / "keep_test").exists()


class TestEnvSourcing:
    """Tests for platform environment sourcing (Req 6.2)."""

    def test_env_file_sourced_successfully(self, tmp_path: Path):
        """The platform env file is sourced and its exports are available."""
        expdir = _setup_expdir(tmp_path)

        # Update the env file to export a test variable
        env_file = expdir / "env" / "hera.env"
        env_file.write_text(
            '#!/bin/bash\n'
            'export TEST_ENV_VAR="sourced_from_hera"\n'
        )

        _create_jjob(expdir, "JTEST_ENV", (
            '#!/bin/bash\n'
            'echo "TEST_ENV_VAR=${TEST_ENV_VAR}"\n'
        ))

        result = _run_wrapper(expdir, "JTEST_ENV")
        assert result.returncode == 0, f"Failed: {result.stderr}"
        assert "TEST_ENV_VAR=sourced_from_hera" in result.stdout

    def test_missing_env_file_causes_fatal_error(self, tmp_path: Path):
        """Missing platform env file causes FATAL ERROR."""
        expdir = _setup_expdir(tmp_path, machine="hera")

        # Remove the env file
        env_file = expdir / "env" / "hera.env"
        env_file.unlink()

        _create_jjob(expdir, "JTEST_NOENV", '#!/bin/bash\nexit 0\n')

        result = _run_wrapper(expdir, "JTEST_NOENV")
        assert result.returncode != 0
        assert "FATAL ERROR" in result.stderr
        assert "not found" in result.stderr

    def test_env_file_receives_jjob_name_as_argument(self, tmp_path: Path):
        """The env file receives the JJob name as its first argument."""
        expdir = _setup_expdir(tmp_path)

        # Create an env file that exports its first argument
        env_file = expdir / "env" / "hera.env"
        env_file.write_text(
            '#!/bin/bash\n'
            'export RECEIVED_JJOB_ARG="$1"\n'
        )

        _create_jjob(expdir, "JTEST_ENVARG", (
            '#!/bin/bash\n'
            'echo "RECEIVED_JJOB_ARG=${RECEIVED_JJOB_ARG}"\n'
        ))

        result = _run_wrapper(expdir, "JTEST_ENVARG")
        assert result.returncode == 0, f"Failed: {result.stderr}"
        assert "RECEIVED_JJOB_ARG=JTEST_ENVARG" in result.stdout

    def test_env_file_failure_causes_abort(self, tmp_path: Path):
        """If the env file returns non-zero, the wrapper aborts."""
        expdir = _setup_expdir(tmp_path)

        # Create an env file that returns non-zero without exiting the shell.
        # Use 'return 42' instead of 'exit 42' since source runs in-process;
        # 'exit' would terminate the entire shell immediately.
        env_file = expdir / "env" / "hera.env"
        env_file.write_text(
            '#!/bin/bash\n'
            'return 42\n'
        )

        _create_jjob(expdir, "JTEST_ENVFAIL", '#!/bin/bash\nexit 0\n')

        result = _run_wrapper(expdir, "JTEST_ENVFAIL")
        assert result.returncode != 0
        assert "FATAL ERROR" in result.stderr
        assert "Failed to source" in result.stderr


class TestErrorHandling:
    """Tests for error handling on JJob failure (Req 6.4)."""

    def test_jjob_nonzero_exit_triggers_err_exit(self, tmp_path: Path):
        """Non-zero JJob exit calls err_exit with descriptive message."""
        expdir = _setup_expdir(tmp_path)

        # Create an err_exit.sh that the wrapper can source
        err_exit_path = expdir / "ush" / "err_exit.sh"
        err_exit_path.write_text(
            '#!/bin/bash\n'
            'err_exit() {\n'
            '    echo "ERR_EXIT: $1" >&2\n'
            '    exit 1\n'
            '}\n'
        )

        _create_jjob(expdir, "JTEST_FAIL", (
            '#!/bin/bash\n'
            'exit 42\n'
        ))

        result = _run_wrapper(expdir, "JTEST_FAIL")
        assert result.returncode != 0
        # Should mention the JJob name and exit status
        assert "JTEST_FAIL" in result.stderr
        assert "42" in result.stderr

    def test_missing_jjob_causes_fatal_error(self, tmp_path: Path):
        """A non-existent JJob causes FATAL ERROR with exit 127."""
        expdir = _setup_expdir(tmp_path)

        result = _run_wrapper(expdir, "JTEST_NONEXISTENT")
        assert result.returncode != 0
        assert "FATAL ERROR" in result.stderr
        assert "not found" in result.stderr

    def test_no_arguments_causes_fatal_error(self, tmp_path: Path):
        """Calling wrapper with no arguments causes FATAL ERROR."""
        expdir = _setup_expdir(tmp_path)
        wrapper_path = expdir / "ush" / "universal_wrapper.sh"

        env = {
            "PATH": os.environ.get("PATH", "/usr/bin:/bin"),
            "HOME": os.environ.get("HOME", "/tmp"),
            "MACHINE": "hera",
            "DATAROOT": str(expdir / "dataroot"),
        }
        (expdir / "dataroot").mkdir(parents=True, exist_ok=True)

        result = subprocess.run(
            ["bash", str(wrapper_path)],
            env=env,
            capture_output=True,
            text=True,
            timeout=30,
        )
        assert result.returncode != 0
        assert "FATAL ERROR" in result.stderr
        assert "requires a JJob name" in result.stderr

    def test_wcoss2_envir_guard_rejects_invalid(self, tmp_path: Path):
        """On WCOSS2, invalid envir value causes FATAL ERROR (Req 11.8)."""
        expdir = _setup_expdir(tmp_path, machine="WCOSS2")

        # Create env file for WCOSS2
        env_file = expdir / "env" / "WCOSS2.env"
        env_file.write_text('#!/bin/bash\nexport ENV_SOURCED="yes"\n')

        _create_jjob(expdir, "JTEST_ENVIR", '#!/bin/bash\nexit 0\n')

        result = _run_wrapper(
            expdir, "JTEST_ENVIR",
            env_overrides={"MACHINE": "WCOSS2", "envir": "invalid_value"}
        )
        assert result.returncode != 0
        assert "FATAL ERROR" in result.stderr
        assert "envir" in result.stderr

    def test_wcoss2_envir_guard_accepts_prod(self, tmp_path: Path):
        """On WCOSS2, envir=prod is accepted."""
        expdir = _setup_expdir(tmp_path, machine="WCOSS2")

        env_file = expdir / "env" / "WCOSS2.env"
        env_file.write_text('#!/bin/bash\nexport ENV_SOURCED="yes"\n')

        _create_jjob(expdir, "JTEST_PROD", '#!/bin/bash\nexit 0\n')

        result = _run_wrapper(
            expdir, "JTEST_PROD",
            env_overrides={"MACHINE": "WCOSS2", "envir": "prod"}
        )
        assert result.returncode == 0, f"Failed: {result.stderr}"

    def test_wcoss2_envir_guard_accepts_para(self, tmp_path: Path):
        """On WCOSS2, envir=para is accepted."""
        expdir = _setup_expdir(tmp_path, machine="WCOSS2")

        env_file = expdir / "env" / "WCOSS2.env"
        env_file.write_text('#!/bin/bash\nexport ENV_SOURCED="yes"\n')

        _create_jjob(expdir, "JTEST_PARA", '#!/bin/bash\nexit 0\n')

        result = _run_wrapper(
            expdir, "JTEST_PARA",
            env_overrides={"MACHINE": "WCOSS2", "envir": "para"}
        )
        assert result.returncode == 0, f"Failed: {result.stderr}"

    def test_wcoss2_envir_guard_accepts_test(self, tmp_path: Path):
        """On WCOSS2, envir=test is accepted."""
        expdir = _setup_expdir(tmp_path, machine="WCOSS2")

        env_file = expdir / "env" / "WCOSS2.env"
        env_file.write_text('#!/bin/bash\nexport ENV_SOURCED="yes"\n')

        _create_jjob(expdir, "JTEST_TEST", '#!/bin/bash\nexit 0\n')

        result = _run_wrapper(
            expdir, "JTEST_TEST",
            env_overrides={"MACHINE": "WCOSS2", "envir": "test"}
        )
        assert result.returncode == 0, f"Failed: {result.stderr}"

    def test_wcoss2_missing_envir_causes_fatal_error(self, tmp_path: Path):
        """On WCOSS2, unset envir causes FATAL ERROR."""
        expdir = _setup_expdir(tmp_path, machine="WCOSS2")

        env_file = expdir / "env" / "WCOSS2.env"
        env_file.write_text('#!/bin/bash\nexport ENV_SOURCED="yes"\n')

        _create_jjob(expdir, "JTEST_NOENVIR", '#!/bin/bash\nexit 0\n')

        # Don't set envir at all
        result = _run_wrapper(
            expdir, "JTEST_NOENVIR",
            env_overrides={"MACHINE": "WCOSS2"}
        )
        assert result.returncode != 0
        assert "FATAL ERROR" in result.stderr
        assert "envir" in result.stderr


class TestLifecycleLogging:
    """Tests for structured JSON lifecycle logging (Req 6.6)."""

    def test_init_event_logged(self, tmp_path: Path):
        """An 'init' lifecycle event is emitted at wrapper start."""
        expdir = _setup_expdir(tmp_path)

        _create_jjob(expdir, "JTEST_LOG_INIT", '#!/bin/bash\nexit 0\n')

        result = _run_wrapper(expdir, "JTEST_LOG_INIT")
        assert result.returncode == 0, f"Failed: {result.stderr}"

        # Parse JSON lifecycle events from stderr
        events = _extract_lifecycle_events(result.stderr)
        init_events = [e for e in events if e.get("state") == "init"]
        assert len(init_events) >= 1, "No 'init' lifecycle event found"
        assert init_events[0]["task"] == "JTEST_LOG_INIT"

    def test_start_event_logged(self, tmp_path: Path):
        """A 'start' lifecycle event is emitted before JJob execution."""
        expdir = _setup_expdir(tmp_path)

        _create_jjob(expdir, "JTEST_LOG_START", '#!/bin/bash\nexit 0\n')

        result = _run_wrapper(expdir, "JTEST_LOG_START")
        assert result.returncode == 0, f"Failed: {result.stderr}"

        events = _extract_lifecycle_events(result.stderr)
        start_events = [e for e in events if e.get("state") == "start"]
        assert len(start_events) >= 1, "No 'start' lifecycle event found"

    def test_succeeded_event_logged_on_success(self, tmp_path: Path):
        """A 'succeeded' lifecycle event is emitted on successful JJob."""
        expdir = _setup_expdir(tmp_path)

        _create_jjob(expdir, "JTEST_LOG_OK", '#!/bin/bash\nexit 0\n')

        result = _run_wrapper(expdir, "JTEST_LOG_OK")
        assert result.returncode == 0, f"Failed: {result.stderr}"

        events = _extract_lifecycle_events(result.stderr)
        succeeded_events = [e for e in events if e.get("state") == "succeeded"]
        assert len(succeeded_events) >= 1, "No 'succeeded' lifecycle event found"

    def test_complete_event_logged_on_exit(self, tmp_path: Path):
        """A 'complete' lifecycle event is emitted on clean exit."""
        expdir = _setup_expdir(tmp_path)

        _create_jjob(expdir, "JTEST_LOG_COMPLETE", '#!/bin/bash\nexit 0\n')

        result = _run_wrapper(expdir, "JTEST_LOG_COMPLETE")
        assert result.returncode == 0, f"Failed: {result.stderr}"

        events = _extract_lifecycle_events(result.stderr)
        complete_events = [e for e in events if e.get("state") == "complete"]
        assert len(complete_events) >= 1, "No 'complete' lifecycle event found"

    def test_failed_event_logged_on_jjob_failure(self, tmp_path: Path):
        """A 'failed' lifecycle event is emitted when JJob fails."""
        expdir = _setup_expdir(tmp_path)

        # Create err_exit.sh so the wrapper can source it
        err_exit_path = expdir / "ush" / "err_exit.sh"
        err_exit_path.write_text(
            '#!/bin/bash\n'
            'err_exit() {\n'
            '    echo "ERR_EXIT: $1" >&2\n'
            '    exit 1\n'
            '}\n'
        )

        _create_jjob(expdir, "JTEST_LOG_FAIL", '#!/bin/bash\nexit 7\n')

        result = _run_wrapper(expdir, "JTEST_LOG_FAIL")
        assert result.returncode != 0

        events = _extract_lifecycle_events(result.stderr)
        failed_events = [e for e in events if e.get("state") == "failed"]
        assert len(failed_events) >= 1, "No 'failed' lifecycle event found"
        assert failed_events[0]["exit_status"] == 7

    def test_lifecycle_event_contains_required_fields(self, tmp_path: Path):
        """Lifecycle events contain task, cycle, jobid, attempt, state, timestamp, duration."""
        expdir = _setup_expdir(tmp_path)

        _create_jjob(expdir, "JTEST_FIELDS", '#!/bin/bash\nexit 0\n')

        result = _run_wrapper(
            expdir, "JTEST_FIELDS",
            env_overrides={"jobid": "JTEST_FIELDS.77777"}
        )
        assert result.returncode == 0, f"Failed: {result.stderr}"

        events = _extract_lifecycle_events(result.stderr)
        assert len(events) > 0, "No lifecycle events found"

        required_fields = ["task", "cycle", "jobid", "attempt", "state",
                           "timestamp", "duration_seconds"]
        for event in events:
            for field in required_fields:
                assert field in event, (
                    f"Missing field '{field}' in lifecycle event: {event}"
                )

    def test_lifecycle_event_has_correct_cycle(self, tmp_path: Path):
        """Lifecycle events include the correct cycle (PDY+cyc)."""
        expdir = _setup_expdir(tmp_path)

        _create_jjob(expdir, "JTEST_CYCLE", '#!/bin/bash\nexit 0\n')

        result = _run_wrapper(
            expdir, "JTEST_CYCLE",
            env_overrides={"PDY": "20250601", "cyc": "12"}
        )
        assert result.returncode == 0, f"Failed: {result.stderr}"

        events = _extract_lifecycle_events(result.stderr)
        assert len(events) > 0
        assert events[0]["cycle"] == "2025060112"

    def test_lifecycle_event_has_snapshot_id(self, tmp_path: Path):
        """Lifecycle events include the snapshot_id."""
        expdir = _setup_expdir(tmp_path)

        _create_jjob(expdir, "JTEST_SNAP", '#!/bin/bash\nexit 0\n')

        result = _run_wrapper(
            expdir, "JTEST_SNAP",
            env_overrides={"SNAPSHOT_ID": "v17.0.0+abc123def456"}
        )
        assert result.returncode == 0, f"Failed: {result.stderr}"

        events = _extract_lifecycle_events(result.stderr)
        assert len(events) > 0
        assert events[0]["snapshot_id"] == "v17.0.0+abc123def456"

    def test_aborted_event_on_env_file_missing(self, tmp_path: Path):
        """An 'aborted' lifecycle event is emitted when env file is missing."""
        expdir = _setup_expdir(tmp_path)

        # Remove the env file
        env_file = expdir / "env" / "hera.env"
        env_file.unlink()

        _create_jjob(expdir, "JTEST_ABORT", '#!/bin/bash\nexit 0\n')

        result = _run_wrapper(expdir, "JTEST_ABORT")
        assert result.returncode != 0

        events = _extract_lifecycle_events(result.stderr)
        aborted_events = [e for e in events if e.get("state") == "aborted"]
        assert len(aborted_events) >= 1, "No 'aborted' lifecycle event found"


class TestShellHardening:
    """Tests for shell hardening settings (Req 6.2)."""

    def test_set_x_is_active(self, tmp_path: Path):
        """set -x produces trace output in stderr."""
        expdir = _setup_expdir(tmp_path)

        _create_jjob(expdir, "JTEST_TRACE", '#!/bin/bash\necho "hello"\n')

        result = _run_wrapper(expdir, "JTEST_TRACE")
        assert result.returncode == 0, f"Failed: {result.stderr}"
        # set -x produces + prefixed lines in stderr
        assert "+ " in result.stderr

    def test_ps4_includes_seconds(self, tmp_path: Path):
        """PS4 is set to include $SECONDS for timing."""
        expdir = _setup_expdir(tmp_path)

        _create_jjob(expdir, "JTEST_PS4", '#!/bin/bash\necho "timing"\n')

        result = _run_wrapper(expdir, "JTEST_PS4")
        assert result.returncode == 0, f"Failed: {result.stderr}"
        # PS4='+ $SECONDS + ' means trace lines should contain numbers
        # Look for the pattern "+ <number> + " in stderr
        import re
        assert re.search(r'\+ \d+ \+', result.stderr), (
            "PS4 timing pattern not found in trace output"
        )


def _extract_lifecycle_events(stderr: str) -> list:
    """Extract JSON lifecycle events from stderr output."""
    events = []
    for line in stderr.splitlines():
        line = line.strip()
        if line.startswith("{") and '"event":"task_lifecycle"' in line:
            try:
                event = json.loads(line)
                events.append(event)
            except json.JSONDecodeError:
                continue
    return events
