"""Unit tests for dev/ush/log_task_event.py utility.

Tests cover:
- Database creation and schema initialization
- Event insertion with all fields
- Index creation on (cycle, task_name) and (state)
- CLI argument parsing
- Default db-path resolution from EXPDIR
- Family path derivation from ECF_NAME
- Error handling for missing EXPDIR and db-path
"""

import os
import sqlite3
import sys
import tempfile
from pathlib import Path
from unittest.mock import patch

import pytest

# Add dev/ush to path so we can import the module
sys.path.insert(0, str(Path(__file__).resolve().parents[2] / "ush"))
import log_task_event  # noqa: E402


class TestInitDatabase:
    """Tests for database initialization."""

    def test_creates_database_file(self, tmp_path):
        """Database file is created if it does not exist."""
        db_path = str(tmp_path / "workflow" / "state.db")
        conn = log_task_event.init_database(db_path)
        assert Path(db_path).exists()
        conn.close()

    def test_creates_parent_directories(self, tmp_path):
        """Parent directories are created if they do not exist."""
        db_path = str(tmp_path / "deep" / "nested" / "state.db")
        conn = log_task_event.init_database(db_path)
        assert Path(db_path).parent.exists()
        conn.close()

    def test_creates_task_events_table(self, tmp_path):
        """The task_events table is created with the correct schema."""
        db_path = str(tmp_path / "state.db")
        conn = log_task_event.init_database(db_path)

        cursor = conn.cursor()
        cursor.execute(
            "SELECT sql FROM sqlite_master WHERE type='table' AND name='task_events'"
        )
        row = cursor.fetchone()
        assert row is not None
        schema = row[0]

        # Verify all expected columns are present
        expected_columns = [
            "snapshot_id", "git_commit", "cycle", "family_path",
            "task_name", "attempt", "scheduler_job_id", "state",
            "exit_status", "timestamp", "duration_seconds"
        ]
        for col in expected_columns:
            assert col in schema

        conn.close()

    def test_creates_cycle_index(self, tmp_path):
        """Index on (cycle, task_name) is created."""
        db_path = str(tmp_path / "state.db")
        conn = log_task_event.init_database(db_path)

        cursor = conn.cursor()
        cursor.execute(
            "SELECT name FROM sqlite_master WHERE type='index' "
            "AND name='idx_task_events_cycle'"
        )
        assert cursor.fetchone() is not None
        conn.close()

    def test_creates_state_index(self, tmp_path):
        """Index on (state) is created."""
        db_path = str(tmp_path / "state.db")
        conn = log_task_event.init_database(db_path)

        cursor = conn.cursor()
        cursor.execute(
            "SELECT name FROM sqlite_master WHERE type='index' "
            "AND name='idx_task_events_state'"
        )
        assert cursor.fetchone() is not None
        conn.close()

    def test_idempotent_initialization(self, tmp_path):
        """Calling init_database twice does not raise errors."""
        db_path = str(tmp_path / "state.db")
        conn1 = log_task_event.init_database(db_path)
        conn1.close()
        # Second call should not raise
        conn2 = log_task_event.init_database(db_path)
        conn2.close()


class TestInsertEvent:
    """Tests for event insertion."""

    def test_insert_complete_event(self, tmp_path):
        """A complete event record is inserted correctly."""
        db_path = str(tmp_path / "state.db")
        conn = log_task_event.init_database(db_path)

        row_id = log_task_event.insert_event(
            conn=conn,
            snapshot_id="v17.0.0+a3f8c1d2e4b6",
            git_commit="abc123def456",
            cycle="2025011500",
            family_path="gdas/atmos/analysis",
            task_name="anal",
            attempt=1,
            scheduler_job_id="12345678",
            state="succeeded",
            exit_status=0,
            timestamp="2025-01-15T14:30:00Z",
            duration_seconds=120
        )

        assert row_id == 1

        cursor = conn.cursor()
        cursor.execute("SELECT * FROM task_events WHERE id = ?", (row_id,))
        row = cursor.fetchone()
        assert row is not None
        # id, snapshot_id, git_commit, cycle, family_path, task_name,
        # attempt, scheduler_job_id, state, exit_status, timestamp, duration_seconds
        assert row[1] == "v17.0.0+a3f8c1d2e4b6"
        assert row[2] == "abc123def456"
        assert row[3] == "2025011500"
        assert row[4] == "gdas/atmos/analysis"
        assert row[5] == "anal"
        assert row[6] == 1
        assert row[7] == "12345678"
        assert row[8] == "succeeded"
        assert row[9] == 0
        assert row[10] == "2025-01-15T14:30:00Z"
        assert row[11] == 120

        conn.close()

    def test_insert_event_with_null_optional_fields(self, tmp_path):
        """Events with None exit_status and duration are stored as NULL."""
        db_path = str(tmp_path / "state.db")
        conn = log_task_event.init_database(db_path)

        row_id = log_task_event.insert_event(
            conn=conn,
            snapshot_id="v17.0.0+a3f8c1d2e4b6",
            git_commit="abc123def456",
            cycle="2025011500",
            family_path="gdas/atmos/forecast",
            task_name="fcst",
            attempt=1,
            scheduler_job_id="99999",
            state="init",
            exit_status=None,
            timestamp="2025-01-15T14:00:00Z",
            duration_seconds=None
        )

        cursor = conn.cursor()
        cursor.execute("SELECT exit_status, duration_seconds FROM task_events WHERE id = ?",
                       (row_id,))
        row = cursor.fetchone()
        assert row[0] is None
        assert row[1] is None

        conn.close()

    def test_insert_multiple_events(self, tmp_path):
        """Multiple events can be inserted for the same task."""
        db_path = str(tmp_path / "state.db")
        conn = log_task_event.init_database(db_path)

        for state in ["init", "start", "succeeded"]:
            log_task_event.insert_event(
                conn=conn,
                snapshot_id="v17.0.0+a3f8c1d2e4b6",
                git_commit="abc123",
                cycle="2025011500",
                family_path="gdas/atmos/analysis",
                task_name="anal",
                attempt=1,
                scheduler_job_id="12345",
                state=state,
                exit_status=0 if state == "succeeded" else None,
                timestamp="2025-01-15T14:00:00Z",
                duration_seconds=60 if state == "succeeded" else None
            )

        cursor = conn.cursor()
        cursor.execute("SELECT COUNT(*) FROM task_events")
        assert cursor.fetchone()[0] == 3

        conn.close()


class TestDeriveFamilyPath:
    """Tests for family path derivation from ECF_NAME."""

    def test_standard_ecf_name(self):
        """Family path is derived from a standard ECF_NAME."""
        with patch.dict(os.environ, {"ECF_NAME": "/gfs_v17/gdas/atmos/analysis/anal"}):
            result = log_task_event.derive_family_path("anal")
        assert result == "gdas/atmos/analysis"

    def test_single_family(self):
        """Family path with a single family level."""
        with patch.dict(os.environ, {"ECF_NAME": "/gfs_v17/post/post_f000"}):
            result = log_task_event.derive_family_path("post_f000")
        assert result == "post"

    def test_no_family(self):
        """Empty family path when task is directly under suite."""
        with patch.dict(os.environ, {"ECF_NAME": "/gfs_v17/prep"}):
            result = log_task_event.derive_family_path("prep")
        assert result == ""

    def test_ecf_name_not_set(self):
        """Empty string when ECF_NAME is not set."""
        env = os.environ.copy()
        env.pop("ECF_NAME", None)
        with patch.dict(os.environ, env, clear=True):
            result = log_task_event.derive_family_path("anal")
        assert result == ""

    def test_ecf_name_empty(self):
        """Empty string when ECF_NAME is empty."""
        with patch.dict(os.environ, {"ECF_NAME": ""}):
            result = log_task_event.derive_family_path("anal")
        assert result == ""


class TestGetDefaultDbPath:
    """Tests for default database path resolution."""

    def test_expdir_set(self):
        """Default path is derived from EXPDIR."""
        with patch.dict(os.environ, {"EXPDIR": "/path/to/exp"}):
            result = log_task_event.get_default_db_path()
        assert result == "/path/to/exp/workflow/state.db"

    def test_expdir_not_set(self):
        """Empty string when EXPDIR is not set."""
        env = os.environ.copy()
        env.pop("EXPDIR", None)
        with patch.dict(os.environ, env, clear=True):
            result = log_task_event.get_default_db_path()
        assert result == ""


class TestParseArgs:
    """Tests for CLI argument parsing."""

    def test_all_required_args(self):
        """All required arguments are parsed correctly."""
        args = log_task_event.parse_args([
            "--task", "anal",
            "--cycle", "2025011500",
            "--jobid", "12345",
            "--attempt", "1",
            "--state", "succeeded"
        ])
        assert args.task == "anal"
        assert args.cycle == "2025011500"
        assert args.jobid == "12345"
        assert args.attempt == 1
        assert args.state == "succeeded"
        assert args.exit_status is None
        assert args.duration is None
        assert args.db_path is None

    def test_all_optional_args(self):
        """Optional arguments are parsed correctly."""
        args = log_task_event.parse_args([
            "--task", "fcst",
            "--cycle", "2025011506",
            "--jobid", "99999",
            "--attempt", "2",
            "--state", "failed",
            "--exit-status", "1",
            "--duration", "3600",
            "--db-path", "/custom/path/state.db"
        ])
        assert args.exit_status == 1
        assert args.duration == 3600
        assert args.db_path == "/custom/path/state.db"

    def test_invalid_state_rejected(self):
        """Invalid state values are rejected."""
        with pytest.raises(SystemExit):
            log_task_event.parse_args([
                "--task", "anal",
                "--cycle", "2025011500",
                "--jobid", "12345",
                "--attempt", "1",
                "--state", "invalid_state"
            ])

    def test_missing_required_arg(self):
        """Missing required arguments cause exit."""
        with pytest.raises(SystemExit):
            log_task_event.parse_args(["--task", "anal"])


class TestMain:
    """Tests for the main entry point."""

    def test_successful_event_logging(self, tmp_path):
        """Main function logs an event and returns 0."""
        db_path = str(tmp_path / "state.db")
        env = {
            "SNAPSHOT_ID": "v17.0.0+a3f8c1d2e4b6",
            "GIT_COMMIT": "abc123def456",
            "ECF_NAME": "/gfs_v17/gdas/atmos/analysis/anal"
        }
        with patch.dict(os.environ, env):
            rc = log_task_event.main([
                "--task", "anal",
                "--cycle", "2025011500",
                "--jobid", "12345",
                "--attempt", "1",
                "--state", "succeeded",
                "--exit-status", "0",
                "--duration", "120",
                "--db-path", db_path
            ])

        assert rc == 0

        # Verify the event was written
        conn = sqlite3.connect(db_path)
        cursor = conn.cursor()
        cursor.execute("SELECT COUNT(*) FROM task_events")
        assert cursor.fetchone()[0] == 1
        cursor.execute("SELECT snapshot_id, git_commit, family_path FROM task_events")
        row = cursor.fetchone()
        assert row[0] == "v17.0.0+a3f8c1d2e4b6"
        assert row[1] == "abc123def456"
        assert row[2] == "gdas/atmos/analysis"
        conn.close()

    def test_missing_db_path_and_expdir(self):
        """Returns 1 when neither --db-path nor EXPDIR is set."""
        env = os.environ.copy()
        env.pop("EXPDIR", None)
        with patch.dict(os.environ, env, clear=True):
            rc = log_task_event.main([
                "--task", "anal",
                "--cycle", "2025011500",
                "--jobid", "12345",
                "--attempt", "1",
                "--state", "init"
            ])
        assert rc == 1

    def test_uses_expdir_default(self, tmp_path):
        """Uses EXPDIR/workflow/state.db when --db-path not specified."""
        expdir = str(tmp_path / "expdir")
        env = {
            "EXPDIR": expdir,
            "SNAPSHOT_ID": "v1.0.0+test",
            "GIT_COMMIT": "deadbeef",
            "ECF_NAME": "/suite/family/task"
        }
        with patch.dict(os.environ, env):
            rc = log_task_event.main([
                "--task", "task",
                "--cycle", "2025010100",
                "--jobid", "111",
                "--attempt", "1",
                "--state", "start"
            ])

        assert rc == 0
        assert Path(expdir, "workflow", "state.db").exists()

    def test_defaults_snapshot_and_commit_to_unknown(self, tmp_path):
        """When env vars not set, defaults to 'unknown'."""
        db_path = str(tmp_path / "state.db")
        env = os.environ.copy()
        env.pop("SNAPSHOT_ID", None)
        env.pop("GIT_COMMIT", None)
        env.pop("ECF_NAME", None)
        with patch.dict(os.environ, env, clear=True):
            rc = log_task_event.main([
                "--task", "anal",
                "--cycle", "2025011500",
                "--jobid", "12345",
                "--attempt", "1",
                "--state", "init",
                "--db-path", db_path
            ])

        assert rc == 0
        conn = sqlite3.connect(db_path)
        cursor = conn.cursor()
        cursor.execute("SELECT snapshot_id, git_commit FROM task_events")
        row = cursor.fetchone()
        assert row[0] == "unknown"
        assert row[1] == "unknown"
        conn.close()
