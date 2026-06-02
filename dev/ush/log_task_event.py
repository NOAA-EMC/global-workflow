#!/usr/bin/env python3
"""Log structured task lifecycle events to the workflow state database.

This utility writes task lifecycle events (init, start, succeeded, failed,
aborted, complete) to a SQLite database at <EXPDIR>/workflow/state.db.
It is called by the Universal_Wrapper at each lifecycle transition.

Schema:
    task_events table with columns:
        id, snapshot_id, git_commit, cycle, family_path, task_name,
        attempt, scheduler_job_id, state, exit_status, timestamp,
        duration_seconds

Usage:
    log_task_event.py --task <name> --cycle <YYYYMMDDHH> --jobid <id> \\
        --attempt <n> --state <state> [--exit-status <code>] \\
        [--duration <seconds>] [--db-path <path>]

Environment Variables:
    EXPDIR          - Experiment directory (used for default db-path)
    SNAPSHOT_ID     - Deployment snapshot identifier
    GIT_COMMIT      - Source git commit hash
    ECF_NAME        - ecFlow task path (used to derive family_path)
"""

import argparse
import os
import sqlite3
import sys
from datetime import datetime, timezone
from pathlib import Path
from typing import List, Optional

# Valid lifecycle states emitted by the Universal_Wrapper
VALID_STATES = frozenset({
    "init", "start", "succeeded", "failed", "aborted", "complete"
})

CREATE_TABLE_SQL = """\
CREATE TABLE IF NOT EXISTS task_events (
    id INTEGER PRIMARY KEY AUTOINCREMENT,
    snapshot_id TEXT NOT NULL,
    git_commit TEXT NOT NULL,
    cycle TEXT NOT NULL,
    family_path TEXT NOT NULL,
    task_name TEXT NOT NULL,
    attempt INTEGER NOT NULL,
    scheduler_job_id TEXT,
    state TEXT NOT NULL,
    exit_status INTEGER,
    timestamp TEXT NOT NULL,
    duration_seconds INTEGER
)
"""

CREATE_INDEX_CYCLE_SQL = """\
CREATE INDEX IF NOT EXISTS idx_task_events_cycle
    ON task_events(cycle, task_name)
"""

CREATE_INDEX_STATE_SQL = """\
CREATE INDEX IF NOT EXISTS idx_task_events_state
    ON task_events(state)
"""

INSERT_EVENT_SQL = """\
INSERT INTO task_events (
    snapshot_id, git_commit, cycle, family_path, task_name,
    attempt, scheduler_job_id, state, exit_status, timestamp,
    duration_seconds
) VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)
"""


def get_default_db_path() -> str:
    """Return the default database path from EXPDIR environment variable."""
    expdir = os.environ.get("EXPDIR", "")
    if not expdir:
        return ""
    return str(Path(expdir) / "workflow" / "state.db")


def derive_family_path(task_name: str) -> str:
    """Derive the family path from ECF_NAME or return empty string.

    ECF_NAME is set by ecFlow and has the form:
        /<suite>/<family1>/<family2>/.../<task_name>

    The family_path is everything between the suite and the task name.
    If ECF_NAME is not set, returns an empty string.
    """
    ecf_name = os.environ.get("ECF_NAME", "")
    if not ecf_name:
        return ""
    # ECF_NAME format: /suite/family1/family2/.../task_name
    parts = ecf_name.strip("/").split("/")
    if len(parts) < 2:
        return ""
    # Skip suite (first) and task (last), join the middle as family path
    family_parts = parts[1:-1]
    return "/".join(family_parts) if family_parts else ""


def init_database(db_path: str) -> sqlite3.Connection:
    """Create the database, table, and indexes if they don't exist.

    Parameters
    ----------
    db_path : str
        Path to the SQLite database file.

    Returns
    -------
    sqlite3.Connection
        Open database connection.
    """
    # Ensure the parent directory exists
    db_dir = Path(db_path).parent
    db_dir.mkdir(parents=True, exist_ok=True)

    conn = sqlite3.connect(db_path)
    cursor = conn.cursor()
    cursor.execute(CREATE_TABLE_SQL)
    cursor.execute(CREATE_INDEX_CYCLE_SQL)
    cursor.execute(CREATE_INDEX_STATE_SQL)
    conn.commit()
    return conn


def insert_event(conn: sqlite3.Connection,
                 snapshot_id: str,
                 git_commit: str,
                 cycle: str,
                 family_path: str,
                 task_name: str,
                 attempt: int,
                 scheduler_job_id: str,
                 state: str,
                 exit_status: Optional[int],
                 timestamp: str,
                 duration_seconds: Optional[int]) -> int:
    """Insert a task lifecycle event into the database.

    Parameters
    ----------
    conn : sqlite3.Connection
        Open database connection.
    snapshot_id : str
        Deployment snapshot identifier.
    git_commit : str
        Source git commit hash.
    cycle : str
        Forecast cycle (e.g., '2025011500').
    family_path : str
        ecFlow family path (e.g., 'gdas/atmos/analysis').
    task_name : str
        Task name (e.g., 'anal').
    attempt : int
        Attempt number (1-based).
    scheduler_job_id : str
        Scheduler job ID (e.g., Slurm/PBS job ID).
    state : str
        Lifecycle state (init, start, succeeded, failed, aborted, complete).
    exit_status : int or None
        Exit status code (None for init/start states).
    timestamp : str
        ISO 8601 timestamp.
    duration_seconds : int or None
        Task duration in seconds (None if not yet complete).

    Returns
    -------
    int
        Row ID of the inserted record.
    """
    cursor = conn.cursor()
    cursor.execute(INSERT_EVENT_SQL, (
        snapshot_id, git_commit, cycle, family_path, task_name,
        attempt, scheduler_job_id, state, exit_status, timestamp,
        duration_seconds
    ))
    conn.commit()
    return cursor.lastrowid


def parse_args(argv: Optional[List[str]] = None) -> argparse.Namespace:
    """Parse command-line arguments.

    Parameters
    ----------
    argv : list of str, optional
        Command-line arguments. Defaults to sys.argv[1:].

    Returns
    -------
    argparse.Namespace
        Parsed arguments.
    """
    parser = argparse.ArgumentParser(
        description="Log task lifecycle events to the workflow state database.",
        formatter_class=argparse.ArgumentDefaultsHelpFormatter
    )
    parser.add_argument(
        "--task", required=True,
        help="Task name (e.g., 'anal', 'fcst')"
    )
    parser.add_argument(
        "--cycle", required=True,
        help="Forecast cycle identifier (e.g., '2025011500')"
    )
    parser.add_argument(
        "--jobid", required=True,
        help="Scheduler job ID"
    )
    parser.add_argument(
        "--attempt", required=True, type=int,
        help="Attempt number (1-based)"
    )
    parser.add_argument(
        "--state", required=True, choices=sorted(VALID_STATES),
        help="Task lifecycle state"
    )
    parser.add_argument(
        "--exit-status", type=int, default=None,
        help="Exit status code (optional, typically set for succeeded/failed)"
    )
    parser.add_argument(
        "--duration", type=int, default=None,
        help="Task duration in seconds (optional)"
    )
    parser.add_argument(
        "--db-path", default=None,
        help="Path to SQLite database (default: $EXPDIR/workflow/state.db)"
    )

    args = parser.parse_args(argv)
    return args


def main(argv: Optional[List[str]] = None) -> int:
    """Main entry point for the log_task_event CLI.

    Parameters
    ----------
    argv : list of str, optional
        Command-line arguments. Defaults to sys.argv[1:].

    Returns
    -------
    int
        Exit code (0 for success, 1 for error).
    """
    args = parse_args(argv)

    # Resolve database path
    db_path = args.db_path
    if db_path is None:
        db_path = get_default_db_path()
    if not db_path:
        print("FATAL ERROR: --db-path not specified and EXPDIR not set",
              file=sys.stderr)
        return 1

    # Get snapshot_id and git_commit from environment
    snapshot_id = os.environ.get("SNAPSHOT_ID", "unknown")
    git_commit = os.environ.get("GIT_COMMIT", "unknown")

    # Derive family_path from ECF_NAME
    family_path = derive_family_path(args.task)

    # Generate ISO 8601 timestamp
    timestamp = datetime.now(timezone.utc).strftime("%Y-%m-%dT%H:%M:%SZ")

    try:
        conn = init_database(db_path)
        insert_event(
            conn=conn,
            snapshot_id=snapshot_id,
            git_commit=git_commit,
            cycle=args.cycle,
            family_path=family_path,
            task_name=args.task,
            attempt=args.attempt,
            scheduler_job_id=args.jobid,
            state=args.state,
            exit_status=args.exit_status,
            timestamp=timestamp,
            duration_seconds=args.duration
        )
        conn.close()
    except sqlite3.Error as e:
        print(f"FATAL ERROR: Database error: {e}", file=sys.stderr)
        return 1
    except OSError as e:
        print(f"FATAL ERROR: File system error: {e}", file=sys.stderr)
        return 1

    return 0


if __name__ == "__main__":
    sys.exit(main())
