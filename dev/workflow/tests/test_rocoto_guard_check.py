"""Unit tests for the Rocoto decommission static guard check.

Validates Requirements 4.4 and 4.5 (Design Component 4):

* The structural scan in ``deployment.rocoto_guard_check`` passes only when
  every residual case-insensitive ``rocoto`` occurrence belongs to the
  documented deprecation-guard structure, and fails on a lone non-guard
  occurrence (Req 4.4).
* Invoking a decommissioned Rocoto code path through ``setup_workflow.py``
  raises a FATAL guard error referencing the ecFlow-only policy (Req 4.5).

These tests use no live RAG connection and do not call any network service.
"""

from __future__ import annotations

import os
import sys
import textwrap
from pathlib import Path
from unittest.mock import MagicMock

import pytest

# Add the workflow directory to the path so we can import the package modules.
sys.path.insert(0, os.path.join(os.path.dirname(__file__), ".."))

from deployment.rocoto_guard_check import (
    GUARD_ALLOWLIST_SYMBOLS,
    check_setup_workflow_rocoto_free,
)

# Path to the real module under test.
WORKFLOW_DIR = Path(__file__).resolve().parents[1]
SETUP_WORKFLOW = WORKFLOW_DIR / "setup_workflow.py"


# Mock heavy dependencies before importing setup_workflow (not installed in the
# unit-test environment). Mirrors test_setup_workflow_rocoto_guard.py.
sys.modules.setdefault("wxflow", MagicMock())
sys.modules.setdefault("applications", MagicMock())
sys.modules.setdefault("applications.application_factory", MagicMock())
sys.modules.setdefault("ecflow", MagicMock())
sys.modules.setdefault("ecflow.ecflow_suite_factory", MagicMock())


def _write(tmp_path: Path, name: str, source: str) -> Path:
    target = tmp_path / name
    target.write_text(textwrap.dedent(source), encoding="utf-8")
    return target


# Representative guard cluster: the three documented guard symbols plus their
# FATAL-ERROR message and reachable wiring.
GUARD_CLUSTER_SOURCE = '''
    """Module docstring. Rocoto has been decommissioned per Requirement 1."""


    class RocotoDecommissionedError(RuntimeError):
        """Raised when a decommissioned Rocoto code path is invoked."""
        pass


    def rocoto_deprecation_guard():
        """Emit a FATAL ERROR if a Rocoto code path is invoked."""
        msg = "FATAL ERROR: Rocoto is decommissioned per Requirement 1."
        raise RocotoDecommissionedError(msg)


    def _check_for_rocoto_invocation(argv):
        # Check for Rocoto invocation before argparse processes the args
        if argv is not None:
            for arg in argv:
                if arg.lower() == 'rocoto':
                    rocoto_deprecation_guard()
'''


class TestCheckSetupWorkflowRocotoFree:
    """Structural scan behavior (Req 4.4)."""

    def test_real_setup_workflow_is_clean(self):
        """The actual setup_workflow.py contains only guard-structure refs."""
        violations = check_setup_workflow_rocoto_free(SETUP_WORKFLOW)
        assert violations == [], (
            "setup_workflow.py should expose only the documented deprecation "
            f"guard, but reported: {violations}"
        )

    def test_guard_cluster_passes(self, tmp_path):
        """A file whose only rocoto refs form the guard cluster passes."""
        target = _write(tmp_path, "guard_only.py", GUARD_CLUSTER_SOURCE)
        assert check_setup_workflow_rocoto_free(target) == []

    def test_lone_rocoto_in_reachable_code_fails(self, tmp_path):
        """A lone rocoto reference outside the guard is a violation (Req 4.4)."""
        target = _write(
            tmp_path,
            "lone.py",
            '''
            def build_workflow():
                workflow = rocoto_xml_factory.create()
                return workflow
            ''',
        )
        violations = check_setup_workflow_rocoto_free(target)
        assert len(violations) == 1
        assert "lone 'rocoto'" in violations[0]
        assert "rocoto_xml_factory" in violations[0]

    def test_lone_rocoto_subparser_branch_fails(self, tmp_path):
        """A residual rocoto subparser / conditioned branch is flagged."""
        target = _write(
            tmp_path,
            "subparser.py",
            '''
            def input_args(parser):
                subparsers = parser.add_subparsers(dest='workflow')
                rocoto_parser = subparsers.add_parser('rocoto')
                return rocoto_parser
            ''',
        )
        violations = check_setup_workflow_rocoto_free(target)
        # Two lines mention rocoto outside the guard structure.
        assert len(violations) >= 1
        assert all("lone 'rocoto'" in v for v in violations)

    def test_guard_cluster_plus_lone_reference_fails_only_on_lone(self, tmp_path):
        """A valid guard cluster does not mask a separate lone reference."""
        source = GUARD_CLUSTER_SOURCE + '''

    def stray():
        return rocoto_xml_factory.build()
'''
        target = _write(tmp_path, "mixed.py", source)
        violations = check_setup_workflow_rocoto_free(target)
        assert len(violations) == 1
        assert "rocoto_xml_factory.build()" in violations[0]

    def test_no_rocoto_at_all_passes(self, tmp_path):
        """A file with zero rocoto references passes trivially."""
        target = _write(
            tmp_path,
            "clean.py",
            '''
            def main():
                return "ecflow only"
            ''',
        )
        assert check_setup_workflow_rocoto_free(target) == []

    def test_decommission_doc_comment_passes(self, tmp_path):
        """A decommission prose notice naming Rocoto is allowed (Req 4.3)."""
        target = _write(
            tmp_path,
            "doc.py",
            '''
            """Entry point. Rocoto has been decommissioned per Requirement 1."""

            def main():
                return "ecflow only"
            ''',
        )
        assert check_setup_workflow_rocoto_free(target) == []

    def test_guard_allowlist_symbols_are_recognized(self, tmp_path):
        """Each allowlisted guard symbol reference is permitted on its own line."""
        for symbol in GUARD_ALLOWLIST_SYMBOLS:
            target = _write(
                tmp_path,
                f"ref_{symbol}.py",
                f'''
                def wiring():
                    return {symbol}
                ''',
            )
            assert check_setup_workflow_rocoto_free(target) == [], symbol

    def test_missing_file_reports_error(self, tmp_path):
        """A missing target file yields a FATAL ERROR message, not a crash."""
        violations = check_setup_workflow_rocoto_free(tmp_path / "nope.py")
        assert len(violations) == 1
        assert "FATAL ERROR" in violations[0]


class TestRocotoInvocationRaisesFatalGuard:
    """Invoking a rocoto path raises the FATAL guard (Req 4.5)."""

    def test_invoking_rocoto_raises_decommissioned_error(self):
        from setup_workflow import (
            RocotoDecommissionedError,
            _check_for_rocoto_invocation,
        )

        with pytest.raises(RocotoDecommissionedError) as exc_info:
            _check_for_rocoto_invocation(["/path/to/expdir", "rocoto"])
        assert "FATAL ERROR" in str(exc_info.value)
        assert "ecFlow-only orchestration" in str(exc_info.value)
