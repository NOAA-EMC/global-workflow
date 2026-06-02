"""Unit tests for setup_workflow.py Rocoto deprecation guard.

Validates Requirements 14.3 and 1.5: The rocoto subparser has been removed
and a FATAL ERROR is emitted if a Rocoto code path is invoked.

Also validates Requirement 4.4 by asserting (via the structural guard check in
``deployment.rocoto_guard_check``) that the only residual ``rocoto`` references
in ``setup_workflow.py`` belong to the documented deprecation-guard structure.
"""

import sys
import os
from pathlib import Path
from unittest.mock import MagicMock

import pytest

# Mock heavy dependencies before importing setup_workflow
# These modules are not available in the test environment
sys.modules.setdefault('wxflow', MagicMock())
sys.modules.setdefault('applications', MagicMock())
sys.modules.setdefault('applications.application_factory', MagicMock())
sys.modules.setdefault('ecflow', MagicMock())
sys.modules.setdefault('ecflow.ecflow_suite_factory', MagicMock())

# Add the workflow directory to the path so we can import setup_workflow
sys.path.insert(0, os.path.join(os.path.dirname(__file__), '..'))

from setup_workflow import (
    RocotoDecommissionedError,
    rocoto_deprecation_guard,
    _check_for_rocoto_invocation,
)
from deployment.rocoto_guard_check import check_setup_workflow_rocoto_free

# Path to the real module under test.
SETUP_WORKFLOW = Path(__file__).resolve().parents[1] / "setup_workflow.py"


class TestRocotoDeprecationGuard:
    """Tests for the Rocoto deprecation guard function."""

    def test_guard_raises_fatal_error(self):
        """rocoto_deprecation_guard() raises RocotoDecommissionedError."""
        with pytest.raises(RocotoDecommissionedError) as exc_info:
            rocoto_deprecation_guard()
        assert "FATAL ERROR" in str(exc_info.value)
        assert "Requirement 1" in str(exc_info.value)
        assert "decommissioned" in str(exc_info.value)

    def test_guard_message_references_ecflow(self):
        """The error message directs users to ecFlow-only orchestration."""
        with pytest.raises(RocotoDecommissionedError) as exc_info:
            rocoto_deprecation_guard()
        assert "ecFlow-only orchestration" in str(exc_info.value)


class TestCheckForRocotoInvocation:
    """Tests for _check_for_rocoto_invocation helper."""

    def test_rocoto_arg_triggers_fatal_error(self):
        """Passing 'rocoto' as an argument triggers FATAL ERROR."""
        with pytest.raises(RocotoDecommissionedError):
            _check_for_rocoto_invocation(['/some/path', 'rocoto'])

    def test_rocoto_case_insensitive(self):
        """The check is case-insensitive (Rocoto, ROCOTO, etc.)."""
        with pytest.raises(RocotoDecommissionedError):
            _check_for_rocoto_invocation(['/some/path', 'Rocoto'])
        with pytest.raises(RocotoDecommissionedError):
            _check_for_rocoto_invocation(['/some/path', 'ROCOTO'])

    def test_ecflow_arg_passes(self):
        """Passing 'ecflow' does not trigger the guard."""
        # Should not raise
        _check_for_rocoto_invocation(['/some/path', 'ecflow'])

    def test_none_argv_passes(self):
        """None argv does not trigger the guard."""
        _check_for_rocoto_invocation(None)

    def test_empty_argv_passes(self):
        """Empty argv does not trigger the guard."""
        _check_for_rocoto_invocation([])

    def test_unrelated_args_pass(self):
        """Arguments that don't contain 'rocoto' pass through."""
        _check_for_rocoto_invocation(['/some/path', 'ecflow', '--verbosity', '10'])


class TestSetupWorkflowStructurallyRocotoFree:
    """Req 4.4: only documented guard-structure rocoto references remain."""

    def test_setup_workflow_only_contains_guard_references(self):
        """No lone rocoto reference exists outside the deprecation guard."""
        violations = check_setup_workflow_rocoto_free(SETUP_WORKFLOW)
        assert violations == [], (
            "setup_workflow.py must contain only the documented deprecation "
            f"guard references to 'rocoto', but found: {violations}"
        )

    def test_subparser_uses_ecflow_only(self):
        """The argument parser registers ecflow and never a rocoto subparser."""
        source = SETUP_WORKFLOW.read_text(encoding="utf-8")
        assert "add_parser('ecflow'" in source or 'add_parser("ecflow"' in source
        assert "add_parser('rocoto'" not in source
        assert 'add_parser("rocoto"' not in source

    def test_no_rocoto_xml_factory_reference(self):
        """rocoto_xml_factory must not be referenced anywhere in the file."""
        source = SETUP_WORKFLOW.read_text(encoding="utf-8")
        assert "rocoto_xml_factory" not in source
