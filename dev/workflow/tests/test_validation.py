"""Unit tests for the input validation stage.

Tests the deployment pipeline's input validation:
  - EXPDIR immutability guard (refuse if manifest exists)
  - Pinned version verification (wxflow/uwtools)
  - Git state check (clean working tree)

Traces to: Requirements 3.5, 9.5
"""

import os
import sys

import pytest

sys.path.insert(0, os.path.join(os.path.dirname(__file__), ".."))

from pathlib import Path
from unittest.mock import patch

from deployment.validation import (
    ValidationError,
    ValidationResult,
    check_expdir_immutability,
    check_git_state,
    check_pinned_versions,
    validate_inputs,
    _extract_snapshot_id,
    _parse_pinned_versions,
)


# ---------------------------------------------------------------------------
# Helpers
# ---------------------------------------------------------------------------


def _make_file(tmp_path: Path, relpath: str, content: str) -> Path:
    """Create a file at tmp_path/relpath with given content."""
    filepath = tmp_path / relpath
    filepath.parent.mkdir(parents=True, exist_ok=True)
    filepath.write_text(content, encoding="utf-8")
    return filepath


# ---------------------------------------------------------------------------
# Tests: EXPDIR immutability guard
# ---------------------------------------------------------------------------


class TestExpdirImmutability:
    """Tests for EXPDIR immutability check (Requirement 3.5)."""

    def test_nonexistent_expdir_passes(self, tmp_path):
        """Non-existent EXPDIR should pass validation."""
        expdir = tmp_path / "nonexistent_expdir"
        # Should not raise
        check_expdir_immutability(expdir)

    def test_empty_expdir_passes(self, tmp_path):
        """Empty EXPDIR (no manifest) should pass validation."""
        expdir = tmp_path / "expdir"
        expdir.mkdir()
        # Should not raise
        check_expdir_immutability(expdir)

    def test_expdir_with_manifest_raises(self, tmp_path):
        """EXPDIR with manifest.yaml should raise ValidationError."""
        expdir = tmp_path / "expdir"
        _make_file(
            tmp_path,
            "expdir/manifest.yaml",
            'snapshot_id: "v17.0.0+a3f8c1d2e4b6"\ngit_commit: abc123\n',
        )

        with pytest.raises(ValidationError) as exc_info:
            check_expdir_immutability(expdir)

        assert "FATAL ERROR" in str(exc_info.value)
        assert "v17.0.0+a3f8c1d2e4b6" in str(exc_info.value)

    def test_expdir_with_manifest_no_snapshot_id_raises(self, tmp_path):
        """EXPDIR with manifest.yaml but no snapshot_id still raises."""
        expdir = tmp_path / "expdir"
        _make_file(
            tmp_path,
            "expdir/manifest.yaml",
            "git_commit: abc123\ndeployed_by: user\n",
        )

        with pytest.raises(ValidationError) as exc_info:
            check_expdir_immutability(expdir)

        assert "FATAL ERROR" in str(exc_info.value)
        assert "sealed" in str(exc_info.value)

    def test_expdir_with_other_files_no_manifest_passes(self, tmp_path):
        """EXPDIR with files but no manifest.yaml should pass."""
        expdir = tmp_path / "expdir"
        _make_file(tmp_path, "expdir/jobs/JGFS_FORECAST", "#!/bin/bash\n")
        _make_file(tmp_path, "expdir/parm/config.yaml", "key: value\n")

        # Should not raise
        check_expdir_immutability(expdir)


# ---------------------------------------------------------------------------
# Tests: Snapshot_ID extraction
# ---------------------------------------------------------------------------


class TestExtractSnapshotId:
    """Tests for _extract_snapshot_id helper."""

    def test_extracts_quoted_snapshot_id(self, tmp_path):
        """Should extract snapshot_id from quoted YAML value."""
        manifest = _make_file(
            tmp_path,
            "manifest.yaml",
            'snapshot_id: "v17.0.0+a3f8c1d2e4b6"\n',
        )
        assert _extract_snapshot_id(manifest) == "v17.0.0+a3f8c1d2e4b6"

    def test_extracts_unquoted_snapshot_id(self, tmp_path):
        """Should extract snapshot_id from unquoted YAML value."""
        manifest = _make_file(
            tmp_path,
            "manifest.yaml",
            "snapshot_id: v16.5.0+deadbeef1234\n",
        )
        assert _extract_snapshot_id(manifest) == "v16.5.0+deadbeef1234"

    def test_returns_none_for_missing_field(self, tmp_path):
        """Should return None if snapshot_id field is missing."""
        manifest = _make_file(
            tmp_path,
            "manifest.yaml",
            "git_commit: abc123\ndeployed_by: user\n",
        )
        assert _extract_snapshot_id(manifest) is None

    def test_returns_none_for_nonexistent_file(self, tmp_path):
        """Should return None for non-existent file."""
        assert _extract_snapshot_id(tmp_path / "nonexistent.yaml") is None


# ---------------------------------------------------------------------------
# Tests: Pinned version verification
# ---------------------------------------------------------------------------


class TestPinnedVersions:
    """Tests for pinned version verification (Requirement 9.5)."""

    def test_missing_requirements_file_errors(self, tmp_path):
        """Missing requirements.txt should produce an error."""
        result = check_pinned_versions(tmp_path / "nonexistent.txt")
        assert not result.passed
        assert any("not found" in e for e in result.errors)

    def test_no_pinned_versions_passes(self, tmp_path):
        """Requirements with no exact pins should pass (nothing to check)."""
        req_file = _make_file(
            tmp_path,
            "requirements.txt",
            "numpy>=1.23\njinja2>=3.0\n",
        )
        result = check_pinned_versions(Path(req_file))
        assert result.passed

    def test_matching_version_passes(self, tmp_path):
        """Installed version matching pinned version should pass."""
        req_file = _make_file(
            tmp_path,
            "requirements.txt",
            "pytest==8.4.2\n",
        )
        # pytest is installed in the test environment
        with patch(
            "deployment.validation._get_installed_version",
            return_value="8.4.2",
        ):
            result = check_pinned_versions(Path(req_file))
        assert result.passed

    def test_mismatched_version_errors(self, tmp_path):
        """Installed version not matching pinned version should error."""
        req_file = _make_file(
            tmp_path,
            "requirements.txt",
            "wxflow==0.3.0\n",
        )
        with patch(
            "deployment.validation._get_installed_version",
            return_value="0.2.0",
        ):
            result = check_pinned_versions(Path(req_file))

        assert not result.passed
        assert any("FATAL ERROR" in e for e in result.errors)
        assert any("wxflow" in e for e in result.errors)
        assert any("0.2.0 != pinned 0.3.0" in e for e in result.errors)

    def test_uninstalled_package_warns(self, tmp_path):
        """Uninstalled pinned package should produce a warning, not error."""
        req_file = _make_file(
            tmp_path,
            "requirements.txt",
            "wxflow==0.3.0\nuwtools==2.16.0\n",
        )
        with patch(
            "deployment.validation._get_installed_version",
            return_value=None,
        ):
            result = check_pinned_versions(Path(req_file))

        assert result.passed  # Warnings don't fail validation
        assert len(result.warnings) == 2
        assert any("wxflow" in w for w in result.warnings)
        assert any("uwtools" in w for w in result.warnings)

    def test_multiple_pins_one_mismatch(self, tmp_path):
        """Multiple pinned packages with one mismatch should error."""
        req_file = _make_file(
            tmp_path,
            "requirements.txt",
            "wxflow==0.3.0\nuwtools==2.16.0\n",
        )

        def mock_version(pkg):
            if pkg == "wxflow":
                return "0.3.0"
            elif pkg == "uwtools":
                return "2.15.0"
            return None

        with patch(
            "deployment.validation._get_installed_version",
            side_effect=mock_version,
        ):
            result = check_pinned_versions(Path(req_file))

        assert not result.passed
        assert any("uwtools" in e for e in result.errors)
        # wxflow should not be in errors
        assert not any("wxflow" in e for e in result.errors)


# ---------------------------------------------------------------------------
# Tests: Parse pinned versions
# ---------------------------------------------------------------------------


class TestParsePinnedVersions:
    """Tests for _parse_pinned_versions helper."""

    def test_parses_exact_pins(self, tmp_path):
        """Should parse exact version pins (==)."""
        req_file = _make_file(
            tmp_path,
            "requirements.txt",
            "wxflow==0.3.0\nuwtools==2.16.0\nnumpy>=1.23\n",
        )
        pinned = _parse_pinned_versions(Path(req_file))
        assert pinned == {"wxflow": "0.3.0", "uwtools": "2.16.0"}

    def test_ignores_comments_and_blanks(self, tmp_path):
        """Should ignore comments and blank lines."""
        req_file = _make_file(
            tmp_path,
            "requirements.txt",
            "# This is a comment\n\nwxflow==0.3.0\n  # Another comment\n",
        )
        pinned = _parse_pinned_versions(Path(req_file))
        assert pinned == {"wxflow": "0.3.0"}

    def test_case_insensitive_package_names(self, tmp_path):
        """Package names should be normalized to lowercase."""
        req_file = _make_file(
            tmp_path,
            "requirements.txt",
            "WxFlow==0.3.0\nUWTools==2.16.0\n",
        )
        pinned = _parse_pinned_versions(Path(req_file))
        assert "wxflow" in pinned
        assert "uwtools" in pinned


# ---------------------------------------------------------------------------
# Tests: Git state check
# ---------------------------------------------------------------------------


class TestGitState:
    """Tests for git state check."""

    def test_clean_repo_passes(self, tmp_path):
        """Clean git repo should pass without warnings."""
        with patch("subprocess.run") as mock_run:
            mock_run.return_value = type(
                "Result", (), {"returncode": 0, "stdout": "", "stderr": ""}
            )()
            result = check_git_state(tmp_path)

        assert result.passed
        assert len(result.warnings) == 0

    def test_dirty_repo_warns(self, tmp_path):
        """Dirty git repo should produce a warning."""
        with patch("subprocess.run") as mock_run:
            mock_run.return_value = type(
                "Result",
                (),
                {
                    "returncode": 0,
                    "stdout": " M file1.py\n?? newfile.txt\n",
                    "stderr": "",
                },
            )()
            result = check_git_state(tmp_path)

        assert result.passed  # Dirty state is a warning, not an error
        assert len(result.warnings) == 1
        assert "not clean" in result.warnings[0]
        assert "2 modified/untracked" in result.warnings[0]

    def test_git_not_found_warns(self, tmp_path):
        """Missing git command should produce a warning."""
        with patch("subprocess.run", side_effect=FileNotFoundError):
            result = check_git_state(tmp_path)

        assert result.passed
        assert len(result.warnings) == 1
        assert "not found" in result.warnings[0]

    def test_git_timeout_warns(self, tmp_path):
        """Git timeout should produce a warning."""
        import subprocess

        with patch("subprocess.run", side_effect=subprocess.TimeoutExpired("git", 30)):
            result = check_git_state(tmp_path)

        assert result.passed
        assert len(result.warnings) == 1
        assert "timed out" in result.warnings[0]

    def test_git_error_warns(self, tmp_path):
        """Git returning non-zero should produce a warning."""
        with patch("subprocess.run") as mock_run:
            mock_run.return_value = type(
                "Result",
                (),
                {
                    "returncode": 128,
                    "stdout": "",
                    "stderr": "fatal: not a git repository",
                },
            )()
            result = check_git_state(tmp_path)

        assert result.passed
        assert len(result.warnings) == 1
        assert "not a git repository" in result.warnings[0]


# ---------------------------------------------------------------------------
# Tests: validate_inputs integration
# ---------------------------------------------------------------------------


class TestValidateInputs:
    """Tests for the full validate_inputs function."""

    def test_fresh_expdir_valid_versions_passes(self, tmp_path):
        """Fresh EXPDIR with matching versions should pass."""
        expdir = tmp_path / "expdir"
        req_file = _make_file(
            tmp_path,
            "requirements.txt",
            "wxflow==0.3.0\n",
        )

        with patch(
            "deployment.validation._get_installed_version",
            return_value="0.3.0",
        ):
            with patch("subprocess.run") as mock_run:
                mock_run.return_value = type(
                    "Result", (), {"returncode": 0, "stdout": "", "stderr": ""}
                )()
                result = validate_inputs(
                    expdir=expdir,
                    requirements_path=Path(req_file),
                    repo_path=tmp_path,
                )

        assert result.passed

    def test_sealed_expdir_raises_immediately(self, tmp_path):
        """Sealed EXPDIR should raise ValidationError immediately."""
        expdir = tmp_path / "expdir"
        _make_file(
            tmp_path,
            "expdir/manifest.yaml",
            'snapshot_id: "v17.0.0+a3f8c1d2e4b6"\n',
        )
        req_file = _make_file(tmp_path, "requirements.txt", "wxflow==0.3.0\n")

        with pytest.raises(ValidationError) as exc_info:
            validate_inputs(
                expdir=expdir,
                requirements_path=Path(req_file),
                repo_path=tmp_path,
            )

        assert "FATAL ERROR" in str(exc_info.value)
        assert "v17.0.0+a3f8c1d2e4b6" in str(exc_info.value)

    def test_version_mismatch_fails(self, tmp_path):
        """Version mismatch should fail validation."""
        expdir = tmp_path / "expdir"
        req_file = _make_file(
            tmp_path,
            "requirements.txt",
            "wxflow==0.3.0\n",
        )

        with patch(
            "deployment.validation._get_installed_version",
            return_value="0.2.0",
        ):
            with patch("subprocess.run") as mock_run:
                mock_run.return_value = type(
                    "Result", (), {"returncode": 0, "stdout": "", "stderr": ""}
                )()
                result = validate_inputs(
                    expdir=expdir,
                    requirements_path=Path(req_file),
                    repo_path=tmp_path,
                )

        assert not result.passed
        assert any("FATAL ERROR" in e for e in result.errors)

    def test_dirty_git_still_passes(self, tmp_path):
        """Dirty git state should warn but not fail."""
        expdir = tmp_path / "expdir"
        req_file = _make_file(
            tmp_path,
            "requirements.txt",
            "wxflow==0.3.0\n",
        )

        with patch(
            "deployment.validation._get_installed_version",
            return_value="0.3.0",
        ):
            with patch("subprocess.run") as mock_run:
                mock_run.return_value = type(
                    "Result",
                    (),
                    {
                        "returncode": 0,
                        "stdout": " M dirty_file.py\n",
                        "stderr": "",
                    },
                )()
                result = validate_inputs(
                    expdir=expdir,
                    requirements_path=Path(req_file),
                    repo_path=tmp_path,
                )

        assert result.passed
        assert len(result.warnings) >= 1
        assert any("not clean" in w for w in result.warnings)
