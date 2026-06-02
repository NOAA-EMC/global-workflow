"""Unit tests for the EXPDIR sealing module (seal.py).

Tests:
- File permissions set to 0444
- Directory permissions set to 0555
- Provenance.yaml written with correct fields
- seal_expdir orchestrates both operations
- Error handling for missing EXPDIR

Traces to: Requirements 3.4, 13.4
"""

import getpass
import os
import platform
import stat
import sys
import tempfile
from pathlib import Path
from unittest.mock import patch

import pytest
import yaml

sys.path.insert(0, os.path.join(os.path.dirname(__file__), ".."))

from deployment.seal import (
    DIR_MODE,
    FILE_MODE,
    PROVENANCE_FILENAME,
    seal_expdir,
    seal_permissions,
    write_provenance,
)


# ---------------------------------------------------------------------------
# Fixtures
# ---------------------------------------------------------------------------


@pytest.fixture
def tmp_expdir(tmp_path):
    """Create a temporary EXPDIR with sample files and directories."""
    # Create directory structure
    (tmp_path / "jobs").mkdir()
    (tmp_path / "scripts").mkdir()
    (tmp_path / "parm" / "config").mkdir(parents=True)
    (tmp_path / "workflow").mkdir()

    # Create sample files
    (tmp_path / "jobs" / "JGFS_FORECAST").write_text("#!/bin/bash\necho forecast\n")
    (tmp_path / "scripts" / "exgfs_forecast.sh").write_text("#!/bin/bash\necho run\n")
    (tmp_path / "parm" / "config" / "config.base").write_text("NET=gfs\n")
    (tmp_path / "workflow" / "state.db").write_bytes(b"\x00" * 100)

    return tmp_path


@pytest.fixture
def nested_expdir(tmp_path):
    """Create a deeply nested EXPDIR structure."""
    deep = tmp_path / "ecf" / "scripts" / "gfs" / "atmos" / "post"
    deep.mkdir(parents=True)
    (deep / "post_f000.ecf").write_text("%include <head.h>\n")
    (deep / "post_f006.ecf").write_text("%include <head.h>\n")

    (tmp_path / "workflow").mkdir(exist_ok=True)
    return tmp_path


# ---------------------------------------------------------------------------
# Tests: seal_permissions
# ---------------------------------------------------------------------------


class TestSealPermissions:
    """Tests for file and directory permission sealing."""

    def test_regular_files_set_to_0444(self, tmp_expdir):
        seal_permissions(tmp_expdir)

        for root, _, files in os.walk(str(tmp_expdir)):
            for f in files:
                filepath = os.path.join(root, f)
                if os.path.isfile(filepath) and not os.path.islink(filepath):
                    mode = stat.S_IMODE(os.stat(filepath).st_mode)
                    assert mode == FILE_MODE, (
                        f"File {filepath} has mode {oct(mode)}, expected {oct(FILE_MODE)}"
                    )

    def test_directories_set_to_0555(self, tmp_expdir):
        seal_permissions(tmp_expdir)

        for root, dirs, _ in os.walk(str(tmp_expdir)):
            # Check the root directory itself
            mode = stat.S_IMODE(os.stat(root).st_mode)
            assert mode == DIR_MODE, (
                f"Directory {root} has mode {oct(mode)}, expected {oct(DIR_MODE)}"
            )

    def test_deeply_nested_structure(self, nested_expdir):
        seal_permissions(nested_expdir)

        # Verify all files sealed
        ecf_file = nested_expdir / "ecf" / "scripts" / "gfs" / "atmos" / "post" / "post_f000.ecf"
        mode = stat.S_IMODE(os.stat(str(ecf_file)).st_mode)
        assert mode == FILE_MODE

        # Verify all directories sealed
        post_dir = nested_expdir / "ecf" / "scripts" / "gfs" / "atmos" / "post"
        mode = stat.S_IMODE(os.stat(str(post_dir)).st_mode)
        assert mode == DIR_MODE

    def test_symlinks_not_modified(self, tmp_expdir):
        """Symlinks should not have their permissions changed."""
        target = tmp_expdir / "jobs" / "JGFS_FORECAST"
        link = tmp_expdir / "jobs" / "JGFS_LINK"
        link.symlink_to(target)

        seal_permissions(tmp_expdir)

        # The symlink itself should still exist and point to target
        assert link.is_symlink()
        assert link.resolve() == target.resolve()

    def test_empty_directory(self, tmp_path):
        """An empty directory should still be sealed."""
        empty = tmp_path / "empty"
        empty.mkdir()

        seal_permissions(tmp_path)

        mode = stat.S_IMODE(os.stat(str(empty)).st_mode)
        assert mode == DIR_MODE


# ---------------------------------------------------------------------------
# Tests: write_provenance
# ---------------------------------------------------------------------------


class TestWriteProvenance:
    """Tests for provenance.yaml generation."""

    def test_provenance_file_created(self, tmp_expdir):
        write_provenance(tmp_expdir)

        provenance_path = tmp_expdir / PROVENANCE_FILENAME
        assert provenance_path.exists()

    def test_provenance_contains_required_fields(self, tmp_expdir):
        write_provenance(tmp_expdir)

        provenance_path = tmp_expdir / PROVENANCE_FILENAME
        with open(provenance_path) as f:
            data = yaml.safe_load(f)

        required_fields = [
            "git_remote",
            "git_commit",
            "git_branch",
            "deployed_by",
            "deployed_on",
            "deployed_at",
            "config",
        ]
        for field in required_fields:
            assert field in data, f"Missing field: {field}"

    def test_deployed_by_is_current_user(self, tmp_expdir):
        write_provenance(tmp_expdir)

        provenance_path = tmp_expdir / PROVENANCE_FILENAME
        with open(provenance_path) as f:
            data = yaml.safe_load(f)

        assert data["deployed_by"] == getpass.getuser()

    def test_deployed_on_is_hostname(self, tmp_expdir):
        write_provenance(tmp_expdir)

        provenance_path = tmp_expdir / PROVENANCE_FILENAME
        with open(provenance_path) as f:
            data = yaml.safe_load(f)

        assert data["deployed_on"] == platform.node()

    def test_deployed_at_is_iso_timestamp(self, tmp_expdir):
        write_provenance(tmp_expdir)

        provenance_path = tmp_expdir / PROVENANCE_FILENAME
        with open(provenance_path) as f:
            data = yaml.safe_load(f)

        # Should be parseable as ISO format datetime
        from datetime import datetime
        ts = data["deployed_at"]
        # datetime.fromisoformat handles the format
        parsed = datetime.fromisoformat(ts)
        assert parsed is not None

    def test_config_values_included(self, tmp_expdir):
        config = {
            "platform": "HERA",
            "version": "v17.0.0",
            "app": "gfs_cycled",
        }
        write_provenance(tmp_expdir, config=config)

        provenance_path = tmp_expdir / PROVENANCE_FILENAME
        with open(provenance_path) as f:
            data = yaml.safe_load(f)

        assert data["config"] == config

    def test_config_defaults_to_empty_dict(self, tmp_expdir):
        write_provenance(tmp_expdir)

        provenance_path = tmp_expdir / PROVENANCE_FILENAME
        with open(provenance_path) as f:
            data = yaml.safe_load(f)

        assert data["config"] == {}

    def test_creates_workflow_directory_if_missing(self, tmp_path):
        """Should create workflow/ directory if it doesn't exist."""
        # tmp_path has no workflow/ subdirectory
        write_provenance(tmp_path)

        provenance_path = tmp_path / PROVENANCE_FILENAME
        assert provenance_path.exists()

    @patch("deployment.seal._git_remote", return_value="https://github.com/NOAA-EMC/global-workflow.git")
    @patch("deployment.seal._git_commit", return_value="abc123def456789")
    @patch("deployment.seal._git_branch", return_value="develop")
    def test_git_metadata_captured(self, mock_branch, mock_commit, mock_remote, tmp_expdir):
        write_provenance(tmp_expdir)

        provenance_path = tmp_expdir / PROVENANCE_FILENAME
        with open(provenance_path) as f:
            data = yaml.safe_load(f)

        assert data["git_remote"] == "https://github.com/NOAA-EMC/global-workflow.git"
        assert data["git_commit"] == "abc123def456789"
        assert data["git_branch"] == "develop"

    def test_provenance_is_valid_yaml(self, tmp_expdir):
        config = {"nested": {"key": "value"}, "list": [1, 2, 3]}
        write_provenance(tmp_expdir, config=config)

        provenance_path = tmp_expdir / PROVENANCE_FILENAME
        with open(provenance_path) as f:
            data = yaml.safe_load(f)

        # Round-trip: should be loadable and match structure
        assert isinstance(data, dict)
        assert data["config"]["nested"]["key"] == "value"
        assert data["config"]["list"] == [1, 2, 3]


# ---------------------------------------------------------------------------
# Tests: seal_expdir (integration of both operations)
# ---------------------------------------------------------------------------


class TestSealExpdir:
    """Tests for the combined seal operation."""

    def test_provenance_written_before_sealing(self, tmp_expdir):
        """Provenance should be written and then sealed (read-only)."""
        seal_expdir(tmp_expdir)

        provenance_path = tmp_expdir / PROVENANCE_FILENAME
        assert provenance_path.exists()

        # File should be read-only after sealing
        mode = stat.S_IMODE(os.stat(str(provenance_path)).st_mode)
        assert mode == FILE_MODE

    def test_all_files_sealed(self, tmp_expdir):
        seal_expdir(tmp_expdir)

        for root, _, files in os.walk(str(tmp_expdir)):
            for f in files:
                filepath = os.path.join(root, f)
                if os.path.isfile(filepath) and not os.path.islink(filepath):
                    mode = stat.S_IMODE(os.stat(filepath).st_mode)
                    assert mode == FILE_MODE

    def test_all_directories_sealed(self, tmp_expdir):
        seal_expdir(tmp_expdir)

        for root, dirs, _ in os.walk(str(tmp_expdir)):
            mode = stat.S_IMODE(os.stat(root).st_mode)
            assert mode == DIR_MODE

    def test_returns_provenance_path(self, tmp_expdir):
        result = seal_expdir(tmp_expdir)
        expected = tmp_expdir / PROVENANCE_FILENAME
        assert result == expected

    def test_raises_on_missing_expdir(self, tmp_path):
        nonexistent = tmp_path / "does_not_exist"
        with pytest.raises(FileNotFoundError, match="EXPDIR does not exist"):
            seal_expdir(nonexistent)

    def test_sealed_files_not_writable(self, tmp_expdir):
        """After sealing, writing to files should raise PermissionError."""
        seal_expdir(tmp_expdir)

        target = tmp_expdir / "jobs" / "JGFS_FORECAST"
        with pytest.raises(PermissionError):
            target.write_text("modified")

    def test_config_passed_to_provenance(self, tmp_expdir):
        config = {"platform": "WCOSS2", "version": "v17.0.0"}
        seal_expdir(tmp_expdir, config=config)

        provenance_path = tmp_expdir / PROVENANCE_FILENAME
        # Need to temporarily make readable (it already is 0444)
        with open(provenance_path) as f:
            data = yaml.safe_load(f)

        assert data["config"] == config
