"""Unit tests for the deployment context builder (pipeline.build_context).

Tests that build_context() correctly assembles the Jinja2 context dict
from Workflow_Configuration YAML, platform, version, and git metadata.

Validates: Requirement 4.1
"""

from __future__ import annotations

import os
import sys
import tempfile
from pathlib import Path
from unittest.mock import patch

import pytest
import yaml

sys.path.insert(0, os.path.join(os.path.dirname(__file__), ".."))

from deployment.pipeline import (
    build_context,
    _derive_net_run_mode,
    _get_git_commit,
    _get_git_branch,
    _get_git_remote,
)


# ---------------------------------------------------------------------------
# Fixtures
# ---------------------------------------------------------------------------


@pytest.fixture
def minimal_config(tmp_path: Path) -> Path:
    """Create a minimal Workflow_Configuration YAML for testing."""
    config = {
        "suite": {
            "name": "gfs_v17",
            "ecf_home": "{{ EXPDIR }}/ecf",
            "ecf_files": "{{ EXPDIR }}/ecf/scripts",
            "ecf_include": "{{ EXPDIR }}/ecf/include",
        },
        "defaults": {
            "ECF_TRIES": 2,
        },
        "cycles": [
            {
                "name": "gdas",
                "repeat": {"type": "date", "variable": "YMD", "start": "20250101", "end": "20250102", "step": 1},
                "time": "00:00 06:00 12:00 18:00",
            },
            {
                "name": "gfs",
                "repeat": {"type": "date", "variable": "YMD", "start": "20250101", "end": "20250102", "step": 1},
                "time": "00:00 06:00 12:00 18:00",
            },
        ],
        "families": [
            {
                "path": "gdas/atmos/prep",
                "tasks": [
                    {"name": "prep", "trigger": "", "jjob": "JGLOBAL_ATMOS_PREP"},
                ],
            },
        ],
        "inter_cycle_dependencies": [
            {
                "task": "gdas/atmos/prep/prep",
                "depends_on": "gdas/atmos/archive/arch == complete",
                "cycle_offset": -1,
            },
        ],
    }
    config_file = tmp_path / "gfs_cycled.yaml"
    with open(config_file, "w") as f:
        yaml.dump(config, f, sort_keys=False)
    return config_file


@pytest.fixture
def forecast_only_config(tmp_path: Path) -> Path:
    """Create a forecast-only Workflow_Configuration YAML for testing."""
    config = {
        "suite": {
            "name": "gfs_v17_fcst_only",
            "ecf_home": "{{ EXPDIR }}/ecf",
        },
        "defaults": {"ECF_TRIES": 2},
        "cycles": [
            {
                "name": "gfs",
                "repeat": {"type": "date", "variable": "YMD", "start": "20250101", "end": "20250102", "step": 1},
                "time": "00:00 06:00 12:00 18:00",
            },
        ],
        "families": [
            {
                "path": "gfs/atmos/forecast",
                "tasks": [
                    {"name": "fcst", "trigger": "", "jjob": "JGLOBAL_FORECAST"},
                ],
            },
        ],
        "inter_cycle_dependencies": [],
    }
    config_file = tmp_path / "gfs_forecast_only.yaml"
    with open(config_file, "w") as f:
        yaml.dump(config, f, sort_keys=False)
    return config_file


# ---------------------------------------------------------------------------
# Tests for build_context()
# ---------------------------------------------------------------------------


class TestBuildContext:
    """Tests for the build_context() function."""

    def test_returns_dict(self, minimal_config: Path, tmp_path: Path):
        """build_context returns a dict."""
        ctx = build_context(
            config_path=minimal_config,
            platform="HERA",
            version="v17.0.0",
            expdir=tmp_path / "EXPDIR",
        )
        assert isinstance(ctx, dict)

    def test_pdy_is_shell_variable(self, minimal_config: Path, tmp_path: Path):
        """PDY should remain as ${PDY} for runtime shell expansion."""
        ctx = build_context(
            config_path=minimal_config,
            platform="HERA",
            version="v17.0.0",
            expdir=tmp_path / "EXPDIR",
        )
        assert ctx["PDY"] == "${PDY}"

    def test_cyc_is_shell_variable(self, minimal_config: Path, tmp_path: Path):
        """cyc should remain as ${cyc} for runtime shell expansion."""
        ctx = build_context(
            config_path=minimal_config,
            platform="HERA",
            version="v17.0.0",
            expdir=tmp_path / "EXPDIR",
        )
        assert ctx["cyc"] == "${cyc}"

    def test_machine_from_platform(self, minimal_config: Path, tmp_path: Path):
        """MACHINE should be set to the platform argument."""
        ctx = build_context(
            config_path=minimal_config,
            platform="WCOSS2",
            version="v17.0.0",
            expdir=tmp_path / "EXPDIR",
        )
        assert ctx["MACHINE"] == "WCOSS2"

    def test_model_ver_from_version(self, minimal_config: Path, tmp_path: Path):
        """model_ver should be set to the version argument."""
        ctx = build_context(
            config_path=minimal_config,
            platform="HERA",
            version="v17.0.0",
            expdir=tmp_path / "EXPDIR",
        )
        assert ctx["model_ver"] == "v17.0.0"

    def test_expdir_from_argument(self, minimal_config: Path, tmp_path: Path):
        """EXPDIR should be set to the expdir argument as a string."""
        expdir = tmp_path / "my_experiment"
        ctx = build_context(
            config_path=minimal_config,
            platform="HERA",
            version="v17.0.0",
            expdir=expdir,
        )
        assert ctx["EXPDIR"] == str(expdir)

    def test_comroot_default(self, minimal_config: Path, tmp_path: Path):
        """COMROOT defaults to <expdir>/com when not specified."""
        expdir = tmp_path / "EXPDIR"
        ctx = build_context(
            config_path=minimal_config,
            platform="HERA",
            version="v17.0.0",
            expdir=expdir,
        )
        assert ctx["COMROOT"] == str(expdir / "com")

    def test_comroot_explicit(self, minimal_config: Path, tmp_path: Path):
        """COMROOT uses the explicit value when provided."""
        ctx = build_context(
            config_path=minimal_config,
            platform="HERA",
            version="v17.0.0",
            expdir=tmp_path / "EXPDIR",
            comroot="/scratch/com/gfs",
        )
        assert ctx["COMROOT"] == "/scratch/com/gfs"

    def test_net_derived_from_suite_name(self, minimal_config: Path, tmp_path: Path):
        """NET should be derived from the suite name prefix."""
        ctx = build_context(
            config_path=minimal_config,
            platform="HERA",
            version="v17.0.0",
            expdir=tmp_path / "EXPDIR",
        )
        assert ctx["NET"] == "gfs"

    def test_run_derived_from_first_cycle(self, minimal_config: Path, tmp_path: Path):
        """RUN should be derived from the first cycle name."""
        ctx = build_context(
            config_path=minimal_config,
            platform="HERA",
            version="v17.0.0",
            expdir=tmp_path / "EXPDIR",
        )
        assert ctx["RUN"] == "gdas"

    def test_mode_cycled_with_inter_cycle_deps(self, minimal_config: Path, tmp_path: Path):
        """MODE should be 'cycled' when inter_cycle_dependencies are present."""
        ctx = build_context(
            config_path=minimal_config,
            platform="HERA",
            version="v17.0.0",
            expdir=tmp_path / "EXPDIR",
        )
        assert ctx["MODE"] == "cycled"

    def test_mode_forecast_only(self, forecast_only_config: Path, tmp_path: Path):
        """MODE should be 'forecast-only' for forecast-only configs."""
        ctx = build_context(
            config_path=forecast_only_config,
            platform="HERA",
            version="v17.0.0",
            expdir=tmp_path / "EXPDIR",
        )
        assert ctx["MODE"] == "forecast-only"

    def test_config_tree_included(self, minimal_config: Path, tmp_path: Path):
        """The full configuration tree should be merged into the context."""
        ctx = build_context(
            config_path=minimal_config,
            platform="HERA",
            version="v17.0.0",
            expdir=tmp_path / "EXPDIR",
        )
        # The suite section from the YAML should be accessible
        assert "suite" in ctx
        assert ctx["suite"]["name"] == "gfs_v17"

    def test_app_derived_from_filename(self, minimal_config: Path, tmp_path: Path):
        """app should be derived from the config filename stem."""
        ctx = build_context(
            config_path=minimal_config,
            platform="HERA",
            version="v17.0.0",
            expdir=tmp_path / "EXPDIR",
        )
        assert ctx["app"] == "gfs_cycled"

    def test_git_metadata_included(self, minimal_config: Path, tmp_path: Path):
        """Git metadata keys should be present in the context."""
        ctx = build_context(
            config_path=minimal_config,
            platform="HERA",
            version="v17.0.0",
            expdir=tmp_path / "EXPDIR",
        )
        assert "git_commit" in ctx
        assert "git_branch" in ctx
        assert "git_remote" in ctx

    def test_deploy_timestamp_included(self, minimal_config: Path, tmp_path: Path):
        """deploy_timestamp should be present and in ISO format."""
        ctx = build_context(
            config_path=minimal_config,
            platform="HERA",
            version="v17.0.0",
            expdir=tmp_path / "EXPDIR",
        )
        assert "deploy_timestamp" in ctx
        # Should be a valid ISO timestamp string
        assert "T" in ctx["deploy_timestamp"]

    def test_file_not_found_raises(self, tmp_path: Path):
        """build_context raises FileNotFoundError for missing config."""
        with pytest.raises(FileNotFoundError, match="Workflow configuration not found"):
            build_context(
                config_path=tmp_path / "nonexistent.yaml",
                platform="HERA",
                version="v17.0.0",
                expdir=tmp_path / "EXPDIR",
            )

    def test_all_required_keys_present(self, minimal_config: Path, tmp_path: Path):
        """All required context keys from Requirement 4.1 should be present."""
        ctx = build_context(
            config_path=minimal_config,
            platform="HERA",
            version="v17.0.0",
            expdir=tmp_path / "EXPDIR",
        )
        required_keys = ["PDY", "cyc", "NET", "RUN", "MODE", "MACHINE", "model_ver", "EXPDIR", "COMROOT"]
        for key in required_keys:
            assert key in ctx, f"Missing required key: {key}"


# ---------------------------------------------------------------------------
# Tests for _derive_net_run_mode()
# ---------------------------------------------------------------------------


class TestDeriveNetRunMode:
    """Tests for the _derive_net_run_mode() helper."""

    def test_gfs_cycled(self):
        """GFS cycled config derives NET=gfs, RUN=gdas, MODE=cycled."""
        config = {
            "suite": {"name": "gfs_v17"},
            "cycles": [{"name": "gdas"}, {"name": "gfs"}],
            "inter_cycle_dependencies": [{"task": "x", "depends_on": "y", "cycle_offset": -1}],
        }
        result = _derive_net_run_mode(config)
        assert result["NET"] == "gfs"
        assert result["RUN"] == "gdas"
        assert result["MODE"] == "cycled"

    def test_gefs_config(self):
        """GEFS config derives NET=gefs."""
        config = {
            "suite": {"name": "gefs_v13"},
            "cycles": [{"name": "gefs"}],
            "inter_cycle_dependencies": [],
        }
        result = _derive_net_run_mode(config)
        assert result["NET"] == "gefs"
        assert result["RUN"] == "gefs"
        assert result["MODE"] == "free-forecast"

    def test_forecast_only_mode(self):
        """Forecast-only suite name triggers forecast-only mode."""
        config = {
            "suite": {"name": "gfs_v17_fcst_only"},
            "cycles": [{"name": "gfs"}],
            "inter_cycle_dependencies": [],
        }
        result = _derive_net_run_mode(config)
        assert result["MODE"] == "forecast-only"

    def test_empty_suite_name(self):
        """Empty suite name defaults to NET=gfs."""
        config = {
            "suite": {"name": ""},
            "cycles": [],
            "inter_cycle_dependencies": [],
        }
        result = _derive_net_run_mode(config)
        assert result["NET"] == "gfs"

    def test_no_cycles(self):
        """Missing cycles defaults RUN to NET."""
        config = {
            "suite": {"name": "sfs_v1"},
            "cycles": [],
            "inter_cycle_dependencies": [],
        }
        result = _derive_net_run_mode(config)
        assert result["NET"] == "sfs"
        assert result["RUN"] == "sfs"


# ---------------------------------------------------------------------------
# Tests for git metadata helpers
# ---------------------------------------------------------------------------


class TestGetGitMetadata:
    """Tests for the git metadata helper functions."""

    def test_non_git_dir_returns_empty_commit(self, tmp_path: Path):
        """Non-git directory should return empty string for commit."""
        result = _get_git_commit(tmp_path)
        assert result == ""

    def test_non_git_dir_returns_empty_branch(self, tmp_path: Path):
        """Non-git directory should return empty string for branch."""
        result = _get_git_branch(tmp_path)
        assert result == ""

    def test_non_git_dir_returns_empty_remote(self, tmp_path: Path):
        """Non-git directory should return empty string for remote."""
        result = _get_git_remote(tmp_path)
        assert result == ""

    def test_real_repo_returns_commit(self):
        """In the actual repo, git_commit should be a non-empty hex string."""
        repo_root = Path("/mdc-mcp-rag/SCRATCH/Barry.Baker/global-workflow")
        if (repo_root / ".git").exists():
            result = _get_git_commit(repo_root)
            assert len(result) == 40
            assert all(c in "0123456789abcdef" for c in result)
