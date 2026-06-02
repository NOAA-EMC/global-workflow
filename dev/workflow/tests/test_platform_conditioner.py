"""Unit tests for platform-conditioned rendering.

Tests the rendering and staging of platform-specific files:
  - env/${PLATFORM}.env
  - parm/config/<app>/config.resources.${PLATFORM}
  - modulefiles/${PLATFORM}/

Validates Requirements 12.2, 12.3
"""

from __future__ import annotations

import os
import sys
from pathlib import Path

import pytest

sys.path.insert(0, os.path.join(os.path.dirname(__file__), ".."))

from deployment.platform_conditioner import (
    PlatformConditionError,
    PlatformRenderResult,
    SUPPORTED_PLATFORMS,
    _PLATFORM_TO_MODULEFILE_SUFFIX,
    get_platform_conditioned_paths,
    render_all_platform_conditioned,
    render_platform_env,
    render_platform_resources,
    stage_platform_modulefiles,
)


# ---------------------------------------------------------------------------
# Fixtures
# ---------------------------------------------------------------------------


@pytest.fixture
def project_tree(tmp_path):
    """Create a minimal project tree with platform-specific files."""
    project_root = tmp_path / "global-workflow"
    project_root.mkdir()

    # Create env/ directory with platform env files
    env_dir = project_root / "env"
    env_dir.mkdir()
    (env_dir / "HERA.env").write_text(
        "# HERA environment\nexport MACHINE=HERA\n"
    )
    (env_dir / "WCOSS2.env").write_text(
        "# WCOSS2 environment\nexport MACHINE=WCOSS2\n"
    )
    (env_dir / "ORION.env").write_text(
        "# ORION environment\nexport MACHINE=ORION\n"
    )

    # Create dev/ directory structure
    dev_dir = project_root / "dev"
    dev_dir.mkdir()
    (dev_dir / "jobs").mkdir()

    # Create parm/config/<app>/ with resource files
    gfs_config = dev_dir / "parm" / "config" / "gfs"
    gfs_config.mkdir(parents=True)
    (gfs_config / "config.resources").write_text(
        "#!/bin/bash\n# Base resources\nexport ntasks=24\n"
    )
    (gfs_config / "config.resources.HERA").write_text(
        "#!/bin/bash\n# HERA resources\nexport ntasks=48\n"
    )
    (gfs_config / "config.resources.WCOSS2").write_text(
        "#!/bin/bash\n# WCOSS2 resources\nexport ntasks=96\n"
    )
    (gfs_config / "config.resources.ORION").write_text(
        "#!/bin/bash\n# ORION resources\nexport ntasks=64\n"
    )

    # Create gefs config with resources
    gefs_config = dev_dir / "parm" / "config" / "gefs"
    gefs_config.mkdir(parents=True)
    (gefs_config / "config.resources").write_text(
        "#!/bin/bash\n# GEFS base resources\n"
    )
    (gefs_config / "config.resources.HERA").write_text(
        "#!/bin/bash\n# GEFS HERA resources\n"
    )

    # Create modulefiles/ directory
    modulefiles_dir = project_root / "modulefiles"
    modulefiles_dir.mkdir()
    (modulefiles_dir / "gw_run.hera.lua").write_text(
        '-- Hera run module\nload("intel")\n'
    )
    (modulefiles_dir / "gw_setup.hera.lua").write_text(
        '-- Hera setup module\nload("cmake")\n'
    )
    (modulefiles_dir / "gw_run.wcoss2.lua").write_text(
        '-- WCOSS2 run module\nload("intel")\n'
    )
    (modulefiles_dir / "gw_setup.wcoss2.lua").write_text(
        '-- WCOSS2 setup module\nload("cmake")\n'
    )
    (modulefiles_dir / "gw_run.orion.lua").write_text(
        '-- Orion run module\nload("intel")\n'
    )
    (modulefiles_dir / "gw_run.common.lua").write_text(
        "-- Common run module\n"
    )
    (modulefiles_dir / "gw_gsi.wcoss2.lua").write_text(
        '-- GSI WCOSS2 module\nload("gsi")\n'
    )

    # Create EXPDIR
    expdir = tmp_path / "expdir"
    expdir.mkdir()

    return {
        "project_root": project_root,
        "dev_dir": dev_dir,
        "env_dir": env_dir,
        "gfs_config": gfs_config,
        "modulefiles_dir": modulefiles_dir,
        "expdir": expdir,
    }


# ---------------------------------------------------------------------------
# Tests: render_platform_env
# ---------------------------------------------------------------------------


class TestRenderPlatformEnv:
    """Tests for platform env file rendering."""

    def test_renders_hera_env(self, project_tree):
        """Renders HERA.env to EXPDIR/env/HERA.env."""
        result = render_platform_env(
            project_root=project_tree["project_root"],
            expdir=project_tree["expdir"],
            platform="HERA",
        )

        assert result is not None
        assert result.exists()
        assert result == project_tree["expdir"] / "env" / "HERA.env"
        content = result.read_text()
        assert "MACHINE=HERA" in content

    def test_renders_wcoss2_env(self, project_tree):
        """Renders WCOSS2.env to EXPDIR/env/WCOSS2.env."""
        result = render_platform_env(
            project_root=project_tree["project_root"],
            expdir=project_tree["expdir"],
            platform="WCOSS2",
        )

        assert result is not None
        assert result.exists()
        content = result.read_text()
        assert "MACHINE=WCOSS2" in content

    def test_case_insensitive_platform(self, project_tree):
        """Platform name is normalized to uppercase."""
        result = render_platform_env(
            project_root=project_tree["project_root"],
            expdir=project_tree["expdir"],
            platform="hera",
        )

        assert result is not None
        assert result.name == "HERA.env"

    def test_missing_env_returns_none(self, project_tree):
        """Returns None when env file doesn't exist for platform."""
        result = render_platform_env(
            project_root=project_tree["project_root"],
            expdir=project_tree["expdir"],
            platform="CONTAINER",
        )

        assert result is None

    def test_creates_env_directory(self, tmp_path):
        """Creates the env/ directory in EXPDIR if it doesn't exist."""
        project_root = tmp_path / "project"
        project_root.mkdir()
        env_dir = project_root / "env"
        env_dir.mkdir()
        (env_dir / "HERA.env").write_text("# test\n")

        expdir = tmp_path / "expdir"
        expdir.mkdir()

        result = render_platform_env(
            project_root=project_root,
            expdir=expdir,
            platform="HERA",
        )

        assert result is not None
        assert (expdir / "env").is_dir()


# ---------------------------------------------------------------------------
# Tests: render_platform_resources
# ---------------------------------------------------------------------------


class TestRenderPlatformResources:
    """Tests for platform resource file rendering."""

    def test_renders_hera_resources(self, project_tree):
        """Renders config.resources.HERA for gfs app."""
        result = render_platform_resources(
            project_root=project_tree["project_root"],
            expdir=project_tree["expdir"],
            platform="HERA",
            app="gfs",
        )

        assert len(result) >= 1
        # Should have the platform-specific file
        platform_file = project_tree["expdir"] / "parm" / "config" / "gfs" / "config.resources.HERA"
        assert platform_file.exists()
        content = platform_file.read_text()
        assert "ntasks=48" in content

    def test_renders_base_resources(self, project_tree):
        """Also copies the base config.resources file."""
        render_platform_resources(
            project_root=project_tree["project_root"],
            expdir=project_tree["expdir"],
            platform="HERA",
            app="gfs",
        )

        base_file = project_tree["expdir"] / "parm" / "config" / "gfs" / "config.resources"
        assert base_file.exists()
        content = base_file.read_text()
        assert "ntasks=24" in content

    def test_renders_wcoss2_resources(self, project_tree):
        """Renders config.resources.WCOSS2 for gfs app."""
        result = render_platform_resources(
            project_root=project_tree["project_root"],
            expdir=project_tree["expdir"],
            platform="WCOSS2",
            app="gfs",
        )

        platform_file = project_tree["expdir"] / "parm" / "config" / "gfs" / "config.resources.WCOSS2"
        assert platform_file.exists()
        content = platform_file.read_text()
        assert "ntasks=96" in content

    def test_missing_app_returns_empty(self, project_tree):
        """Returns empty list when app config directory doesn't exist."""
        result = render_platform_resources(
            project_root=project_tree["project_root"],
            expdir=project_tree["expdir"],
            platform="HERA",
            app="nonexistent_app",
        )

        assert result == []

    def test_missing_platform_resource_returns_base_only(self, project_tree):
        """Returns only base resource when platform-specific doesn't exist."""
        result = render_platform_resources(
            project_root=project_tree["project_root"],
            expdir=project_tree["expdir"],
            platform="CONTAINER",
            app="gfs",
        )

        # Should have the base config.resources but not CONTAINER-specific
        base_file = project_tree["expdir"] / "parm" / "config" / "gfs" / "config.resources"
        assert base_file.exists()

        platform_file = project_tree["expdir"] / "parm" / "config" / "gfs" / "config.resources.CONTAINER"
        assert not platform_file.exists()


# ---------------------------------------------------------------------------
# Tests: stage_platform_modulefiles
# ---------------------------------------------------------------------------


class TestStagePlatformModulefiles:
    """Tests for platform modulefile staging."""

    def test_stages_hera_modulefiles(self, project_tree):
        """Stages HERA-specific modulefiles to modulefiles/HERA/."""
        result = stage_platform_modulefiles(
            project_root=project_tree["project_root"],
            expdir=project_tree["expdir"],
            platform="HERA",
        )

        # Should have staged hera-specific files
        assert len(result) >= 2  # gw_run.hera.lua, gw_setup.hera.lua + common

        hera_dir = project_tree["expdir"] / "modulefiles" / "HERA"
        assert hera_dir.is_dir()
        assert (hera_dir / "gw_run.hera.lua").exists()
        assert (hera_dir / "gw_setup.hera.lua").exists()

    def test_stages_wcoss2_modulefiles(self, project_tree):
        """Stages WCOSS2-specific modulefiles to modulefiles/WCOSS2/."""
        result = stage_platform_modulefiles(
            project_root=project_tree["project_root"],
            expdir=project_tree["expdir"],
            platform="WCOSS2",
        )

        wcoss2_dir = project_tree["expdir"] / "modulefiles" / "WCOSS2"
        assert wcoss2_dir.is_dir()
        assert (wcoss2_dir / "gw_run.wcoss2.lua").exists()
        assert (wcoss2_dir / "gw_setup.wcoss2.lua").exists()
        assert (wcoss2_dir / "gw_gsi.wcoss2.lua").exists()

    def test_stages_common_modulefiles(self, project_tree):
        """Also stages common modulefiles."""
        stage_platform_modulefiles(
            project_root=project_tree["project_root"],
            expdir=project_tree["expdir"],
            platform="HERA",
        )

        common_file = project_tree["expdir"] / "modulefiles" / "gw_run.common.lua"
        assert common_file.exists()

    def test_no_cross_platform_contamination(self, project_tree):
        """HERA deployment doesn't include WCOSS2 modulefiles."""
        stage_platform_modulefiles(
            project_root=project_tree["project_root"],
            expdir=project_tree["expdir"],
            platform="HERA",
        )

        hera_dir = project_tree["expdir"] / "modulefiles" / "HERA"
        # Should NOT have wcoss2 files in the HERA directory
        assert not (hera_dir / "gw_run.wcoss2.lua").exists()
        assert not (hera_dir / "gw_gsi.wcoss2.lua").exists()

    def test_missing_modulefiles_dir(self, tmp_path):
        """Returns empty list when no modulefiles directory exists."""
        project_root = tmp_path / "project"
        project_root.mkdir()
        expdir = tmp_path / "expdir"
        expdir.mkdir()

        result = stage_platform_modulefiles(
            project_root=project_root,
            expdir=expdir,
            platform="HERA",
        )

        assert result == []


# ---------------------------------------------------------------------------
# Tests: render_all_platform_conditioned
# ---------------------------------------------------------------------------


class TestRenderAllPlatformConditioned:
    """Tests for the combined platform-conditioned rendering."""

    def test_renders_all_platform_files(self, project_tree):
        """Renders env, resources, and modulefiles for a platform."""
        result = render_all_platform_conditioned(
            project_root=project_tree["project_root"],
            expdir=project_tree["expdir"],
            platform="HERA",
            app="gfs",
        )

        assert isinstance(result, PlatformRenderResult)
        assert result.env_file is not None
        assert len(result.resource_files) >= 1
        assert len(result.modulefile_paths) >= 2

    def test_platform_isolation_env(self, project_tree):
        """Different platforms get different env files."""
        expdir_hera = project_tree["expdir"] / "hera"
        expdir_hera.mkdir()
        expdir_wcoss2 = project_tree["expdir"] / "wcoss2"
        expdir_wcoss2.mkdir()

        render_all_platform_conditioned(
            project_root=project_tree["project_root"],
            expdir=expdir_hera,
            platform="HERA",
            app="gfs",
        )
        render_all_platform_conditioned(
            project_root=project_tree["project_root"],
            expdir=expdir_wcoss2,
            platform="WCOSS2",
            app="gfs",
        )

        hera_env = (expdir_hera / "env" / "HERA.env").read_text()
        wcoss2_env = (expdir_wcoss2 / "env" / "WCOSS2.env").read_text()

        assert "MACHINE=HERA" in hera_env
        assert "MACHINE=WCOSS2" in wcoss2_env
        assert hera_env != wcoss2_env

    def test_platform_isolation_resources(self, project_tree):
        """Different platforms get different resource files."""
        expdir_hera = project_tree["expdir"] / "hera"
        expdir_hera.mkdir()
        expdir_wcoss2 = project_tree["expdir"] / "wcoss2"
        expdir_wcoss2.mkdir()

        render_all_platform_conditioned(
            project_root=project_tree["project_root"],
            expdir=expdir_hera,
            platform="HERA",
            app="gfs",
        )
        render_all_platform_conditioned(
            project_root=project_tree["project_root"],
            expdir=expdir_wcoss2,
            platform="WCOSS2",
            app="gfs",
        )

        hera_res = (
            expdir_hera / "parm" / "config" / "gfs" / "config.resources.HERA"
        ).read_text()
        wcoss2_res = (
            expdir_wcoss2 / "parm" / "config" / "gfs" / "config.resources.WCOSS2"
        ).read_text()

        assert "ntasks=48" in hera_res
        assert "ntasks=96" in wcoss2_res

    def test_platform_isolation_modulefiles(self, project_tree):
        """Different platforms get different modulefiles."""
        expdir_hera = project_tree["expdir"] / "hera"
        expdir_hera.mkdir()
        expdir_wcoss2 = project_tree["expdir"] / "wcoss2"
        expdir_wcoss2.mkdir()

        render_all_platform_conditioned(
            project_root=project_tree["project_root"],
            expdir=expdir_hera,
            platform="HERA",
            app="gfs",
        )
        render_all_platform_conditioned(
            project_root=project_tree["project_root"],
            expdir=expdir_wcoss2,
            platform="WCOSS2",
            app="gfs",
        )

        # HERA should have hera modulefiles
        assert (expdir_hera / "modulefiles" / "HERA" / "gw_run.hera.lua").exists()
        assert not (expdir_hera / "modulefiles" / "HERA" / "gw_run.wcoss2.lua").exists()

        # WCOSS2 should have wcoss2 modulefiles
        assert (expdir_wcoss2 / "modulefiles" / "WCOSS2" / "gw_run.wcoss2.lua").exists()
        assert not (expdir_wcoss2 / "modulefiles" / "WCOSS2" / "gw_run.hera.lua").exists()


# ---------------------------------------------------------------------------
# Tests: get_platform_conditioned_paths
# ---------------------------------------------------------------------------


class TestGetPlatformConditionedPaths:
    """Tests for the platform-conditioned path patterns."""

    def test_returns_env_pattern(self):
        """Includes env file pattern for the platform."""
        paths = get_platform_conditioned_paths("HERA")
        assert "env/HERA.env" in paths

    def test_returns_resource_pattern(self):
        """Includes resource file pattern for the platform."""
        paths = get_platform_conditioned_paths("HERA")
        assert "parm/config/*/config.resources.HERA" in paths

    def test_returns_modulefile_pattern(self):
        """Includes modulefile directory pattern for the platform."""
        paths = get_platform_conditioned_paths("HERA")
        assert any("modulefiles/HERA" in p for p in paths)

    def test_returns_ecf_pattern(self):
        """Includes ecf scripts pattern (scheduler directives differ)."""
        paths = get_platform_conditioned_paths("HERA")
        assert "ecf/scripts/**/*.ecf" in paths

    def test_case_normalization(self):
        """Platform name is normalized to uppercase."""
        paths = get_platform_conditioned_paths("hera")
        assert "env/HERA.env" in paths


# ---------------------------------------------------------------------------
# Tests: Platform suffix mapping
# ---------------------------------------------------------------------------


class TestPlatformSuffixMapping:
    """Tests for the platform-to-modulefile suffix mapping."""

    def test_all_supported_platforms_have_suffix(self):
        """Every supported platform has a modulefile suffix mapping."""
        for platform in SUPPORTED_PLATFORMS:
            assert platform in _PLATFORM_TO_MODULEFILE_SUFFIX, (
                f"Platform {platform} missing from suffix mapping"
            )

    def test_cloud_platforms_use_noaacloud(self):
        """Cloud platforms (AWSPW, AZUREPW, GOOGLEPW) use 'noaacloud'."""
        assert _PLATFORM_TO_MODULEFILE_SUFFIX["AWSPW"] == "noaacloud"
        assert _PLATFORM_TO_MODULEFILE_SUFFIX["AZUREPW"] == "noaacloud"
        assert _PLATFORM_TO_MODULEFILE_SUFFIX["GOOGLEPW"] == "noaacloud"

    def test_hpc_platforms_use_lowercase_name(self):
        """HPC platforms use their lowercase name as suffix."""
        assert _PLATFORM_TO_MODULEFILE_SUFFIX["HERA"] == "hera"
        assert _PLATFORM_TO_MODULEFILE_SUFFIX["WCOSS2"] == "wcoss2"
        assert _PLATFORM_TO_MODULEFILE_SUFFIX["ORION"] == "orion"
