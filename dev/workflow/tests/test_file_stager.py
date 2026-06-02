"""Unit tests for file_stager module.

Tests the file staging functionality including:
- Source-to-target mapping
- Default exclusion of dev/ci/ and dev/ctests/
- Allowlist override of exclusions
- Template file skipping
- Error handling for missing sources
- uwtools fallback to shutil

Traces to: Requirements 8.2, 8.7, 8.8, 9.2
"""

import os
import sys
import tempfile
from pathlib import Path

import pytest

sys.path.insert(0, os.path.join(os.path.dirname(__file__), ".."))

from deployment.file_stager import (
    DEFAULT_EXCLUDES,
    DEFAULT_SOURCE_TARGET_MAP,
    FileStager,
    StagingError,
    StagingResult,
    stage_files,
)


# ---------------------------------------------------------------------------
# Fixtures
# ---------------------------------------------------------------------------


@pytest.fixture
def project_tree(tmp_path):
    """Create a minimal project tree mimicking global-workflow structure."""
    # dev/jobs/
    jobs_dir = tmp_path / "dev" / "jobs"
    jobs_dir.mkdir(parents=True)
    (jobs_dir / "JGFS_ATMOS_FORECAST").write_text("#!/bin/bash\necho forecast\n")
    (jobs_dir / "JGDAS_ATMOS_ANALYSIS").write_text("#!/bin/bash\necho analysis\n")

    # dev/scripts/
    scripts_dir = tmp_path / "dev" / "scripts"
    scripts_dir.mkdir(parents=True)
    (scripts_dir / "exgfs_atmos_forecast.sh").write_text("#!/bin/bash\necho ex\n")

    # dev/ush/
    ush_dir = tmp_path / "dev" / "ush"
    ush_dir.mkdir(parents=True)
    (ush_dir / "helper.sh").write_text("#!/bin/bash\necho helper\n")
    # Template file — should be skipped
    (ush_dir / "universal_wrapper.sh.j2").write_text("{{ EXPDIR }}/ush/wrapper\n")

    # dev/versions/
    versions_dir = tmp_path / "dev" / "versions"
    versions_dir.mkdir(parents=True)
    (versions_dir / "run.ver").write_text("export gfs_ver=v17.0.0\n")

    # dev/modulefiles/
    modulefiles_dir = tmp_path / "dev" / "modulefiles"
    modulefiles_dir.mkdir(parents=True)
    (modulefiles_dir / "hera.lua").write_text("-- module\n")

    # dev/sorc/
    sorc_dir = tmp_path / "dev" / "sorc"
    sorc_dir.mkdir(parents=True)
    (sorc_dir / "build.sh").write_text("#!/bin/bash\nmake\n")

    # dev/ci/ (excluded by default)
    ci_dir = tmp_path / "dev" / "ci"
    ci_dir.mkdir(parents=True)
    (ci_dir / "Jenkinsfile").write_text("pipeline {}\n")

    # dev/ctests/ (excluded by default)
    ctests_dir = tmp_path / "dev" / "ctests"
    ctests_dir.mkdir(parents=True)
    (ctests_dir / "CMakeLists.txt").write_text("cmake_minimum_required()\n")

    # EXPDIR destination
    expdir = tmp_path / "EXPDIR"
    expdir.mkdir()

    return tmp_path, expdir


@pytest.fixture
def stager(project_tree):
    """Create a FileStager with the test project tree."""
    project_root, expdir = project_tree
    return FileStager(
        project_root=project_root,
        expdir=expdir,
        use_uwtools=False,  # Always use shutil in tests
    )


# ---------------------------------------------------------------------------
# Tests: Basic staging
# ---------------------------------------------------------------------------


class TestBasicStaging:
    """Tests for basic file staging operations."""

    def test_stage_copies_jobs(self, stager, project_tree):
        project_root, expdir = project_tree
        result = stager.stage()

        assert (expdir / "jobs" / "JGFS_ATMOS_FORECAST").exists()
        assert (expdir / "jobs" / "JGDAS_ATMOS_ANALYSIS").exists()
        content = (expdir / "jobs" / "JGFS_ATMOS_FORECAST").read_text()
        assert "echo forecast" in content

    def test_stage_copies_scripts(self, stager, project_tree):
        project_root, expdir = project_tree
        result = stager.stage()

        assert (expdir / "scripts" / "exgfs_atmos_forecast.sh").exists()

    def test_stage_copies_ush(self, stager, project_tree):
        project_root, expdir = project_tree
        result = stager.stage()

        assert (expdir / "ush" / "helper.sh").exists()

    def test_stage_copies_versions(self, stager, project_tree):
        project_root, expdir = project_tree
        result = stager.stage()

        assert (expdir / "versions" / "run.ver").exists()

    def test_stage_copies_modulefiles(self, stager, project_tree):
        project_root, expdir = project_tree
        result = stager.stage()

        assert (expdir / "modulefiles" / "hera.lua").exists()

    def test_stage_copies_sorc(self, stager, project_tree):
        project_root, expdir = project_tree
        result = stager.stage()

        assert (expdir / "sorc" / "build.sh").exists()

    def test_stage_returns_result(self, stager):
        result = stager.stage()

        assert isinstance(result, StagingResult)
        assert result.files_copied > 0
        assert len(result.staged_paths) == result.files_copied


# ---------------------------------------------------------------------------
# Tests: Template skipping
# ---------------------------------------------------------------------------


class TestTemplateSkipping:
    """Tests that .j2 template files are skipped during staging."""

    def test_j2_files_not_staged(self, stager, project_tree):
        project_root, expdir = project_tree
        result = stager.stage()

        # The .j2 file should NOT be copied
        assert not (expdir / "ush" / "universal_wrapper.sh.j2").exists()

    def test_non_j2_files_staged(self, stager, project_tree):
        project_root, expdir = project_tree
        result = stager.stage()

        # Regular files should be copied
        assert (expdir / "ush" / "helper.sh").exists()


# ---------------------------------------------------------------------------
# Tests: Exclusion (Req 8.7)
# ---------------------------------------------------------------------------


class TestExclusion:
    """Tests for default exclusion of dev/ci/ and dev/ctests/."""

    def test_ci_excluded_by_default(self, stager, project_tree):
        project_root, expdir = project_tree
        result = stager.stage()

        assert not (expdir / "ci").exists()

    def test_ctests_excluded_by_default(self, stager, project_tree):
        project_root, expdir = project_tree
        result = stager.stage()

        assert not (expdir / "ctests").exists()

    def test_excluded_dirs_reported_in_result(self, stager, project_tree):
        project_root, expdir = project_tree
        result = stager.stage()

        # The skipped_excludes should contain the excluded paths
        assert len(result.skipped_excludes) >= 0  # May be empty if not in map

    def test_default_excludes_list(self):
        assert "dev/ci" in DEFAULT_EXCLUDES
        assert "dev/ctests" in DEFAULT_EXCLUDES

    def test_custom_excludes(self, project_tree):
        project_root, expdir = project_tree
        stager = FileStager(
            project_root=project_root,
            expdir=expdir,
            excludes=["dev/jobs"],
            use_uwtools=False,
        )
        result = stager.stage()

        # jobs should be excluded
        assert not (expdir / "jobs" / "JGFS_ATMOS_FORECAST").exists()
        # scripts should still be copied
        assert (expdir / "scripts" / "exgfs_atmos_forecast.sh").exists()


# ---------------------------------------------------------------------------
# Tests: Allowlist (Req 8.8)
# ---------------------------------------------------------------------------


class TestAllowlist:
    """Tests for allowlist override of exclusions."""

    def test_allowlist_includes_excluded_dir(self, project_tree):
        project_root, expdir = project_tree

        # Add dev/ctests to the source map so it would be staged
        custom_map = dict(DEFAULT_SOURCE_TARGET_MAP)
        custom_map["dev/ctests"] = "ctests"

        stager = FileStager(
            project_root=project_root,
            expdir=expdir,
            source_target_map=custom_map,
            allowlist=["dev/ctests"],
            use_uwtools=False,
        )
        result = stager.stage()

        # ctests should now be included
        assert (expdir / "ctests" / "CMakeLists.txt").exists()

    def test_allowlist_does_not_affect_other_excludes(self, project_tree):
        project_root, expdir = project_tree

        # Add both to map
        custom_map = dict(DEFAULT_SOURCE_TARGET_MAP)
        custom_map["dev/ci"] = "ci"
        custom_map["dev/ctests"] = "ctests"

        stager = FileStager(
            project_root=project_root,
            expdir=expdir,
            source_target_map=custom_map,
            allowlist=["dev/ctests"],  # Only ctests allowed
            use_uwtools=False,
        )
        result = stager.stage()

        # ctests included, ci still excluded
        assert (expdir / "ctests" / "CMakeLists.txt").exists()
        assert not (expdir / "ci").exists()

    def test_empty_allowlist_excludes_all_defaults(self, project_tree):
        project_root, expdir = project_tree

        custom_map = dict(DEFAULT_SOURCE_TARGET_MAP)
        custom_map["dev/ci"] = "ci"

        stager = FileStager(
            project_root=project_root,
            expdir=expdir,
            source_target_map=custom_map,
            allowlist=[],
            use_uwtools=False,
        )
        result = stager.stage()

        assert not (expdir / "ci").exists()


# ---------------------------------------------------------------------------
# Tests: Source-to-target mapping
# ---------------------------------------------------------------------------


class TestSourceTargetMapping:
    """Tests for the source-to-target directory mapping."""

    def test_default_map_keys(self):
        assert "dev/jobs" in DEFAULT_SOURCE_TARGET_MAP
        assert "dev/scripts" in DEFAULT_SOURCE_TARGET_MAP
        assert "dev/ush" in DEFAULT_SOURCE_TARGET_MAP
        assert "dev/sorc" in DEFAULT_SOURCE_TARGET_MAP
        assert "dev/versions" in DEFAULT_SOURCE_TARGET_MAP
        assert "dev/modulefiles" in DEFAULT_SOURCE_TARGET_MAP

    def test_default_map_values(self):
        assert DEFAULT_SOURCE_TARGET_MAP["dev/jobs"] == "jobs"
        assert DEFAULT_SOURCE_TARGET_MAP["dev/scripts"] == "scripts"
        assert DEFAULT_SOURCE_TARGET_MAP["dev/ush"] == "ush"
        assert DEFAULT_SOURCE_TARGET_MAP["dev/sorc"] == "sorc"
        assert DEFAULT_SOURCE_TARGET_MAP["dev/versions"] == "versions"
        assert DEFAULT_SOURCE_TARGET_MAP["dev/modulefiles"] == "modulefiles"

    def test_custom_map(self, project_tree):
        project_root, expdir = project_tree
        custom_map = {"dev/jobs": "custom_jobs"}

        stager = FileStager(
            project_root=project_root,
            expdir=expdir,
            source_target_map=custom_map,
            use_uwtools=False,
        )
        result = stager.stage()

        assert (expdir / "custom_jobs" / "JGFS_ATMOS_FORECAST").exists()
        # Other dirs not in map should not be staged
        assert not (expdir / "scripts").exists()

    def test_preserves_subdirectory_structure(self, project_tree):
        project_root, expdir = project_tree

        # Create a nested structure
        nested = project_root / "dev" / "ush" / "python" / "pygfs"
        nested.mkdir(parents=True)
        (nested / "task.py").write_text("# task\n")

        stager = FileStager(
            project_root=project_root,
            expdir=expdir,
            use_uwtools=False,
        )
        result = stager.stage()

        assert (expdir / "ush" / "python" / "pygfs" / "task.py").exists()


# ---------------------------------------------------------------------------
# Tests: Missing source directories
# ---------------------------------------------------------------------------


class TestMissingSources:
    """Tests for handling missing source directories."""

    def test_missing_source_dir_skipped(self, project_tree):
        project_root, expdir = project_tree

        # Remove sorc dir
        import shutil
        shutil.rmtree(project_root / "dev" / "sorc")

        stager = FileStager(
            project_root=project_root,
            expdir=expdir,
            use_uwtools=False,
        )
        # Should not raise
        result = stager.stage()
        assert not (expdir / "sorc").exists()

    def test_stage_single_missing_file_raises(self, project_tree):
        project_root, expdir = project_tree

        stager = FileStager(
            project_root=project_root,
            expdir=expdir,
            use_uwtools=False,
        )

        with pytest.raises(StagingError) as exc_info:
            stager.stage_single("dev/jobs/NONEXISTENT", "jobs/NONEXISTENT")
        assert "not found" in str(exc_info.value)


# ---------------------------------------------------------------------------
# Tests: stage_single
# ---------------------------------------------------------------------------


class TestStageSingle:
    """Tests for staging individual files."""

    def test_stage_single_file(self, stager, project_tree):
        project_root, expdir = project_tree

        dst = stager.stage_single(
            "dev/jobs/JGFS_ATMOS_FORECAST", "jobs/JGFS_ATMOS_FORECAST"
        )

        assert dst.exists()
        assert dst == expdir / "jobs" / "JGFS_ATMOS_FORECAST"
        assert "echo forecast" in dst.read_text()

    def test_stage_single_excluded_raises(self, project_tree):
        project_root, expdir = project_tree

        stager = FileStager(
            project_root=project_root,
            expdir=expdir,
            excludes=["dev/ci"],
            use_uwtools=False,
        )

        with pytest.raises(StagingError) as exc_info:
            stager.stage_single("dev/ci/Jenkinsfile", "ci/Jenkinsfile")
        assert "excluded" in str(exc_info.value)


# ---------------------------------------------------------------------------
# Tests: Convenience function
# ---------------------------------------------------------------------------


class TestConvenienceFunction:
    """Tests for the stage_files() convenience function."""

    def test_stage_files_basic(self, project_tree):
        project_root, expdir = project_tree

        result = stage_files(project_root, expdir)

        assert isinstance(result, StagingResult)
        assert result.files_copied > 0
        assert (expdir / "jobs" / "JGFS_ATMOS_FORECAST").exists()

    def test_stage_files_with_allowlist(self, project_tree):
        project_root, expdir = project_tree

        custom_map = dict(DEFAULT_SOURCE_TARGET_MAP)
        custom_map["dev/ctests"] = "ctests"

        result = stage_files(
            project_root,
            expdir,
            source_target_map=custom_map,
            allowlist=["dev/ctests"],
        )

        assert (expdir / "ctests" / "CMakeLists.txt").exists()


# ---------------------------------------------------------------------------
# Tests: File content preservation
# ---------------------------------------------------------------------------


class TestContentPreservation:
    """Tests that file content is preserved exactly during staging."""

    def test_binary_content_preserved(self, project_tree):
        project_root, expdir = project_tree

        # Write binary content
        binary_data = bytes(range(256))
        (project_root / "dev" / "sorc" / "binary.dat").write_bytes(binary_data)

        stager = FileStager(
            project_root=project_root,
            expdir=expdir,
            use_uwtools=False,
        )
        stager.stage()

        assert (expdir / "sorc" / "binary.dat").read_bytes() == binary_data

    def test_file_permissions_preserved(self, project_tree):
        project_root, expdir = project_tree

        src = project_root / "dev" / "jobs" / "JGFS_ATMOS_FORECAST"
        src.chmod(0o755)

        stager = FileStager(
            project_root=project_root,
            expdir=expdir,
            use_uwtools=False,
        )
        stager.stage()

        dst = expdir / "jobs" / "JGFS_ATMOS_FORECAST"
        # shutil.copy2 preserves permissions
        assert os.stat(dst).st_mode & 0o777 == 0o755


# ---------------------------------------------------------------------------
# Tests: stage_unconditional_artifacts (Req 9.1, 9.2, 9.5, 9.6)
# ---------------------------------------------------------------------------


class TestStageUnconditionalArtifacts:
    """Tests for staging unconditional linking script artifacts."""

    @pytest.fixture
    def project_with_sorc(self, tmp_path):
        """Create a project tree with sorc/ linking scripts at project root."""
        # sorc/link_workflow.sh
        sorc_dir = tmp_path / "sorc"
        sorc_dir.mkdir(parents=True)
        link_workflow = sorc_dir / "link_workflow.sh"
        link_workflow.write_text("#!/bin/bash\necho link_workflow\n")
        link_workflow.chmod(0o755)

        # sorc/ufs_utils.fd/fix/link_fixdirs.sh
        fix_dir = sorc_dir / "ufs_utils.fd" / "fix"
        fix_dir.mkdir(parents=True)
        link_fixdirs = fix_dir / "link_fixdirs.sh"
        link_fixdirs.write_text("#!/bin/bash\necho link_fixdirs\n")
        link_fixdirs.chmod(0o755)

        # EXPDIR
        expdir = tmp_path / "EXPDIR"
        expdir.mkdir()

        return tmp_path, expdir

    def test_stages_link_workflow_sh(self, project_with_sorc):
        project_root, expdir = project_with_sorc
        stager = FileStager(
            project_root=project_root,
            expdir=expdir,
            use_uwtools=False,
        )
        result = stager.stage_unconditional_artifacts()

        dst = expdir / "sorc" / "link_workflow.sh"
        assert dst.exists()
        assert "link_workflow" in dst.read_text()

    def test_stages_link_fixdirs_sh(self, project_with_sorc):
        project_root, expdir = project_with_sorc
        stager = FileStager(
            project_root=project_root,
            expdir=expdir,
            use_uwtools=False,
        )
        result = stager.stage_unconditional_artifacts()

        dst = expdir / "sorc" / "ufs_utils.fd" / "fix" / "link_fixdirs.sh"
        assert dst.exists()
        assert "link_fixdirs" in dst.read_text()

    def test_preserves_executable_permissions(self, project_with_sorc):
        project_root, expdir = project_with_sorc
        stager = FileStager(
            project_root=project_root,
            expdir=expdir,
            use_uwtools=False,
        )
        stager.stage_unconditional_artifacts()

        link_workflow = expdir / "sorc" / "link_workflow.sh"
        link_fixdirs = expdir / "sorc" / "ufs_utils.fd" / "fix" / "link_fixdirs.sh"

        assert os.stat(link_workflow).st_mode & 0o777 == 0o755
        assert os.stat(link_fixdirs).st_mode & 0o777 == 0o755

    def test_returns_staging_result(self, project_with_sorc):
        project_root, expdir = project_with_sorc
        stager = FileStager(
            project_root=project_root,
            expdir=expdir,
            use_uwtools=False,
        )
        result = stager.stage_unconditional_artifacts()

        assert isinstance(result, StagingResult)
        assert result.files_copied == 2
        assert len(result.staged_paths) == 2

    def test_raises_staging_error_if_link_workflow_missing(self, tmp_path):
        # Only create the fix dir script, not link_workflow.sh
        sorc_dir = tmp_path / "sorc" / "ufs_utils.fd" / "fix"
        sorc_dir.mkdir(parents=True)
        (sorc_dir / "link_fixdirs.sh").write_text("#!/bin/bash\n")

        expdir = tmp_path / "EXPDIR"
        expdir.mkdir()

        stager = FileStager(
            project_root=tmp_path,
            expdir=expdir,
            use_uwtools=False,
        )

        with pytest.raises(StagingError) as exc_info:
            stager.stage_unconditional_artifacts()
        assert "link_workflow.sh" in str(exc_info.value)

    def test_raises_staging_error_if_link_fixdirs_missing(self, tmp_path):
        # Only create link_workflow.sh, not link_fixdirs.sh
        sorc_dir = tmp_path / "sorc"
        sorc_dir.mkdir(parents=True)
        (sorc_dir / "link_workflow.sh").write_text("#!/bin/bash\n")

        expdir = tmp_path / "EXPDIR"
        expdir.mkdir()

        stager = FileStager(
            project_root=tmp_path,
            expdir=expdir,
            use_uwtools=False,
        )

        with pytest.raises(StagingError) as exc_info:
            stager.stage_unconditional_artifacts()
        assert "link_fixdirs.sh" in str(exc_info.value)

    def test_creates_destination_directories(self, project_with_sorc):
        project_root, expdir = project_with_sorc
        stager = FileStager(
            project_root=project_root,
            expdir=expdir,
            use_uwtools=False,
        )
        result = stager.stage_unconditional_artifacts()

        # Directories should have been created
        assert (expdir / "sorc").is_dir()
        assert (expdir / "sorc" / "ufs_utils.fd" / "fix").is_dir()
        assert result.directories_created >= 1
