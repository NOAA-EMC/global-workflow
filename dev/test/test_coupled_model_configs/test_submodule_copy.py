"""Unit tests for submodule file copy (Stage 4c).

Tests the _stage_submodule_copy function that copies submodule-owned
files (NEXUS configs, UPP parm files) verbatim into the EXPDIR without
Jinja2 rendering.

Traces to: Requirements 13.1, 13.2, 13.3, 13.4, 13.5
"""

from __future__ import annotations

import os
import stat
import sys
from pathlib import Path

import pytest

sys.path.insert(0, os.path.join(os.path.dirname(__file__), "..", "..", "workflow"))

from deployment.pipeline import (
    SUBMODULE_COPY_MANIFEST,
    PipelineError,
    _stage_submodule_copy,
)


@pytest.fixture
def project_tree(tmp_path: Path) -> Path:
    """Create a minimal project tree with submodule source files."""
    project_root = tmp_path / "global-workflow"
    project_root.mkdir()

    # Create NEXUS config files
    nexus_dir = project_root / "sorc" / "nexus.fd" / "config" / "gocart"
    nexus_dir.mkdir(parents=True)
    (nexus_dir / "NEXUS_Config.rc").write_text("! NEXUS config\nkey = value\n")
    (nexus_dir / "HEMCO_sa_Config.rc").write_text("! HEMCO config\n")

    # Create a subdirectory in NEXUS
    nexus_sub = nexus_dir / "species"
    nexus_sub.mkdir()
    (nexus_sub / "dust.rc").write_text("! dust species config\n")

    # Create UPP parm files
    upp_dir = project_root / "sorc" / "upp.fd" / "parm"
    upp_dir.mkdir(parents=True)
    (upp_dir / "params_grib2_tbl_new").write_text("# grib2 table\n")
    (upp_dir / "postxconfig-NT-GFS.txt").write_text("post config\n")

    # Create UPP parm subdirectory
    upp_gfs = upp_dir / "gfs"
    upp_gfs.mkdir()
    (upp_gfs / "postxconfig-NT.txt").write_text("gfs post config\n")

    return project_root


@pytest.fixture
def expdir(tmp_path: Path) -> Path:
    """Create an empty EXPDIR."""
    exp = tmp_path / "EXPDIR"
    exp.mkdir()
    return exp


class TestSubmoduleCopyManifest:
    """Tests for the SUBMODULE_COPY_MANIFEST constant."""

    def test_manifest_contains_nexus_entry(self):
        """NEXUS gocart config mapping is in the manifest."""
        sources = [src for src, _ in SUBMODULE_COPY_MANIFEST]
        assert "sorc/nexus.fd/config/gocart/" in sources

    def test_manifest_contains_upp_entry(self):
        """UPP parm mapping is in the manifest."""
        sources = [src for src, _ in SUBMODULE_COPY_MANIFEST]
        assert "sorc/upp.fd/parm/" in sources

    def test_nexus_destination(self):
        """NEXUS files map to parm/chem/nexus/gocart/ in EXPDIR."""
        for src, dst in SUBMODULE_COPY_MANIFEST:
            if "nexus.fd" in src:
                assert dst == "parm/chem/nexus/gocart/"

    def test_upp_destination(self):
        """UPP files map to parm/post/ in EXPDIR."""
        for src, dst in SUBMODULE_COPY_MANIFEST:
            if "upp.fd" in src:
                assert dst == "parm/post/"


class TestStageSubmoduleCopy:
    """Tests for _stage_submodule_copy function."""

    def test_copies_nexus_files(self, project_tree: Path, expdir: Path):
        """NEXUS config files are copied to EXPDIR."""
        copied = _stage_submodule_copy(project_tree, expdir)

        nexus_dst = expdir / "parm" / "chem" / "nexus" / "gocart"
        assert nexus_dst.is_dir()
        assert (nexus_dst / "NEXUS_Config.rc").exists()
        assert (nexus_dst / "HEMCO_sa_Config.rc").exists()

    def test_copies_nexus_subdirectories(self, project_tree: Path, expdir: Path):
        """NEXUS subdirectories are copied recursively."""
        _stage_submodule_copy(project_tree, expdir)

        nexus_sub = expdir / "parm" / "chem" / "nexus" / "gocart" / "species"
        assert nexus_sub.is_dir()
        assert (nexus_sub / "dust.rc").exists()

    def test_copies_upp_files(self, project_tree: Path, expdir: Path):
        """UPP parm files are copied to EXPDIR."""
        _stage_submodule_copy(project_tree, expdir)

        upp_dst = expdir / "parm" / "post"
        assert upp_dst.is_dir()
        assert (upp_dst / "params_grib2_tbl_new").exists()
        assert (upp_dst / "postxconfig-NT-GFS.txt").exists()

    def test_copies_upp_subdirectories(self, project_tree: Path, expdir: Path):
        """UPP parm subdirectories are copied recursively."""
        _stage_submodule_copy(project_tree, expdir)

        upp_gfs = expdir / "parm" / "post" / "gfs"
        assert upp_gfs.is_dir()
        assert (upp_gfs / "postxconfig-NT.txt").exists()

    def test_file_content_preserved(self, project_tree: Path, expdir: Path):
        """Copied files are byte-identical to source (no Jinja2 rendering)."""
        _stage_submodule_copy(project_tree, expdir)

        src_content = (
            project_tree / "sorc" / "nexus.fd" / "config" / "gocart" / "NEXUS_Config.rc"
        ).read_text()
        dst_content = (
            expdir / "parm" / "chem" / "nexus" / "gocart" / "NEXUS_Config.rc"
        ).read_text()
        assert dst_content == src_content

    def test_preserves_permissions(self, project_tree: Path, expdir: Path):
        """File permissions are preserved (cp -rp semantics)."""
        # Set a specific permission on a source file
        src_file = (
            project_tree / "sorc" / "nexus.fd" / "config" / "gocart" / "NEXUS_Config.rc"
        )
        os.chmod(src_file, 0o755)

        _stage_submodule_copy(project_tree, expdir)

        dst_file = expdir / "parm" / "chem" / "nexus" / "gocart" / "NEXUS_Config.rc"
        src_mode = stat.S_IMODE(os.stat(src_file).st_mode)
        dst_mode = stat.S_IMODE(os.stat(dst_file).st_mode)
        assert dst_mode == src_mode

    def test_returns_copied_file_paths(self, project_tree: Path, expdir: Path):
        """Returns list of all copied file paths in EXPDIR."""
        copied = _stage_submodule_copy(project_tree, expdir)

        # Should include all files from both NEXUS and UPP
        assert len(copied) > 0
        # All paths should be under expdir
        for path in copied:
            assert str(path).startswith(str(expdir))
            assert path.is_file()

    def test_fatal_error_if_source_not_found(self, tmp_path: Path, expdir: Path):
        """Emits FATAL ERROR if submodule source directory does not exist."""
        # Create a project root without the submodule directories
        empty_project = tmp_path / "empty-project"
        empty_project.mkdir()

        with pytest.raises(PipelineError) as exc_info:
            _stage_submodule_copy(empty_project, expdir)

        assert "FATAL ERROR" in str(exc_info.value)
        assert "Submodule source not found" in str(exc_info.value)

    def test_no_jinja2_rendering_on_copied_files(
        self, project_tree: Path, expdir: Path
    ):
        """Files with Jinja2-like syntax are NOT rendered — copied verbatim."""
        # Write a file with Jinja2 syntax that should NOT be rendered
        nexus_dir = project_tree / "sorc" / "nexus.fd" / "config" / "gocart"
        (nexus_dir / "template_like.rc").write_text(
            "value = {{ should_not_render }}\n"
        )

        _stage_submodule_copy(project_tree, expdir)

        dst = expdir / "parm" / "chem" / "nexus" / "gocart" / "template_like.rc"
        assert dst.read_text() == "value = {{ should_not_render }}\n"

    def test_merges_into_existing_directory(
        self, project_tree: Path, expdir: Path
    ):
        """Copies merge into existing EXPDIR directories without error."""
        # Pre-create the destination directory with an existing file
        existing_dir = expdir / "parm" / "post"
        existing_dir.mkdir(parents=True)
        (existing_dir / "existing_file.txt").write_text("pre-existing\n")

        _stage_submodule_copy(project_tree, expdir)

        # Both existing and new files should be present
        assert (existing_dir / "existing_file.txt").exists()
        assert (existing_dir / "params_grib2_tbl_new").exists()
