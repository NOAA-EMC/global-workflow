"""Integration tests for the coupled-model rendering pipeline.

Tests the full end-to-end rendering pipeline for coupled-model configs
including template rendering for all ocean resolutions, all component
combinations, submodule copy integrity, EXPDIR manifest completeness
with SHA-256 hashes, and no symlinks in the EXPDIR.

Traces to: Requirements 9.1, 9.2, 9.3, 9.4, 9.5, 9.6, 9.8, 13.3, 14.1
"""

from __future__ import annotations

import hashlib
import os
import shutil
import sys
from pathlib import Path

import pytest

sys.path.insert(0, os.path.join(os.path.dirname(__file__), "..", "..", "workflow"))

from deployment.model_config_renderer import ModelConfigRenderer, RenderedFile
from deployment.pipeline import (
    SUBMODULE_COPY_MANIFEST,
    _stage_submodule_copy,
)
from deployment.template_renderer import TemplateRenderError

# ---------------------------------------------------------------------------
# Constants
# ---------------------------------------------------------------------------

DEV_ROOT = Path(__file__).resolve().parent.parent.parent

# Expected output paths for coupled-model configs (relative to EXPDIR/parm/ufs/)
EXPECTED_COUPLED_OUTPUTS = {
    "ocean/MOM_input",
    "ocean/MOM6_data_table",
    "ice/ice_in",
    "wave/ww3_shel.nml",
    "fv3/input_global_nest.nml",
    "post/post_itag",
}

# Coupled-model subdirectories
COUPLED_SUBDIRS = ["ocean", "ice", "wave", "post"]


# ---------------------------------------------------------------------------
# Helper: build a full valid model context
# ---------------------------------------------------------------------------


def _full_model_context(
    ocean_resolution: str = "025",
    warm_start: bool = True,
    ice_input: str = "CPL",
    current_input: str = "CPL",
    post_system: str = "gfs",
    use_waves: bool = True,
    river_runoff: bool = True,
    oda_incupd: bool = False,
    do_sppt: bool = False,
    do_nest: bool = True,
) -> dict:
    """Build a complete model context for full pipeline rendering.

    Includes all required sections: top-level, fv3, ocean, ice, wave, post.
    """
    return {
        "resolution": "C96",
        "physics_suite": "gfdl",
        "coupling_mode": "s2sw",
        "dt_atmos": 450,
        "output_grid": "gaussian_grid",
        "output_fields": "standard",
        "pbl_scheme": "satmedmf",
        "progsigma": True,
        "coupling_interval_slow": 1800,
        "coupling_interval_fast": 450,
        "active_components": ["atmosphere", "ocean", "ice", "wave"],
        "start_date": {
            "year": 2024,
            "month": 1,
            "day": 15,
            "hour": 0,
        },
        "fv3": {
            "npx": 97,
            "npy": 97,
            "npz": 127,
            "layout": [2, 2],
            "io_layout": [1, 1],
            "quilting": True,
            "write_group": 1,
            "wrttask_per_group": 24,
            "restart_interval": 12,
            "blocksize": 32,
            "total_tasks": 24,
            "omp_threads": 1,
            "type": "nh",
            "imp_physics": 11,
            "ccpp_suite": "FV3_GFS_v17_p8",
            "fhrot": 0,
            "imo": 384,
            "jmo": 190,
            "output_fh": "0 1 2 3 6 12",
            "iau_offset": 0,
            "output_filetype_atm": "netcdf",
            "output_filetype_sfc": "netcdf",
            "num_output_files": 2,
            "quilting_restart": True,
            "d2_bg_k1": 0.20,
            "d2_bg_k2": 0.04,
            "dz_min": 6,
            "n_sponge": 42,
            "hord_mt": 5,
            "hord_vt": 5,
            "hord_tm": 5,
            "hord_dp": -5,
            "nord": 2,
            "dddmp": 0.1,
            "d4_bg": 0.12,
            "dnats": 1,
            "do_sat_adj": True,
            "atm_model": "fv3",
            "do_nest": do_nest,
            "npx_nest": 361,
            "npy_nest": 361,
        },
        "ocean": {
            "resolution": ocean_resolution,
            "dt_ocean": 900,
            "dt_therm": 3600,
            "tasks": 120,
            "omp_threads": 1,
            "output_dir": "./MOM6_OUTPUT",
            "restart_dir": "./MOM6_RESTART",
            "output_frequency_hours": 6,
            "use_mommesh": True,
            "use_waves": use_waves,
            "oda_incupd": oda_incupd,
            "oda_incupd_nhours": 6,
            "do_sppt": do_sppt,
            "river_runoff": river_runoff,
            "diag_coord_def_z_file": "oceanda_zgrid_75L.nc",
            "frunoff": "INPUT/runoff.daitren.clim.nc",
            "nx_glb": 1440,
            "ny_glb": 1080,
            "nk": 75,
        },
        "ice": {
            "resolution": "025",
            "nprocs": 48,
            "omp_threads": 1,
            "decomposition": "slenderX2",
            "dt_ice": 900,
            "restart_interval": 6,
            "grid": "grid_cice_NEMS_mx025.nc",
            "mask": "kmtu_cice_NEMS_mx025.nc",
            "nx_glb": 1440,
            "ny_glb": 1080,
            "warm_start": warm_start,
            "histfreq_n": 1,
            "hist_avg": True,
            "dumpfreq": "d",
            "dumpfreq_n": 1,
            "ktherm": 2,
            "tr_pond_lvl": True,
        },
        "wave": {
            "resolution": "gwes_30m",
            "tasks": 100,
            "omp_threads": 1,
            "mesh": "mesh.ww3.gwes_30m",
            "dt_wave": 900,
            "output_frequency_hours": 6,
            "ice_input": ice_input,
            "current_input": current_input,
            "output_params": "HS FP DP PHS PTP PDIR CHA",
            "dt_field_output": 10800,
            "dt_point_output": 3600,
            "grid_output_dir": "./",
            "point_output_dir": "./",
            "restart_output_dir": "./RESTART/",
        },
        "post": {
            "system": post_system,
        },
        "aerosol": {
            "emission_dataset": "qfed",
            "active_collections": ["inst_aod"],
            "grid_label": "PC384x190-DC",
            "grid_im": 384,
            "grid_jm": 190,
            "frequencies": {},
        },
    }


def _compute_sha256(filepath: Path) -> str:
    """Compute SHA-256 hex digest of a file."""
    h = hashlib.sha256()
    with open(filepath, "rb") as f:
        for chunk in iter(lambda: f.read(8192), b""):
            h.update(chunk)
    return h.hexdigest()


# ---------------------------------------------------------------------------
# Fixtures
# ---------------------------------------------------------------------------


@pytest.fixture
def expdir(tmp_path: Path) -> Path:
    """Create an empty EXPDIR for testing."""
    exp = tmp_path / "EXPDIR"
    exp.mkdir()
    return exp


@pytest.fixture
def project_tree(tmp_path: Path) -> Path:
    """Create a minimal project tree with submodule source files."""
    project_root = tmp_path / "global-workflow"
    project_root.mkdir()

    # Create NEXUS config files
    nexus_dir = project_root / "sorc" / "nexus.fd" / "config" / "gocart"
    nexus_dir.mkdir(parents=True)
    (nexus_dir / "NEXUS_Config.rc").write_text(
        "! NEXUS config\nkey = value\n"
    )
    (nexus_dir / "HEMCO_sa_Config.rc").write_text("! HEMCO config\n")

    # Create UPP parm files
    upp_dir = project_root / "sorc" / "upp.fd" / "parm"
    upp_dir.mkdir(parents=True)
    (upp_dir / "params_grib2_tbl_new").write_text("# grib2 table\n")
    (upp_dir / "postxconfig-NT-GFS.txt").write_text("post config\n")

    return project_root


# ---------------------------------------------------------------------------
# Test Class: Full Rendering Pipeline per Ocean Resolution
# ---------------------------------------------------------------------------


class TestFullRenderingPipeline:
    """Test full rendering pipeline for each ocean resolution.

    Traces to: Requirements 9.1, 9.2, 9.3, 9.4, 9.5, 9.6
    """

    @pytest.mark.parametrize("resolution", ["025", "050", "100", "500"])
    def test_renders_all_coupled_configs_for_resolution(
        self, resolution: str, expdir: Path
    ):
        """Full pipeline renders all coupled-model configs for each resolution."""
        context = _full_model_context(ocean_resolution=resolution)
        renderer = ModelConfigRenderer(dev_root=DEV_ROOT)

        results = renderer.render_all(context, expdir)

        # Verify all expected coupled-model output files exist
        parm_ufs = expdir / "parm" / "ufs"
        for expected in EXPECTED_COUPLED_OUTPUTS:
            output_path = parm_ufs / expected
            assert output_path.exists(), (
                f"Expected output '{expected}' not found for "
                f"resolution={resolution}"
            )

    @pytest.mark.parametrize("resolution", ["025", "050", "100", "500"])
    def test_rendered_files_are_non_empty(
        self, resolution: str, expdir: Path
    ):
        """All rendered coupled-model configs are non-empty files."""
        context = _full_model_context(ocean_resolution=resolution)
        renderer = ModelConfigRenderer(dev_root=DEV_ROOT)

        renderer.render_all(context, expdir)

        parm_ufs = expdir / "parm" / "ufs"
        for expected in EXPECTED_COUPLED_OUTPUTS:
            output_path = parm_ufs / expected
            assert output_path.stat().st_size > 0, (
                f"Rendered file '{expected}' is empty for "
                f"resolution={resolution}"
            )

    @pytest.mark.parametrize("resolution", ["025", "050", "100", "500"])
    def test_mom_input_contains_resolution_grid_dims(
        self, resolution: str, expdir: Path
    ):
        """MOM_input contains correct grid dimensions for each resolution."""
        expected_dims = {
            "025": ("NIGLOBAL = 1440", "NJGLOBAL = 1080"),
            "050": ("NIGLOBAL = 720", "NJGLOBAL = 576"),
            "100": ("NIGLOBAL = 360", "NJGLOBAL = 320"),
            "500": ("NIGLOBAL = 72", "NJGLOBAL = 35"),
        }
        context = _full_model_context(ocean_resolution=resolution)
        renderer = ModelConfigRenderer(dev_root=DEV_ROOT)

        renderer.render_all(context, expdir)

        mom_input = (expdir / "parm" / "ufs" / "ocean" / "MOM_input").read_text()
        ni, nj = expected_dims[resolution]
        assert ni in mom_input, (
            f"Expected '{ni}' in MOM_input for resolution={resolution}"
        )
        assert nj in mom_input, (
            f"Expected '{nj}' in MOM_input for resolution={resolution}"
        )

    @pytest.mark.parametrize("resolution", ["025", "050", "100", "500"])
    def test_render_all_returns_rendered_file_objects(
        self, resolution: str, expdir: Path
    ):
        """render_all returns RenderedFile objects with sha256 and source."""
        context = _full_model_context(ocean_resolution=resolution)
        renderer = ModelConfigRenderer(dev_root=DEV_ROOT)

        results = renderer.render_all(context, expdir)

        # All results should be RenderedFile instances
        for rf in results:
            assert isinstance(rf, RenderedFile)
            assert rf.sha256 != ""
            assert rf.source != ""
            assert rf.method in ("render", "copy")
            assert rf.path.exists()


# ---------------------------------------------------------------------------
# Test Class: Component Combinations
# ---------------------------------------------------------------------------


class TestComponentCombinations:
    """Test coupled-model rendering with all component combinations.

    Traces to: Requirements 9.1, 9.2, 9.3, 9.4, 9.5, 9.6
    """

    @pytest.mark.parametrize(
        "warm_start,ice_input,current_input,post_system",
        [
            (True, "CPL", "CPL", "gfs"),
            (False, "YES", "YES", "gcafs"),
            (True, "YES", "CPL", "gfs"),
            (False, "CPL", "YES", "gcafs"),
        ],
    )
    def test_all_component_combinations_render_successfully(
        self,
        warm_start: bool,
        ice_input: str,
        current_input: str,
        post_system: str,
        expdir: Path,
    ):
        """Pipeline renders successfully for all component combinations."""
        context = _full_model_context(
            warm_start=warm_start,
            ice_input=ice_input,
            current_input=current_input,
            post_system=post_system,
        )
        renderer = ModelConfigRenderer(dev_root=DEV_ROOT)

        results = renderer.render_all(context, expdir)

        # All expected coupled-model outputs should exist
        parm_ufs = expdir / "parm" / "ufs"
        for expected in EXPECTED_COUPLED_OUTPUTS:
            assert (parm_ufs / expected).exists()

    def test_warm_start_true_produces_continue_runtype(self, expdir: Path):
        """warm_start=True renders ice_in with runtype='continue'."""
        context = _full_model_context(warm_start=True)
        renderer = ModelConfigRenderer(dev_root=DEV_ROOT)

        renderer.render_all(context, expdir)

        ice_in = (expdir / "parm" / "ufs" / "ice" / "ice_in").read_text()
        assert "runtype        = 'continue'" in ice_in
        assert "use_restart_time = .true." in ice_in

    def test_warm_start_false_produces_initial_runtype(self, expdir: Path):
        """warm_start=False renders ice_in with runtype='initial'."""
        context = _full_model_context(warm_start=False)
        renderer = ModelConfigRenderer(dev_root=DEV_ROOT)

        renderer.render_all(context, expdir)

        ice_in = (expdir / "parm" / "ufs" / "ice" / "ice_in").read_text()
        assert "runtype        = 'initial'" in ice_in
        assert "use_restart_time = .false." in ice_in

    def test_wave_cpl_mode_produces_c_flag(self, expdir: Path):
        """ice_input=CPL and current_input=CPL produce 'C' flags in ww3."""
        context = _full_model_context(ice_input="CPL", current_input="CPL")
        renderer = ModelConfigRenderer(dev_root=DEV_ROOT)

        renderer.render_all(context, expdir)

        ww3 = (expdir / "parm" / "ufs" / "wave" / "ww3_shel.nml").read_text()
        assert "'C'" in ww3 or "= 'C'" in ww3 or "='C'" in ww3

    def test_wave_yes_mode_produces_t_flag(self, expdir: Path):
        """ice_input=YES produces 'T' flag in ww3."""
        context = _full_model_context(ice_input="YES", current_input="YES")
        renderer = ModelConfigRenderer(dev_root=DEV_ROOT)

        renderer.render_all(context, expdir)

        ww3 = (expdir / "parm" / "ufs" / "wave" / "ww3_shel.nml").read_text()
        assert "'T'" in ww3 or "= 'T'" in ww3 or "='T'" in ww3

    def test_ocean_conditionals_use_waves(self, expdir: Path):
        """use_waves=True produces USE_WAVES in MOM_input."""
        context = _full_model_context(use_waves=True)
        renderer = ModelConfigRenderer(dev_root=DEV_ROOT)

        renderer.render_all(context, expdir)

        mom_input = (expdir / "parm" / "ufs" / "ocean" / "MOM_input").read_text()
        assert "USE_WAVES = True" in mom_input

    def test_ocean_conditionals_river_runoff(self, expdir: Path):
        """river_runoff=True produces RIVER_RUNOFF in MOM_input."""
        context = _full_model_context(river_runoff=True)
        renderer = ModelConfigRenderer(dev_root=DEV_ROOT)

        renderer.render_all(context, expdir)

        mom_input = (expdir / "parm" / "ufs" / "ocean" / "MOM_input").read_text()
        assert "RIVER_RUNOFF = True" in mom_input

    def test_post_system_gfs(self, expdir: Path):
        """post.system=gfs renders post_itag with gfs parameters."""
        context = _full_model_context(post_system="gfs")
        renderer = ModelConfigRenderer(dev_root=DEV_ROOT)

        renderer.render_all(context, expdir)

        post_itag = (expdir / "parm" / "ufs" / "post" / "post_itag").read_text()
        assert len(post_itag.strip()) > 0

    def test_post_system_gcafs(self, expdir: Path):
        """post.system=gcafs renders post_itag with gcafs parameters."""
        context = _full_model_context(post_system="gcafs")
        renderer = ModelConfigRenderer(dev_root=DEV_ROOT)

        renderer.render_all(context, expdir)

        post_itag = (expdir / "parm" / "ufs" / "post" / "post_itag").read_text()
        assert len(post_itag.strip()) > 0


# ---------------------------------------------------------------------------
# Test Class: Submodule Copy Integration
# ---------------------------------------------------------------------------


class TestSubmoduleCopyIntegration:
    """Test submodule copy (NEXUS, UPP files copied verbatim).

    Traces to: Requirement 13.3
    """

    def test_nexus_files_copied_verbatim(
        self, project_tree: Path, expdir: Path
    ):
        """NEXUS config files are copied verbatim to EXPDIR."""
        _stage_submodule_copy(project_tree, expdir)

        src = (
            project_tree
            / "sorc"
            / "nexus.fd"
            / "config"
            / "gocart"
            / "NEXUS_Config.rc"
        )
        dst = expdir / "parm" / "chem" / "nexus" / "gocart" / "NEXUS_Config.rc"

        assert dst.exists()
        assert dst.read_bytes() == src.read_bytes()

    def test_upp_files_copied_verbatim(
        self, project_tree: Path, expdir: Path
    ):
        """UPP parm files are copied verbatim to EXPDIR."""
        _stage_submodule_copy(project_tree, expdir)

        src = project_tree / "sorc" / "upp.fd" / "parm" / "params_grib2_tbl_new"
        dst = expdir / "parm" / "post" / "params_grib2_tbl_new"

        assert dst.exists()
        assert dst.read_bytes() == src.read_bytes()

    def test_no_jinja2_rendering_on_submodule_files(
        self, project_tree: Path, expdir: Path
    ):
        """Submodule files with Jinja2-like syntax are NOT rendered."""
        # Add a file with Jinja2 syntax to the NEXUS source
        nexus_dir = (
            project_tree / "sorc" / "nexus.fd" / "config" / "gocart"
        )
        jinja_content = "value = {{ should_not_render }}\n@[ALSO_NOT]\n"
        (nexus_dir / "test_template.rc").write_text(jinja_content)

        _stage_submodule_copy(project_tree, expdir)

        dst = (
            expdir / "parm" / "chem" / "nexus" / "gocart" / "test_template.rc"
        )
        assert dst.read_text() == jinja_content

    def test_submodule_copy_returns_all_copied_paths(
        self, project_tree: Path, expdir: Path
    ):
        """_stage_submodule_copy returns paths of all copied files."""
        copied = _stage_submodule_copy(project_tree, expdir)

        assert len(copied) > 0
        for path in copied:
            assert path.is_file()
            assert str(path).startswith(str(expdir))


# ---------------------------------------------------------------------------
# Test Class: EXPDIR Manifest with SHA-256 Hashes
# ---------------------------------------------------------------------------


class TestEXPDIRManifest:
    """Test EXPDIR manifest includes all rendered configs with SHA-256 hashes.

    Traces to: Requirement 9.8
    """

    def test_all_rendered_files_have_sha256_hashes(self, expdir: Path):
        """Every RenderedFile from render_all has a valid SHA-256 hash."""
        context = _full_model_context()
        renderer = ModelConfigRenderer(dev_root=DEV_ROOT)

        results = renderer.render_all(context, expdir)

        for rf in results:
            # SHA-256 hash should be 64 hex characters
            assert len(rf.sha256) == 64, (
                f"Invalid SHA-256 hash length for {rf.path}: "
                f"got {len(rf.sha256)}"
            )
            # Hash should be valid hex
            int(rf.sha256, 16)

    def test_sha256_matches_file_content(self, expdir: Path):
        """SHA-256 in RenderedFile matches actual file content on disk."""
        context = _full_model_context()
        renderer = ModelConfigRenderer(dev_root=DEV_ROOT)

        results = renderer.render_all(context, expdir)

        for rf in results:
            actual_hash = _compute_sha256(rf.path)
            assert rf.sha256 == actual_hash, (
                f"SHA-256 mismatch for {rf.path}: "
                f"manifest={rf.sha256}, actual={actual_hash}"
            )

    def test_manifest_includes_all_coupled_model_configs(self, expdir: Path):
        """Manifest (render_all results) includes all coupled-model configs."""
        context = _full_model_context()
        renderer = ModelConfigRenderer(dev_root=DEV_ROOT)

        results = renderer.render_all(context, expdir)

        # Collect relative paths from results
        parm_ufs = expdir / "parm" / "ufs"
        rendered_rel_paths = set()
        for rf in results:
            if str(rf.path).startswith(str(parm_ufs)):
                rel = str(rf.path.relative_to(parm_ufs))
                rendered_rel_paths.add(rel)

        # All expected coupled-model outputs should be in the manifest
        for expected in EXPECTED_COUPLED_OUTPUTS:
            assert expected in rendered_rel_paths, (
                f"Expected '{expected}' in manifest but not found. "
                f"Got: {sorted(rendered_rel_paths)}"
            )

    def test_manifest_records_render_method(self, expdir: Path):
        """Coupled-model configs in manifest have method='render'."""
        context = _full_model_context()
        renderer = ModelConfigRenderer(dev_root=DEV_ROOT)

        results = renderer.render_all(context, expdir)

        parm_ufs = expdir / "parm" / "ufs"
        for rf in results:
            if str(rf.path).startswith(str(parm_ufs)):
                rel = str(rf.path.relative_to(parm_ufs))
                if rel in EXPECTED_COUPLED_OUTPUTS:
                    assert rf.method == "render", (
                        f"Expected method='render' for {rel}, "
                        f"got '{rf.method}'"
                    )


# ---------------------------------------------------------------------------
# Test Class: No Symlinks in EXPDIR
# ---------------------------------------------------------------------------


class TestNoSymlinksInEXPDIR:
    """Test no symlinks in EXPDIR after deployment.

    Traces to: Requirement 14.1
    """

    def test_no_symlinks_in_coupled_model_dirs(self, expdir: Path):
        """No symlinks exist in coupled-model config directories."""
        context = _full_model_context()
        renderer = ModelConfigRenderer(dev_root=DEV_ROOT)

        renderer.render_all(context, expdir)

        parm_ufs = expdir / "parm" / "ufs"
        for subdir in COUPLED_SUBDIRS:
            coupled_dir = parm_ufs / subdir
            if not coupled_dir.exists():
                continue
            for root, dirs, files in os.walk(coupled_dir):
                for filename in files:
                    filepath = Path(root) / filename
                    assert not filepath.is_symlink(), (
                        f"Found symlink in EXPDIR: {filepath}"
                    )

    def test_all_rendered_files_are_regular_files(self, expdir: Path):
        """All rendered coupled-model config files are regular files."""
        context = _full_model_context()
        renderer = ModelConfigRenderer(dev_root=DEV_ROOT)

        results = renderer.render_all(context, expdir)

        for rf in results:
            assert rf.path.is_file(), (
                f"Expected regular file at {rf.path}"
            )
            assert not rf.path.is_symlink(), (
                f"Expected regular file but found symlink at {rf.path}"
            )

    def test_no_symlinks_to_sorc_ufs_model(self, expdir: Path):
        """No symlinks to sorc/ufs_model.fd/tests/parm/ in EXPDIR."""
        context = _full_model_context()
        renderer = ModelConfigRenderer(dev_root=DEV_ROOT)

        renderer.render_all(context, expdir)

        for root, dirs, files in os.walk(expdir):
            for filename in files:
                filepath = Path(root) / filename
                if filepath.is_symlink():
                    link_target = str(os.readlink(filepath))
                    assert "sorc/ufs_model.fd/tests/parm/" not in link_target, (
                        f"Found symlink to sorc/ufs_model.fd/tests/parm/ "
                        f"in EXPDIR: {filepath} -> {link_target}"
                    )

    @pytest.mark.parametrize("resolution", ["025", "050", "100", "500"])
    def test_no_symlinks_for_any_resolution(
        self, resolution: str, expdir: Path
    ):
        """No symlinks in EXPDIR for any ocean resolution."""
        context = _full_model_context(ocean_resolution=resolution)
        renderer = ModelConfigRenderer(dev_root=DEV_ROOT)

        renderer.render_all(context, expdir)

        parm_ufs = expdir / "parm" / "ufs"
        for subdir in COUPLED_SUBDIRS:
            coupled_dir = parm_ufs / subdir
            if not coupled_dir.exists():
                continue
            for filepath in coupled_dir.rglob("*"):
                if filepath.is_file():
                    assert not filepath.is_symlink(), (
                        f"Found symlink for resolution={resolution}: "
                        f"{filepath}"
                    )
