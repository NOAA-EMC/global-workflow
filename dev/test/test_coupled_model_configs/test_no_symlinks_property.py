"""Property-based test: No Symlinks in EXPDIR for coupled-model configs.

Uses hypothesis to generate valid deployment configurations and asserts
that the EXPDIR produced by ModelConfigRenderer.render_all() contains
no symlinks to sorc/ufs_model.fd/tests/parm/ for coupled-model configs,
and that all config files under parm/ufs/ocean/, parm/ufs/ice/,
parm/ufs/wave/, and parm/ufs/post/ are regular files (not symlinks).

Feature: coupled-model-configs, Property 10: No Symlinks in EXPDIR

**Validates: Requirements 14.1, 14.2**
"""

from __future__ import annotations

import os
import sys
from pathlib import Path

import pytest
from hypothesis import given, settings, HealthCheck
from hypothesis import strategies as st

sys.path.insert(0, os.path.join(os.path.dirname(__file__), "..", "..", "workflow"))

from deployment.model_config_renderer import ModelConfigRenderer
from deployment.template_renderer import TemplateRenderError

# ---------------------------------------------------------------------------
# Constants
# ---------------------------------------------------------------------------

DEV_ROOT = Path(__file__).resolve().parent.parent.parent

# Coupled-model subdirectories that must contain only regular files
COUPLED_SUBDIRS = ["ocean", "ice", "wave", "post"]

# Pattern that must NOT appear as a symlink target
SORC_SYMLINK_TARGET = "sorc/ufs_model.fd/tests/parm/"


# ---------------------------------------------------------------------------
# Hypothesis strategies for valid deployment configurations
# ---------------------------------------------------------------------------


@st.composite
def valid_deployment_config(draw: st.DrawFn) -> dict:
    """Generate a valid full model context for coupled-model rendering.

    Produces a complete model context with all required sections
    (top-level, fv3, ocean, ice, wave, post, aerosol) that will
    pass schema validation and render all templates successfully.
    """
    # Top-level atmosphere settings
    resolution = draw(st.sampled_from(["C96", "C384"]))
    physics_suite = draw(st.sampled_from(["gfdl", "thompson"]))
    coupling_mode = draw(st.sampled_from(["s2s", "s2sw", "s2swa"]))
    dt_atmos = draw(st.sampled_from([225, 450, 600, 900]))

    # Ocean settings
    ocean_resolution = draw(st.sampled_from(["025", "050", "100", "500"]))
    use_waves = draw(st.booleans())
    river_runoff = draw(st.booleans())
    oda_incupd = draw(st.booleans())
    do_sppt = draw(st.booleans())
    dt_ocean = draw(st.sampled_from([450, 900, 1800, 3600, 7200]))
    dt_therm = draw(st.sampled_from([1800, 3600, 7200, 14400]))

    # Ice settings
    warm_start = draw(st.booleans())
    ice_nprocs = draw(st.sampled_from([24, 48, 96, 192]))
    ice_decomposition = draw(st.sampled_from(["slenderX2", "slenderX1"]))
    dt_ice = draw(st.sampled_from([450, 600, 900, 1800]))
    histfreq_n = draw(st.integers(min_value=1, max_value=6))
    hist_avg = draw(st.booleans())
    ktherm = draw(st.integers(min_value=0, max_value=2))
    tr_pond_lvl = draw(st.booleans())

    # Wave settings
    ice_input = draw(st.sampled_from(["CPL", "YES"]))
    current_input = draw(st.sampled_from(["CPL", "YES"]))
    dt_field_output = draw(st.sampled_from([3600, 10800, 21600]))
    dt_point_output = draw(st.sampled_from([900, 1800, 3600]))

    # Post settings
    post_system = draw(st.sampled_from(["gfs", "gcafs"]))

    # Determine active components based on coupling mode
    if coupling_mode == "s2s":
        active_components = ["atmosphere", "ocean", "ice"]
    elif coupling_mode == "s2sw":
        active_components = ["atmosphere", "ocean", "ice", "wave"]
    elif coupling_mode == "s2swa":
        active_components = ["atmosphere", "ocean", "ice", "wave", "aerosol"]
    else:
        active_components = ["atmosphere", "ocean", "ice"]

    context = {
        "resolution": resolution,
        "physics_suite": physics_suite,
        "coupling_mode": coupling_mode,
        "dt_atmos": dt_atmos,
        "output_grid": "gaussian_grid",
        "output_fields": "standard",
        "pbl_scheme": "satmedmf",
        "progsigma": True,
        "coupling_interval_slow": 1800,
        "coupling_interval_fast": dt_atmos,
        "active_components": active_components,
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
            "do_nest": False,
        },
        "ocean": {
            "resolution": ocean_resolution,
            "dt_ocean": dt_ocean,
            "dt_therm": dt_therm,
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
            "nprocs": ice_nprocs,
            "omp_threads": 1,
            "decomposition": ice_decomposition,
            "dt_ice": dt_ice,
            "restart_interval": 6,
            "grid": "grid_cice_NEMS_mx025.nc",
            "mask": "kmtu_cice_NEMS_mx025.nc",
            "nx_glb": 1440,
            "ny_glb": 1080,
            "warm_start": warm_start,
            "histfreq_n": histfreq_n,
            "hist_avg": hist_avg,
            "dumpfreq": "d",
            "dumpfreq_n": 1,
            "ktherm": ktherm,
            "tr_pond_lvl": tr_pond_lvl,
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
            "dt_field_output": dt_field_output,
            "dt_point_output": dt_point_output,
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

    return context


# ---------------------------------------------------------------------------
# Property 10: No Symlinks in EXPDIR
# ---------------------------------------------------------------------------


class TestNoSymlinksInEXPDIR:
    """Property 10: No Symlinks in EXPDIR.

    **Validates: Requirements 14.1, 14.2**

    For any valid deployment configuration, the EXPDIR SHALL NOT contain
    symlinks to sorc/ufs_model.fd/tests/parm/ for any coupled-model
    configuration file. All config files SHALL be regular files.
    """

    @settings(
        max_examples=100,
        suppress_health_check=[HealthCheck.too_slow],
        deadline=None,
    )
    @given(model_context=valid_deployment_config())
    def test_no_symlinks_in_expdir(self, model_context: dict, tmp_path_factory):
        """Assert EXPDIR contains no symlinks for coupled-model configs.

        **Validates: Requirements 14.1, 14.2**

        Feature: coupled-model-configs, Property 10: No Symlinks in EXPDIR

        Uses ModelConfigRenderer.render_all() to render all templates to a
        temp EXPDIR, then walks the EXPDIR and asserts:
        1. No symlinks to sorc/ufs_model.fd/tests/parm/ exist
        2. All files under parm/ufs/ocean/, parm/ufs/ice/, parm/ufs/wave/,
           parm/ufs/post/ are regular files (not symlinks)
        """
        expdir = tmp_path_factory.mktemp("expdir")

        renderer = ModelConfigRenderer(dev_root=DEV_ROOT)

        # Render all templates — this produces the EXPDIR content
        try:
            results = renderer.render_all(model_context, expdir)
        except TemplateRenderError as e:
            pytest.fail(
                f"render_all() failed for context "
                f"(ocean.resolution={model_context['ocean']['resolution']}, "
                f"coupling_mode={model_context['coupling_mode']}, "
                f"ice.warm_start={model_context['ice']['warm_start']}): {e}"
            )

        # At least one file must have been rendered
        assert len(results) > 0, "No files were rendered"

        # --- Assertion 1: No symlinks to sorc/ufs_model.fd/tests/parm/ ---
        # Walk the entire EXPDIR looking for symlinks
        for root, dirs, files in os.walk(expdir):
            for filename in files:
                filepath = Path(root) / filename
                if filepath.is_symlink():
                    link_target = str(os.readlink(filepath))
                    assert SORC_SYMLINK_TARGET not in link_target, (
                        f"Found symlink to sorc/ufs_model.fd/tests/parm/ "
                        f"in EXPDIR: {filepath} -> {link_target}"
                    )

        # --- Assertion 2: All coupled-model config files are regular files ---
        parm_ufs_dir = expdir / "parm" / "ufs"
        for subdir in COUPLED_SUBDIRS:
            coupled_dir = parm_ufs_dir / subdir
            if not coupled_dir.exists():
                continue

            for root, dirs, files in os.walk(coupled_dir):
                for filename in files:
                    filepath = Path(root) / filename
                    assert not filepath.is_symlink(), (
                        f"Expected regular file but found symlink at "
                        f"{filepath} (target: {os.readlink(filepath)})"
                    )
                    assert filepath.is_file(), (
                        f"Expected regular file at {filepath}, "
                        f"but it is not a regular file"
                    )

    @settings(
        max_examples=100,
        suppress_health_check=[HealthCheck.too_slow],
        deadline=None,
    )
    @given(model_context=valid_deployment_config())
    def test_rendered_files_are_regular_files_in_manifest(
        self, model_context: dict, tmp_path_factory
    ):
        """Assert all RenderedFile entries point to regular files, not symlinks.

        **Validates: Requirements 14.1, 14.2**

        Verifies that every file in the render_all() result list is a
        regular file on disk (not a symlink, directory, or special file).
        """
        expdir = tmp_path_factory.mktemp("expdir")

        renderer = ModelConfigRenderer(dev_root=DEV_ROOT)

        try:
            results = renderer.render_all(model_context, expdir)
        except TemplateRenderError as e:
            pytest.fail(
                f"render_all() failed: {e}"
            )

        # Every rendered file must be a regular file (not a symlink)
        for rendered_file in results:
            path = rendered_file.path
            assert path.exists(), (
                f"Rendered file does not exist: {path}"
            )
            assert not path.is_symlink(), (
                f"Rendered file is a symlink (should be regular file): "
                f"{path} -> {os.readlink(path)}"
            )
            assert path.is_file(), (
                f"Rendered file is not a regular file: {path}"
            )
