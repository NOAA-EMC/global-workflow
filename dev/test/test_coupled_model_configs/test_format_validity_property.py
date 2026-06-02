"""Property-based tests for Format Validity of all rendered coupled-model configs.

Uses hypothesis to verify Property 2: Format Validity (All Rendered Configs).
Generates valid Model_Context (ocean resolution × ice decomposition × wave coupling
mode × post system) and asserts every rendered coupled-model config passes its
format-specific validator.

Feature: coupled-model-configs, Property 2: Format Validity (All Rendered Configs)

Traces to: Requirements 3.5, 4.5, 10.1, 10.2, 10.3, 10.4
"""

from __future__ import annotations

import os
import sys
from pathlib import Path

import pytest
from hypothesis import given, settings, HealthCheck
from hypothesis import strategies as st

# Add the workflow module to the path
sys.path.insert(0, os.path.join(os.path.dirname(__file__), "..", "..", "workflow"))

from deployment.template_renderer import TemplateRenderer
from deployment.validators import (
    MOM6ParameterValidator,
    ModelConfigureValidator,
    NamelistValidator,
)

# ---------------------------------------------------------------------------
# Constants
# ---------------------------------------------------------------------------

DEV_ROOT = Path(__file__).resolve().parent.parent.parent

# Template paths
MOM_INPUT_TEMPLATE = DEV_ROOT / "parm" / "ufs" / "ocean" / "MOM_input.j2"
MOM6_DATA_TABLE_TEMPLATE = DEV_ROOT / "parm" / "ufs" / "ocean" / "MOM6_data_table.j2"
ICE_IN_TEMPLATE = DEV_ROOT / "parm" / "ufs" / "ice" / "ice_in.j2"
WW3_SHEL_TEMPLATE = DEV_ROOT / "parm" / "ufs" / "wave" / "ww3_shel.nml.j2"
INPUT_GLOBAL_NEST_TEMPLATE = DEV_ROOT / "parm" / "ufs" / "fv3" / "input_global_nest.nml.j2"
POST_ITAG_TEMPLATE = DEV_ROOT / "parm" / "ufs" / "post" / "post_itag.j2"

# Validator instances
mom6_validator = MOM6ParameterValidator()
namelist_validator = NamelistValidator()
model_configure_validator = ModelConfigureValidator()


# ---------------------------------------------------------------------------
# Hypothesis strategies for valid Model_Context generation
# ---------------------------------------------------------------------------


@st.composite
def valid_ocean_context(draw: st.DrawFn) -> dict:
    """Generate a valid ocean Model_Context dict covering all 4 resolutions."""
    resolution = draw(st.sampled_from(["025", "050", "100", "500"]))
    dt_ocean = draw(st.sampled_from([900, 1800, 3600, 7200]))
    dt_therm = draw(st.sampled_from([3600, 7200, 14400]))
    use_waves = draw(st.booleans())
    oda_incupd = draw(st.booleans())
    oda_incupd_nhours = draw(st.integers(min_value=1, max_value=24))
    do_sppt = draw(st.booleans())
    river_runoff = draw(st.booleans())
    diag_coord_def_z_file = draw(st.sampled_from([
        "oceanda_zgrid_75L.nc",
        "oceanda_zgrid_50L.nc",
        "ocean_zgrid_100L.nc",
    ]))
    frunoff = draw(st.sampled_from([
        "INPUT/runoff.daitren.clim.nc",
        "INPUT/runoff.monthly.nc",
    ]))

    return {
        "resolution": resolution,
        "dt_ocean": dt_ocean,
        "dt_therm": dt_therm,
        "use_waves": use_waves,
        "oda_incupd": oda_incupd,
        "oda_incupd_nhours": oda_incupd_nhours,
        "do_sppt": do_sppt,
        "river_runoff": river_runoff,
        "diag_coord_def_z_file": diag_coord_def_z_file,
        "frunoff": frunoff,
    }


@st.composite
def valid_ice_context(draw: st.DrawFn) -> dict:
    """Generate a valid ice Model_Context dict with decomposition params."""
    nprocs = draw(st.integers(min_value=1, max_value=512))
    decomposition = draw(st.sampled_from([
        "slenderX2", "slenderX1", "cartesian", "roundrobin",
    ]))
    dt_ice = draw(st.sampled_from([450, 600, 900, 1800, 3600]))
    grid = draw(st.sampled_from([
        "grid_cice_NEMS_mx025.nc",
        "grid_cice_NEMS_mx050.nc",
        "grid_cice_NEMS_mx100.nc",
    ]))
    mask = draw(st.sampled_from([
        "kmtu_cice_NEMS_mx025.nc",
        "kmtu_cice_NEMS_mx050.nc",
        "kmtu_cice_NEMS_mx100.nc",
    ]))
    nx_glb = draw(st.sampled_from([72, 360, 720, 1440]))
    ny_glb = draw(st.sampled_from([35, 320, 576, 1080]))
    warm_start = draw(st.booleans())
    histfreq_n = draw(st.integers(min_value=1, max_value=30))
    hist_avg = draw(st.booleans())
    dumpfreq = draw(st.sampled_from(["d", "h", "m", "y"]))
    dumpfreq_n = draw(st.integers(min_value=1, max_value=30))
    ktherm = draw(st.integers(min_value=0, max_value=2))
    tr_pond_lvl = draw(st.booleans())

    return {
        "nprocs": nprocs,
        "decomposition": decomposition,
        "dt_ice": dt_ice,
        "grid": grid,
        "mask": mask,
        "nx_glb": nx_glb,
        "ny_glb": ny_glb,
        "warm_start": warm_start,
        "histfreq_n": histfreq_n,
        "hist_avg": hist_avg,
        "dumpfreq": dumpfreq,
        "dumpfreq_n": dumpfreq_n,
        "ktherm": ktherm,
        "tr_pond_lvl": tr_pond_lvl,
    }


@st.composite
def valid_wave_context(draw: st.DrawFn) -> dict:
    """Generate a valid wave Model_Context dict with coupling modes."""
    ice_input = draw(st.sampled_from(["CPL", "YES"]))
    current_input = draw(st.sampled_from(["CPL", "YES"]))
    output_params = draw(st.sampled_from([
        "HS FP DP PHS PTP PDIR CHA",
        "HS FP DP",
        "HS LM T02 T01 DIR DP SPR",
    ]))
    dt_field_output = draw(st.integers(min_value=1, max_value=86400))
    dt_point_output = draw(st.integers(min_value=1, max_value=86400))
    grid_output_dir = draw(st.sampled_from(["./", "./OUTPUT/"]))
    point_output_dir = draw(st.sampled_from(["./", "./OUTPUT/"]))
    restart_output_dir = draw(st.sampled_from(["./RESTART/", "./restart/"]))

    return {
        "ice_input": ice_input,
        "current_input": current_input,
        "output_params": output_params,
        "dt_field_output": dt_field_output,
        "dt_point_output": dt_point_output,
        "grid_output_dir": grid_output_dir,
        "point_output_dir": point_output_dir,
        "restart_output_dir": restart_output_dir,
    }


@st.composite
def valid_post_context(draw: st.DrawFn) -> dict:
    """Generate a valid post Model_Context dict with all post systems."""
    system = draw(st.sampled_from(["gfs", "gcafs", "gefs", "sfs"]))
    return {"system": system}


@st.composite
def valid_fv3_context(draw: st.DrawFn) -> dict:
    """Generate a valid fv3 Model_Context dict for nested grid config."""
    do_nest = draw(st.booleans())
    total_tasks = draw(st.integers(min_value=1, max_value=2048))
    fhrot = draw(st.integers(min_value=0, max_value=24))
    restart_interval = draw(st.sampled_from([6, 12, 24, 48]))
    quilting = draw(st.booleans())
    write_group = draw(st.integers(min_value=1, max_value=8))
    wrttask_per_group = draw(st.integers(min_value=1, max_value=120))
    num_output_files = draw(st.sampled_from([2, 3]))
    output_filetype_atm = draw(st.sampled_from(["netcdf", "netcdf_parallel"]))
    output_filetype_sfc = draw(st.sampled_from(["netcdf", "netcdf_parallel"]))
    imo = draw(st.sampled_from([384, 768, 1536, 3072]))
    jmo = draw(st.sampled_from([192, 384, 768, 1536]))
    output_fh = draw(st.sampled_from(["1 -1", "3 -1", "6 -1"]))
    iau_offset = draw(st.integers(min_value=0, max_value=6))

    fv3 = {
        "do_nest": do_nest,
        "total_tasks": total_tasks,
        "fhrot": fhrot,
        "restart_interval": restart_interval,
        "quilting": quilting,
        "quilting_restart": quilting,
        "write_group": write_group,
        "wrttask_per_group": wrttask_per_group,
        "num_output_files": num_output_files,
        "output_filetype_atm": output_filetype_atm,
        "output_filetype_sfc": output_filetype_sfc,
        "imo": imo,
        "jmo": jmo,
        "output_fh": output_fh,
        "iau_offset": iau_offset,
    }

    if do_nest:
        fv3["npx_nest"] = draw(st.sampled_from([397, 793, 1585]))
        fv3["npy_nest"] = draw(st.sampled_from([397, 793, 1585]))

    return fv3


@st.composite
def valid_full_model_context(draw: st.DrawFn) -> dict:
    """Generate a complete valid Model_Context covering all coupled components.

    Combines ocean, ice, wave, post, and fv3 contexts into a single
    model context dict suitable for rendering all templates.

    Note: dt_atmos and output_grid are placed inside the 'model' dict
    because templates access them as model.dt_atmos and model.output_grid.
    """
    ocean = draw(valid_ocean_context())
    ice = draw(valid_ice_context())
    wave = draw(valid_wave_context())
    post = draw(valid_post_context())
    fv3 = draw(valid_fv3_context())

    dt_atmos = draw(st.sampled_from([225, 450, 600, 900]))
    output_grid = draw(st.sampled_from([
        "gaussian_grid", "regional_latlon", "cubed_sphere_grid",
    ]))

    return {
        "model": {
            "ocean": ocean,
            "ice": ice,
            "wave": wave,
            "post": post,
            "fv3": fv3,
            "dt_atmos": dt_atmos,
            "output_grid": output_grid,
        },
    }


# ---------------------------------------------------------------------------
# Helper: create a renderer for the given context
# ---------------------------------------------------------------------------


def _create_renderer(context: dict) -> TemplateRenderer:
    """Create a TemplateRenderer with the standard searchpath for UFS templates."""
    searchpath = [
        str(DEV_ROOT / "parm" / "ufs"),
        str(DEV_ROOT / "parm" / "ufs" / "ocean"),
        str(DEV_ROOT / "parm" / "ufs" / "ice"),
        str(DEV_ROOT / "parm" / "ufs" / "wave"),
        str(DEV_ROOT / "parm" / "ufs" / "fv3"),
        str(DEV_ROOT / "parm" / "ufs" / "post"),
        str(DEV_ROOT / "parm"),
    ]
    searchpath = [p for p in searchpath if Path(p).is_dir()]
    return TemplateRenderer(context=context, searchpath=searchpath, strict=True)


# ---------------------------------------------------------------------------
# Property 2: Format Validity (All Rendered Configs)
# ---------------------------------------------------------------------------


class TestFormatValidityAllRenderedConfigs:
    """Property 2: Format Validity (All Rendered Configs).

    **Validates: Requirements 3.5, 4.5, 10.1, 10.2, 10.3, 10.4**

    For any valid Model_Context (any supported ocean resolution × ice
    decomposition × wave coupling mode × post system), every rendered
    coupled-model config file SHALL pass its format-specific validator
    without errors:
    - MOM_input passes MOM6ParameterValidator
    - ice_in passes NamelistValidator
    - ww3_shel.nml passes NamelistValidator
    - input_global_nest.nml passes ModelConfigureValidator
    """

    @settings(
        max_examples=100,
        suppress_health_check=[HealthCheck.too_slow],
        deadline=None,
    )
    @given(context=valid_full_model_context())
    def test_format_validity_all_rendered_configs(self, context: dict):
        """Assert every rendered coupled-model config passes its format validator.

        **Validates: Requirements 3.5, 4.5, 10.1, 10.2, 10.3, 10.4**

        Feature: coupled-model-configs, Property 2: Format Validity (All Rendered Configs)

        Generates valid Model_Context covering all 4 ocean resolutions, ice
        decomposition params, wave coupling modes (CPL/YES), and post systems
        (gfs/gcafs/gefs/sfs). Renders ALL coupled-model templates and validates
        each with its format-specific validator.
        """
        # Flatten context for rendering: model.X accessed as model.X in templates
        renderer = _create_renderer(context)

        # --- MOM_input → MOM6ParameterValidator ---
        mom_input_rendered = renderer.render_string(
            MOM_INPUT_TEMPLATE.read_text(encoding="utf-8")
        )
        mom_errors = mom6_validator.validate(mom_input_rendered, "MOM_input")
        assert mom_errors == [], (
            f"MOM_input failed MOM6ParameterValidator with context "
            f"resolution={context['model']['ocean']['resolution']}: {mom_errors}"
        )

        # --- MOM6_data_table: no format-specific validator (simple text) ---
        # Render to ensure no template errors, but no validator needed
        mom6_data_table_rendered = renderer.render_string(
            MOM6_DATA_TABLE_TEMPLATE.read_text(encoding="utf-8")
        )
        # MOM6_data_table is simple CSV-like text; just verify it renders
        assert mom6_data_table_rendered.strip() != "", (
            "MOM6_data_table rendered to empty content"
        )

        # --- ice_in → NamelistValidator ---
        ice_in_rendered = renderer.render_string(
            ICE_IN_TEMPLATE.read_text(encoding="utf-8")
        )
        ice_errors = namelist_validator.validate(ice_in_rendered, "ice_in")
        assert ice_errors == [], (
            f"ice_in failed NamelistValidator with context "
            f"nprocs={context['model']['ice']['nprocs']}, "
            f"decomposition={context['model']['ice']['decomposition']}: "
            f"{ice_errors}"
        )

        # --- ww3_shel.nml → NamelistValidator ---
        ww3_rendered = renderer.render_string(
            WW3_SHEL_TEMPLATE.read_text(encoding="utf-8")
        )
        ww3_errors = namelist_validator.validate(ww3_rendered, "ww3_shel.nml")
        assert ww3_errors == [], (
            f"ww3_shel.nml failed NamelistValidator with context "
            f"ice_input={context['model']['wave']['ice_input']}, "
            f"current_input={context['model']['wave']['current_input']}: "
            f"{ww3_errors}"
        )

        # --- input_global_nest.nml → ModelConfigureValidator ---
        nest_rendered = renderer.render_string(
            INPUT_GLOBAL_NEST_TEMPLATE.read_text(encoding="utf-8")
        )
        nest_errors = model_configure_validator.validate(
            nest_rendered, "input_global_nest.nml"
        )
        assert nest_errors == [], (
            f"input_global_nest.nml failed ModelConfigureValidator with context "
            f"do_nest={context['model']['fv3']['do_nest']}: {nest_errors}"
        )

        # --- post_itag: rendered as namelist format (uses &group / /) ---
        # The post_itag.j2 template produces Fortran namelist syntax
        # but per the design doc, post_itag has no validator (simple text).
        # We still render it to ensure no template errors.
        post_rendered = renderer.render_string(
            POST_ITAG_TEMPLATE.read_text(encoding="utf-8")
        )
        assert post_rendered.strip() != "", (
            f"post_itag rendered to empty content for system="
            f"{context['model']['post']['system']}"
        )
