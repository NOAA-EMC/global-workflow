"""Property-based tests for Shell Variable Preservation across coupled-model templates.

Uses hypothesis to verify Property 3: Shell Variable Preservation.
Generates valid Model_Context values, renders each template, and asserts
all ${VAR} shell variable patterns appear verbatim in the rendered output.

Feature: coupled-model-configs, Property 3: Shell Variable Preservation

Traces to: Requirements 1.8, 2.4, 3.7, 4.7, 5.5, 8.5

**Validates: Requirements 1.8, 2.4, 3.7, 4.7, 5.5, 8.5**
"""

from __future__ import annotations

import os
import re
import sys
from pathlib import Path

import pytest
from hypothesis import given, settings, HealthCheck
from hypothesis import strategies as st

# Add the workflow module to the path
sys.path.insert(0, os.path.join(os.path.dirname(__file__), "..", "..", "workflow"))

from deployment.template_renderer import TemplateRenderer

# ---------------------------------------------------------------------------
# Constants
# ---------------------------------------------------------------------------

DEV_ROOT = Path(__file__).resolve().parent.parent.parent

# Template paths
MOM_INPUT_TEMPLATE = DEV_ROOT / "parm" / "ufs" / "ocean" / "MOM_input.j2"
ICE_IN_TEMPLATE = DEV_ROOT / "parm" / "ufs" / "ice" / "ice_in.j2"
WW3_SHEL_TEMPLATE = DEV_ROOT / "parm" / "ufs" / "wave" / "ww3_shel.nml.j2"
INPUT_GLOBAL_NEST_TEMPLATE = DEV_ROOT / "parm" / "ufs" / "fv3" / "input_global_nest.nml.j2"

# Expected shell variables per template
MOM_INPUT_SHELL_VARS_ALWAYS = ["${TOPOEDITS}"]
MOM_INPUT_SHELL_VARS_RIVER_RUNOFF = ["${CHLCLIM}"]

ICE_IN_SHELL_VARS = ["${SYEAR}", "${SMONTH}", "${SDAY}", "${FHMAX}"]

WW3_SHEL_SHELL_VARS = ["${FHMAX_WAV}"]

# input_global_nest.nml.j2 uses both standard and substring syntax
INPUT_GLOBAL_NEST_SHELL_VARS = [
    "${FHMAX}",
    "${PDY:0:4}",
    "${PDY:4:2}",
    "${PDY:6:2}",
    "${cyc}",
]


# ---------------------------------------------------------------------------
# Hypothesis strategies for valid Model_Context generation
# ---------------------------------------------------------------------------


@st.composite
def valid_ocean_model_context(draw: st.DrawFn) -> dict:
    """Generate a valid ocean Model_Context with all required keys.

    Generates contexts that exercise both river_runoff=True and False
    to test conditional shell variable preservation.
    """
    resolution = draw(st.sampled_from(["025", "050", "100", "500"]))
    dt_ocean = draw(st.sampled_from([900, 1800, 3600, 7200]))
    dt_therm = draw(st.sampled_from([3600, 7200, 14400]))
    use_waves = draw(st.booleans())
    oda_incupd = draw(st.booleans())
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

    ocean_context = {
        "resolution": resolution,
        "dt_ocean": dt_ocean,
        "dt_therm": dt_therm,
        "use_waves": use_waves,
        "oda_incupd": oda_incupd,
        "do_sppt": do_sppt,
        "river_runoff": river_runoff,
        "diag_coord_def_z_file": diag_coord_def_z_file,
        "frunoff": frunoff,
    }

    return {"model": {"ocean": ocean_context}}


@st.composite
def valid_ice_model_context(draw: st.DrawFn) -> dict:
    """Generate a valid ice Model_Context with all required keys."""
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

    ice_context = {
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

    return {"model": {"ice": ice_context}}


@st.composite
def valid_wave_model_context(draw: st.DrawFn) -> dict:
    """Generate a valid wave Model_Context with all required keys."""
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

    wave_context = {
        "ice_input": ice_input,
        "current_input": current_input,
        "output_params": output_params,
        "dt_field_output": dt_field_output,
        "dt_point_output": dt_point_output,
        "grid_output_dir": grid_output_dir,
        "point_output_dir": point_output_dir,
        "restart_output_dir": restart_output_dir,
    }

    return {"model": {"wave": wave_context}}


@st.composite
def valid_fv3_nest_model_context(draw: st.DrawFn) -> dict:
    """Generate a valid FV3 nested grid Model_Context with all required keys."""
    total_tasks = draw(st.integers(min_value=1, max_value=2048))
    dt_atmos = draw(st.sampled_from([120, 180, 225, 300, 450, 600, 900]))
    restart_interval = draw(st.sampled_from([6, 12, 24, 48]))
    quilting = draw(st.booleans())
    write_group = draw(st.integers(min_value=1, max_value=4))
    wrttask_per_group = draw(st.integers(min_value=1, max_value=120))
    output_grid = draw(st.sampled_from([
        "gaussian_grid", "regional_latlon", "cubed_sphere_grid",
    ]))
    output_filetype_atm = draw(st.sampled_from(["netcdf", "netcdf_parallel"]))
    output_filetype_sfc = draw(st.sampled_from(["netcdf", "netcdf_parallel"]))
    imo = draw(st.sampled_from([384, 768, 1536, 3072]))
    jmo = draw(st.sampled_from([192, 384, 768, 1536]))
    output_fh = draw(st.sampled_from(["1 -1", "3 -1", "6 -1", "0 1 -1"]))
    do_nest = draw(st.booleans())
    npx_nest = draw(st.integers(min_value=100, max_value=2000))
    npy_nest = draw(st.integers(min_value=100, max_value=2000))

    fv3_context = {
        "total_tasks": total_tasks,
        "fhrot": 0,
        "restart_interval": restart_interval,
        "quilting": quilting,
        "quilting_restart": quilting,
        "write_group": write_group,
        "wrttask_per_group": wrttask_per_group,
        "output_filetype_atm": output_filetype_atm,
        "output_filetype_sfc": output_filetype_sfc,
        "imo": imo,
        "jmo": jmo,
        "output_fh": output_fh,
        "do_nest": do_nest,
        "npx_nest": npx_nest,
        "npy_nest": npy_nest,
    }

    model_context = {
        "model": {
            "fv3": fv3_context,
            "dt_atmos": dt_atmos,
            "output_grid": output_grid,
        },
    }

    return model_context


# ---------------------------------------------------------------------------
# Helper: render a template with given context
# ---------------------------------------------------------------------------


def _render_template(template_path: Path, context: dict) -> str:
    """Render a template with the given context and return the output string.

    Args:
        template_path: Path to the .j2 template file.
        context: Full rendering context dict.

    Returns:
        Rendered template content as a string.
    """
    searchpath = [
        str(DEV_ROOT / "parm" / "ufs"),
        str(DEV_ROOT / "parm" / "ufs" / "ocean"),
        str(DEV_ROOT / "parm" / "ufs" / "ice"),
        str(DEV_ROOT / "parm" / "ufs" / "wave"),
        str(DEV_ROOT / "parm" / "ufs" / "fv3"),
        str(DEV_ROOT / "parm"),
    ]
    searchpath = [p for p in searchpath if Path(p).is_dir()]

    renderer = TemplateRenderer(context=context, searchpath=searchpath, strict=True)
    return renderer.render_string(template_path.read_text(encoding="utf-8"))


# ---------------------------------------------------------------------------
# Property 3: Shell Variable Preservation
# ---------------------------------------------------------------------------


class TestShellVariablePreservation:
    """Property 3: Shell Variable Preservation.

    **Validates: Requirements 1.8, 2.4, 3.7, 4.7, 5.5, 8.5**

    For any rendered coupled-model config file, all ${VAR} shell variable
    patterns present in the source template SHALL appear verbatim in the
    rendered output, unmodified by Jinja2 resolution.
    """

    @settings(
        max_examples=100,
        suppress_health_check=[HealthCheck.too_slow],
        deadline=None,
    )
    @given(context=valid_ocean_model_context())
    def test_mom_input_preserves_topoedits(self, context: dict):
        """MOM_input always preserves ${TOPOEDITS} shell variable.

        **Validates: Requirements 1.8**
        """
        rendered = _render_template(MOM_INPUT_TEMPLATE, context)

        for shell_var in MOM_INPUT_SHELL_VARS_ALWAYS:
            assert shell_var in rendered, (
                f"Shell variable '{shell_var}' not preserved in rendered MOM_input. "
                f"Context: resolution={context['model']['ocean']['resolution']}"
            )

    @settings(
        max_examples=100,
        suppress_health_check=[HealthCheck.too_slow],
        deadline=None,
    )
    @given(context=valid_ocean_model_context())
    def test_mom_input_preserves_chlclim_when_river_runoff(self, context: dict):
        """MOM_input preserves ${CHLCLIM} when river_runoff is True.

        **Validates: Requirements 1.8**
        """
        ocean = context["model"]["ocean"]
        rendered = _render_template(MOM_INPUT_TEMPLATE, context)

        if ocean["river_runoff"]:
            for shell_var in MOM_INPUT_SHELL_VARS_RIVER_RUNOFF:
                assert shell_var in rendered, (
                    f"Shell variable '{shell_var}' not preserved in rendered "
                    f"MOM_input when river_runoff=True. "
                    f"Context: resolution={ocean['resolution']}"
                )

    @settings(
        max_examples=100,
        suppress_health_check=[HealthCheck.too_slow],
        deadline=None,
    )
    @given(context=valid_ice_model_context())
    def test_ice_in_preserves_shell_variables(self, context: dict):
        """ice_in preserves ${SYEAR}, ${SMONTH}, ${SDAY}, ${FHMAX}.

        **Validates: Requirements 3.7**
        """
        rendered = _render_template(ICE_IN_TEMPLATE, context)

        for shell_var in ICE_IN_SHELL_VARS:
            assert shell_var in rendered, (
                f"Shell variable '{shell_var}' not preserved in rendered ice_in. "
                f"Context: warm_start={context['model']['ice']['warm_start']}"
            )

    @settings(
        max_examples=100,
        suppress_health_check=[HealthCheck.too_slow],
        deadline=None,
    )
    @given(context=valid_wave_model_context())
    def test_ww3_shel_preserves_fhmax_wav(self, context: dict):
        """ww3_shel.nml preserves ${FHMAX_WAV} shell variable.

        **Validates: Requirements 4.7**
        """
        rendered = _render_template(WW3_SHEL_TEMPLATE, context)

        for shell_var in WW3_SHEL_SHELL_VARS:
            assert shell_var in rendered, (
                f"Shell variable '{shell_var}' not preserved in rendered "
                f"ww3_shel.nml. Context: ice_input={context['model']['wave']['ice_input']}"
            )

    @settings(
        max_examples=100,
        suppress_health_check=[HealthCheck.too_slow],
        deadline=None,
    )
    @given(context=valid_fv3_nest_model_context())
    def test_input_global_nest_preserves_shell_variables(self, context: dict):
        """input_global_nest.nml preserves ${FHMAX}, ${PDY:0:4}, ${cyc}.

        **Validates: Requirements 5.5**

        Note: ${PDY:0:4}, ${PDY:4:2}, ${PDY:6:2} use bash substring syntax.
        ${cyc} uses lowercase. Both forms must be preserved verbatim.
        """
        rendered = _render_template(INPUT_GLOBAL_NEST_TEMPLATE, context)

        for shell_var in INPUT_GLOBAL_NEST_SHELL_VARS:
            assert shell_var in rendered, (
                f"Shell variable '{shell_var}' not preserved in rendered "
                f"input_global_nest.nml. "
                f"Context: do_nest={context['model']['fv3']['do_nest']}"
            )
