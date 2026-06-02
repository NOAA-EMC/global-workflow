"""Property-based test: No Legacy atparse Tokens in rendered coupled-model configs.

Uses hypothesis to generate valid Model_Context values covering all 4 ocean
resolutions and boolean flag combinations, renders ALL coupled-model templates,
and asserts none contain the regex pattern @\\[[A-Za-z_][A-Za-z0-9_]*\\].

Feature: coupled-model-configs, Property 4: No Legacy atparse Tokens

**Validates: Requirements 11.1, 11.2, 11.3, 11.4**
"""

from __future__ import annotations

import os
import re
import sys
from pathlib import Path

import pytest
from hypothesis import given, settings, HealthCheck
from hypothesis import strategies as st

sys.path.insert(0, os.path.join(os.path.dirname(__file__), "..", "..", "workflow"))

from deployment.template_renderer import TemplateRenderer

# ---------------------------------------------------------------------------
# Constants
# ---------------------------------------------------------------------------

DEV_ROOT = Path(__file__).resolve().parent.parent.parent

# All coupled-model templates to render and check
COUPLED_TEMPLATES = {
    "ocean/MOM_input": DEV_ROOT / "parm" / "ufs" / "ocean" / "MOM_input.j2",
    "ocean/MOM6_data_table": DEV_ROOT / "parm" / "ufs" / "ocean" / "MOM6_data_table.j2",
    "ice/ice_in": DEV_ROOT / "parm" / "ufs" / "ice" / "ice_in.j2",
    "wave/ww3_shel.nml": DEV_ROOT / "parm" / "ufs" / "wave" / "ww3_shel.nml.j2",
    "fv3/input_global_nest.nml": DEV_ROOT / "parm" / "ufs" / "fv3" / "input_global_nest.nml.j2",
    "post/post_itag": DEV_ROOT / "parm" / "ufs" / "post" / "post_itag.j2",
}

# Legacy atparse substitution pattern: @[VAR_NAME]
_ATPARSE_RE = re.compile(r"@\[[A-Za-z_][A-Za-z0-9_]*\]")


# ---------------------------------------------------------------------------
# Hypothesis strategies for valid Model_Context generation
# ---------------------------------------------------------------------------


@st.composite
def valid_model_context(draw: st.DrawFn) -> dict:
    """Generate a valid Model_Context covering all coupled-model components.

    Produces contexts with all required keys for ocean, ice, wave, post,
    and fv3 sections, covering all 4 ocean resolutions and boolean flag
    combinations.
    """
    # --- Ocean context ---
    resolution = draw(st.sampled_from(["025", "050", "100", "500"]))
    use_waves = draw(st.booleans())
    river_runoff = draw(st.booleans())
    oda_incupd = draw(st.booleans())
    do_sppt = draw(st.booleans())
    dt_ocean = draw(st.sampled_from([450, 900, 1800, 3600, 7200]))
    dt_therm = draw(st.sampled_from([1800, 3600, 7200, 14400]))
    diag_coord_def_z_file = draw(st.sampled_from([
        "oceanda_zgrid_75L.nc",
        "oceanda_zgrid_100L.nc",
        "ocean_zgrid_50L.nc",
    ]))
    frunoff = draw(st.sampled_from([
        "INPUT/runoff.daitren.clim.nc",
        "INPUT/runoff.monthly.nc",
    ]))

    ocean_context = {
        "resolution": resolution,
        "dt_ocean": dt_ocean,
        "dt_therm": dt_therm,
        "nk": draw(st.sampled_from([50, 75, 100])),
        "use_waves": use_waves,
        "river_runoff": river_runoff,
        "oda_incupd": oda_incupd,
        "oda_incupd_nhours": draw(st.sampled_from([3, 6, 12, 24])),
        "do_sppt": do_sppt,
        "diag_coord_def_z_file": diag_coord_def_z_file,
        "frunoff": frunoff,
    }

    # --- Ice context ---
    warm_start = draw(st.booleans())
    ice_context = {
        "nprocs": draw(st.integers(min_value=1, max_value=512)),
        "decomposition": draw(st.sampled_from([
            "slenderX2", "slenderX1", "cartesian", "roundrobin",
        ])),
        "dt_ice": draw(st.sampled_from([450, 600, 900, 1800, 3600])),
        "grid": draw(st.sampled_from([
            "grid_cice_NEMS_mx025.nc",
            "grid_cice_NEMS_mx050.nc",
            "grid_cice_NEMS_mx100.nc",
        ])),
        "mask": draw(st.sampled_from([
            "kmtu_cice_NEMS_mx025.nc",
            "kmtu_cice_NEMS_mx050.nc",
            "kmtu_cice_NEMS_mx100.nc",
        ])),
        "nx_glb": draw(st.sampled_from([72, 360, 720, 1440])),
        "ny_glb": draw(st.sampled_from([35, 320, 576, 1080])),
        "warm_start": warm_start,
        "histfreq_n": draw(st.integers(min_value=1, max_value=30)),
        "hist_avg": draw(st.booleans()),
        "dumpfreq": draw(st.sampled_from(["d", "h", "m", "y"])),
        "dumpfreq_n": draw(st.integers(min_value=1, max_value=30)),
        "ktherm": draw(st.integers(min_value=0, max_value=2)),
        "tr_pond_lvl": draw(st.booleans()),
    }

    # --- Wave context ---
    wave_context = {
        "ice_input": draw(st.sampled_from(["CPL", "YES"])),
        "current_input": draw(st.sampled_from(["CPL", "YES"])),
        "output_params": draw(st.sampled_from([
            "HS FP DP PHS PTP PDIR CHA",
            "HS FP DP",
            "HS LM T02 T01 DIR DP SPR",
        ])),
        "dt_field_output": draw(st.integers(min_value=1, max_value=86400)),
        "dt_point_output": draw(st.integers(min_value=1, max_value=86400)),
        "grid_output_dir": draw(st.sampled_from(["./", "./OUTPUT/"])),
        "point_output_dir": draw(st.sampled_from(["./", "./OUTPUT/"])),
        "restart_output_dir": draw(st.sampled_from(["./RESTART/", "./restart/"])),
    }

    # --- Post context ---
    post_context = {
        "system": draw(st.sampled_from(["gfs", "gcafs", "gefs", "sfs"])),
    }

    # --- FV3 context (needed for input_global_nest.nml.j2) ---
    do_nest = draw(st.booleans())
    fv3_context = {
        "total_tasks": draw(st.integers(min_value=1, max_value=2048)),
        "fhrot": draw(st.sampled_from([0, 3, 6])),
        "restart_interval": draw(st.sampled_from([6, 12, 24, 48])),
        "quilting": draw(st.booleans()),
        "write_group": draw(st.integers(min_value=1, max_value=8)),
        "wrttask_per_group": draw(st.integers(min_value=1, max_value=120)),
        "output_filetype_atm": draw(st.sampled_from(["netcdf", "netcdf_parallel"])),
        "output_filetype_sfc": draw(st.sampled_from(["netcdf", "netcdf_parallel"])),
        "imo": draw(st.sampled_from([384, 768, 1536, 3072])),
        "jmo": draw(st.sampled_from([192, 384, 768, 1536])),
        "output_fh": draw(st.sampled_from(["1 -1", "3 -1", "6 -1"])),
        "iau_offset": draw(st.sampled_from([0, 3, 6])),
        "do_nest": do_nest,
    }
    if do_nest:
        fv3_context["npx_nest"] = draw(st.integers(min_value=100, max_value=2000))
        fv3_context["npy_nest"] = draw(st.integers(min_value=100, max_value=2000))

    # --- Top-level model context ---
    model_context = {
        "model": {
            "ocean": ocean_context,
            "ice": ice_context,
            "wave": wave_context,
            "post": post_context,
            "fv3": fv3_context,
            "dt_atmos": draw(st.sampled_from([225, 450, 600, 900])),
            "output_grid": draw(st.sampled_from([
                "gaussian_grid", "regional_latlon",
            ])),
        },
    }

    return model_context


# ---------------------------------------------------------------------------
# Helper: render a coupled-model template
# ---------------------------------------------------------------------------


def _render_template(template_path: Path, context: dict) -> str:
    """Render a coupled-model template with the given context.

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
        str(DEV_ROOT / "parm" / "ufs" / "post"),
        str(DEV_ROOT / "parm"),
    ]
    searchpath = [p for p in searchpath if Path(p).is_dir()]

    renderer = TemplateRenderer(context=context, searchpath=searchpath, strict=True)
    return renderer.render_string(template_path.read_text(encoding="utf-8"))


# ---------------------------------------------------------------------------
# Property 4: No Legacy atparse Tokens
# ---------------------------------------------------------------------------


class TestNoLegacyAtparseTokens:
    """Property 4: No Legacy atparse Tokens.

    **Validates: Requirements 11.1, 11.2, 11.3, 11.4**

    For any valid Model_Context, no rendered coupled-model config file SHALL
    contain the legacy @[...] atparse substitution pattern.
    """

    @settings(
        max_examples=100,
        suppress_health_check=[HealthCheck.too_slow],
        deadline=None,
    )
    @given(context=valid_model_context())
    def test_no_atparse_tokens_in_any_rendered_config(self, context: dict):
        """Assert no rendered coupled-model config contains @[...] atparse patterns.

        **Validates: Requirements 11.1, 11.2, 11.3, 11.4**

        Feature: coupled-model-configs, Property 4: No Legacy atparse Tokens

        Generates valid Model_Context values covering all 4 ocean resolutions
        and boolean flag combinations, renders ALL coupled-model templates,
        and asserts none contain the regex pattern @\\[[A-Za-z_][A-Za-z0-9_]*\\].
        """
        for template_name, template_path in COUPLED_TEMPLATES.items():
            if not template_path.exists():
                pytest.skip(f"Template {template_path} not found")

            rendered = _render_template(template_path, context)

            # Assert no legacy atparse tokens remain
            atparse_matches = _ATPARSE_RE.findall(rendered)
            assert atparse_matches == [], (
                f"Legacy @[...] atparse tokens found in rendered "
                f"'{template_name}': {atparse_matches}"
            )
