"""Property-based test: Template Equivalence for MOM_input.j2.

Uses hypothesis to generate all 4 ocean resolutions × valid Model_Context
variable combinations and asserts the rendered MOM_input.j2 output:
1. Contains correct resolution-specific values (NIGLOBAL, NJGLOBAL, KHTH, KHTR)
2. Contains correct conditional blocks based on boolean flags
3. Passes MOM6ParameterValidator
4. Contains no @[...] atparse tokens

Feature: coupled-model-configs, Property 1: Template Equivalence

Validates: Requirements 1.2, 1.3, 1.4, 1.5, 2.3, 11.5
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
from deployment.validators.mom6_parameter import MOM6ParameterValidator

# ---------------------------------------------------------------------------
# Constants
# ---------------------------------------------------------------------------

DEV_ROOT = Path(__file__).resolve().parent.parent.parent
TEMPLATE_PATH = DEV_ROOT / "parm" / "ufs" / "ocean" / "MOM_input.j2"

# Atparse pattern regex
_ATPARSE_RE = re.compile(r"@\[[A-Za-z_][A-Za-z0-9_]*\]")

# Resolution-specific expected values from the design document
RESOLUTION_EXPECTED = {
    "025": {"NIGLOBAL": 1440, "NJGLOBAL": 1080, "KHTH": 10.0, "KHTR": 10.0},
    "050": {"NIGLOBAL": 720, "NJGLOBAL": 576, "KHTH": 50.0, "KHTR": 50.0},
    "100": {"NIGLOBAL": 360, "NJGLOBAL": 320, "KHTH": 600.0, "KHTR": 600.0},
    "500": {"NIGLOBAL": 72, "NJGLOBAL": 35, "KHTH": 1000.0, "KHTR": 1000.0},
}

# Additional resolution-specific mixing params (non-500 resolutions)
RESOLUTION_MIXING = {
    "025": {"SMAG_BI_CONST": 0.06, "AH_VEL_SCALE": 0.01},
    "050": {"SMAG_BI_CONST": 0.06, "AH_VEL_SCALE": 0.01},
    "100": {"SMAG_BI_CONST": 0.15, "AH_VEL_SCALE": 0.05},
}


# ---------------------------------------------------------------------------
# Hypothesis strategies for valid ocean Model_Context generation
# ---------------------------------------------------------------------------

@st.composite
def valid_ocean_model_context(draw: st.DrawFn) -> dict:
    """Generate a valid ocean Model_Context dict for MOM_input rendering.

    Generates all 4 ocean resolutions with valid variable combinations
    for boolean flags, string values, and integer values.
    """
    resolution = draw(st.sampled_from(["025", "050", "100", "500"]))
    use_waves = draw(st.booleans())
    river_runoff = draw(st.booleans())
    oda_incupd = draw(st.booleans())
    do_sppt = draw(st.booleans())

    # Valid string values
    diag_coord_def_z_file = draw(st.sampled_from([
        "oceanda_zgrid_75L.nc",
        "oceanda_zgrid_100L.nc",
        "ocean_zgrid_50L.nc",
        "diag_coord_z.nc",
    ]))
    frunoff = draw(st.sampled_from([
        "INPUT/runoff.daitren.clim.nc",
        "INPUT/runoff.monthly.nc",
        "INPUT/river_runoff.nc",
    ]))

    # Valid int values
    dt_ocean = draw(st.sampled_from([450, 900, 1800, 3600, 7200]))
    dt_therm = draw(st.sampled_from([1800, 3600, 7200, 14400]))
    nk = draw(st.sampled_from([50, 75, 100, 127]))
    oda_incupd_nhours = draw(st.sampled_from([3, 6, 12, 24]))

    ocean_context = {
        "resolution": resolution,
        "dt_ocean": dt_ocean,
        "dt_therm": dt_therm,
        "nk": nk,
        "use_waves": use_waves,
        "river_runoff": river_runoff,
        "oda_incupd": oda_incupd,
        "oda_incupd_nhours": oda_incupd_nhours,
        "do_sppt": do_sppt,
        "diag_coord_def_z_file": diag_coord_def_z_file,
        "frunoff": frunoff,
    }

    return {"model": {"ocean": ocean_context}}


# ---------------------------------------------------------------------------
# Helper functions
# ---------------------------------------------------------------------------

def _render_mom_input(context: dict) -> str:
    """Render MOM_input.j2 with the given context and return the output string."""
    searchpath = [
        str(DEV_ROOT / "parm" / "ufs" / "ocean"),
        str(DEV_ROOT / "parm" / "ufs"),
        str(DEV_ROOT / "parm"),
    ]
    searchpath = [p for p in searchpath if Path(p).is_dir()]

    renderer = TemplateRenderer(context=context, searchpath=searchpath, strict=True)
    return renderer.render_string(TEMPLATE_PATH.read_text(encoding="utf-8"))


def _extract_param_value(content: str, param_name: str) -> str | None:
    """Extract the value of a MOM6 parameter from rendered content.

    Looks for lines matching: PARAM_NAME = VALUE
    Returns the value string (stripped) or None if not found.
    """
    pattern = re.compile(rf"^\s*{re.escape(param_name)}\s*=\s*(.+?)(?:\s*!.*)?$", re.MULTILINE)
    match = pattern.search(content)
    if match:
        return match.group(1).strip()
    return None


# ---------------------------------------------------------------------------
# Property Test: Template Equivalence (MOM_input)
# ---------------------------------------------------------------------------


class TestMOMInputTemplateEquivalence:
    """Property 1: Template Equivalence (Coupled-Model Configs).

    For any supported ocean resolution and valid Model_Context variable values,
    rendering MOM_input.j2 SHALL produce output that:
    1. Contains correct resolution-specific values (NIGLOBAL, NJGLOBAL, KHTH, KHTR)
    2. Contains correct conditional blocks based on boolean flags
    3. Passes MOM6ParameterValidator
    4. Contains no @[...] atparse tokens

    Feature: coupled-model-configs, Property 1: Template Equivalence

    **Validates: Requirements 1.2, 1.3, 1.4, 1.5, 2.3, 11.5**
    """

    @settings(
        max_examples=100,
        suppress_health_check=[HealthCheck.too_slow],
        deadline=None,
    )
    @given(context=valid_ocean_model_context())
    def test_template_equivalence_mom_input(self, context: dict):
        """Assert rendered MOM_input.j2 is functionally equivalent to legacy output.

        **Validates: Requirements 1.2, 1.3, 1.4, 1.5, 2.3, 11.5**

        Generates valid ocean Model_Context dicts across all 4 resolutions
        and valid variable combinations, renders MOM_input.j2, and verifies:
        - Resolution-specific grid dimensions and mixing coefficients
        - Conditional blocks for boolean flags
        - MOM6 format validity
        - No legacy atparse tokens
        """
        ocean = context["model"]["ocean"]
        resolution = ocean["resolution"]

        # Render the template
        rendered = _render_mom_input(context)

        # --- 1. Resolution-specific values ---
        expected = RESOLUTION_EXPECTED[resolution]

        # Check NIGLOBAL
        niglobal = _extract_param_value(rendered, "NIGLOBAL")
        assert niglobal is not None, f"NIGLOBAL not found in rendered output for res={resolution}"
        assert int(niglobal) == expected["NIGLOBAL"], (
            f"NIGLOBAL={niglobal} != expected {expected['NIGLOBAL']} for res={resolution}"
        )

        # Check NJGLOBAL
        njglobal = _extract_param_value(rendered, "NJGLOBAL")
        assert njglobal is not None, f"NJGLOBAL not found in rendered output for res={resolution}"
        assert int(njglobal) == expected["NJGLOBAL"], (
            f"NJGLOBAL={njglobal} != expected {expected['NJGLOBAL']} for res={resolution}"
        )

        # Check KHTH
        khth = _extract_param_value(rendered, "KHTH")
        assert khth is not None, f"KHTH not found in rendered output for res={resolution}"
        assert float(khth) == expected["KHTH"], (
            f"KHTH={khth} != expected {expected['KHTH']} for res={resolution}"
        )

        # Check KHTR
        khtr = _extract_param_value(rendered, "KHTR")
        assert khtr is not None, f"KHTR not found in rendered output for res={resolution}"
        assert float(khtr) == expected["KHTR"], (
            f"KHTR={khtr} != expected {expected['KHTR']} for res={resolution}"
        )

        # Check resolution-specific mixing params (non-500 only)
        if resolution in RESOLUTION_MIXING:
            for param, expected_val in RESOLUTION_MIXING[resolution].items():
                val = _extract_param_value(rendered, param)
                assert val is not None, (
                    f"{param} not found in rendered output for res={resolution}"
                )
                assert float(val) == expected_val, (
                    f"{param}={val} != expected {expected_val} for res={resolution}"
                )

        # Check USE_VARIABLE_MIXING and SMAGORINSKY_AH
        use_var_mixing = _extract_param_value(rendered, "USE_VARIABLE_MIXING")
        smagorinsky_ah = _extract_param_value(rendered, "SMAGORINSKY_AH")
        if resolution != "500":
            assert use_var_mixing == "True", (
                f"USE_VARIABLE_MIXING should be True for res={resolution}"
            )
            assert smagorinsky_ah == "True", (
                f"SMAGORINSKY_AH should be True for res={resolution}"
            )
        else:
            assert use_var_mixing == "False", (
                f"USE_VARIABLE_MIXING should be False for res=500"
            )
            assert smagorinsky_ah == "False", (
                f"SMAGORINSKY_AH should be False for res=500"
            )

        # --- 2. Conditional blocks based on boolean flags ---

        # DT and DT_THERM should match context values
        dt_val = _extract_param_value(rendered, "DT")
        assert dt_val is not None, "DT not found in rendered output"
        assert int(dt_val) == ocean["dt_ocean"], (
            f"DT={dt_val} != expected {ocean['dt_ocean']}"
        )

        dt_therm_val = _extract_param_value(rendered, "DT_THERM")
        assert dt_therm_val is not None, "DT_THERM not found in rendered output"
        assert int(dt_therm_val) == ocean["dt_therm"], (
            f"DT_THERM={dt_therm_val} != expected {ocean['dt_therm']}"
        )

        # NK should match context value
        nk_val = _extract_param_value(rendered, "NK")
        assert nk_val is not None, "NK not found in rendered output"
        assert int(nk_val) == ocean["nk"], (
            f"NK={nk_val} != expected {ocean['nk']}"
        )

        # use_waves conditional
        if ocean["use_waves"]:
            assert "USE_WAVES = True" in rendered, (
                "USE_WAVES = True should appear when use_waves is True"
            )
            assert 'WAVE_METHOD = "SURFACE_BANDS"' in rendered, (
                "WAVE_METHOD should appear when use_waves is True"
            )
        else:
            assert "USE_WAVES = True" not in rendered, (
                "USE_WAVES should not appear when use_waves is False"
            )

        # river_runoff conditional
        if ocean["river_runoff"]:
            assert "RIVER_RUNOFF = True" in rendered, (
                "RIVER_RUNOFF = True should appear when river_runoff is True"
            )
            assert "${CHLCLIM}" in rendered, (
                "FRUNOFF shell variable should be preserved when river_runoff is True"
            )
        else:
            assert "RIVER_RUNOFF = True" not in rendered, (
                "RIVER_RUNOFF should not appear when river_runoff is False"
            )

        # oda_incupd conditional
        if ocean["oda_incupd"]:
            assert "ODA_INCUPD = True" in rendered, (
                "ODA_INCUPD = True should appear when oda_incupd is True"
            )
            oda_nhours = _extract_param_value(rendered, "ODA_INCUPD_NHOURS")
            assert oda_nhours is not None, (
                "ODA_INCUPD_NHOURS should appear when oda_incupd is True"
            )
            assert int(oda_nhours) == ocean["oda_incupd_nhours"], (
                f"ODA_INCUPD_NHOURS={oda_nhours} != expected {ocean['oda_incupd_nhours']}"
            )
        else:
            assert "ODA_INCUPD = False" in rendered, (
                "ODA_INCUPD = False should appear when oda_incupd is False"
            )
            assert "ODA_INCUPD_NHOURS" not in rendered, (
                "ODA_INCUPD_NHOURS should not appear when oda_incupd is False"
            )

        # do_sppt conditional
        if ocean["do_sppt"]:
            assert "DO_SPPT = True" in rendered, (
                "DO_SPPT = True should appear when do_sppt is True"
            )
        else:
            assert "DO_SPPT = False" in rendered, (
                "DO_SPPT = False should appear when do_sppt is False"
            )

        # diag_coord_def_z_file should appear in rendered output
        assert ocean["diag_coord_def_z_file"] in rendered, (
            f"diag_coord_def_z_file '{ocean['diag_coord_def_z_file']}' "
            f"not found in rendered output"
        )

        # --- 3. MOM6ParameterValidator passes ---
        validator = MOM6ParameterValidator()
        errors = validator.validate(rendered, "MOM_input")
        assert errors == [], (
            f"MOM6ParameterValidator found errors for res={resolution}: {errors}"
        )

        # --- 4. No legacy atparse tokens ---
        atparse_matches = _ATPARSE_RE.findall(rendered)
        assert atparse_matches == [], (
            f"Legacy @[...] atparse tokens found in rendered output: {atparse_matches}"
        )

        # Shell variables should be preserved
        assert "${TOPOEDITS}" in rendered, (
            "Shell variable ${TOPOEDITS} should be preserved in rendered output"
        )
