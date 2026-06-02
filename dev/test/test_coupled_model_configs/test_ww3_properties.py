"""Property-based tests for WW3 wave model template rendering.

Uses hypothesis to verify Property 8: WW3 Forcing Mode Mapping.
Generates valid wave Model_Context dicts and asserts correct flag
character mapping in the rendered ww3_shel.nml output.

Feature: coupled-model-configs, Property 8: WW3 Forcing Mode Mapping

Traces to: Requirements 4.2, 4.3, 4.4
"""

from __future__ import annotations

import os
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
TEMPLATE_PATH = DEV_ROOT / "parm" / "ufs" / "wave" / "ww3_shel.nml.j2"


# ---------------------------------------------------------------------------
# Hypothesis strategies for valid wave Model_Context generation
# ---------------------------------------------------------------------------


@st.composite
def valid_wave_model_context(draw: st.DrawFn) -> dict:
    """Generate a valid wave Model_Context dict with all required keys.

    Generates:
    - ice_input from sampled_from(["CPL", "YES"])
    - current_input from sampled_from(["CPL", "YES"])
    - output_params: text (non-empty wave output parameter string)
    - dt_field_output: positive int
    - dt_point_output: positive int
    - grid_output_dir: text (non-empty path string)
    - point_output_dir: text (non-empty path string)
    - restart_output_dir: text (non-empty path string)
    """
    ice_input = draw(st.sampled_from(["CPL", "YES"]))
    current_input = draw(st.sampled_from(["CPL", "YES"]))
    output_params = draw(
        st.sampled_from([
            "HS FP DP PHS PTP PDIR CHA",
            "HS FP DP",
            "HS LM T02 T01 DIR DP SPR",
            "HS FP DP PHS PTP PDIR CHA UST CUR",
        ])
    )
    dt_field_output = draw(st.integers(min_value=1, max_value=86400))
    dt_point_output = draw(st.integers(min_value=1, max_value=86400))
    grid_output_dir = draw(
        st.sampled_from(["./", "./OUTPUT/", "/scratch/wave/grid/"])
    )
    point_output_dir = draw(
        st.sampled_from(["./", "./OUTPUT/", "/scratch/wave/point/"])
    )
    restart_output_dir = draw(
        st.sampled_from(["./RESTART/", "./restart/", "/scratch/wave/restart/"])
    )

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


# ---------------------------------------------------------------------------
# Helper: render the ww3_shel.nml.j2 template
# ---------------------------------------------------------------------------


def _render_ww3_template(context: dict) -> str:
    """Render the ww3_shel.nml.j2 template with the given context.

    Args:
        context: Full rendering context dict with model.wave section.

    Returns:
        Rendered template content as a string.
    """
    searchpath = [
        str(DEV_ROOT / "parm" / "ufs"),
        str(DEV_ROOT / "parm" / "ufs" / "wave"),
        str(DEV_ROOT / "parm"),
    ]
    searchpath = [p for p in searchpath if Path(p).is_dir()]

    renderer = TemplateRenderer(context=context, searchpath=searchpath, strict=True)
    return renderer.render_string(TEMPLATE_PATH.read_text(encoding="utf-8"))


# ---------------------------------------------------------------------------
# Property 8: WW3 Forcing Mode Mapping
# ---------------------------------------------------------------------------


class TestWW3ForcingModeMapping:
    """Property 8: WW3 Forcing Mode Mapping.

    **Validates: Requirements 4.2, 4.3, 4.4**

    For any valid wave Model_Context, the rendered ww3_shel.nml SHALL map
    forcing input modes correctly:
    - wave.ice_input == "CPL" → ICE_CONC = 'C'
    - wave.ice_input == "YES" → ICE_CONC = 'T'
    - wave.current_input == "CPL" → CURRENTS = 'C'
    - wave.current_input == "YES" → CURRENTS = 'T'
    """

    @settings(
        max_examples=100,
        suppress_health_check=[HealthCheck.too_slow],
        deadline=None,
    )
    @given(context=valid_wave_model_context())
    def test_ice_input_cpl_maps_to_c(self, context: dict):
        """When ice_input == "CPL", rendered output contains ICE_CONC = 'C'.

        **Validates: Requirements 4.2**
        """
        wave = context["model"]["wave"]
        if wave["ice_input"] != "CPL":
            return  # Only test CPL mapping in this assertion

        rendered = _render_ww3_template(context)
        assert "ICE_CONC" in rendered, (
            "ICE_CONC not found in rendered ww3_shel.nml"
        )
        # Check that ICE_CONC is set to 'C' for coupled mode
        assert "'C'" in rendered.split("ICE_CONC")[1].split("\n")[0], (
            f"Expected ICE_CONC = 'C' for ice_input='CPL', "
            f"got: {rendered.split('ICE_CONC')[1].split(chr(10))[0]}"
        )

    @settings(
        max_examples=100,
        suppress_health_check=[HealthCheck.too_slow],
        deadline=None,
    )
    @given(context=valid_wave_model_context())
    def test_ice_input_yes_maps_to_t(self, context: dict):
        """When ice_input == "YES", rendered output contains ICE_CONC = 'T'.

        **Validates: Requirements 4.3**
        """
        wave = context["model"]["wave"]
        if wave["ice_input"] != "YES":
            return  # Only test YES mapping in this assertion

        rendered = _render_ww3_template(context)
        assert "ICE_CONC" in rendered, (
            "ICE_CONC not found in rendered ww3_shel.nml"
        )
        # Check that ICE_CONC is set to 'T' for file input mode
        assert "'T'" in rendered.split("ICE_CONC")[1].split("\n")[0], (
            f"Expected ICE_CONC = 'T' for ice_input='YES', "
            f"got: {rendered.split('ICE_CONC')[1].split(chr(10))[0]}"
        )

    @settings(
        max_examples=100,
        suppress_health_check=[HealthCheck.too_slow],
        deadline=None,
    )
    @given(context=valid_wave_model_context())
    def test_current_input_cpl_maps_to_c(self, context: dict):
        """When current_input == "CPL", rendered output contains CURRENTS = 'C'.

        **Validates: Requirements 4.4**
        """
        wave = context["model"]["wave"]
        if wave["current_input"] != "CPL":
            return  # Only test CPL mapping in this assertion

        rendered = _render_ww3_template(context)
        assert "CURRENTS" in rendered, (
            "CURRENTS not found in rendered ww3_shel.nml"
        )
        # Check that CURRENTS is set to 'C' for coupled mode
        assert "'C'" in rendered.split("CURRENTS")[1].split("\n")[0], (
            f"Expected CURRENTS = 'C' for current_input='CPL', "
            f"got: {rendered.split('CURRENTS')[1].split(chr(10))[0]}"
        )

    @settings(
        max_examples=100,
        suppress_health_check=[HealthCheck.too_slow],
        deadline=None,
    )
    @given(context=valid_wave_model_context())
    def test_current_input_yes_maps_to_t(self, context: dict):
        """When current_input == "YES", rendered output contains CURRENTS = 'T'.

        **Validates: Requirements 4.4**
        """
        wave = context["model"]["wave"]
        if wave["current_input"] != "YES":
            return  # Only test YES mapping in this assertion

        rendered = _render_ww3_template(context)
        assert "CURRENTS" in rendered, (
            "CURRENTS not found in rendered ww3_shel.nml"
        )
        # Check that CURRENTS is set to 'T' for file input mode
        assert "'T'" in rendered.split("CURRENTS")[1].split("\n")[0], (
            f"Expected CURRENTS = 'T' for current_input='YES', "
            f"got: {rendered.split('CURRENTS')[1].split(chr(10))[0]}"
        )

    @settings(
        max_examples=100,
        suppress_health_check=[HealthCheck.too_slow],
        deadline=None,
    )
    @given(context=valid_wave_model_context())
    def test_all_forcing_mode_combinations_map_correctly(self, context: dict):
        """For any valid combination of ice_input and current_input,
        the rendered output maps both forcing modes correctly.

        **Validates: Requirements 4.2, 4.3, 4.4**
        """
        wave = context["model"]["wave"]
        rendered = _render_ww3_template(context)

        # Determine expected ICE_CONC flag
        expected_ice_flag = "'C'" if wave["ice_input"] == "CPL" else "'T'"
        # Determine expected CURRENTS flag
        expected_current_flag = "'C'" if wave["current_input"] == "CPL" else "'T'"

        # Extract the ICE_CONC line
        assert "ICE_CONC" in rendered, "ICE_CONC not found in rendered output"
        ice_conc_line = rendered.split("ICE_CONC")[1].split("\n")[0]
        assert expected_ice_flag in ice_conc_line, (
            f"Expected ICE_CONC = {expected_ice_flag} for "
            f"ice_input='{wave['ice_input']}', "
            f"got line: 'ICE_CONC{ice_conc_line}'"
        )

        # Extract the CURRENTS line
        assert "CURRENTS" in rendered, "CURRENTS not found in rendered output"
        currents_line = rendered.split("CURRENTS")[1].split("\n")[0]
        assert expected_current_flag in currents_line, (
            f"Expected CURRENTS = {expected_current_flag} for "
            f"current_input='{wave['current_input']}', "
            f"got line: 'CURRENTS{currents_line}'"
        )
