"""Unit tests for WW3 wave namelist template (ww3_shel.nml.j2).

Tests forcing mode mapping (CPL→C, YES→T) for ice_input and current_input,
output parameter rendering, shell variable preservation, and Fortran
namelist format validity via NamelistValidator.

Traces to: Requirements 4.2, 4.3, 4.4, 4.5, 5.1
"""

import os
import sys
from pathlib import Path

import pytest
from jinja2 import Environment, FileSystemLoader, StrictUndefined

sys.path.insert(0, os.path.join(os.path.dirname(__file__), "..", "..", "workflow"))

from deployment.validators.namelist import NamelistValidator


# ---------------------------------------------------------------------------
# Fixtures
# ---------------------------------------------------------------------------

TEMPLATE_DIR = Path(__file__).resolve().parents[2] / "parm" / "ufs" / "wave"
TEMPLATE_NAME = "ww3_shel.nml.j2"


@pytest.fixture
def jinja_env():
    """Create a Jinja2 environment configured for the wave template directory."""
    env = Environment(
        loader=FileSystemLoader(str(TEMPLATE_DIR)),
        undefined=StrictUndefined,
        keep_trailing_newline=True,
    )
    return env


@pytest.fixture
def valid_wave_context():
    """Minimal valid wave context for rendering ww3_shel.nml.j2."""
    return {
        "model": {
            "wave": {
                "ice_input": "CPL",
                "current_input": "CPL",
                "output_params": "HS FP DP PHS PTP PDIR CHA",
                "dt_field_output": 10800,
                "dt_point_output": 3600,
                "dt_restart": 21600,
                "grid_output_dir": "./",
                "point_output_dir": "./",
                "restart_output_dir": "./RESTART/",
            }
        }
    }


@pytest.fixture
def validator():
    """Create a NamelistValidator instance."""
    return NamelistValidator()


def _render_ww3(jinja_env, context):
    """Helper to render the ww3_shel.nml.j2 template with shell var preservation."""
    import re

    template_text = (TEMPLATE_DIR / TEMPLATE_NAME).read_text()

    # Protect shell variables from Jinja2 resolution
    shell_var_pattern = re.compile(r'\$\{[A-Z_][A-Z0-9_]*\}')
    replacements = {}
    counter = 0

    def _replace(match):
        nonlocal counter
        placeholder = f"__SHELL_VAR_PRESERVE__{counter}__"
        replacements[placeholder] = match.group(0)
        counter += 1
        return placeholder

    protected_text = shell_var_pattern.sub(_replace, template_text)

    template = jinja_env.from_string(protected_text)
    rendered = template.render(context)

    # Restore shell variables
    for placeholder, original in replacements.items():
        rendered = rendered.replace(placeholder, original)

    return rendered


# ---------------------------------------------------------------------------
# Tests: Forcing Mode Mapping (Requirements 4.2, 4.3, 4.4)
# ---------------------------------------------------------------------------


class TestForcingModeMapping:
    """Tests for WW3 forcing mode mapping (ice_input and current_input)."""

    def test_ice_input_cpl_maps_to_c(self, jinja_env, valid_wave_context):
        """ice_input='CPL' should render ICE_CONC = 'C'."""
        valid_wave_context["model"]["wave"]["ice_input"] = "CPL"
        rendered = _render_ww3(jinja_env, valid_wave_context)
        assert "ICE_CONC = 'C'" in rendered

    def test_ice_input_yes_maps_to_t(self, jinja_env, valid_wave_context):
        """ice_input='YES' should render ICE_CONC = 'T'."""
        valid_wave_context["model"]["wave"]["ice_input"] = "YES"
        rendered = _render_ww3(jinja_env, valid_wave_context)
        assert "ICE_CONC = 'T'" in rendered

    def test_current_input_cpl_maps_to_c(self, jinja_env, valid_wave_context):
        """current_input='CPL' should render CURRENTS = 'C'."""
        valid_wave_context["model"]["wave"]["current_input"] = "CPL"
        rendered = _render_ww3(jinja_env, valid_wave_context)
        assert "CURRENTS = 'C'" in rendered

    def test_current_input_yes_maps_to_t(self, jinja_env, valid_wave_context):
        """current_input='YES' should render CURRENTS = 'T'."""
        valid_wave_context["model"]["wave"]["current_input"] = "YES"
        rendered = _render_ww3(jinja_env, valid_wave_context)
        assert "CURRENTS = 'T'" in rendered


# ---------------------------------------------------------------------------
# Tests: Output Parameter Rendering (Requirement 4.5)
# ---------------------------------------------------------------------------


class TestOutputParameterRendering:
    """Tests for output parameter rendering in ww3_shel.nml."""

    def test_output_params_rendered(self, jinja_env, valid_wave_context):
        """output_params value should appear in rendered output."""
        valid_wave_context["model"]["wave"]["output_params"] = "HS FP DP PHS PTP PDIR CHA"
        rendered = _render_ww3(jinja_env, valid_wave_context)
        assert "HS FP DP PHS PTP PDIR CHA" in rendered

    def test_custom_output_params(self, jinja_env, valid_wave_context):
        """Custom output_params value should appear in rendered output."""
        valid_wave_context["model"]["wave"]["output_params"] = "HS FP DP"
        rendered = _render_ww3(jinja_env, valid_wave_context)
        assert "HS FP DP" in rendered

    def test_dt_field_output_rendered(self, jinja_env, valid_wave_context):
        """dt_field_output value should appear in rendered output."""
        valid_wave_context["model"]["wave"]["dt_field_output"] = 7200
        rendered = _render_ww3(jinja_env, valid_wave_context)
        assert "7200" in rendered

    def test_dt_point_output_rendered(self, jinja_env, valid_wave_context):
        """dt_point_output value should appear in rendered output."""
        valid_wave_context["model"]["wave"]["dt_point_output"] = 1800
        rendered = _render_ww3(jinja_env, valid_wave_context)
        assert "1800" in rendered


# ---------------------------------------------------------------------------
# Tests: Shell Variable Preservation (Requirement 4.7)
# ---------------------------------------------------------------------------


class TestShellVariablePreservation:
    """Tests for shell variable preservation in ww3_shel.nml."""

    def test_fhmax_wav_preserved(self, jinja_env, valid_wave_context):
        """${FHMAX_WAV} should be preserved as a literal shell variable."""
        rendered = _render_ww3(jinja_env, valid_wave_context)
        assert "${FHMAX_WAV}" in rendered


# ---------------------------------------------------------------------------
# Tests: Namelist Format Validity (Requirement 4.5, 10.3)
# ---------------------------------------------------------------------------


class TestNamelistFormatValidity:
    """Tests that rendered ww3_shel.nml passes NamelistValidator."""

    def test_namelist_validator_passes_cpl_mode(
        self, jinja_env, valid_wave_context, validator
    ):
        """Rendered output with CPL mode should pass NamelistValidator."""
        valid_wave_context["model"]["wave"]["ice_input"] = "CPL"
        valid_wave_context["model"]["wave"]["current_input"] = "CPL"
        rendered = _render_ww3(jinja_env, valid_wave_context)
        errors = validator.validate(rendered, "ww3_shel.nml")
        assert errors == [], f"Validation errors: {errors}"

    def test_namelist_validator_passes_yes_mode(
        self, jinja_env, valid_wave_context, validator
    ):
        """Rendered output with YES mode should pass NamelistValidator."""
        valid_wave_context["model"]["wave"]["ice_input"] = "YES"
        valid_wave_context["model"]["wave"]["current_input"] = "YES"
        rendered = _render_ww3(jinja_env, valid_wave_context)
        errors = validator.validate(rendered, "ww3_shel.nml")
        assert errors == [], f"Validation errors: {errors}"

    def test_namelist_validator_passes_mixed_mode(
        self, jinja_env, valid_wave_context, validator
    ):
        """Rendered output with mixed CPL/YES modes should pass NamelistValidator."""
        valid_wave_context["model"]["wave"]["ice_input"] = "CPL"
        valid_wave_context["model"]["wave"]["current_input"] = "YES"
        rendered = _render_ww3(jinja_env, valid_wave_context)
        errors = validator.validate(rendered, "ww3_shel.nml")
        assert errors == [], f"Validation errors: {errors}"
