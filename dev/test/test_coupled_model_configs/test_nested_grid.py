"""Unit tests for FV3 nested grid template (input_global_nest.nml.j2).

Tests nest-specific parameters (NEST_IMO, NEST_JMO) included when do_nest=True,
excluded when do_nest=False, shell variable preservation, and model_configure
format validity via ModelConfigureValidator.

Traces to: Requirements 5.1, 5.2, 5.3, 5.4, 5.5
"""

import os
import sys
from pathlib import Path

import pytest
from jinja2 import Environment, FileSystemLoader, StrictUndefined

sys.path.insert(0, os.path.join(os.path.dirname(__file__), "..", "..", "workflow"))

from deployment.validators.model_configure import ModelConfigureValidator


# ---------------------------------------------------------------------------
# Fixtures
# ---------------------------------------------------------------------------

TEMPLATE_DIR = Path(__file__).resolve().parents[2] / "parm" / "ufs" / "fv3"
TEMPLATE_NAME = "input_global_nest.nml.j2"


def _fortran_logical(value):
    """Jinja2 filter that converts Python boolean to Fortran logical literal."""
    return ".true." if value else ".false."


@pytest.fixture
def jinja_env():
    """Create a Jinja2 environment configured for the fv3 template directory."""
    env = Environment(
        loader=FileSystemLoader(str(TEMPLATE_DIR)),
        undefined=StrictUndefined,
        keep_trailing_newline=True,
    )
    env.filters["fortran_logical"] = _fortran_logical
    return env


@pytest.fixture
def valid_fv3_context_with_nest():
    """Valid FV3 context with nesting enabled."""
    return {
        "model": {
            "dt_atmos": 450,
            "output_grid": "gaussian_grid",
            "fv3": {
                "total_tasks": 384,
                "fhrot": 0,
                "restart_interval": 12,
                "quilting": True,
                "quilting_restart": True,
                "write_group": 1,
                "wrttask_per_group": 24,
                "num_output_files": 2,
                "output_filetype_atm": "netcdf",
                "output_filetype_sfc": "netcdf",
                "imo": 384,
                "jmo": 192,
                "output_fh": "0 1 2 3",
                "iau_offset": 0,
                "do_nest": True,
                "npx_nest": 961,
                "npy_nest": 961,
            },
        }
    }


@pytest.fixture
def valid_fv3_context_without_nest():
    """Valid FV3 context with nesting disabled."""
    return {
        "model": {
            "dt_atmos": 450,
            "output_grid": "gaussian_grid",
            "fv3": {
                "total_tasks": 384,
                "fhrot": 0,
                "restart_interval": 12,
                "quilting": True,
                "quilting_restart": True,
                "write_group": 1,
                "wrttask_per_group": 24,
                "num_output_files": 2,
                "output_filetype_atm": "netcdf",
                "output_filetype_sfc": "netcdf",
                "imo": 384,
                "jmo": 192,
                "output_fh": "0 1 2 3",
                "iau_offset": 0,
                "do_nest": False,
            },
        }
    }


@pytest.fixture
def validator():
    """Create a ModelConfigureValidator instance."""
    return ModelConfigureValidator()


def _render_nested_grid(jinja_env, context):
    """Helper to render the input_global_nest.nml.j2 template with shell var preservation."""
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
# Tests: Nest-Specific Parameters (Requirement 5.2)
# ---------------------------------------------------------------------------


class TestNestSpecificParameters:
    """Tests that nest-specific parameters are included/excluded based on do_nest."""

    def test_nest_imo_present_when_do_nest_true(
        self, jinja_env, valid_fv3_context_with_nest
    ):
        """NEST_IMO should be present when do_nest=True."""
        rendered = _render_nested_grid(jinja_env, valid_fv3_context_with_nest)
        assert "NEST_IMO:" in rendered

    def test_nest_jmo_present_when_do_nest_true(
        self, jinja_env, valid_fv3_context_with_nest
    ):
        """NEST_JMO should be present when do_nest=True."""
        rendered = _render_nested_grid(jinja_env, valid_fv3_context_with_nest)
        assert "NEST_JMO:" in rendered

    def test_nest_imo_value_correct(self, jinja_env, valid_fv3_context_with_nest):
        """NEST_IMO should have the correct npx_nest value."""
        rendered = _render_nested_grid(jinja_env, valid_fv3_context_with_nest)
        assert "961" in rendered

    def test_nest_jmo_value_correct(self, jinja_env, valid_fv3_context_with_nest):
        """NEST_JMO should have the correct npy_nest value."""
        rendered = _render_nested_grid(jinja_env, valid_fv3_context_with_nest)
        # Both npx_nest and npy_nest are 961 in our fixture
        lines = [l for l in rendered.splitlines() if "NEST_JMO" in l]
        assert len(lines) == 1
        assert "961" in lines[0]

    def test_nest_imo_absent_when_do_nest_false(
        self, jinja_env, valid_fv3_context_without_nest
    ):
        """NEST_IMO should NOT be present when do_nest=False."""
        rendered = _render_nested_grid(jinja_env, valid_fv3_context_without_nest)
        assert "NEST_IMO" not in rendered

    def test_nest_jmo_absent_when_do_nest_false(
        self, jinja_env, valid_fv3_context_without_nest
    ):
        """NEST_JMO should NOT be present when do_nest=False."""
        rendered = _render_nested_grid(jinja_env, valid_fv3_context_without_nest)
        assert "NEST_JMO" not in rendered


# ---------------------------------------------------------------------------
# Tests: Shell Variable Preservation (Requirement 5.5)
# ---------------------------------------------------------------------------


class TestShellVariablePreservation:
    """Tests for shell variable preservation in input_global_nest.nml."""

    def test_fhmax_preserved(self, jinja_env, valid_fv3_context_with_nest):
        """${FHMAX} should be preserved as a literal shell variable."""
        rendered = _render_nested_grid(jinja_env, valid_fv3_context_with_nest)
        assert "${FHMAX}" in rendered

    def test_pdy_preserved(self, jinja_env, valid_fv3_context_with_nest):
        """${PDY} should be preserved as a literal shell variable (in date fields)."""
        rendered = _render_nested_grid(jinja_env, valid_fv3_context_with_nest)
        assert "${PDY" in rendered

    def test_cyc_preserved(self, jinja_env, valid_fv3_context_with_nest):
        """${cyc} should be preserved as a literal shell variable."""
        rendered = _render_nested_grid(jinja_env, valid_fv3_context_with_nest)
        assert "${cyc}" in rendered


# ---------------------------------------------------------------------------
# Tests: ModelConfigure Format Validity (Requirement 5.3, 10.4)
# ---------------------------------------------------------------------------


class TestModelConfigureFormatValidity:
    """Tests that rendered input_global_nest.nml passes ModelConfigureValidator."""

    def test_validator_passes_with_nest(
        self, jinja_env, valid_fv3_context_with_nest, validator
    ):
        """Rendered output with do_nest=True should pass ModelConfigureValidator."""
        rendered = _render_nested_grid(jinja_env, valid_fv3_context_with_nest)
        errors = validator.validate(rendered, "input_global_nest.nml")
        assert errors == [], f"Validation errors: {errors}"

    def test_validator_passes_without_nest(
        self, jinja_env, valid_fv3_context_without_nest, validator
    ):
        """Rendered output with do_nest=False should pass ModelConfigureValidator."""
        rendered = _render_nested_grid(jinja_env, valid_fv3_context_without_nest)
        errors = validator.validate(rendered, "input_global_nest.nml")
        assert errors == [], f"Validation errors: {errors}"
