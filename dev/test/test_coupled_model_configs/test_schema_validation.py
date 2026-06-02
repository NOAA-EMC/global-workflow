"""Unit tests for coupled-model schema validation.

Tests the validate_coupled_model_context function and
merge_ocean_resolution_defaults function for the coupled-model
sections (ocean, ice, wave, post) of the Workflow_Configuration.

Traces to: Requirements 7.1, 7.2, 7.3, 7.4, 7.5, 12.1, 12.2
"""

import os
import sys

import pytest

sys.path.insert(0, os.path.join(os.path.dirname(__file__), "..", "..", "workflow"))

from deployment.model_context import (
    COUPLED_REQUIRED_KEYS,
    FatalDeploymentError,
    OCEAN_RESOLUTION_DEFAULTS,
    SUPPORTED_OCEAN_RESOLUTIONS,
    SUPPORTED_POST_SYSTEMS,
    SUPPORTED_WAVE_CURRENT_INPUT,
    SUPPORTED_WAVE_ICE_INPUT,
    merge_ocean_resolution_defaults,
    validate_coupled_model_context,
)


# ---------------------------------------------------------------------------
# Fixtures
# ---------------------------------------------------------------------------


@pytest.fixture
def valid_ocean():
    """Minimal valid model.ocean section."""
    return {
        "resolution": "025",
        "dt_ocean": 900,
        "dt_therm": 3600,
        "use_waves": False,
        "oda_incupd": False,
        "do_sppt": False,
        "river_runoff": True,
        "diag_coord_def_z_file": "oceanda_zgrid_75L.nc",
        "frunoff": "INPUT/runoff.daitren.clim.nc",
        "tasks": 120,
    }


@pytest.fixture
def valid_ice():
    """Minimal valid model.ice section."""
    return {
        "nprocs": 48,
        "decomposition": "slenderX2",
        "dt_ice": 900,
        "grid": "grid_cice_NEMS_mx025.nc",
        "mask": "kmtu_cice_NEMS_mx025.nc",
        "nx_glb": 1440,
        "ny_glb": 1080,
        "warm_start": True,
        "histfreq_n": 1,
        "hist_avg": True,
        "dumpfreq": "d",
        "dumpfreq_n": 1,
        "ktherm": 2,
        "tr_pond_lvl": True,
    }


@pytest.fixture
def valid_wave():
    """Minimal valid model.wave section."""
    return {
        "ice_input": "CPL",
        "current_input": "CPL",
        "output_params": "HS FP DP PHS PTP PDIR CHA",
        "dt_field_output": 10800,
        "dt_point_output": 3600,
        "grid_output_dir": "./",
        "point_output_dir": "./",
        "restart_output_dir": "./RESTART/",
    }


@pytest.fixture
def valid_post():
    """Minimal valid model.post section."""
    return {"system": "gfs"}


@pytest.fixture
def valid_model_context(valid_ocean, valid_ice, valid_wave, valid_post):
    """Full valid coupled-model context with all sections."""
    return {
        "ocean": dict(valid_ocean),
        "ice": dict(valid_ice),
        "wave": dict(valid_wave),
        "post": dict(valid_post),
    }


# ---------------------------------------------------------------------------
# Tests: Valid context produces no errors (Requirement 7.1, 7.2, 7.3, 7.5)
# ---------------------------------------------------------------------------


class TestValidContextPassesValidation:
    """Tests that a complete, valid model context passes validation."""

    def test_valid_complete_context_produces_no_errors(self, valid_model_context):
        """A fully valid model context should produce no errors."""
        errors = validate_coupled_model_context(valid_model_context)
        assert errors == []

    @pytest.mark.parametrize("resolution", sorted(SUPPORTED_OCEAN_RESOLUTIONS))
    def test_all_supported_ocean_resolutions_pass(
        self, valid_model_context, resolution
    ):
        """All supported ocean resolutions should pass validation."""
        valid_model_context["ocean"]["resolution"] = resolution
        errors = validate_coupled_model_context(valid_model_context)
        assert errors == []

    @pytest.mark.parametrize("ice_input", sorted(SUPPORTED_WAVE_ICE_INPUT))
    def test_all_supported_wave_ice_input_pass(self, valid_model_context, ice_input):
        """All supported wave.ice_input values should pass validation."""
        valid_model_context["wave"]["ice_input"] = ice_input
        errors = validate_coupled_model_context(valid_model_context)
        assert errors == []

    @pytest.mark.parametrize("current_input", sorted(SUPPORTED_WAVE_CURRENT_INPUT))
    def test_all_supported_wave_current_input_pass(
        self, valid_model_context, current_input
    ):
        """All supported wave.current_input values should pass validation."""
        valid_model_context["wave"]["current_input"] = current_input
        errors = validate_coupled_model_context(valid_model_context)
        assert errors == []

    @pytest.mark.parametrize("system", sorted(SUPPORTED_POST_SYSTEMS))
    def test_all_supported_post_systems_pass(self, valid_model_context, system):
        """All supported post.system values should pass validation."""
        valid_model_context["post"]["system"] = system
        errors = validate_coupled_model_context(valid_model_context)
        assert errors == []


# ---------------------------------------------------------------------------
# Tests: Missing entire section produces FATAL ERROR (Requirement 7.4)
# ---------------------------------------------------------------------------


class TestMissingSectionValidation:
    """Tests that missing entire sections produce FATAL ERROR messages."""

    @pytest.mark.parametrize("section", COUPLED_REQUIRED_KEYS.keys())
    def test_missing_section_produces_fatal_error(self, valid_model_context, section):
        """Missing an entire section should produce a FATAL ERROR."""
        del valid_model_context[section]
        errors = validate_coupled_model_context(valid_model_context)
        matching = [e for e in errors if f"model.{section}" in e]
        assert len(matching) >= 1
        assert all("FATAL ERROR" in e for e in matching)

    def test_empty_context_produces_errors_for_all_sections(self):
        """Empty dict should produce one error per required section."""
        errors = validate_coupled_model_context({})
        assert len(errors) == len(COUPLED_REQUIRED_KEYS)
        for section in COUPLED_REQUIRED_KEYS:
            assert any(f"model.{section}" in e for e in errors)
        assert all("FATAL ERROR" in e for e in errors)


# ---------------------------------------------------------------------------
# Tests: Missing individual required keys produce FATAL ERROR (Req 7.1-7.3)
# ---------------------------------------------------------------------------


class TestMissingRequiredKeys:
    """Tests that missing individual required keys produce FATAL ERROR."""

    @pytest.mark.parametrize("key", COUPLED_REQUIRED_KEYS["ocean"])
    def test_missing_ocean_key_produces_fatal_error(
        self, valid_model_context, key
    ):
        """Each missing ocean key should produce a FATAL ERROR."""
        del valid_model_context["ocean"][key]
        errors = validate_coupled_model_context(valid_model_context)
        matching = [e for e in errors if f"model.ocean.{key}" in e]
        assert len(matching) == 1
        assert "FATAL ERROR" in matching[0]

    @pytest.mark.parametrize("key", COUPLED_REQUIRED_KEYS["ice"])
    def test_missing_ice_key_produces_fatal_error(
        self, valid_model_context, key
    ):
        """Each missing ice key should produce a FATAL ERROR."""
        del valid_model_context["ice"][key]
        errors = validate_coupled_model_context(valid_model_context)
        matching = [e for e in errors if f"model.ice.{key}" in e]
        assert len(matching) == 1
        assert "FATAL ERROR" in matching[0]

    @pytest.mark.parametrize("key", COUPLED_REQUIRED_KEYS["wave"])
    def test_missing_wave_key_produces_fatal_error(
        self, valid_model_context, key
    ):
        """Each missing wave key should produce a FATAL ERROR."""
        del valid_model_context["wave"][key]
        errors = validate_coupled_model_context(valid_model_context)
        matching = [e for e in errors if f"model.wave.{key}" in e]
        assert len(matching) == 1
        assert "FATAL ERROR" in matching[0]

    @pytest.mark.parametrize("key", COUPLED_REQUIRED_KEYS["post"])
    def test_missing_post_key_produces_fatal_error(
        self, valid_model_context, key
    ):
        """Each missing post key should produce a FATAL ERROR."""
        del valid_model_context["post"][key]
        errors = validate_coupled_model_context(valid_model_context)
        matching = [e for e in errors if f"model.post.{key}" in e]
        assert len(matching) == 1
        assert "FATAL ERROR" in matching[0]


# ---------------------------------------------------------------------------
# Tests: Invalid enum values produce FATAL ERROR (Requirement 7.1, 7.3, 7.5)
# ---------------------------------------------------------------------------


class TestEnumConstraintValidation:
    """Tests that invalid enum values produce FATAL ERROR messages."""

    def test_invalid_ocean_resolution(self, valid_model_context):
        """Invalid ocean.resolution should produce FATAL ERROR."""
        valid_model_context["ocean"]["resolution"] = "999"
        errors = validate_coupled_model_context(valid_model_context)
        matching = [e for e in errors if "ocean.resolution" in e and "999" in e]
        assert len(matching) == 1
        assert "FATAL ERROR" in matching[0]

    def test_invalid_wave_ice_input(self, valid_model_context):
        """Invalid wave.ice_input should produce FATAL ERROR."""
        valid_model_context["wave"]["ice_input"] = "INVALID"
        errors = validate_coupled_model_context(valid_model_context)
        matching = [e for e in errors if "wave.ice_input" in e and "INVALID" in e]
        assert len(matching) == 1
        assert "FATAL ERROR" in matching[0]

    def test_invalid_wave_current_input(self, valid_model_context):
        """Invalid wave.current_input should produce FATAL ERROR."""
        valid_model_context["wave"]["current_input"] = "NONE"
        errors = validate_coupled_model_context(valid_model_context)
        matching = [
            e for e in errors if "wave.current_input" in e and "NONE" in e
        ]
        assert len(matching) == 1
        assert "FATAL ERROR" in matching[0]

    def test_invalid_post_system(self, valid_model_context):
        """Invalid post.system should produce FATAL ERROR."""
        valid_model_context["post"]["system"] = "unknown"
        errors = validate_coupled_model_context(valid_model_context)
        matching = [e for e in errors if "post.system" in e and "unknown" in e]
        assert len(matching) == 1
        assert "FATAL ERROR" in matching[0]

    def test_all_errors_are_fatal(self, valid_model_context):
        """All validation errors should start with FATAL ERROR."""
        valid_model_context["ocean"]["resolution"] = "bad"
        valid_model_context["wave"]["ice_input"] = "bad"
        valid_model_context["post"]["system"] = "bad"
        errors = validate_coupled_model_context(valid_model_context)
        assert all("FATAL ERROR" in e for e in errors)


# ---------------------------------------------------------------------------
# Tests: Ocean resolution defaults merge (Requirements 12.1, 12.2)
# ---------------------------------------------------------------------------


class TestMergeOceanResolutionDefaults:
    """Tests for merge_ocean_resolution_defaults function."""

    @pytest.mark.parametrize("resolution", sorted(SUPPORTED_OCEAN_RESOLUTIONS))
    def test_defaults_applied_for_all_resolutions(self, resolution):
        """Built-in defaults should be applied for each supported resolution."""
        context = {"ocean": {"resolution": resolution}}
        merged = merge_ocean_resolution_defaults(context)
        expected_defaults = OCEAN_RESOLUTION_DEFAULTS[resolution]
        for key, value in expected_defaults.items():
            assert merged["ocean"][key] == value

    def test_explicit_values_override_defaults(self):
        """Explicit model.ocean values should override resolution defaults."""
        context = {
            "ocean": {
                "resolution": "025",
                "nx_glb": 2880,  # Explicit override (default is 1440)
                "dt_ocean": 450,  # Explicit override (default is 900)
            }
        }
        merged = merge_ocean_resolution_defaults(context)
        # Explicit values win
        assert merged["ocean"]["nx_glb"] == 2880
        assert merged["ocean"]["dt_ocean"] == 450
        # Defaults fill in missing keys
        assert merged["ocean"]["ny_glb"] == 1080
        assert merged["ocean"]["dt_therm"] == 3600
        assert merged["ocean"]["KHTH"] == 10.0

    def test_defaults_fill_missing_keys(self):
        """Default values should fill in keys not explicitly set."""
        context = {"ocean": {"resolution": "100"}}
        merged = merge_ocean_resolution_defaults(context)
        assert merged["ocean"]["nx_glb"] == 360
        assert merged["ocean"]["ny_glb"] == 320
        assert merged["ocean"]["dt_ocean"] == 3600
        assert merged["ocean"]["dt_therm"] == 7200
        assert merged["ocean"]["KHTH"] == 600.0
        assert merged["ocean"]["KHTR"] == 600.0
        assert merged["ocean"]["SMAG_BI_CONST"] == 0.15

    def test_user_provided_defaults_override_builtin(self):
        """User-provided defaults in ocean.defaults[res] override built-in."""
        context = {
            "ocean": {
                "resolution": "050",
                "defaults": {
                    "050": {
                        "nx_glb": 999,
                        "ny_glb": 888,
                    }
                },
            }
        }
        merged = merge_ocean_resolution_defaults(context)
        assert merged["ocean"]["nx_glb"] == 999
        assert merged["ocean"]["ny_glb"] == 888

    def test_resolution_025_defaults(self):
        """Resolution 025 should produce correct default values."""
        context = {"ocean": {"resolution": "025"}}
        merged = merge_ocean_resolution_defaults(context)
        assert merged["ocean"]["nx_glb"] == 1440
        assert merged["ocean"]["ny_glb"] == 1080
        assert merged["ocean"]["dt_ocean"] == 900
        assert merged["ocean"]["dt_therm"] == 3600
        assert merged["ocean"]["KHTH"] == 10.0
        assert merged["ocean"]["KHTR"] == 10.0
        assert merged["ocean"]["SMAG_BI_CONST"] == 0.06

    def test_resolution_050_defaults(self):
        """Resolution 050 should produce correct default values."""
        context = {"ocean": {"resolution": "050"}}
        merged = merge_ocean_resolution_defaults(context)
        assert merged["ocean"]["nx_glb"] == 720
        assert merged["ocean"]["ny_glb"] == 576
        assert merged["ocean"]["dt_ocean"] == 1800
        assert merged["ocean"]["dt_therm"] == 3600
        assert merged["ocean"]["KHTH"] == 50.0
        assert merged["ocean"]["KHTR"] == 50.0
        assert merged["ocean"]["SMAG_BI_CONST"] == 0.06

    def test_resolution_100_defaults(self):
        """Resolution 100 should produce correct default values."""
        context = {"ocean": {"resolution": "100"}}
        merged = merge_ocean_resolution_defaults(context)
        assert merged["ocean"]["nx_glb"] == 360
        assert merged["ocean"]["ny_glb"] == 320
        assert merged["ocean"]["dt_ocean"] == 3600
        assert merged["ocean"]["dt_therm"] == 7200
        assert merged["ocean"]["KHTH"] == 600.0
        assert merged["ocean"]["KHTR"] == 600.0
        assert merged["ocean"]["SMAG_BI_CONST"] == 0.15

    def test_resolution_500_defaults(self):
        """Resolution 500 should produce correct default values."""
        context = {"ocean": {"resolution": "500"}}
        merged = merge_ocean_resolution_defaults(context)
        assert merged["ocean"]["nx_glb"] == 72
        assert merged["ocean"]["ny_glb"] == 35
        assert merged["ocean"]["dt_ocean"] == 7200
        assert merged["ocean"]["dt_therm"] == 14400
        assert merged["ocean"]["KHTH"] == 1000.0
        assert merged["ocean"]["KHTR"] == 1000.0

    def test_unsupported_resolution_raises_fatal_error(self):
        """Unsupported resolution should raise FatalDeploymentError."""
        context = {"ocean": {"resolution": "999"}}
        with pytest.raises(FatalDeploymentError, match="Unsupported ocean resolution"):
            merge_ocean_resolution_defaults(context)

    def test_none_resolution_raises_fatal_error(self):
        """None resolution should raise FatalDeploymentError."""
        context = {"ocean": {}}
        with pytest.raises(FatalDeploymentError, match="Unsupported ocean resolution"):
            merge_ocean_resolution_defaults(context)

    def test_missing_ocean_section_raises_fatal_error(self):
        """Missing ocean section should raise FatalDeploymentError."""
        context = {}
        with pytest.raises(FatalDeploymentError, match="Unsupported ocean resolution"):
            merge_ocean_resolution_defaults(context)

    def test_explicit_override_preserves_all_explicit_keys(self):
        """All explicitly set keys should be preserved after merge."""
        explicit_keys = {
            "resolution": "025",
            "nx_glb": 2000,
            "ny_glb": 1500,
            "dt_ocean": 600,
            "dt_therm": 1800,
            "KHTH": 5.0,
            "KHTR": 5.0,
            "SMAG_BI_CONST": 0.03,
        }
        context = {"ocean": dict(explicit_keys)}
        merged = merge_ocean_resolution_defaults(context)
        for key, value in explicit_keys.items():
            assert merged["ocean"][key] == value

    def test_merge_does_not_remove_extra_keys(self):
        """Merge should not remove keys already in ocean that are not in defaults."""
        context = {
            "ocean": {
                "resolution": "025",
                "custom_key": "custom_value",
                "another_key": 42,
            }
        }
        merged = merge_ocean_resolution_defaults(context)
        assert merged["ocean"]["custom_key"] == "custom_value"
        assert merged["ocean"]["another_key"] == 42
