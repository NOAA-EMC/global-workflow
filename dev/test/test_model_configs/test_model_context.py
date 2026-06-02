"""Unit tests for Model_Context schema validation module.

Tests the schema validation and resolution-dependent default merging
for the `model` section of the Workflow_Configuration YAML.

Traces to: Requirements 4.1, 4.2, 4.3, 4.5, 4.6, 4.7
"""

import os
import sys

import pytest

sys.path.insert(0, os.path.join(os.path.dirname(__file__), "..", "..", "workflow"))

from deployment.model_context import (
    ModelContextSchema,
    merge_resolution_defaults,
    SUPPORTED_RESOLUTIONS,
    SUPPORTED_PHYSICS_SUITES,
    SUPPORTED_COUPLING_MODES,
    SUPPORTED_EMISSION_DATASETS,
    REQUIRED_TOP_LEVEL_KEYS,
    REQUIRED_FV3_KEYS,
    REQUIRED_AEROSOL_KEYS,
)


# ---------------------------------------------------------------------------
# Fixtures
# ---------------------------------------------------------------------------


@pytest.fixture
def schema():
    """Create a ModelContextSchema instance."""
    return ModelContextSchema()


@pytest.fixture
def valid_top_level():
    """Minimal valid top-level model context (no subsections)."""
    return {
        "resolution": "C384",
        "physics_suite": "gfdl",
        "coupling_mode": "s2swa",
        "dt_atmos": 225,
        "output_grid": "gaussian_grid",
        "output_fields": "standard",
    }


@pytest.fixture
def valid_fv3():
    """Valid model.fv3 subsection."""
    return {
        "npx": 385,
        "npy": 385,
        "npz": 127,
        "layout": [6, 6],
        "io_layout": [1, 1],
        "quilting": True,
        "write_group": 2,
        "wrttask_per_group": 40,
        "restart_interval": 12,
    }


@pytest.fixture
def valid_aerosol():
    """Valid model.aerosol subsection."""
    return {
        "emission_dataset": "qfed",
        "active_collections": ["inst_aod"],
        "grid_label": "PC720x361-DC",
    }


@pytest.fixture
def full_valid_context(valid_top_level, valid_fv3, valid_aerosol):
    """Full valid model context with all subsections."""
    ctx = dict(valid_top_level)
    ctx["fv3"] = dict(valid_fv3)
    ctx["aerosol"] = dict(valid_aerosol)
    return ctx


# ---------------------------------------------------------------------------
# Tests: Required key validation (Requirement 4.1, 4.5)
# ---------------------------------------------------------------------------


class TestRequiredKeyValidation:
    """Tests that missing required keys produce FATAL ERROR messages."""

    def test_empty_context_produces_errors_for_all_required_keys(self, schema):
        """Empty dict should produce one error per required top-level key."""
        errors = schema.validate({})
        assert len(errors) == len(REQUIRED_TOP_LEVEL_KEYS)
        for key in REQUIRED_TOP_LEVEL_KEYS:
            assert any(f"model.{key}" in e for e in errors)

    def test_all_errors_are_fatal(self, schema):
        """All validation errors should start with FATAL ERROR."""
        errors = schema.validate({})
        assert all("FATAL ERROR" in e for e in errors)

    @pytest.mark.parametrize("missing_key", REQUIRED_TOP_LEVEL_KEYS)
    def test_single_missing_key_produces_error(self, schema, valid_top_level, missing_key):
        """Each individually missing key should produce exactly one error for it."""
        context = dict(valid_top_level)
        del context[missing_key]
        errors = schema.validate(context)
        matching = [e for e in errors if f"model.{missing_key}" in e]
        assert len(matching) == 1

    def test_valid_context_produces_no_errors(self, schema, full_valid_context):
        """Fully valid context should produce no errors."""
        errors = schema.validate(full_valid_context)
        assert errors == []

    def test_missing_fv3_keys_produce_errors(self, schema, valid_top_level):
        """Missing required fv3 keys should produce FATAL ERROR."""
        context = dict(valid_top_level)
        context["fv3"] = {}  # Empty fv3 section
        errors = schema.validate(context)
        assert len(errors) == len(REQUIRED_FV3_KEYS)
        for key in REQUIRED_FV3_KEYS:
            assert any(f"model.fv3.{key}" in e for e in errors)

    def test_missing_aerosol_keys_produce_errors(self, schema, valid_top_level):
        """Missing required aerosol keys should produce FATAL ERROR."""
        context = dict(valid_top_level)
        context["aerosol"] = {}  # Empty aerosol section
        errors = schema.validate(context)
        assert len(errors) == len(REQUIRED_AEROSOL_KEYS)
        for key in REQUIRED_AEROSOL_KEYS:
            assert any(f"model.aerosol.{key}" in e for e in errors)


# ---------------------------------------------------------------------------
# Tests: Unsupported value detection
# ---------------------------------------------------------------------------


class TestUnsupportedValues:
    """Tests that unsupported enum values produce FATAL ERROR."""

    def test_invalid_resolution(self, schema, valid_top_level):
        """Unsupported resolution should produce FATAL ERROR."""
        context = dict(valid_top_level)
        context["resolution"] = "C999"
        errors = schema.validate(context)
        assert any("C999" in e for e in errors)
        assert any("FATAL ERROR" in e for e in errors)

    def test_invalid_physics_suite(self, schema, valid_top_level):
        """Unsupported physics_suite should produce FATAL ERROR."""
        context = dict(valid_top_level)
        context["physics_suite"] = "nonexistent"
        errors = schema.validate(context)
        assert any("nonexistent" in e for e in errors)

    def test_invalid_coupling_mode(self, schema, valid_top_level):
        """Unsupported coupling_mode should produce FATAL ERROR."""
        context = dict(valid_top_level)
        context["coupling_mode"] = "invalid_mode"
        errors = schema.validate(context)
        assert any("invalid_mode" in e for e in errors)

    def test_invalid_emission_dataset(self, schema, valid_top_level):
        """Unsupported emission_dataset should produce FATAL ERROR."""
        context = dict(valid_top_level)
        context["aerosol"] = {
            "emission_dataset": "bad_dataset",
            "active_collections": ["inst_aod"],
            "grid_label": "PC720x361-DC",
        }
        errors = schema.validate(context)
        assert any("bad_dataset" in e for e in errors)

    @pytest.mark.parametrize("resolution", sorted(SUPPORTED_RESOLUTIONS))
    def test_all_supported_resolutions_pass(self, schema, valid_top_level, resolution):
        """All supported resolutions should pass validation."""
        context = dict(valid_top_level)
        context["resolution"] = resolution
        errors = schema.validate(context)
        assert not any("resolution" in e.lower() for e in errors)

    @pytest.mark.parametrize("suite", sorted(SUPPORTED_PHYSICS_SUITES))
    def test_all_supported_physics_suites_pass(self, schema, valid_top_level, suite):
        """All supported physics suites should pass validation."""
        context = dict(valid_top_level)
        context["physics_suite"] = suite
        errors = schema.validate(context)
        assert not any("physics_suite" in e for e in errors)

    @pytest.mark.parametrize("mode", sorted(SUPPORTED_COUPLING_MODES))
    def test_all_supported_coupling_modes_pass(self, schema, valid_top_level, mode):
        """All supported coupling modes should pass validation."""
        context = dict(valid_top_level)
        context["coupling_mode"] = mode
        errors = schema.validate(context)
        assert not any("coupling_mode" in e for e in errors)


# ---------------------------------------------------------------------------
# Tests: Type validation
# ---------------------------------------------------------------------------


class TestTypeValidation:
    """Tests that type constraints are enforced."""

    def test_dt_atmos_must_be_positive_int(self, schema, valid_top_level):
        """dt_atmos must be a positive integer."""
        context = dict(valid_top_level)
        context["dt_atmos"] = -10
        errors = schema.validate(context)
        assert any("dt_atmos" in e for e in errors)

    def test_dt_atmos_zero_is_invalid(self, schema, valid_top_level):
        """dt_atmos of 0 is invalid (must be positive)."""
        context = dict(valid_top_level)
        context["dt_atmos"] = 0
        errors = schema.validate(context)
        assert any("dt_atmos" in e for e in errors)

    def test_dt_atmos_float_is_invalid(self, schema, valid_top_level):
        """dt_atmos as float is invalid (must be int)."""
        context = dict(valid_top_level)
        context["dt_atmos"] = 225.5
        errors = schema.validate(context)
        assert any("dt_atmos" in e for e in errors)

    def test_fv3_npx_must_be_positive_int(self, schema, valid_top_level, valid_fv3):
        """model.fv3.npx must be a positive integer."""
        context = dict(valid_top_level)
        fv3 = dict(valid_fv3)
        fv3["npx"] = -1
        context["fv3"] = fv3
        errors = schema.validate(context)
        assert any("npx" in e for e in errors)

    def test_fv3_layout_must_be_two_positive_ints(self, schema, valid_top_level, valid_fv3):
        """model.fv3.layout must be a two-element list of positive integers."""
        context = dict(valid_top_level)
        fv3 = dict(valid_fv3)
        fv3["layout"] = [0, 6]  # 0 is not positive
        context["fv3"] = fv3
        errors = schema.validate(context)
        assert any("layout" in e for e in errors)

    def test_fv3_layout_wrong_length(self, schema, valid_top_level, valid_fv3):
        """model.fv3.layout with wrong length should fail."""
        context = dict(valid_top_level)
        fv3 = dict(valid_fv3)
        fv3["layout"] = [6, 6, 6]
        context["fv3"] = fv3
        errors = schema.validate(context)
        assert any("layout" in e for e in errors)

    def test_fv3_io_layout_allows_zero(self, schema, valid_top_level, valid_fv3):
        """model.fv3.io_layout allows zero values (non-negative)."""
        context = dict(valid_top_level)
        fv3 = dict(valid_fv3)
        fv3["io_layout"] = [0, 0]
        context["fv3"] = fv3
        errors = schema.validate(context)
        assert not any("io_layout" in e for e in errors)

    def test_fv3_io_layout_negative_fails(self, schema, valid_top_level, valid_fv3):
        """model.fv3.io_layout with negative values should fail."""
        context = dict(valid_top_level)
        fv3 = dict(valid_fv3)
        fv3["io_layout"] = [-1, 1]
        context["fv3"] = fv3
        errors = schema.validate(context)
        assert any("io_layout" in e for e in errors)

    def test_fv3_quilting_must_be_bool(self, schema, valid_top_level, valid_fv3):
        """model.fv3.quilting must be a boolean."""
        context = dict(valid_top_level)
        fv3 = dict(valid_fv3)
        fv3["quilting"] = "yes"
        context["fv3"] = fv3
        errors = schema.validate(context)
        assert any("quilting" in e for e in errors)

    def test_fv3_restart_interval_allows_zero(self, schema, valid_top_level, valid_fv3):
        """model.fv3.restart_interval allows zero (non-negative)."""
        context = dict(valid_top_level)
        fv3 = dict(valid_fv3)
        fv3["restart_interval"] = 0
        context["fv3"] = fv3
        errors = schema.validate(context)
        assert not any("restart_interval" in e for e in errors)

    def test_fv3_restart_interval_negative_fails(self, schema, valid_top_level, valid_fv3):
        """model.fv3.restart_interval with negative value should fail."""
        context = dict(valid_top_level)
        fv3 = dict(valid_fv3)
        fv3["restart_interval"] = -1
        context["fv3"] = fv3
        errors = schema.validate(context)
        assert any("restart_interval" in e for e in errors)

    def test_aerosol_active_collections_must_be_nonempty_list(
        self, schema, valid_top_level
    ):
        """model.aerosol.active_collections must be a non-empty list."""
        context = dict(valid_top_level)
        context["aerosol"] = {
            "emission_dataset": "qfed",
            "active_collections": [],
            "grid_label": "PC720x361-DC",
        }
        errors = schema.validate(context)
        assert any("active_collections" in e for e in errors)

    def test_aerosol_grid_label_must_be_nonempty_string(
        self, schema, valid_top_level
    ):
        """model.aerosol.grid_label must be a non-empty string."""
        context = dict(valid_top_level)
        context["aerosol"] = {
            "emission_dataset": "qfed",
            "active_collections": ["inst_aod"],
            "grid_label": "",
        }
        errors = schema.validate(context)
        assert any("grid_label" in e for e in errors)


# ---------------------------------------------------------------------------
# Tests: Resolution defaults merge (Requirement 4.6, 4.7)
# ---------------------------------------------------------------------------


class TestMergeResolutionDefaults:
    """Tests for merge_resolution_defaults function."""

    def test_defaults_applied_when_fv3_key_missing(self):
        """Default values should be applied for keys not in model.fv3."""
        context = {
            "resolution": "C384",
            "defaults": {
                "C384": {
                    "npx": 385,
                    "npy": 385,
                    "layout": [6, 6],
                    "write_group": 2,
                    "wrttask_per_group": 40,
                },
            },
            "fv3": {},
        }
        merged = merge_resolution_defaults(context)
        assert merged["fv3"]["npx"] == 385
        assert merged["fv3"]["npy"] == 385
        assert merged["fv3"]["layout"] == [6, 6]
        assert merged["fv3"]["write_group"] == 2
        assert merged["fv3"]["wrttask_per_group"] == 40

    def test_explicit_fv3_values_override_defaults(self):
        """Explicit model.fv3 values should override resolution defaults."""
        context = {
            "resolution": "C384",
            "defaults": {
                "C384": {
                    "npx": 385,
                    "npy": 385,
                    "layout": [6, 6],
                    "write_group": 2,
                    "wrttask_per_group": 40,
                },
            },
            "fv3": {
                "npx": 400,  # Explicit override
                "layout": [8, 8],  # Explicit override
            },
        }
        merged = merge_resolution_defaults(context)
        assert merged["fv3"]["npx"] == 400  # Explicit wins
        assert merged["fv3"]["layout"] == [8, 8]  # Explicit wins
        assert merged["fv3"]["npy"] == 385  # Default applied
        assert merged["fv3"]["write_group"] == 2  # Default applied

    def test_no_defaults_section_is_noop(self):
        """Missing defaults section should not modify fv3."""
        context = {
            "resolution": "C384",
            "fv3": {"npx": 385},
        }
        merged = merge_resolution_defaults(context)
        assert merged["fv3"] == {"npx": 385}

    def test_resolution_not_in_defaults_is_noop(self):
        """Resolution not found in defaults should not modify fv3."""
        context = {
            "resolution": "C1152",
            "defaults": {
                "C384": {"npx": 385},
            },
            "fv3": {"npx": 1153},
        }
        merged = merge_resolution_defaults(context)
        assert merged["fv3"] == {"npx": 1153}

    def test_missing_fv3_section_creates_it(self):
        """If model.fv3 doesn't exist, it should be created from defaults."""
        context = {
            "resolution": "C48",
            "defaults": {
                "C48": {
                    "npx": 49,
                    "npy": 49,
                    "layout": [1, 1],
                    "write_group": 1,
                    "wrttask_per_group": 6,
                },
            },
        }
        merged = merge_resolution_defaults(context)
        assert merged["fv3"]["npx"] == 49
        assert merged["fv3"]["npy"] == 49
        assert merged["fv3"]["layout"] == [1, 1]

    def test_missing_resolution_key_is_noop(self):
        """Missing resolution key should return context unchanged."""
        context = {"fv3": {"npx": 385}}
        merged = merge_resolution_defaults(context)
        assert merged["fv3"] == {"npx": 385}

    def test_does_not_mutate_defaults(self):
        """merge_resolution_defaults should not mutate the defaults dict."""
        defaults_c384 = {
            "npx": 385,
            "npy": 385,
        }
        context = {
            "resolution": "C384",
            "defaults": {"C384": defaults_c384},
            "fv3": {"npx": 400},
        }
        merge_resolution_defaults(context)
        # Original defaults should be unchanged
        assert defaults_c384 == {"npx": 385, "npy": 385}
