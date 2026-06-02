"""Property-based tests for coupled-model schema validation.

Uses hypothesis to verify that missing required keys from any coupled-model
section (ocean, ice, wave, post) produce FATAL ERROR messages, and that
unsupported ocean.resolution values produce FATAL ERROR.

Feature: coupled-model-configs, Property 5: Schema Validation (Missing Keys Cause FATAL ERROR)

Traces to: Requirements 1.6, 7.1, 7.2, 7.3, 7.4, 7.5
"""

from __future__ import annotations

import os
import sys

import pytest
from hypothesis import given, settings, HealthCheck
from hypothesis import strategies as st

# Add the workflow module to the path
sys.path.insert(0, os.path.join(os.path.dirname(__file__), "..", "..", "workflow"))

from deployment.model_context import (
    COUPLED_REQUIRED_KEYS,
    SUPPORTED_OCEAN_RESOLUTIONS,
    validate_coupled_model_context,
)


# ---------------------------------------------------------------------------
# Hypothesis strategies for generating valid complete model contexts
# ---------------------------------------------------------------------------


@st.composite
def valid_ocean_section(draw: st.DrawFn) -> dict:
    """Generate a valid model.ocean section with all required keys."""
    return {
        "resolution": draw(st.sampled_from(sorted(SUPPORTED_OCEAN_RESOLUTIONS))),
        "dt_ocean": draw(st.integers(min_value=60, max_value=7200)),
        "dt_therm": draw(st.integers(min_value=60, max_value=14400)),
        "use_waves": draw(st.booleans()),
        "oda_incupd": draw(st.booleans()),
        "do_sppt": draw(st.booleans()),
        "river_runoff": draw(st.booleans()),
        "diag_coord_def_z_file": draw(st.sampled_from([
            "oceanda_zgrid_75L.nc",
            "oceanda_zgrid_50L.nc",
            "oceanda_zgrid_100L.nc",
        ])),
        "frunoff": draw(st.sampled_from([
            "INPUT/runoff.daitren.clim.nc",
            "INPUT/runoff.monthly.nc",
        ])),
        "tasks": draw(st.integers(min_value=1, max_value=1000)),
    }


@st.composite
def valid_ice_section(draw: st.DrawFn) -> dict:
    """Generate a valid model.ice section with all required keys."""
    return {
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
        "warm_start": draw(st.booleans()),
        "histfreq_n": draw(st.integers(min_value=1, max_value=30)),
        "hist_avg": draw(st.booleans()),
        "dumpfreq": draw(st.sampled_from(["d", "h", "m", "y"])),
        "dumpfreq_n": draw(st.integers(min_value=1, max_value=30)),
        "ktherm": draw(st.integers(min_value=0, max_value=2)),
        "tr_pond_lvl": draw(st.booleans()),
    }


@st.composite
def valid_wave_section(draw: st.DrawFn) -> dict:
    """Generate a valid model.wave section with all required keys."""
    return {
        "ice_input": draw(st.sampled_from(["YES", "CPL"])),
        "current_input": draw(st.sampled_from(["YES", "CPL"])),
        "output_params": draw(st.sampled_from([
            "HS FP DP PHS PTP PDIR CHA",
            "HS FP DP",
            "HS LM",
        ])),
        "dt_field_output": draw(st.integers(min_value=3600, max_value=86400)),
        "dt_point_output": draw(st.integers(min_value=900, max_value=43200)),
        "grid_output_dir": draw(st.sampled_from(["./", "./output/"])),
        "point_output_dir": draw(st.sampled_from(["./", "./points/"])),
        "restart_output_dir": draw(st.sampled_from(["./RESTART/", "./restart/"])),
    }


@st.composite
def valid_post_section(draw: st.DrawFn) -> dict:
    """Generate a valid model.post section with all required keys."""
    return {
        "system": draw(st.sampled_from(["gfs", "gcafs", "gefs", "sfs"])),
    }


@st.composite
def valid_complete_model_context(draw: st.DrawFn) -> dict:
    """Generate a complete valid model context with all coupled sections."""
    return {
        "ocean": draw(valid_ocean_section()),
        "ice": draw(valid_ice_section()),
        "wave": draw(valid_wave_section()),
        "post": draw(valid_post_section()),
    }


# ---------------------------------------------------------------------------
# Strategy: Generate context with randomly removed required keys
# ---------------------------------------------------------------------------


@st.composite
def model_context_with_removed_keys(draw: st.DrawFn) -> tuple[dict, list[tuple[str, str]]]:
    """Generate a model context with one or more required keys randomly removed.

    Returns a tuple of (modified_context, list_of_removed_keys) where each
    removed key is a (section, key) tuple.
    """
    context = draw(valid_complete_model_context())

    # Collect all possible (section, key) pairs
    all_keys: list[tuple[str, str]] = []
    for section, keys in COUPLED_REQUIRED_KEYS.items():
        for key in keys:
            all_keys.append((section, key))

    # Choose at least 1 key to remove, up to 5 for efficiency
    num_to_remove = draw(st.integers(min_value=1, max_value=min(5, len(all_keys))))
    keys_to_remove = draw(
        st.lists(
            st.sampled_from(all_keys),
            min_size=num_to_remove,
            max_size=num_to_remove,
            unique=True,
        )
    )

    # Remove the selected keys
    for section, key in keys_to_remove:
        if section in context and key in context[section]:
            del context[section][key]

    return context, keys_to_remove


# ---------------------------------------------------------------------------
# Strategy: Generate unsupported ocean resolution values
# ---------------------------------------------------------------------------


@st.composite
def unsupported_ocean_resolution(draw: st.DrawFn) -> str:
    """Generate a random string that is NOT a supported ocean resolution.

    Generates strings that are not in {025, 050, 100, 500}.
    """
    # Generate random strings that are unlikely to be valid resolutions
    resolution = draw(st.text(
        alphabet=st.characters(whitelist_categories=("Nd", "Lu", "Ll")),
        min_size=1,
        max_size=6,
    ).filter(lambda s: s not in SUPPORTED_OCEAN_RESOLUTIONS))
    return resolution


# ---------------------------------------------------------------------------
# Property 5: Schema Validation (Missing Keys Cause FATAL ERROR)
# ---------------------------------------------------------------------------


class TestSchemaValidationProperty:
    """Property 5: Schema Validation (Missing Keys Cause FATAL ERROR).

    **Validates: Requirements 1.6, 7.1, 7.2, 7.3, 7.4, 7.5**

    For any required key in the coupled-model Model_Context schema,
    removing that key from the context SHALL cause the validator to emit
    a FATAL ERROR identifying the missing key. Additionally, for any
    unsupported ocean.resolution value (not in {025, 050, 100, 500}),
    the validator SHALL emit a FATAL ERROR.
    """

    @settings(
        max_examples=100,
        suppress_health_check=[HealthCheck.too_slow],
        deadline=None,
    )
    @given(data=model_context_with_removed_keys())
    def test_missing_required_keys_produce_fatal_errors(
        self, data: tuple[dict, list[tuple[str, str]]]
    ):
        """Assert FATAL ERROR emitted for each missing required key.

        **Validates: Requirements 7.1, 7.2, 7.3, 7.4, 7.5**

        Feature: coupled-model-configs, Property 5: Schema Validation (Missing Keys Cause FATAL ERROR)

        Generates valid Model_Context dicts with randomly removed required
        keys from ocean/ice/wave/post sections and asserts that a FATAL ERROR
        message is emitted for each missing key.
        """
        context, removed_keys = data

        errors = validate_coupled_model_context(context)

        # There must be at least one error
        assert len(errors) >= 1, (
            f"Expected FATAL ERROR(s) for removed keys {removed_keys}, "
            f"but got no errors"
        )

        # Every error must contain "FATAL ERROR"
        for error in errors:
            assert "FATAL ERROR" in error, (
                f"Error message does not contain 'FATAL ERROR': {error}"
            )

        # Each removed key should be identified in the error messages
        for section, key in removed_keys:
            matching = [
                e for e in errors
                if f"model.{section}.{key}" in e or f"model.{section}" in e
            ]
            assert len(matching) >= 1, (
                f"No FATAL ERROR found for removed key "
                f"'model.{section}.{key}'. Errors: {errors}"
            )

    @settings(
        max_examples=100,
        suppress_health_check=[HealthCheck.too_slow, HealthCheck.filter_too_much],
        deadline=None,
    )
    @given(bad_resolution=unsupported_ocean_resolution())
    def test_unsupported_ocean_resolution_produces_fatal_error(
        self, bad_resolution: str
    ):
        """Assert FATAL ERROR for unsupported ocean.resolution values.

        **Validates: Requirements 1.6, 7.1**

        Feature: coupled-model-configs, Property 5: Schema Validation (Missing Keys Cause FATAL ERROR)

        Generates random strings not in {025, 050, 100, 500} and asserts
        that validate_coupled_model_context emits a FATAL ERROR identifying
        the invalid resolution value.
        """
        # Build a context with all required keys but an invalid resolution
        context = {
            "ocean": {
                "resolution": bad_resolution,
                "dt_ocean": 900,
                "dt_therm": 3600,
                "use_waves": False,
                "oda_incupd": False,
                "do_sppt": False,
                "river_runoff": True,
                "diag_coord_def_z_file": "oceanda_zgrid_75L.nc",
                "frunoff": "INPUT/runoff.daitren.clim.nc",
                "tasks": 120,
            },
            "ice": {
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
            },
            "wave": {
                "ice_input": "CPL",
                "current_input": "CPL",
                "output_params": "HS FP DP PHS PTP PDIR CHA",
                "dt_field_output": 10800,
                "dt_point_output": 3600,
                "grid_output_dir": "./",
                "point_output_dir": "./",
                "restart_output_dir": "./RESTART/",
            },
            "post": {"system": "gfs"},
        }

        errors = validate_coupled_model_context(context)

        # Must have at least one error about the resolution
        assert len(errors) >= 1, (
            f"Expected FATAL ERROR for unsupported resolution '{bad_resolution}', "
            f"but got no errors"
        )

        # Find the resolution-specific error
        resolution_errors = [
            e for e in errors
            if "ocean.resolution" in e and bad_resolution in e
        ]
        assert len(resolution_errors) >= 1, (
            f"Expected FATAL ERROR mentioning 'ocean.resolution' and "
            f"'{bad_resolution}', but got: {errors}"
        )

        # The error must be a FATAL ERROR
        for error in resolution_errors:
            assert "FATAL ERROR" in error, (
                f"Resolution error does not contain 'FATAL ERROR': {error}"
            )

    @settings(
        max_examples=100,
        suppress_health_check=[HealthCheck.too_slow],
        deadline=None,
    )
    @given(context=valid_complete_model_context())
    def test_valid_context_produces_no_errors(self, context: dict):
        """Assert that a valid complete context produces no FATAL ERRORs.

        **Validates: Requirements 7.1, 7.2, 7.3, 7.5**

        Feature: coupled-model-configs, Property 5: Schema Validation (Missing Keys Cause FATAL ERROR)

        This is the complementary property: valid contexts should pass
        validation without any errors, confirming that the validator
        only fires on genuinely invalid inputs.
        """
        errors = validate_coupled_model_context(context)
        assert errors == [], (
            f"Valid context should produce no errors, but got: {errors}"
        )
