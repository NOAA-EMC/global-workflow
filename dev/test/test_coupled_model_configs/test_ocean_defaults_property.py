"""Property-based tests for Ocean Resolution Default Override.

Uses hypothesis to verify that explicit model.ocean values always override
resolution-dependent defaults, and that default-only keys appear in the
merged context with correct default values.

Feature: coupled-model-configs, Property 6: Ocean Resolution Default Override

Traces to: Requirements 12.1, 12.2, 12.3
"""

from __future__ import annotations

import os
import sys
from copy import deepcopy

import pytest
from hypothesis import given, settings, HealthCheck
from hypothesis import strategies as st

# Add the workflow module to the path
sys.path.insert(0, os.path.join(os.path.dirname(__file__), "..", "..", "workflow"))

from deployment.model_context import (
    OCEAN_RESOLUTION_DEFAULTS,
    SUPPORTED_OCEAN_RESOLUTIONS,
    merge_ocean_resolution_defaults,
)

# ---------------------------------------------------------------------------
# Constants
# ---------------------------------------------------------------------------

RESOLUTIONS = sorted(SUPPORTED_OCEAN_RESOLUTIONS)

# All possible default keys across all resolutions
ALL_DEFAULT_KEYS: set[str] = set()
for res_defaults in OCEAN_RESOLUTION_DEFAULTS.values():
    ALL_DEFAULT_KEYS.update(res_defaults.keys())


# ---------------------------------------------------------------------------
# Hypothesis strategies
# ---------------------------------------------------------------------------

# Strategy for generating a supported ocean resolution
resolution_strategy = st.sampled_from(RESOLUTIONS)

# Strategy for generating explicit ocean override values
# These are numeric values that could plausibly override defaults
explicit_value_strategy = st.one_of(
    st.integers(min_value=1, max_value=10000),
    st.floats(min_value=0.001, max_value=10000.0, allow_nan=False, allow_infinity=False),
)


@st.composite
def ocean_context_with_overrides(draw: st.DrawFn) -> dict:
    """Generate an ocean context with a mix of explicit values and defaults.

    Produces a model context dict with:
    - A valid resolution
    - Some keys from the resolution defaults explicitly set to different values
    - Some keys left unset (to be filled by defaults)

    Returns a tuple-like dict with metadata for assertions:
    - 'model_context': the input model context
    - 'resolution': the chosen resolution
    - 'explicit_keys': keys explicitly set in ocean (that also exist in defaults)
    - 'default_only_keys': keys NOT explicitly set (should come from defaults)
    """
    resolution = draw(resolution_strategy)
    defaults_for_res = OCEAN_RESOLUTION_DEFAULTS.get(resolution, {})
    default_keys = list(defaults_for_res.keys())

    # Decide which default keys to override explicitly (at least 1, up to all)
    if default_keys:
        num_explicit = draw(st.integers(min_value=1, max_value=len(default_keys)))
        explicit_keys = draw(
            st.lists(
                st.sampled_from(default_keys),
                min_size=num_explicit,
                max_size=num_explicit,
                unique=True,
            )
        )
    else:
        explicit_keys = []

    default_only_keys = [k for k in default_keys if k not in explicit_keys]

    # Build the ocean section with explicit overrides
    ocean = {
        "resolution": resolution,
        # Always include required keys that aren't in defaults
        "use_waves": False,
        "oda_incupd": False,
        "do_sppt": False,
        "river_runoff": True,
        "diag_coord_def_z_file": "oceanda_zgrid_75L.nc",
        "frunoff": "INPUT/runoff.daitren.clim.nc",
        "tasks": 120,
    }

    # Set explicit override values (different from defaults)
    explicit_values = {}
    for key in explicit_keys:
        # Generate a value that is different from the default
        default_val = defaults_for_res[key]
        if isinstance(default_val, int):
            override_val = draw(
                st.integers(min_value=1, max_value=99999).filter(
                    lambda v, d=default_val: v != d
                )
            )
        elif isinstance(default_val, float):
            override_val = draw(
                st.floats(
                    min_value=0.001, max_value=99999.0,
                    allow_nan=False, allow_infinity=False,
                ).filter(lambda v, d=default_val: v != d)
            )
        else:
            override_val = draw(explicit_value_strategy)

        ocean[key] = override_val
        explicit_values[key] = override_val

    model_context = {"ocean": ocean}

    return {
        "model_context": model_context,
        "resolution": resolution,
        "explicit_keys": explicit_keys,
        "explicit_values": explicit_values,
        "default_only_keys": default_only_keys,
    }


# ---------------------------------------------------------------------------
# Property 6: Ocean Resolution Default Override
# ---------------------------------------------------------------------------


class TestOceanResolutionDefaultOverride:
    """Property 6: Ocean Resolution Default Override.

    **Validates: Requirements 12.1, 12.2, 12.3**

    For any key present in both model.ocean (explicit) and
    model.ocean.defaults[resolution], the merged context SHALL contain
    the explicit model.ocean value. Conversely, for any key present only
    in defaults, the merged context SHALL contain the default value.
    """

    @settings(
        max_examples=100,
        suppress_health_check=[HealthCheck.too_slow],
        deadline=None,
    )
    @given(data=ocean_context_with_overrides())
    def test_explicit_values_override_defaults(self, data: dict):
        """Assert explicit model.ocean values are preserved after merge.

        **Validates: Requirements 12.1, 12.2, 12.3**

        Feature: coupled-model-configs, Property 6: Ocean Resolution Default Override

        Generates ocean contexts with both explicit values and resolution
        defaults, calls merge_ocean_resolution_defaults(), and asserts:
        - Explicit values are NOT overwritten by defaults
        - Default-only keys appear in the merged context with correct values
        """
        model_context = deepcopy(data["model_context"])
        resolution = data["resolution"]
        explicit_keys = data["explicit_keys"]
        explicit_values = data["explicit_values"]
        default_only_keys = data["default_only_keys"]

        # Perform the merge
        merged = merge_ocean_resolution_defaults(model_context)
        merged_ocean = merged["ocean"]

        # Assert: explicit values are preserved (not overwritten by defaults)
        for key in explicit_keys:
            assert key in merged_ocean, (
                f"Explicit key '{key}' should be present in merged ocean context"
            )
            assert merged_ocean[key] == explicit_values[key], (
                f"Explicit value for '{key}' should be {explicit_values[key]!r}, "
                f"but got {merged_ocean[key]!r}. "
                f"Explicit values must override resolution defaults."
            )

        # Assert: default-only keys appear with correct default values
        defaults_for_res = OCEAN_RESOLUTION_DEFAULTS.get(resolution, {})
        for key in default_only_keys:
            assert key in merged_ocean, (
                f"Default-only key '{key}' should appear in merged ocean context "
                f"for resolution '{resolution}'"
            )
            assert merged_ocean[key] == defaults_for_res[key], (
                f"Default-only key '{key}' should have value "
                f"{defaults_for_res[key]!r} from resolution '{resolution}' "
                f"defaults, but got {merged_ocean[key]!r}"
            )

    @settings(
        max_examples=100,
        suppress_health_check=[HealthCheck.too_slow],
        deadline=None,
    )
    @given(resolution=resolution_strategy)
    def test_all_defaults_applied_when_no_explicit_keys(self, resolution: str):
        """Assert all defaults are applied when ocean has no overlapping keys.

        **Validates: Requirements 12.1, 12.3**

        Feature: coupled-model-configs, Property 6: Ocean Resolution Default Override

        Generates a minimal ocean context (only required non-default keys)
        and asserts that ALL resolution defaults are applied to the merged
        context.
        """
        # Minimal ocean context with no keys that overlap with defaults
        model_context = {
            "ocean": {
                "resolution": resolution,
                "use_waves": False,
                "oda_incupd": False,
                "do_sppt": False,
                "river_runoff": True,
                "diag_coord_def_z_file": "oceanda_zgrid_75L.nc",
                "frunoff": "INPUT/runoff.daitren.clim.nc",
                "tasks": 120,
            }
        }

        merged = merge_ocean_resolution_defaults(model_context)
        merged_ocean = merged["ocean"]

        # All defaults for this resolution should be present
        defaults_for_res = OCEAN_RESOLUTION_DEFAULTS.get(resolution, {})
        for key, expected_value in defaults_for_res.items():
            assert key in merged_ocean, (
                f"Default key '{key}' should be present in merged ocean "
                f"context for resolution '{resolution}'"
            )
            assert merged_ocean[key] == expected_value, (
                f"Default key '{key}' should have value {expected_value!r} "
                f"for resolution '{resolution}', got {merged_ocean[key]!r}"
            )
