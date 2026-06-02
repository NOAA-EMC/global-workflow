"""Property-based tests for CICE6 ice_in template rendering.

Uses hypothesis to verify the Warm Start Conditional Rendering property
across all valid ice Model_Context inputs.

Feature: coupled-model-configs, Property 7: Warm Start Conditional Rendering

Traces to: Requirements 3.3, 3.4
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

# Path to the dev/ root containing templates
DEV_ROOT = Path(__file__).resolve().parent.parent.parent

# Path to the ice_in.j2 template
ICE_IN_TEMPLATE = DEV_ROOT / "parm" / "ufs" / "ice" / "ice_in.j2"


# ---------------------------------------------------------------------------
# Hypothesis strategies for valid ice Model_Context generation
# ---------------------------------------------------------------------------


@st.composite
def valid_ice_model_context(draw: st.DrawFn) -> dict:
    """Generate a valid ice Model_Context dict with varying warm_start.

    Produces contexts with all required ice keys and randomized values
    to exercise the warm_start conditional rendering logic.
    """
    warm_start = draw(st.booleans())
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

    return ice_context


# ---------------------------------------------------------------------------
# Property 7: Warm Start Conditional Rendering
# ---------------------------------------------------------------------------


class TestWarmStartConditionalRendering:
    """Property 7: Warm Start Conditional Rendering.

    **Validates: Requirements 3.3, 3.4**

    For any valid ice Model_Context where model.ice.warm_start is true,
    the rendered ice_in SHALL contain runtype = 'continue' and
    use_restart_time = .true.. When false, it SHALL contain
    runtype = 'initial' and use_restart_time = .false..
    """

    @settings(
        max_examples=100,
        suppress_health_check=[HealthCheck.too_slow],
        deadline=None,
    )
    @given(ice_context=valid_ice_model_context())
    def test_warm_start_conditional_rendering(self, ice_context: dict, tmp_path_factory):
        """Assert warm_start controls runtype and use_restart_time correctly.

        **Validates: Requirements 3.3, 3.4**

        Feature: coupled-model-configs, Property 7: Warm Start Conditional Rendering

        Generates valid ice Model_Context dicts with warm_start as True/False,
        renders the ice_in.j2 template, and asserts:
        - warm_start=True → runtype = 'continue' and use_restart_time = .true.
        - warm_start=False → runtype = 'initial' and use_restart_time = .false.
        """
        # Build the full model context with the ice section
        model_context = {"model": {"ice": ice_context}}

        # Set up the template renderer with the fortran_logical filter
        searchpath = [
            str(DEV_ROOT / "parm" / "ufs"),
            str(DEV_ROOT / "parm"),
        ]
        searchpath = [p for p in searchpath if Path(p).is_dir()]

        renderer = TemplateRenderer(
            context=model_context,
            searchpath=searchpath,
            strict=True,
        )

        # Render the template
        expdir = tmp_path_factory.mktemp("expdir")
        output_path = expdir / "ice_in"
        renderer.render_file(ICE_IN_TEMPLATE, output_path)

        # Read the rendered output
        rendered = output_path.read_text(encoding="utf-8")

        # Extract runtype value from rendered output
        runtype_match = re.search(r"runtype\s*=\s*'(\w+)'", rendered)
        assert runtype_match is not None, (
            "Could not find 'runtype' assignment in rendered ice_in"
        )
        runtype_value = runtype_match.group(1)

        # Extract use_restart_time value from rendered output
        use_restart_time_match = re.search(
            r"use_restart_time\s*=\s*(\.\w+\.)", rendered
        )
        assert use_restart_time_match is not None, (
            "Could not find 'use_restart_time' assignment in rendered ice_in"
        )
        use_restart_time_value = use_restart_time_match.group(1)

        # Assert the warm start conditional logic
        if ice_context["warm_start"]:
            assert runtype_value == "continue", (
                f"warm_start=True should produce runtype='continue', "
                f"got runtype='{runtype_value}'"
            )
            assert use_restart_time_value == ".true.", (
                f"warm_start=True should produce use_restart_time=.true., "
                f"got use_restart_time={use_restart_time_value}"
            )
        else:
            assert runtype_value == "initial", (
                f"warm_start=False should produce runtype='initial', "
                f"got runtype='{runtype_value}'"
            )
            assert use_restart_time_value == ".false.", (
                f"warm_start=False should produce use_restart_time=.false., "
                f"got use_restart_time={use_restart_time_value}"
            )
