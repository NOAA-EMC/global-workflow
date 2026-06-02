"""Unit tests for MOM_input.j2 template rendering.

Tests resolution-dependent grid dimensions and mixing parameters,
conditional blocks (use_waves, river_runoff, oda_incupd, do_sppt),
shell variable preservation, and unsupported resolution error handling.

Traces to: Requirements 1.2, 1.3, 1.4, 1.5, 1.6, 1.7, 1.8
"""

import os
import sys
from pathlib import Path

import pytest
from jinja2 import Environment, FileSystemLoader, StrictUndefined, UndefinedError

# ---------------------------------------------------------------------------
# Path constants
# ---------------------------------------------------------------------------

DEV_ROOT = Path(__file__).resolve().parent.parent.parent
TEMPLATE_DIR = DEV_ROOT / "parm" / "ufs" / "ocean"
TEMPLATE_FILE = "MOM_input.j2"


# ---------------------------------------------------------------------------
# Helper: render MOM_input.j2 with a given ocean context
# ---------------------------------------------------------------------------


def _render_mom_input(ocean_context: dict) -> str:
    """Render MOM_input.j2 with the given ocean context dict.

    Uses Jinja2 StrictUndefined to catch undefined variables,
    matching the Template_Renderer behavior.

    Args:
        ocean_context: Dictionary of ocean model parameters.

    Returns:
        Rendered MOM_input content as a string.
    """
    env = Environment(
        loader=FileSystemLoader(str(TEMPLATE_DIR)),
        undefined=StrictUndefined,
        keep_trailing_newline=True,
    )
    template = env.get_template(TEMPLATE_FILE)
    context = {"model": {"ocean": ocean_context}}
    return template.render(context)


def _base_ocean_context(resolution: str = "025") -> dict:
    """Build a minimal valid ocean context for the given resolution.

    Args:
        resolution: Ocean resolution string (025, 050, 100, 500).

    Returns:
        Dictionary with all required keys for MOM_input.j2 rendering.
    """
    return {
        "resolution": resolution,
        "dt_ocean": 900,
        "dt_therm": 3600,
        "nk": 75,
        "diag_coord_def_z_file": "oceanda_zgrid_75L.nc",
        "use_waves": False,
        "river_runoff": False,
        "oda_incupd": False,
        "oda_incupd_nhours": 6,
        "do_sppt": False,
    }


# ---------------------------------------------------------------------------
# Tests: Resolution-dependent grid dimensions (Requirements 1.2-1.5)
# ---------------------------------------------------------------------------


class TestResolutionGridDimensions:
    """Tests that each resolution produces correct NIGLOBAL/NJGLOBAL values."""

    def test_resolution_025_grid_dims(self):
        """Resolution 025: NIGLOBAL=1440, NJGLOBAL=1080."""
        ctx = _base_ocean_context("025")
        output = _render_mom_input(ctx)
        assert "NIGLOBAL = 1440" in output
        assert "NJGLOBAL = 1080" in output

    def test_resolution_050_grid_dims(self):
        """Resolution 050: NIGLOBAL=720, NJGLOBAL=576."""
        ctx = _base_ocean_context("050")
        output = _render_mom_input(ctx)
        assert "NIGLOBAL = 720" in output
        assert "NJGLOBAL = 576" in output

    def test_resolution_100_grid_dims(self):
        """Resolution 100: NIGLOBAL=360, NJGLOBAL=320."""
        ctx = _base_ocean_context("100")
        output = _render_mom_input(ctx)
        assert "NIGLOBAL = 360" in output
        assert "NJGLOBAL = 320" in output

    def test_resolution_500_grid_dims(self):
        """Resolution 500: NIGLOBAL=72, NJGLOBAL=35."""
        ctx = _base_ocean_context("500")
        output = _render_mom_input(ctx)
        assert "NIGLOBAL = 72" in output
        assert "NJGLOBAL = 35" in output


# ---------------------------------------------------------------------------
# Tests: Resolution-dependent mixing parameters (Requirements 1.2-1.5)
# ---------------------------------------------------------------------------


class TestResolutionMixingParams:
    """Tests that each resolution produces correct KHTH/KHTR mixing values."""

    def test_resolution_025_mixing(self):
        """Resolution 025: KHTH=10.0, KHTR=10.0."""
        ctx = _base_ocean_context("025")
        output = _render_mom_input(ctx)
        assert "KHTH = 10.0" in output
        assert "KHTR = 10.0" in output

    def test_resolution_050_mixing(self):
        """Resolution 050: KHTH=50.0, KHTR=50.0."""
        ctx = _base_ocean_context("050")
        output = _render_mom_input(ctx)
        assert "KHTH = 50.0" in output
        assert "KHTR = 50.0" in output

    def test_resolution_100_mixing(self):
        """Resolution 100: KHTH=600.0, KHTR=600.0."""
        ctx = _base_ocean_context("100")
        output = _render_mom_input(ctx)
        assert "KHTH = 600.0" in output
        assert "KHTR = 600.0" in output

    def test_resolution_500_mixing(self):
        """Resolution 500: KHTH=1000.0, KHTR=1000.0."""
        ctx = _base_ocean_context("500")
        output = _render_mom_input(ctx)
        assert "KHTH = 1000.0" in output
        assert "KHTR = 1000.0" in output


# ---------------------------------------------------------------------------
# Tests: Conditional blocks (Requirement 1.7)
# ---------------------------------------------------------------------------


class TestConditionalBlocks:
    """Tests conditional rendering of use_waves, river_runoff, oda_incupd, do_sppt."""

    def test_use_waves_true(self):
        """use_waves=True produces USE_WAVES and WAVE_METHOD lines."""
        ctx = _base_ocean_context("025")
        ctx["use_waves"] = True
        output = _render_mom_input(ctx)
        assert "USE_WAVES = True" in output
        assert "WAVE_METHOD" in output

    def test_use_waves_false(self):
        """use_waves=False omits USE_WAVES line entirely."""
        ctx = _base_ocean_context("025")
        ctx["use_waves"] = False
        output = _render_mom_input(ctx)
        assert "USE_WAVES" not in output
        assert "WAVE_METHOD" not in output

    def test_river_runoff_true(self):
        """river_runoff=True produces RIVER_RUNOFF and FRUNOFF lines."""
        ctx = _base_ocean_context("025")
        ctx["river_runoff"] = True
        output = _render_mom_input(ctx)
        assert "RIVER_RUNOFF = True" in output
        assert "FRUNOFF" in output

    def test_river_runoff_false(self):
        """river_runoff=False omits RIVER_RUNOFF line entirely."""
        ctx = _base_ocean_context("025")
        ctx["river_runoff"] = False
        output = _render_mom_input(ctx)
        assert "RIVER_RUNOFF" not in output

    def test_oda_incupd_true(self):
        """oda_incupd=True produces ODA_INCUPD = True and ODA_INCUPD_NHOURS."""
        ctx = _base_ocean_context("025")
        ctx["oda_incupd"] = True
        ctx["oda_incupd_nhours"] = 6
        output = _render_mom_input(ctx)
        assert "ODA_INCUPD = True" in output
        assert "ODA_INCUPD_NHOURS" in output

    def test_oda_incupd_false(self):
        """oda_incupd=False produces ODA_INCUPD = False, no NHOURS."""
        ctx = _base_ocean_context("025")
        ctx["oda_incupd"] = False
        output = _render_mom_input(ctx)
        assert "ODA_INCUPD = False" in output
        assert "ODA_INCUPD_NHOURS" not in output

    def test_do_sppt_true(self):
        """do_sppt=True produces DO_SPPT = True."""
        ctx = _base_ocean_context("025")
        ctx["do_sppt"] = True
        output = _render_mom_input(ctx)
        assert "DO_SPPT = True" in output

    def test_do_sppt_false(self):
        """do_sppt=False produces DO_SPPT = False."""
        ctx = _base_ocean_context("025")
        ctx["do_sppt"] = False
        output = _render_mom_input(ctx)
        assert "DO_SPPT = False" in output


# ---------------------------------------------------------------------------
# Tests: Shell variable preservation (Requirement 1.8)
# ---------------------------------------------------------------------------


class TestShellVariablePreservation:
    """Tests that ${TOPOEDITS} and ${CHLCLIM} appear verbatim in output."""

    def test_topoedits_preserved(self):
        """${TOPOEDITS} shell variable appears in rendered output."""
        ctx = _base_ocean_context("025")
        output = _render_mom_input(ctx)
        assert "${TOPOEDITS}" in output

    def test_chlclim_preserved_when_river_runoff(self):
        """${CHLCLIM} shell variable appears when river_runoff is True."""
        ctx = _base_ocean_context("025")
        ctx["river_runoff"] = True
        output = _render_mom_input(ctx)
        assert "${CHLCLIM}" in output


# ---------------------------------------------------------------------------
# Tests: Unsupported resolution triggers UndefinedError (Requirement 1.6)
# ---------------------------------------------------------------------------


class TestUnsupportedResolution:
    """Tests that unsupported resolution triggers jinja2.UndefinedError."""

    def test_unsupported_resolution_raises_undefined_error(self):
        """Unsupported resolution triggers UndefinedError via undefined_resolution_error."""
        ctx = _base_ocean_context("025")
        ctx["resolution"] = "999"
        with pytest.raises(UndefinedError):
            _render_mom_input(ctx)

    def test_empty_resolution_raises_undefined_error(self):
        """Empty string resolution triggers UndefinedError."""
        ctx = _base_ocean_context("025")
        ctx["resolution"] = ""
        with pytest.raises(UndefinedError):
            _render_mom_input(ctx)
