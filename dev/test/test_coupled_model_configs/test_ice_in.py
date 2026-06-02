"""Unit tests for ice_in.j2 template rendering.

Tests warm start conditional, decomposition parameters, shell variable
preservation, grid/mask filenames, namelist format validity, and
presence of all required namelist groups.

Traces to: Requirements 3.1, 3.2, 3.3, 3.4, 3.5, 3.6, 3.7
"""

import os
import sys
from pathlib import Path

import pytest
from jinja2 import Environment, FileSystemLoader, StrictUndefined

sys.path.insert(0, os.path.join(os.path.dirname(__file__), "..", "..", "workflow"))

from deployment.validators.namelist import NamelistValidator


# ---------------------------------------------------------------------------
# Path constants
# ---------------------------------------------------------------------------

DEV_ROOT = Path(__file__).resolve().parent.parent.parent
TEMPLATE_DIR = DEV_ROOT / "parm" / "ufs" / "ice"
TEMPLATE_FILE = "ice_in.j2"


# ---------------------------------------------------------------------------
# Helpers
# ---------------------------------------------------------------------------


def _fortran_logical(value) -> str:
    """Convert Python boolean to Fortran logical literal."""
    return ".true." if value else ".false."


def _render_ice_in(ice_context: dict) -> str:
    """Render the ice_in.j2 template with the given ice context.

    Registers the fortran_logical filter and wraps the context
    in a 'model' namespace as the template expects.
    """
    env = Environment(
        loader=FileSystemLoader(str(TEMPLATE_DIR)),
        undefined=StrictUndefined,
        keep_trailing_newline=True,
    )
    env.filters["fortran_logical"] = _fortran_logical

    template = env.get_template(TEMPLATE_FILE)
    rendered = template.render(model={"ice": ice_context})
    return rendered


# ---------------------------------------------------------------------------
# Fixtures
# ---------------------------------------------------------------------------


@pytest.fixture
def ice_context():
    """A valid ice context fixture with all required keys."""
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
        "block_size_x": 0,
        "block_size_y": 0,
        "diagfreq": 24,
    }


@pytest.fixture
def namelist_validator():
    """NamelistValidator instance for format validation."""
    return NamelistValidator()


# ===========================================================================
# Test Class: Warm Start Conditional (Requirements 3.3, 3.4)
# ===========================================================================


class TestWarmStartConditional:
    """Tests that warm_start=True/False produces correct runtype and restart settings."""

    def test_warm_start_true_produces_continue(self, ice_context):
        """warm_start=True → runtype='continue', use_restart_time=.true., ice_ic='./cice_model.res'."""
        ice_context["warm_start"] = True
        rendered = _render_ice_in(ice_context)

        assert "runtype        = 'continue'" in rendered
        assert "use_restart_time = .true." in rendered
        assert "ice_ic         = './cice_model.res'" in rendered

    def test_warm_start_false_produces_initial(self, ice_context):
        """warm_start=False → runtype='initial', use_restart_time=.false., ice_ic='default'."""
        ice_context["warm_start"] = False
        rendered = _render_ice_in(ice_context)

        assert "runtype        = 'initial'" in rendered
        assert "use_restart_time = .false." in rendered
        assert "ice_ic         = 'default'" in rendered


# ===========================================================================
# Test Class: Decomposition Parameters (Requirement 3.2)
# ===========================================================================


class TestDecompositionParameters:
    """Tests that nprocs, decomposition, block_size_x/y render correctly in &domain_nml."""

    def test_nprocs_rendered(self, ice_context):
        """nprocs value appears in &domain_nml."""
        ice_context["nprocs"] = 48
        rendered = _render_ice_in(ice_context)
        assert "nprocs            = 48" in rendered

    def test_processor_shape_rendered(self, ice_context):
        """decomposition value appears as processor_shape."""
        ice_context["decomposition"] = "slenderX2"
        rendered = _render_ice_in(ice_context)
        assert "processor_shape   = 'slenderX2'" in rendered

    def test_block_size_x_rendered(self, ice_context):
        """block_size_x value appears in &domain_nml."""
        ice_context["block_size_x"] = 10
        rendered = _render_ice_in(ice_context)
        assert "block_size_x      = 10" in rendered

    def test_block_size_y_rendered(self, ice_context):
        """block_size_y value appears in &domain_nml."""
        ice_context["block_size_y"] = 20
        rendered = _render_ice_in(ice_context)
        assert "block_size_y      = 20" in rendered

    def test_nx_ny_global_rendered(self, ice_context):
        """nx_global and ny_global values appear in &domain_nml."""
        ice_context["nx_glb"] = 1440
        ice_context["ny_glb"] = 1080
        rendered = _render_ice_in(ice_context)
        assert "nx_global         = 1440" in rendered
        assert "ny_global         = 1080" in rendered


# ===========================================================================
# Test Class: Shell Variable Preservation (Requirement 3.7)
# ===========================================================================


class TestShellVariablePreservation:
    """Tests that ${SYEAR}, ${SMONTH}, ${SDAY}, ${FHMAX} appear verbatim in output."""

    def test_syear_preserved(self, ice_context):
        """${SYEAR} appears verbatim in rendered output."""
        rendered = _render_ice_in(ice_context)
        assert "${SYEAR}" in rendered

    def test_smonth_preserved(self, ice_context):
        """${SMONTH} appears verbatim in rendered output."""
        rendered = _render_ice_in(ice_context)
        assert "${SMONTH}" in rendered

    def test_sday_preserved(self, ice_context):
        """${SDAY} appears verbatim in rendered output."""
        rendered = _render_ice_in(ice_context)
        assert "${SDAY}" in rendered

    def test_fhmax_preserved(self, ice_context):
        """${FHMAX} appears verbatim in rendered output."""
        rendered = _render_ice_in(ice_context)
        assert "${FHMAX}" in rendered


# ===========================================================================
# Test Class: Grid and Mask Filenames (Requirement 3.6)
# ===========================================================================


class TestGridMaskFilenames:
    """Tests that grid and mask filenames appear in &grid_nml."""

    def test_grid_file_rendered(self, ice_context):
        """Grid filename appears in &grid_nml."""
        ice_context["grid"] = "grid_cice_NEMS_mx025.nc"
        rendered = _render_ice_in(ice_context)
        assert "grid_file    = 'grid_cice_NEMS_mx025.nc'" in rendered

    def test_mask_file_rendered(self, ice_context):
        """Mask filename appears in &grid_nml as kmt_file."""
        ice_context["mask"] = "kmtu_cice_NEMS_mx025.nc"
        rendered = _render_ice_in(ice_context)
        assert "kmt_file     = 'kmtu_cice_NEMS_mx025.nc'" in rendered


# ===========================================================================
# Test Class: Namelist Format Validity (Requirement 3.5)
# ===========================================================================


class TestNamelistFormatValidity:
    """Tests that rendered output passes NamelistValidator."""

    def test_warm_start_true_valid_namelist(self, ice_context, namelist_validator):
        """Rendered ice_in with warm_start=True is a valid Fortran namelist."""
        ice_context["warm_start"] = True
        rendered = _render_ice_in(ice_context)
        errors = namelist_validator.validate(rendered, "ice_in")
        assert errors == [], f"Validation errors: {errors}"

    def test_warm_start_false_valid_namelist(self, ice_context, namelist_validator):
        """Rendered ice_in with warm_start=False is a valid Fortran namelist."""
        ice_context["warm_start"] = False
        rendered = _render_ice_in(ice_context)
        errors = namelist_validator.validate(rendered, "ice_in")
        assert errors == [], f"Validation errors: {errors}"


# ===========================================================================
# Test Class: All Namelist Groups Present (Requirement 3.6)
# ===========================================================================


class TestNamelistGroupsPresent:
    """Tests that all expected namelist groups are present in rendered output."""

    EXPECTED_GROUPS = [
        "&setup_nml",
        "&grid_nml",
        "&domain_nml",
        "&tracer_nml",
        "&thermo_nml",
        "&dynamics_nml",
        "&shortwave_nml",
        "&ponds_nml",
        "&snow_nml",
        "&forcing_nml",
        "&icefields_nml",
    ]

    def test_all_namelist_groups_present(self, ice_context):
        """All 11 expected namelist groups are present in rendered output."""
        rendered = _render_ice_in(ice_context)
        for group in self.EXPECTED_GROUPS:
            assert group in rendered, f"Missing namelist group: {group}"
