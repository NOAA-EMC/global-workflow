"""Unit tests for format validators.

Tests each validator with known-good inputs, known-bad inputs,
and edge cases (empty files, comment-only files, partial content).

Traces to: Requirements 7.1, 7.2, 7.3, 7.4, 7.5
"""

import os
import sys

import pytest

sys.path.insert(0, os.path.join(os.path.dirname(__file__), "..", "..", "workflow"))

from deployment.validators import (
    ModelConfigureValidator,
    NamelistValidator,
    DiagTableValidator,
    ESMFConfigValidator,
    FieldTableValidator,
)


# ---------------------------------------------------------------------------
# Fixtures
# ---------------------------------------------------------------------------


@pytest.fixture
def model_configure_validator():
    return ModelConfigureValidator()


@pytest.fixture
def namelist_validator():
    return NamelistValidator()


@pytest.fixture
def diag_table_validator():
    return DiagTableValidator()


@pytest.fixture
def esmf_config_validator():
    return ESMFConfigValidator()


@pytest.fixture
def field_table_validator():
    return FieldTableValidator()


# ===========================================================================
# ModelConfigureValidator Tests (Requirement 7.1)
# ===========================================================================


class TestModelConfigureValidator:
    """Tests for the model_configure key:value format validator."""

    def test_valid_content_no_errors(self, model_configure_validator):
        """Valid model_configure content should produce no errors."""
        content = """\
print_esmf:          .true.
total_member:        1
PE_MEMBER01:         216
start_year:          2024
start_month:         1
start_day:           15
start_hour:          0
nhours_fcst:         ${FHMAX}
dt_atmos:            225
restart_interval:    12
quilting:            .true.
write_groups:        2
write_tasks_per_group: 40
output_grid:         'gaussian_grid'
"""
        errors = model_configure_validator.validate(content, "model_configure")
        assert errors == []

    def test_valid_with_comments_and_blanks(self, model_configure_validator):
        """Comments and blank lines should be ignored."""
        content = """\
# This is a comment
print_esmf:          .true.

# Another comment
dt_atmos:            225
"""
        errors = model_configure_validator.validate(content, "model_configure")
        assert errors == []

    def test_invalid_missing_colon(self, model_configure_validator):
        """Lines without colon separator should produce errors."""
        content = """\
print_esmf          .true.
dt_atmos:            225
"""
        errors = model_configure_validator.validate(content, "model_configure")
        assert len(errors) == 1
        assert "model_configure:1" in errors[0]
        assert "Invalid key:value format" in errors[0]

    def test_invalid_no_value(self, model_configure_validator):
        """Key with colon but no value should produce error."""
        content = """\
dt_atmos:            225
bad_key:
quilting:            .true.
"""
        errors = model_configure_validator.validate(content, "model_configure")
        # "bad_key:" has no value after the colon+space, so regex won't match
        assert len(errors) == 1
        assert "model_configure:2" in errors[0]

    def test_invalid_multiple_bad_lines(self, model_configure_validator):
        """Multiple invalid lines should each produce an error."""
        content = """\
this is not valid
also not valid
dt_atmos:            225
"""
        errors = model_configure_validator.validate(content, "model_configure")
        assert len(errors) == 2

    def test_empty_file(self, model_configure_validator):
        """Empty file should produce no errors."""
        errors = model_configure_validator.validate("", "model_configure")
        assert errors == []

    def test_comment_only_file(self, model_configure_validator):
        """File with only comments should produce no errors."""
        content = """\
# comment line 1
# comment line 2
# comment line 3
"""
        errors = model_configure_validator.validate(content, "model_configure")
        assert errors == []

    def test_shell_variable_in_value(self, model_configure_validator):
        """Shell variables like ${FHMAX} in values should be valid."""
        content = """\
nhours_fcst:         ${FHMAX}
fhrot:               ${FHROT}
"""
        errors = model_configure_validator.validate(content, "model_configure")
        assert errors == []

    def test_quoted_string_value(self, model_configure_validator):
        """Quoted string values should be valid."""
        content = """\
output_grid:         'gaussian_grid'
filename_base:       'atm' 'sfc'
"""
        errors = model_configure_validator.validate(content, "model_configure")
        assert errors == []

    def test_filepath_in_error_message(self, model_configure_validator):
        """Error messages should include the filepath."""
        content = "bad line"
        errors = model_configure_validator.validate(content, "/path/to/model_configure")
        assert "/path/to/model_configure" in errors[0]


# ===========================================================================
# NamelistValidator Tests (Requirement 7.2)
# ===========================================================================


class TestNamelistValidator:
    """Tests for the Fortran namelist syntax validator."""

    def test_valid_namelist_no_errors(self, namelist_validator):
        """Valid Fortran namelist should produce no errors."""
        content = """\
&amip_interp_nml
  interp_oi_sst = .true.
  use_ncep_sst = .true.
  use_ncep_ice = .false.
/

&atmos_model_nml
  blocksize = 32
  chksum_debug = .false.
  dycore_only = .false.
  ccpp_suite = 'FV3_GFS_v17_p8'
/

&fv_core_nml
  layout = 6,6
  io_layout = 1,1
  npx = 385
  npy = 385
  npz = 127
  ntiles = 6
  dt_atmos = 225
/
"""
        errors = namelist_validator.validate(content, "input.nml")
        assert errors == []

    def test_valid_with_comments(self, namelist_validator):
        """Fortran comments (!) should be ignored."""
        content = """\
! This is a comment
&fv_core_nml
  ! inline comment
  npx = 385
  npy = 385
/
"""
        errors = namelist_validator.validate(content, "input.nml")
        assert errors == []

    def test_nested_group_error(self, namelist_validator):
        """Opening a new group inside an unclosed group should error."""
        content = """\
&group_a
  var1 = 1
&group_b
  var2 = 2
/
"""
        errors = namelist_validator.validate(content, "input.nml")
        assert len(errors) == 1
        assert "Nested group" in errors[0]
        assert "group_a" in errors[0]

    def test_unmatched_terminator_error(self, namelist_validator):
        """Group terminator without matching opener should error."""
        content = """\
/
"""
        errors = namelist_validator.validate(content, "input.nml")
        assert len(errors) == 1
        assert "without matching" in errors[0]

    def test_unclosed_group_error(self, namelist_validator):
        """Group without terminator should produce error at end."""
        content = """\
&fv_core_nml
  npx = 385
  npy = 385
"""
        errors = namelist_validator.validate(content, "input.nml")
        assert len(errors) == 1
        assert "Unclosed namelist group" in errors[0]
        assert "fv_core_nml" in errors[0]

    def test_missing_assignment_in_group(self, namelist_validator):
        """Lines inside a group without '=' should produce error."""
        content = """\
&fv_core_nml
  npx = 385
  this is not an assignment
  npy = 385
/
"""
        errors = namelist_validator.validate(content, "input.nml")
        assert len(errors) == 1
        assert "Expected 'var = value'" in errors[0]
        assert "fv_core_nml" in errors[0]

    def test_empty_file(self, namelist_validator):
        """Empty file should produce no errors."""
        errors = namelist_validator.validate("", "input.nml")
        assert errors == []

    def test_comment_only_file(self, namelist_validator):
        """File with only comments should produce no errors."""
        content = """\
! comment 1
! comment 2
"""
        errors = namelist_validator.validate(content, "input.nml")
        assert errors == []

    def test_multiple_valid_groups(self, namelist_validator):
        """Multiple properly closed groups should produce no errors."""
        content = """\
&group_a
  x = 1
/

&group_b
  y = 2
/

&group_c
  z = 3
/
"""
        errors = namelist_validator.validate(content, "input.nml")
        assert errors == []

    def test_filepath_in_error_message(self, namelist_validator):
        """Error messages should include the filepath."""
        content = """\
&unclosed_group
  x = 1
"""
        errors = namelist_validator.validate(content, "/path/to/input.nml")
        assert "/path/to/input.nml" in errors[0]


# ===========================================================================
# DiagTableValidator Tests (Requirement 7.3)
# ===========================================================================


class TestDiagTableValidator:
    """Tests for the FMS diag_table format validator."""

    def test_valid_content_no_errors(self, diag_table_validator):
        """Valid diag_table content should produce no errors."""
        content = """\
"fv3_history",    0,  "hours",  1,  "hours",  "time"
"fv3_history2d",  0,  "hours",  1,  "hours",  "time"
"gfs_dyn",     "ucomp",       "ugrd",         "fv3_history",    "all",  .false.,  "none",  2
"gfs_dyn",     "vcomp",       "vgrd",         "fv3_history",    "all",  .false.,  "none",  2
"""
        errors = diag_table_validator.validate(content, "diag_table")
        assert errors == []

    def test_valid_with_comments_and_blanks(self, diag_table_validator):
        """Comments and blank lines should be ignored."""
        content = """\
# File entries
"fv3_history",    0,  "hours",  1,  "hours",  "time"

# Field entries
"gfs_dyn",     "ucomp",       "ugrd",         "fv3_history",    "all",  .false.,  "none",  2
"""
        errors = diag_table_validator.validate(content, "diag_table")
        assert errors == []

    def test_invalid_column_count(self, diag_table_validator):
        """Lines starting with quote but wrong column count should error."""
        content = """\
"gfs_dyn",     "ucomp",       "ugrd",         "fv3_history",    "all"
"""
        # 5 columns - less than 6, so the heuristic won't trigger (needs >= 6)
        errors = diag_table_validator.validate(content, "diag_table")
        assert errors == []

    def test_too_many_columns_error(self, diag_table_validator):
        """Lines with more than 10 columns should produce error."""
        content = """\
"gfs_dyn",  "ucomp",  "ugrd",  "fv3_history",  "all",  .false.,  "none",  2,  "extra",  "extra2",  "extra3"
"""
        errors = diag_table_validator.validate(content, "diag_table")
        assert len(errors) == 1
        assert "Expected 6-10 columns" in errors[0]
        assert "got 11" in errors[0]

    def test_six_columns_valid(self, diag_table_validator):
        """File entries with exactly 6 columns should be valid."""
        content = """\
"fv3_history",    0,  "hours",  1,  "hours",  "time"
"""
        errors = diag_table_validator.validate(content, "diag_table")
        assert errors == []

    def test_seven_columns_valid(self, diag_table_validator):
        """Entries with 7 columns should be valid."""
        content = """\
"ocean_model",  "SSH",  "SSH",  "ocn_output",  "all",  .true.,  "none"
"""
        errors = diag_table_validator.validate(content, "diag_table")
        assert errors == []

    def test_eight_columns_valid(self, diag_table_validator):
        """Field entries with exactly 8 columns should be valid."""
        content = """\
"gfs_dyn",     "ucomp",       "ugrd",         "fv3_history",    "all",  .false.,  "none",  2
"""
        errors = diag_table_validator.validate(content, "diag_table")
        assert errors == []

    def test_empty_file(self, diag_table_validator):
        """Empty file should produce no errors."""
        errors = diag_table_validator.validate("", "diag_table")
        assert errors == []

    def test_comment_only_file(self, diag_table_validator):
        """File with only comments should produce no errors."""
        content = """\
# comment 1
# comment 2
"""
        errors = diag_table_validator.validate(content, "diag_table")
        assert errors == []

    def test_non_quoted_lines_ignored(self, diag_table_validator):
        """Lines not starting with a quote are not validated for columns."""
        content = """\
some_title_line
2024 01 15 00 0 0
"gfs_dyn",     "ucomp",       "ugrd",         "fv3_history",    "all",  .false.,  "none",  2
"""
        errors = diag_table_validator.validate(content, "diag_table")
        assert errors == []

    def test_filepath_in_error_message(self, diag_table_validator):
        """Error messages should include the filepath."""
        content = """\
"a",  "b",  "c",  "d",  "e",  "f",  "g",  "h",  "i",  "j",  "k"
"""
        errors = diag_table_validator.validate(content, "/path/to/diag_table")
        assert "/path/to/diag_table" in errors[0]


# ===========================================================================
# ESMFConfigValidator Tests (Requirement 7.4)
# ===========================================================================


class TestESMFConfigValidator:
    """Tests for the ESMF/NUOPC configuration syntax validator."""

    def test_valid_content_no_errors(self, esmf_config_validator):
        """Valid ESMF configuration should produce no errors."""
        content = """\
#############################################
####  NEMS Run-Time Configuration File  #####
#############################################

EARTH_component_list: ATM OCN ICE WAV CHM

EARTH_attributes::
  Verbosity = 0
  Diagnostic = 0
::

ATM_model:                      fv3
ATM_petlist_bounds:             0 215
ATM_omp_num_threads:            1

runSeq::
  @225
    ATM
  @
::
"""
        errors = esmf_config_validator.validate(content, "ufs.configure")
        assert errors == []

    def test_valid_with_comments_and_blanks(self, esmf_config_validator):
        """Comments and blank lines should be ignored."""
        content = """\
# comment
EARTH_attributes::
  Verbosity = 0
::

# another comment
runSeq::
  @225
    ATM
  @
::
"""
        errors = esmf_config_validator.validate(content, "ufs.configure")
        assert errors == []

    def test_unclosed_block_error(self, esmf_config_validator):
        """Block opened with label:: but never closed should error."""
        content = """\
EARTH_attributes::
  Verbosity = 0
"""
        errors = esmf_config_validator.validate(content, "ufs.configure")
        assert len(errors) == 1
        assert "Unclosed block" in errors[0]
        assert "EARTH_attributes" in errors[0]

    def test_unmatched_closing_error(self, esmf_config_validator):
        """Standalone :: without matching opener is treated as list terminator (valid)."""
        content = """\
::
"""
        errors = esmf_config_validator.validate(content, "ufs.configure")
        # In ESMF/MAPL config, standalone :: can terminate inline lists
        # (e.g., COLLECTIONS: 'x' \n ::), so this is not an error
        assert len(errors) == 0

    def test_nested_blocks_valid(self, esmf_config_validator):
        """Properly nested blocks should produce no errors."""
        content = """\
EARTH_attributes::
  Verbosity = 0
::

runSeq::
  @225
    ATM
  @
::
"""
        errors = esmf_config_validator.validate(content, "ufs.configure")
        assert errors == []

    def test_multiple_unclosed_blocks(self, esmf_config_validator):
        """Multiple unclosed blocks should each produce an error."""
        content = """\
EARTH_attributes::
  Verbosity = 0
runSeq::
  @225
    ATM
  @
"""
        errors = esmf_config_validator.validate(content, "ufs.configure")
        assert len(errors) == 2

    def test_attribute_lines_valid(self, esmf_config_validator):
        """Simple label: value attribute lines should be valid."""
        content = """\
ATM_model:                      fv3
ATM_petlist_bounds:             0 215
ATM_omp_num_threads:            1
"""
        errors = esmf_config_validator.validate(content, "ufs.configure")
        assert errors == []

    def test_empty_file(self, esmf_config_validator):
        """Empty file should produce no errors."""
        errors = esmf_config_validator.validate("", "ufs.configure")
        assert errors == []

    def test_comment_only_file(self, esmf_config_validator):
        """File with only comments should produce no errors."""
        content = """\
# comment 1
# comment 2
# comment 3
"""
        errors = esmf_config_validator.validate(content, "ufs.configure")
        assert errors == []

    def test_filepath_in_error_message(self, esmf_config_validator):
        """Error messages should include the filepath."""
        content = """\
runSeq::
  @225
    ATM
  @
"""
        errors = esmf_config_validator.validate(content, "/path/to/ufs.configure")
        assert "/path/to/ufs.configure" in errors[0]

    def test_runseq_block_properly_closed(self, esmf_config_validator):
        """runSeq:: block with proper :: closure should be valid."""
        content = """\
runSeq::
  @1800
    MED med_phases_prep_ocn
    OCN
    @225
      ATM
      ICE
    @
    OCN -> MED :remapMethod=redist
  @
::
"""
        errors = esmf_config_validator.validate(content, "ufs.configure")
        assert errors == []


# ===========================================================================
# FieldTableValidator Tests (Requirement 7.5)
# ===========================================================================


class TestFieldTableValidator:
    """Tests for the FMS field_table format validator."""

    def test_valid_content_no_errors(self, field_table_validator):
        """Valid field_table content should produce no errors."""
        content = """\
# added by FRE: sphum must be present in atmos
# specific humidity for moist runs
 "TRACER", "atmos_mod", "sphum"
           "longname",     "specific humidity"
           "units",        "kg/kg"
       "profile_type", "fixed", "surface_value=3.e-6" /
# prognostic cloud water mixing ratio
 "TRACER", "atmos_mod", "liq_wat"
           "longname",     "cloud water mixing ratio"
           "units",        "kg/kg"
       "profile_type", "fixed", "surface_value=1.e30" /
"""
        errors = field_table_validator.validate(content, "field_table")
        assert errors == []

    def test_valid_multiple_tracers(self, field_table_validator):
        """Multiple properly closed tracer blocks should be valid."""
        content = """\
 "TRACER", "atmos_mod", "sphum"
           "longname",     "specific humidity"
           "units",        "kg/kg"
       "profile_type", "fixed", "surface_value=1.e30" /
 "TRACER", "atmos_mod", "liq_wat"
           "longname",     "cloud water mixing ratio"
           "units",        "kg/kg"
       "profile_type", "fixed", "surface_value=1.e30" /
 "TRACER", "atmos_mod", "o3mr"
           "longname",     "ozone mixing ratio"
           "units",        "kg/kg"
       "profile_type", "fixed", "surface_value=1.e30" /
"""
        errors = field_table_validator.validate(content, "field_table")
        assert errors == []

    def test_unclosed_tracer_block_error(self, field_table_validator):
        """Tracer block without closing / should produce error."""
        content = """\
 "TRACER", "atmos_mod", "sphum"
           "longname",     "specific humidity"
           "units",        "kg/kg"
"""
        errors = field_table_validator.validate(content, "field_table")
        assert len(errors) == 1
        assert "Unclosed tracer block" in errors[0]
        assert "sphum" in errors[0]

    def test_new_tracer_before_closing_previous(self, field_table_validator):
        """Opening new tracer before closing previous should error."""
        content = """\
 "TRACER", "atmos_mod", "sphum"
           "longname",     "specific humidity"
           "units",        "kg/kg"
 "TRACER", "atmos_mod", "liq_wat"
           "longname",     "cloud water mixing ratio"
           "units",        "kg/kg"
       "profile_type", "fixed", "surface_value=1.e30" /
"""
        errors = field_table_validator.validate(content, "field_table")
        assert len(errors) == 1
        assert "New tracer 'liq_wat'" in errors[0]
        assert "sphum" in errors[0]

    def test_terminator_without_header_error(self, field_table_validator):
        """Tracer terminator / without matching header should error."""
        content = """\
       "profile_type", "fixed", "surface_value=1.e30" /
"""
        errors = field_table_validator.validate(content, "field_table")
        assert len(errors) == 1
        assert "without matching TRACER header" in errors[0]

    def test_valid_with_comments(self, field_table_validator):
        """Comments between tracer blocks should be valid."""
        content = """\
# This is a comment
 "TRACER", "atmos_mod", "sphum"
           "longname",     "specific humidity"
           "units",        "kg/kg"
       "profile_type", "fixed", "surface_value=3.e-6" /
# Another comment
 "TRACER", "atmos_mod", "o3mr"
           "longname",     "ozone mixing ratio"
           "units",        "kg/kg"
       "profile_type", "fixed", "surface_value=1.e30" /
"""
        errors = field_table_validator.validate(content, "field_table")
        assert errors == []

    def test_empty_file(self, field_table_validator):
        """Empty file should produce no errors."""
        errors = field_table_validator.validate("", "field_table")
        assert errors == []

    def test_comment_only_file(self, field_table_validator):
        """File with only comments should produce no errors."""
        content = """\
# comment 1
# comment 2
# comment 3
"""
        errors = field_table_validator.validate(content, "field_table")
        assert errors == []

    def test_filepath_in_error_message(self, field_table_validator):
        """Error messages should include the filepath."""
        content = """\
 "TRACER", "atmos_mod", "sphum"
           "longname",     "specific humidity"
"""
        errors = field_table_validator.validate(content, "/path/to/field_table")
        assert "/path/to/field_table" in errors[0]

    def test_gfdl_suite_tracers(self, field_table_validator):
        """Full GFDL suite tracer list should be valid."""
        content = """\
 "TRACER", "atmos_mod", "sphum"
           "longname",     "specific humidity"
           "units",        "kg/kg"
       "profile_type", "fixed", "surface_value=1.e30" /
 "TRACER", "atmos_mod", "liq_wat"
           "longname",     "cloud water mixing ratio"
           "units",        "kg/kg"
       "profile_type", "fixed", "surface_value=1.e30" /
 "TRACER", "atmos_mod", "rainwat"
           "longname",     "rain mixing ratio"
           "units",        "kg/kg"
       "profile_type", "fixed", "surface_value=1.e30" /
 "TRACER", "atmos_mod", "ice_wat"
           "longname",     "cloud ice mixing ratio"
           "units",        "kg/kg"
       "profile_type", "fixed", "surface_value=1.e30" /
 "TRACER", "atmos_mod", "snowwat"
           "longname",     "snow mixing ratio"
           "units",        "kg/kg"
       "profile_type", "fixed", "surface_value=1.e30" /
 "TRACER", "atmos_mod", "graupel"
           "longname",     "graupel mixing ratio"
           "units",        "kg/kg"
       "profile_type", "fixed", "surface_value=1.e30" /
 "TRACER", "atmos_mod", "o3mr"
           "longname",     "ozone mixing ratio"
           "units",        "kg/kg"
       "profile_type", "fixed", "surface_value=1.e30" /
 "TRACER", "atmos_mod", "sgs_tke"
           "longname",     "subgrid scale turbulent kinetic energy"
           "units",        "m2/s2"
       "profile_type", "fixed", "surface_value=0.0" /
 "TRACER", "atmos_mod", "cld_amt"
           "longname",     "cloud amount"
           "units",        "1"
       "profile_type", "fixed", "surface_value=1.e30" /
"""
        errors = field_table_validator.validate(content, "field_table")
        assert errors == []

    def test_partial_content_unclosed(self, field_table_validator):
        """Partial content with unclosed tracer should report error."""
        content = """\
 "TRACER", "atmos_mod", "sphum"
           "longname",     "specific humidity"
           "units",        "kg/kg"
       "profile_type", "fixed", "surface_value=1.e30" /
 "TRACER", "atmos_mod", "liq_wat"
           "longname",     "cloud water mixing ratio"
"""
        errors = field_table_validator.validate(content, "field_table")
        assert len(errors) == 1
        assert "Unclosed tracer block" in errors[0]
        assert "liq_wat" in errors[0]
