"""Unit tests for MOM6ParameterValidator.

Tests known-good MOM6 parameter file inputs, known-bad inputs
(missing `=`, invalid section headers, stray characters),
and edge cases (empty file, comment-only file, shell variables in values).

Traces to: Requirements 10.1, 10.5
"""

import os
import sys

import pytest

sys.path.insert(0, os.path.join(os.path.dirname(__file__), "..", "..", "workflow"))

from deployment.validators.mom6_parameter import MOM6ParameterValidator


@pytest.fixture
def validator():
    return MOM6ParameterValidator()


# ===========================================================================
# Valid MOM6 Content (No Errors Expected)
# ===========================================================================


class TestValidContent:
    """Tests for known-good MOM6 parameter file inputs."""

    def test_valid_mom6_content_no_errors(self, validator):
        """Valid MOM6 content with section headers, params, and empty lines produces no errors."""
        content = """\
! === module MOM ===
DT = 900
DT_THERM = 3600
THICKNESSDIFFUSE = True
THICKNESSDIFFUSE_FIRST = True
USE_REGRIDDING = True

! === module MOM_domains ===
NIGLOBAL = 1440
NJGLOBAL = 1080
NIHALO = 4
NJHALO = 4

! === module MOM_verticalGrid ===
NK = 75

! === module MOM_grid_init ===
GRID_CONFIG = "mosaic"
GRID_FILE = "ocean_mosaic.nc"
TOPO_CONFIG = "file"
TOPO_FILE = "ocean_topog.nc"
MAXIMUM_DEPTH = 6500.0
"""
        errors = validator.validate(content, "MOM_input")
        assert errors == []

    def test_shell_variables_in_values(self, validator):
        """Shell variables like ${TOPOEDITS} in values should be valid."""
        content = """\
! === module MOM_grid_init ===
TOPO_EDITS_FILE = "${TOPOEDITS}"
COORD_FILE = "${CHLCLIM}"
FRUNOFF = "${FRUNOFF_PATH}"
"""
        errors = validator.validate(content, "MOM_input")
        assert errors == []

    def test_numeric_values(self, validator):
        """Numeric values (int, float, scientific notation) should be valid."""
        content = """\
DT = 900
MAXIMUM_DEPTH = 6500.0
KD_MIN = 2.0E-6
KD = 0.0
SMAG_BI_CONST = 0.06
AH_VEL_SCALE = 0.01
"""
        errors = validator.validate(content, "MOM_input")
        assert errors == []

    def test_boolean_values(self, validator):
        """Boolean values (True, False) should be valid."""
        content = """\
THICKNESSDIFFUSE = True
DIABATIC_FIRST = True
USE_KPP = True
DO_SPPT = False
ODA_INCUPD = False
"""
        errors = validator.validate(content, "MOM_input")
        assert errors == []

    def test_string_values_with_quotes(self, validator):
        """Quoted string values should be valid."""
        content = """\
GRID_CONFIG = "mosaic"
EQN_OF_STATE = "WRIGHT"
DIAG_COORDS = "z Z ZSTAR"
COORD_CONFIG = "file"
"""
        errors = validator.validate(content, "MOM_input")
        assert errors == []

    def test_comment_only_file(self, validator):
        """File with only comments should produce no errors."""
        content = """\
! === module MOM ===
! This is a comment-only file
! No parameters defined yet
"""
        errors = validator.validate(content, "MOM_input")
        assert errors == []

    def test_empty_file(self, validator):
        """Empty file should produce no errors."""
        errors = validator.validate("", "MOM_input")
        assert errors == []

    def test_whitespace_only_file(self, validator):
        """File with only whitespace lines should produce no errors."""
        content = "   \n\n   \n\t\n"
        errors = validator.validate(content, "MOM_input")
        assert errors == []

    def test_params_with_extra_whitespace(self, validator):
        """Parameters with extra whitespace around = should be valid."""
        content = """\
DT   =   900
NIGLOBAL  =  1440
NK =75
"""
        errors = validator.validate(content, "MOM_input")
        assert errors == []


# ===========================================================================
# Invalid MOM6 Content (Errors Expected)
# ===========================================================================


class TestInvalidContent:
    """Tests for known-bad MOM6 parameter file inputs."""

    def test_missing_equals_sign(self, validator):
        """Lines with parameter name but no = should produce error."""
        content = """\
! === module MOM_domains ===
NIGLOBAL 1440
NJGLOBAL = 1080
"""
        errors = validator.validate(content, "MOM_input")
        assert len(errors) == 1
        assert "MOM_input:2" in errors[0]
        assert "NIGLOBAL 1440" in errors[0]

    def test_invalid_comment_style(self, validator):
        """Comments using # instead of ! should produce error."""
        content = """\
# wrong comment style
DT = 900
"""
        errors = validator.validate(content, "MOM_input")
        assert len(errors) == 1
        assert "MOM_input:1" in errors[0]
        assert "# wrong comment style" in errors[0]

    def test_stray_characters_ampersand(self, validator):
        """Fortran namelist syntax (&group_name) should produce error."""
        content = """\
&group_name
DT = 900
/
"""
        errors = validator.validate(content, "MOM_input")
        assert len(errors) == 2
        assert "MOM_input:1" in errors[0]
        assert "&group_name" in errors[0]
        assert "MOM_input:3" in errors[1]

    def test_lowercase_parameter_name(self, validator):
        """Lowercase parameter names should produce error."""
        content = """\
! === module MOM ===
dt = 900
DT_THERM = 3600
"""
        errors = validator.validate(content, "MOM_input")
        assert len(errors) == 1
        assert "MOM_input:2" in errors[0]
        assert "dt = 900" in errors[0]

    def test_mixed_case_parameter_name(self, validator):
        """Mixed-case parameter names should produce error."""
        content = """\
DtOcean = 900
"""
        errors = validator.validate(content, "MOM_input")
        assert len(errors) == 1
        assert "MOM_input:1" in errors[0]

    def test_stray_text_no_assignment(self, validator):
        """Random text without assignment should produce error."""
        content = """\
! === module MOM ===
DT = 900
this is just random text
NIGLOBAL = 1440
"""
        errors = validator.validate(content, "MOM_input")
        assert len(errors) == 1
        assert "MOM_input:3" in errors[0]
        assert "this is just random text" in errors[0]

    def test_multiple_invalid_lines(self, validator):
        """Multiple invalid lines should each produce an error with correct line numbers."""
        content = """\
! === module MOM ===
DT = 900
bad line one
NIGLOBAL = 1440
another bad line
NK = 75
"""
        errors = validator.validate(content, "MOM_input")
        assert len(errors) == 2
        assert "MOM_input:3" in errors[0]
        assert "bad line one" in errors[0]
        assert "MOM_input:5" in errors[1]
        assert "another bad line" in errors[1]

    def test_mixed_valid_and_invalid_correct_line_numbers(self, validator):
        """Errors should report correct line numbers in mixed content."""
        content = """\
! === module MOM ===
DT = 900
DT_THERM = 3600

! === module MOM_domains ===
NIGLOBAL 1440
NJGLOBAL = 1080

! === module MOM_verticalGrid ===
&bad_section
NK = 75
"""
        errors = validator.validate(content, "MOM_input")
        assert len(errors) == 2
        assert "MOM_input:6" in errors[0]
        assert "NIGLOBAL 1440" in errors[0]
        assert "MOM_input:10" in errors[1]
        assert "&bad_section" in errors[1]


# ===========================================================================
# Error Message Format
# ===========================================================================


class TestErrorFormat:
    """Tests for error message format."""

    def test_error_includes_filepath(self, validator):
        """Error messages should include the filepath."""
        content = "bad line"
        errors = validator.validate(content, "/path/to/MOM_input")
        assert len(errors) == 1
        assert "/path/to/MOM_input" in errors[0]

    def test_error_includes_line_number(self, validator):
        """Error messages should include the line number."""
        content = """\
DT = 900
bad line
"""
        errors = validator.validate(content, "MOM_input")
        assert "MOM_input:2" in errors[0]

    def test_error_includes_content(self, validator):
        """Error messages should include the offending line content."""
        content = "some invalid content here"
        errors = validator.validate(content, "MOM_input")
        assert "some invalid content here" in errors[0]

    def test_error_format_matches_spec(self, validator):
        """Error format should match: 'MOM6 parameter format error at <filepath>:<lineno>: <content>'."""
        content = "bad line"
        errors = validator.validate(content, "MOM_input")
        assert errors[0] == "MOM6 parameter format error at MOM_input:1: bad line"
