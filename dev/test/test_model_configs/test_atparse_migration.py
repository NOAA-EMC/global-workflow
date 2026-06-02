"""Unit tests for atparse-to-Jinja2 migration utility.

Tests the conversion of legacy `@[VAR_NAME]` atparse syntax to Jinja2
`{{ expr }}` syntax, shell variable preservation, and unknown variable
handling.

Traces to: Requirements 8.1, 8.2, 8.3, 8.4, 8.5
"""

import os
import sys
import warnings

import pytest

sys.path.insert(0, os.path.join(os.path.dirname(__file__), "..", "..", "workflow"))

from deployment.atparse_migration import (
    atparse_to_jinja2,
    get_mapping_for_file,
    list_atparse_variables,
    validate_no_atparse_remaining,
    MigrationResult,
    DEFAULT_VAR_MAPPING,
    MODEL_CONFIGURE_MAPPING,
    UFS_CONFIGURE_MAPPING,
    DIAG_TABLE_MAPPING,
    AERO_HISTORY_MAPPING,
    _ATPARSE_PATTERN,
    _SHELL_VAR_PATTERN,
)


# ---------------------------------------------------------------------------
# Fixtures
# ---------------------------------------------------------------------------


@pytest.fixture
def simple_mapping():
    """A minimal mapping for testing."""
    return {
        "DT_ATMOS": "model.dt_atmos",
        "TOTAL_TASKS": "model.fv3.total_tasks",
        "QUILTING": "model.fv3.quilting | fortran_logical",
    }


@pytest.fixture
def model_configure_content():
    """Sample model_configure content with atparse patterns."""
    return """\
print_esmf:          .true.
total_member:        1
PE_MEMBER01:         @[TOTAL_TASKS]
start_year:          @[SYEAR]
start_month:         @[SMONTH]
start_day:           @[SDAY]
start_hour:          @[SHOUR]
nhours_fcst:         ${FHMAX}
dt_atmos:            @[DT_ATMOS]
restart_interval:    @[RESTART_INTERVAL]
quilting:            @[QUILTING]
write_groups:        @[WRITE_GROUP]
write_tasks_per_group: @[WRTTASK_PER_GROUP]
output_grid:         '@[OUTPUT_GRID]'
imo:                 @[IMO]
jmo:                 @[JMO]
"""


@pytest.fixture
def diag_table_content():
    """Sample diag_table content with atparse patterns and shell vars."""
    return """\
"fv3_history",    0,  "hours",  1,  "hours",  "time"
"@[MOM6_OUTPUT_DIR]/ocn%4yr%2mo%2dy%2hr%2mi", @[FHOUT_OCN],  "hours",  1,  "hours",  "time",  @[FHOUT_OCN],  "hours",  "@[SYEAR] @[SMONTH] @[SDAY] @[CHOUR] 0 0"
"ocean_model", "SSH", "SSH", "@[MOM6_OUTPUT_DIR]/ocn%4yr%2mo%2dy%2hr%2mi", "all", .true., "none", 2
"""


@pytest.fixture
def aero_history_content():
    """Sample AERO_HISTORY.rc content with atparse patterns."""
    return """\
  inst_aod.frequency:  @[inst_aod_freq],
  inst_du_ss.frequency:   @[inst_du_ss_freq] ,
  tavg_2d_rad.frequency:  @[tavg_2d_rad_freq],
"""


# ---------------------------------------------------------------------------
# Tests: Basic conversion
# ---------------------------------------------------------------------------


class TestBasicConversion:
    """Tests for basic @[VAR] to {{ expr }} conversion."""

    def test_single_variable_converted(self, simple_mapping):
        """A single @[VAR] should be converted to {{ expr }}."""
        content = "dt_atmos: @[DT_ATMOS]"
        result = atparse_to_jinja2(content, simple_mapping)
        assert result.content == "dt_atmos: {{ model.dt_atmos }}"
        assert result.converted == ["DT_ATMOS"]
        assert result.unknown == []

    def test_multiple_variables_converted(self, simple_mapping):
        """Multiple @[VAR] patterns should all be converted."""
        content = "tasks: @[TOTAL_TASKS]\ndt: @[DT_ATMOS]"
        result = atparse_to_jinja2(content, simple_mapping)
        assert "{{ model.fv3.total_tasks }}" in result.content
        assert "{{ model.dt_atmos }}" in result.content
        assert set(result.converted) == {"TOTAL_TASKS", "DT_ATMOS"}

    def test_variable_with_filter_converted(self, simple_mapping):
        """Variables mapped to expressions with filters should work."""
        content = "quilting: @[QUILTING]"
        result = atparse_to_jinja2(content, simple_mapping)
        assert result.content == "quilting: {{ model.fv3.quilting | fortran_logical }}"

    def test_same_variable_multiple_occurrences(self, simple_mapping):
        """Same variable appearing multiple times should be converted each time."""
        content = "@[DT_ATMOS] and @[DT_ATMOS]"
        result = atparse_to_jinja2(content, simple_mapping)
        assert result.content == "{{ model.dt_atmos }} and {{ model.dt_atmos }}"
        assert result.converted == ["DT_ATMOS", "DT_ATMOS"]

    def test_empty_content_returns_empty(self, simple_mapping):
        """Empty content should return empty result."""
        result = atparse_to_jinja2("", simple_mapping)
        assert result.content == ""
        assert result.converted == []
        assert result.unknown == []

    def test_no_atparse_patterns_unchanged(self, simple_mapping):
        """Content without @[VAR] patterns should pass through unchanged."""
        content = "print_esmf: .true.\ntotal_member: 1"
        result = atparse_to_jinja2(content, simple_mapping)
        assert result.content == content
        assert result.converted == []
        assert result.unknown == []

    def test_returns_migration_result_namedtuple(self, simple_mapping):
        """Result should be a MigrationResult named tuple."""
        result = atparse_to_jinja2("@[DT_ATMOS]", simple_mapping)
        assert isinstance(result, MigrationResult)
        assert hasattr(result, "content")
        assert hasattr(result, "converted")
        assert hasattr(result, "unknown")


# ---------------------------------------------------------------------------
# Tests: Shell variable preservation
# ---------------------------------------------------------------------------


class TestShellVariablePreservation:
    """Tests that ${VAR} shell variables are preserved verbatim."""

    def test_shell_var_braces_preserved(self, simple_mapping):
        """${VAR} shell variables should not be modified."""
        content = "nhours_fcst: ${FHMAX}\ndt: @[DT_ATMOS]"
        result = atparse_to_jinja2(content, simple_mapping)
        assert "${FHMAX}" in result.content
        assert "{{ model.dt_atmos }}" in result.content

    def test_multiple_shell_vars_preserved(self, simple_mapping):
        """Multiple shell variables should all be preserved."""
        content = "base_dtg: ${PDY}${cyc}\ndt: @[DT_ATMOS]"
        result = atparse_to_jinja2(content, simple_mapping)
        assert "${PDY}" in result.content
        assert "${cyc}" in result.content

    def test_shell_var_not_in_converted_list(self, simple_mapping):
        """Shell variables should not appear in converted or unknown lists."""
        content = "${FHMAX} @[DT_ATMOS]"
        result = atparse_to_jinja2(content, simple_mapping)
        assert "FHMAX" not in result.converted
        assert "FHMAX" not in result.unknown

    def test_shell_var_adjacent_to_atparse(self, simple_mapping):
        """Shell vars adjacent to atparse patterns should both be handled."""
        content = "stop_n = ${FHMAX}\ndt = @[DT_ATMOS]"
        result = atparse_to_jinja2(content, simple_mapping)
        assert "${FHMAX}" in result.content
        assert "{{ model.dt_atmos }}" in result.content

    def test_shell_var_inside_quotes_preserved(self, simple_mapping):
        """Shell variables inside quotes should be preserved."""
        content = 'base_dtg = "${PDY}${cyc}"'
        result = atparse_to_jinja2(content, simple_mapping)
        assert '${PDY}' in result.content
        assert '${cyc}' in result.content


# ---------------------------------------------------------------------------
# Tests: Unknown variable handling
# ---------------------------------------------------------------------------


class TestUnknownVariables:
    """Tests for handling @[VAR] patterns not in the mapping."""

    def test_unknown_variable_left_unchanged(self, simple_mapping):
        """Unknown @[VAR] should be left as-is in the output."""
        content = "value: @[UNKNOWN_VAR]"
        with warnings.catch_warnings():
            warnings.simplefilter("ignore")
            result = atparse_to_jinja2(content, simple_mapping)
        assert result.content == "value: @[UNKNOWN_VAR]"

    def test_unknown_variable_in_unknown_list(self, simple_mapping):
        """Unknown variables should appear in the unknown list."""
        content = "@[UNKNOWN_VAR]"
        with warnings.catch_warnings():
            warnings.simplefilter("ignore")
            result = atparse_to_jinja2(content, simple_mapping)
        assert "UNKNOWN_VAR" in result.unknown

    def test_unknown_variable_emits_warning(self, simple_mapping):
        """Unknown variables should emit a warning."""
        content = "@[UNKNOWN_VAR]"
        with warnings.catch_warnings(record=True) as w:
            warnings.simplefilter("always")
            atparse_to_jinja2(content, simple_mapping)
        assert len(w) == 1
        assert "UNKNOWN_VAR" in str(w[0].message)

    def test_mix_of_known_and_unknown(self, simple_mapping):
        """Mix of known and unknown variables should be handled correctly."""
        content = "@[DT_ATMOS] @[UNKNOWN_VAR]"
        with warnings.catch_warnings():
            warnings.simplefilter("ignore")
            result = atparse_to_jinja2(content, simple_mapping)
        assert "{{ model.dt_atmos }}" in result.content
        assert "@[UNKNOWN_VAR]" in result.content
        assert "DT_ATMOS" in result.converted
        assert "UNKNOWN_VAR" in result.unknown


# ---------------------------------------------------------------------------
# Tests: Default mapping coverage
# ---------------------------------------------------------------------------


class TestDefaultMapping:
    """Tests that the default mapping covers expected variables."""

    def test_default_mapping_is_nonempty(self):
        """Default mapping should contain entries."""
        assert len(DEFAULT_VAR_MAPPING) > 0

    def test_model_configure_vars_in_default(self):
        """model_configure variables should be in the default mapping."""
        for key in MODEL_CONFIGURE_MAPPING:
            assert key in DEFAULT_VAR_MAPPING

    def test_ufs_configure_vars_in_default(self):
        """ufs.configure variables should be in the default mapping."""
        for key in UFS_CONFIGURE_MAPPING:
            assert key in DEFAULT_VAR_MAPPING

    def test_diag_table_vars_in_default(self):
        """diag_table variables should be in the default mapping."""
        for key in DIAG_TABLE_MAPPING:
            assert key in DEFAULT_VAR_MAPPING

    def test_aero_history_vars_in_default(self):
        """AERO_HISTORY.rc variables should be in the default mapping."""
        for key in AERO_HISTORY_MAPPING:
            assert key in DEFAULT_VAR_MAPPING

    def test_uses_default_mapping_when_none(self):
        """When var_mapping is None, should use DEFAULT_VAR_MAPPING."""
        content = "@[DT_ATMOS]"
        result = atparse_to_jinja2(content, None)
        assert "{{ model.dt_atmos }}" in result.content

    def test_model_configure_has_key_variables(self):
        """model_configure mapping should have essential variables."""
        essential = [
            "TOTAL_TASKS", "DT_ATMOS", "RESTART_INTERVAL",
            "QUILTING", "WRITE_GROUP", "WRTTASK_PER_GROUP",
            "IMO", "JMO", "OUTPUT_FH",
        ]
        for var in essential:
            assert var in MODEL_CONFIGURE_MAPPING

    def test_ufs_configure_has_pet_bounds(self):
        """ufs.configure mapping should have PET bound variables."""
        pet_vars = [
            "atm_petlist_bounds", "ocn_petlist_bounds",
            "ice_petlist_bounds", "wav_petlist_bounds",
        ]
        for var in pet_vars:
            assert var in UFS_CONFIGURE_MAPPING

    def test_aero_history_has_frequency_vars(self):
        """AERO_HISTORY mapping should have collection frequency variables."""
        freq_vars = [
            "inst_aod_freq", "inst_du_ss_freq", "tavg_2d_rad_freq",
        ]
        for var in freq_vars:
            assert var in AERO_HISTORY_MAPPING


# ---------------------------------------------------------------------------
# Tests: Full file conversion
# ---------------------------------------------------------------------------


class TestFullFileConversion:
    """Tests for converting complete file content."""

    def test_model_configure_conversion(self, model_configure_content):
        """model_configure content should be fully converted."""
        result = atparse_to_jinja2(model_configure_content, MODEL_CONFIGURE_MAPPING)
        # All known variables should be converted
        assert "@[TOTAL_TASKS]" not in result.content
        assert "@[DT_ATMOS]" not in result.content
        assert "@[QUILTING]" not in result.content
        # Shell variables preserved
        assert "${FHMAX}" in result.content
        # Jinja2 expressions present
        assert "{{ model.fv3.total_tasks }}" in result.content
        assert "{{ model.dt_atmos }}" in result.content
        assert result.unknown == []

    def test_diag_table_conversion(self, diag_table_content):
        """diag_table content should be converted with shell vars preserved."""
        result = atparse_to_jinja2(diag_table_content, DIAG_TABLE_MAPPING)
        # atparse variables converted
        assert "@[MOM6_OUTPUT_DIR]" not in result.content
        assert "@[FHOUT_OCN]" not in result.content
        # Jinja2 expressions present
        assert "{{ model.ocean.output_dir | default('./MOM6_OUTPUT') }}" in result.content
        assert "{{ model.ocean.output_frequency_hours | default(6) }}" in result.content
        assert result.unknown == []

    def test_aero_history_conversion(self, aero_history_content):
        """AERO_HISTORY.rc content should be converted."""
        result = atparse_to_jinja2(aero_history_content, AERO_HISTORY_MAPPING)
        assert "@[inst_aod_freq]" not in result.content
        assert "@[inst_du_ss_freq]" not in result.content
        assert "{{ model.aerosol.get('inst_aod_freq', '010000') }}" in result.content
        assert result.unknown == []


# ---------------------------------------------------------------------------
# Tests: get_mapping_for_file
# ---------------------------------------------------------------------------


class TestGetMappingForFile:
    """Tests for file-type-specific mapping selection."""

    def test_model_configure_file(self):
        """model_configure files should get MODEL_CONFIGURE_MAPPING."""
        mapping = get_mapping_for_file("model_configure.IN")
        assert mapping is MODEL_CONFIGURE_MAPPING

    def test_ufs_configure_file(self):
        """ufs.configure files should get UFS_CONFIGURE_MAPPING."""
        mapping = get_mapping_for_file("ufs.configure.s2sw.IN")
        assert mapping is UFS_CONFIGURE_MAPPING

    def test_diag_table_file(self):
        """diag_table files should get DIAG_TABLE_MAPPING."""
        mapping = get_mapping_for_file("diag_table")
        assert mapping is DIAG_TABLE_MAPPING

    def test_aero_history_file(self):
        """AERO_HISTORY.rc files should get AERO_HISTORY_MAPPING."""
        mapping = get_mapping_for_file("AERO_HISTORY.rc")
        assert mapping is AERO_HISTORY_MAPPING

    def test_unknown_file_gets_default(self):
        """Unknown file types should get DEFAULT_VAR_MAPPING."""
        mapping = get_mapping_for_file("some_other_file.txt")
        assert mapping is DEFAULT_VAR_MAPPING

    def test_case_insensitive_matching(self):
        """File type detection should be case-insensitive."""
        mapping = get_mapping_for_file("MODEL_CONFIGURE.IN")
        assert mapping is MODEL_CONFIGURE_MAPPING


# ---------------------------------------------------------------------------
# Tests: list_atparse_variables
# ---------------------------------------------------------------------------


class TestListAtparseVariables:
    """Tests for listing atparse variables in content."""

    def test_finds_all_variables(self):
        """Should find all unique @[VAR] patterns."""
        content = "@[VAR_A] @[VAR_B] @[VAR_A]"
        result = list_atparse_variables(content)
        assert result == ["VAR_A", "VAR_B"]

    def test_empty_content(self):
        """Empty content should return empty list."""
        assert list_atparse_variables("") == []

    def test_no_atparse_patterns(self):
        """Content without @[VAR] should return empty list."""
        assert list_atparse_variables("just plain text ${SHELL}") == []

    def test_sorted_output(self):
        """Output should be sorted alphabetically."""
        content = "@[ZZZ] @[AAA] @[MMM]"
        result = list_atparse_variables(content)
        assert result == ["AAA", "MMM", "ZZZ"]


# ---------------------------------------------------------------------------
# Tests: validate_no_atparse_remaining
# ---------------------------------------------------------------------------


class TestValidateNoAtparseRemaining:
    """Tests for validating no @[VAR] patterns remain."""

    def test_clean_content_returns_empty(self):
        """Content without @[VAR] should return empty list."""
        content = "dt_atmos: {{ model.dt_atmos }}\nfhmax: ${FHMAX}"
        assert validate_no_atparse_remaining(content) == []

    def test_remaining_patterns_detected(self):
        """Remaining @[VAR] patterns should be detected."""
        content = "value: @[LEFTOVER]"
        result = validate_no_atparse_remaining(content)
        assert "LEFTOVER" in result

    def test_multiple_remaining_detected(self):
        """Multiple remaining patterns should all be detected."""
        content = "@[A] text @[B]"
        result = validate_no_atparse_remaining(content)
        assert "A" in result
        assert "B" in result


# ---------------------------------------------------------------------------
# Tests: Regex patterns
# ---------------------------------------------------------------------------


class TestRegexPatterns:
    """Tests for the regex patterns used in the module."""

    def test_atparse_pattern_matches_uppercase(self):
        """@[UPPERCASE_VAR] should match."""
        assert _ATPARSE_PATTERN.search("@[MY_VAR]") is not None

    def test_atparse_pattern_matches_mixed_case(self):
        """@[Mixed_Case_123] should match."""
        assert _ATPARSE_PATTERN.search("@[Mixed_Case_123]") is not None

    def test_atparse_pattern_no_match_empty_brackets(self):
        """@[] should not match (empty brackets)."""
        assert _ATPARSE_PATTERN.search("@[]") is None

    def test_atparse_pattern_no_match_number_start(self):
        """@[123VAR] should not match (starts with number)."""
        assert _ATPARSE_PATTERN.search("@[123VAR]") is None

    def test_shell_var_pattern_matches_braces(self):
        """${VAR_NAME} should match."""
        assert _SHELL_VAR_PATTERN.search("${MY_VAR}") is not None

    def test_shell_var_pattern_matches_bare(self):
        """$VAR_NAME should match."""
        assert _SHELL_VAR_PATTERN.search("$MY_VAR") is not None

    def test_shell_var_pattern_no_match_number_start(self):
        """${123} should not match."""
        assert _SHELL_VAR_PATTERN.search("${123}") is None
