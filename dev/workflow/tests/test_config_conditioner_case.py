"""Unit tests for ConfigConditioner case-block handling (task 4.3).

Tests the case ${VAR} in ... esac block resolution logic including:
- Deploy-time variable case blocks are resolved
- Runtime variable case blocks are preserved unchanged
- Glob pattern matching (*gfs, gdas | gfs, *, etc.)
- Inline case patterns (single-line body with ;;)
- Resolution comments are inserted correctly
- No-match scenario produces a comment-only replacement
"""

import pytest

from deployment.config_conditioner import ConfigConditioner, ConditionerResult


# ---------------------------------------------------------------------------
# Fixtures
# ---------------------------------------------------------------------------

@pytest.fixture
def conditioner_gfs():
    """Conditioner with RUN=gfs as a deploy-time variable."""
    return ConfigConditioner(deploy_time_vars={"RUN": "gfs"})


@pytest.fixture
def conditioner_gdas():
    """Conditioner with RUN=gdas as a deploy-time variable."""
    return ConfigConditioner(deploy_time_vars={"RUN": "gdas"})


@pytest.fixture
def conditioner_multi():
    """Conditioner with multiple deploy-time variables."""
    return ConfigConditioner(deploy_time_vars={
        "RUN": "gfs",
        "CASE": "C384",
        "MACHINE": "HERA",
    })


# ---------------------------------------------------------------------------
# Basic case block resolution
# ---------------------------------------------------------------------------

class TestCaseBlockResolution:
    """Test that deploy-time case blocks are resolved correctly."""

    def test_resolves_glob_prefix_pattern(self, conditioner_gfs):
        """case ${RUN} with *gfs) pattern matches RUN=gfs."""
        content = """\
case ${RUN} in
  *gfs)
    export FHOUT=${FHOUT_GFS}
    export FHOUT_HF=${FHOUT_HF_GFS}
    ;;
  *gdas)
    export FHMAX_HF=0
    export FHOUT_HF=0
    ;;
  *)
    echo "FATAL ERROR: Unsupported RUN '${RUN}'"
    exit 1
esac
"""
        result = conditioner_gfs.condition_file(content)

        assert "# Resolved: case ${RUN}" in result.output
        assert "*gfs" in result.output
        assert "export FHOUT=${FHOUT_GFS}" in result.output
        assert "export FHOUT_HF=${FHOUT_HF_GFS}" in result.output
        # Dead branches removed
        assert "*gdas)" not in result.output
        assert "FHMAX_HF=0" not in result.output
        assert "esac" not in result.output
        assert "case ${RUN} in" not in result.output
        assert result.eliminated_branches == 1

    def test_resolves_gdas_branch(self, conditioner_gdas):
        """case ${RUN} with *gdas) pattern matches RUN=gdas."""
        content = """\
case ${RUN} in
  *gfs)
    export FHOUT=${FHOUT_GFS}
    ;;
  *gdas)
    export FHMAX_HF=0
    ;;
esac
"""
        result = conditioner_gdas.condition_file(content)

        assert "export FHMAX_HF=0" in result.output
        assert "FHOUT_GFS" not in result.output
        assert "gdas" in result.output  # In the comment
        assert result.eliminated_branches == 1

    def test_resolves_exact_match_pattern(self):
        """Exact pattern 'gfs' matches only 'gfs', not 'anygfs'."""
        conditioner = ConfigConditioner(deploy_time_vars={"RUN": "gfs"})
        content = """\
case ${RUN} in
  gfs)
    export MODE=forecast
    ;;
  gdas)
    export MODE=analysis
    ;;
esac
"""
        result = conditioner.condition_file(content)

        assert "export MODE=forecast" in result.output
        assert "MODE=analysis" not in result.output
        assert result.eliminated_branches == 1

    def test_resolves_pipe_separated_pattern(self):
        """Pipe-separated pattern 'gdas | gfs' matches 'gfs'."""
        conditioner = ConfigConditioner(deploy_time_vars={"RUN": "gfs"})
        content = """\
case ${RUN} in
  gdas | gfs)
    export ARCHIVE=YES
    ;;
  enkf*)
    export ARCHIVE=NO
    ;;
esac
"""
        result = conditioner.condition_file(content)

        assert "export ARCHIVE=YES" in result.output
        assert "ARCHIVE=NO" not in result.output
        assert result.eliminated_branches == 1

    def test_resolves_default_branch_when_no_specific_match(self):
        """Default *) branch matches when no other pattern does."""
        conditioner = ConfigConditioner(deploy_time_vars={"RUN": "unknown"})
        content = """\
case ${RUN} in
  gfs)
    export MODE=forecast
    ;;
  gdas)
    export MODE=analysis
    ;;
  *)
    export MODE=default
    ;;
esac
"""
        result = conditioner.condition_file(content)

        assert "export MODE=default" in result.output
        assert "MODE=forecast" not in result.output
        assert "MODE=analysis" not in result.output

    def test_no_match_produces_comment_only(self):
        """When no branch matches (no default), output is comment-only."""
        conditioner = ConfigConditioner(deploy_time_vars={"RUN": "unknown"})
        content = """\
case ${RUN} in
  gfs)
    export MODE=forecast
    ;;
  gdas)
    export MODE=analysis
    ;;
esac
"""
        result = conditioner.condition_file(content)

        assert "# Resolved: case ${RUN}" in result.output
        assert "no branch matched" in result.output
        assert "MODE=forecast" not in result.output
        assert "MODE=analysis" not in result.output


# ---------------------------------------------------------------------------
# Runtime variable preservation
# ---------------------------------------------------------------------------

class TestRuntimePreservation:
    """Test that case blocks on runtime variables are preserved."""

    def test_preserves_runtime_variable_case_block(self, conditioner_gfs):
        """case ${step} (runtime var, lowercase) passes through unchanged.

        Note: lowercase variables don't match _CASE_BLOCK_PATTERN (which
        requires [A-Z_][A-Z0-9_]*), so they pass through without being
        counted as preserved_conditionals. This is correct — the regex
        only targets uppercase variables that COULD be deploy-time.
        """
        content = """\
case ${step} in
  "prep")
    export walltime="00:30:00"
    ;;
  "fcst")
    export walltime="06:00:00"
    ;;
esac
"""
        result = conditioner_gfs.condition_file(content)

        assert result.output == content
        # Lowercase vars don't match the pattern, so not counted
        assert result.eliminated_branches == 0

    def test_preserves_lowercase_runtime_variable(self, conditioner_gfs):
        """case ${fv3_res} (lowercase, runtime) is preserved unchanged."""
        content = """\
case ${fv3_res} in
  "C48" | "C96")
    zstandard_level=0
    ;;
  *)
    zstandard_level=5
    ;;
esac
"""
        # fv3_res is lowercase so won't match _CASE_BLOCK_PATTERN
        # (pattern requires [A-Z_][A-Z0-9_]*)
        result = conditioner_gfs.condition_file(content)

        # Lowercase vars don't match the pattern, so content is unchanged
        assert result.output == content

    def test_preserves_unknown_uppercase_runtime_variable(self, conditioner_gfs):
        """case ${PDY} (uppercase but not in deploy_time_vars) preserved."""
        content = """\
case ${PDY} in
  20240101)
    export SPECIAL=yes
    ;;
  *)
    export SPECIAL=no
    ;;
esac
"""
        result = conditioner_gfs.condition_file(content)

        assert result.output == content
        assert result.preserved_conditionals == 1


# ---------------------------------------------------------------------------
# Inline case patterns
# ---------------------------------------------------------------------------

class TestInlineCasePatterns:
    """Test inline case patterns (body on same line as pattern)."""

    def test_resolves_inline_pattern(self):
        """Inline pattern like 'gfs) body ;;' is handled."""
        conditioner = ConfigConditioner(deploy_time_vars={"RUN": "gfs"})
        content = """\
case ${RUN} in
    gdas | gfs)\tselective_exclude_string+="*prepbufr*" ;;
    enkf*)\tselective_exclude_string+="*f006.ens*" ;;
    *)\tselective_exclude_string="" ;;
esac
"""
        result = conditioner.condition_file(content)

        assert 'selective_exclude_string+="*prepbufr*"' in result.output
        assert "f006.ens" not in result.output
        assert "esac" not in result.output
        assert result.eliminated_branches == 1


# ---------------------------------------------------------------------------
# Multiple case blocks
# ---------------------------------------------------------------------------

class TestMultipleCaseBlocks:
    """Test files with multiple case blocks."""

    def test_resolves_multiple_deploy_time_blocks(self, conditioner_multi):
        """Multiple case blocks on different deploy-time vars are resolved."""
        content = """\
case ${RUN} in
  gfs)
    export MODE=forecast
    ;;
  gdas)
    export MODE=analysis
    ;;
esac

case ${CASE} in
  "C384")
    CASE_ANL="C384"
    ;;
  "C768")
    CASE_ANL="C384"
    ;;
esac
"""
        result = conditioner_multi.condition_file(content)

        assert "export MODE=forecast" in result.output
        assert 'CASE_ANL="C384"' in result.output
        assert "MODE=analysis" not in result.output
        assert result.eliminated_branches == 2

    def test_mixed_deploy_and_runtime_blocks(self, conditioner_multi):
        """Deploy-time blocks resolved, runtime blocks preserved."""
        content = """\
case ${RUN} in
  gfs)
    export MODE=forecast
    ;;
esac

case ${step} in
  "prep")
    export walltime="00:30:00"
    ;;
esac
"""
        result = conditioner_multi.condition_file(content)

        assert "export MODE=forecast" in result.output
        assert "case ${RUN} in" not in result.output
        # Runtime block preserved (lowercase var passes through unchanged)
        assert 'case ${step} in' in result.output
        assert 'export walltime="00:30:00"' in result.output
        assert "esac" in result.output
        assert result.eliminated_branches == 1
        # step is lowercase, so not counted by the pattern
        # Only uppercase runtime vars get counted as preserved


# ---------------------------------------------------------------------------
# Resolution comment format
# ---------------------------------------------------------------------------

class TestResolutionComments:
    """Test that resolution comments follow the expected format."""

    def test_comment_includes_variable_and_value(self, conditioner_gfs):
        """Resolution comment includes var name, pattern, and value."""
        content = """\
case ${RUN} in
  *gfs)
    export X=1
    ;;
esac
"""
        result = conditioner_gfs.condition_file(content)

        assert "# Resolved: case ${RUN} → *gfs at deploy time (RUN=gfs)" in result.output

    def test_comment_preserves_indentation(self):
        """Resolution comment uses the same indentation as the case line."""
        conditioner = ConfigConditioner(deploy_time_vars={"RUN": "gfs"})
        content = """\
    case ${RUN} in
      gfs)
        export X=1
        ;;
    esac
"""
        result = conditioner.condition_file(content)

        # Comment should be indented with 4 spaces (same as 'case' line)
        assert "    # Resolved:" in result.output


# ---------------------------------------------------------------------------
# Edge cases
# ---------------------------------------------------------------------------

class TestEdgeCases:
    """Test edge cases in case block handling."""

    def test_empty_body_branch(self):
        """Branch with empty body produces comment only."""
        conditioner = ConfigConditioner(deploy_time_vars={"RUN": "gfs"})
        content = """\
case ${RUN} in
  gfs)
    ;;
  gdas)
    export X=1
    ;;
esac
"""
        result = conditioner.condition_file(content)

        assert "# Resolved:" in result.output
        assert "X=1" not in result.output
        assert result.eliminated_branches == 1

    def test_case_block_without_esac_preserved(self):
        """Malformed case block (no esac) is preserved unchanged."""
        conditioner = ConfigConditioner(deploy_time_vars={"RUN": "gfs"})
        content = """\
case ${RUN} in
  gfs)
    export X=1
    ;;
"""
        result = conditioner.condition_file(content)

        # Cannot parse — preserved unchanged
        assert result.output == content
        assert result.preserved_conditionals == 1

    def test_quoted_pattern_matching(self):
        """Quoted patterns like "C384" match correctly."""
        conditioner = ConfigConditioner(deploy_time_vars={"CASE": "C384"})
        content = """\
case ${CASE} in
  "C1152" | "C768" | "C384")
    CASE_ANL="C384"
    ;;
  "C192" | "C96" | "C48")
    CASE_ANL="C96"
    ;;
esac
"""
        result = conditioner.condition_file(content)

        assert 'CASE_ANL="C384"' in result.output
        assert 'CASE_ANL="C96"' not in result.output

    def test_content_before_and_after_case_preserved(self):
        """Content before and after the case block is preserved."""
        conditioner = ConfigConditioner(deploy_time_vars={"RUN": "gfs"})
        content = """\
export BEFORE=yes

case ${RUN} in
  gfs)
    export INSIDE=yes
    ;;
esac

export AFTER=yes
"""
        result = conditioner.condition_file(content)

        assert "export BEFORE=yes" in result.output
        assert "export INSIDE=yes" in result.output
        assert "export AFTER=yes" in result.output

    def test_glob_suffix_pattern(self):
        """Pattern 'enkf*' matches 'enkfgdas'."""
        conditioner = ConfigConditioner(deploy_time_vars={"RUN": "enkfgdas"})
        content = """\
case ${RUN} in
  gfs)
    export X=1
    ;;
  enkf*)
    export X=2
    ;;
esac
"""
        result = conditioner.condition_file(content)

        assert "export X=2" in result.output
        assert "export X=1" not in result.output
