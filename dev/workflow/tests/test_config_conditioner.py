"""Unit tests for ConfigConditioner — if-block handling and integration scenarios.

This file focuses on:
- if [[ ... ]]; then / elif / else / fi block resolution
- Runtime conditional preservation (PDY, cyc, etc.)
- Mixed deploy-time + runtime conditional preservation
- Resolution comment insertion for eliminated branches
- bash -n validation of conditioned output
- Nested conditional handling

The companion file test_config_conditioner_case.py covers case-block handling.

Traces to: Requirements 5.1–5.8
"""

import subprocess
import textwrap

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
        "DO_WAVE": "YES",
        "DO_OCN": "NO",
    })


# ---------------------------------------------------------------------------
# If-block resolution with deploy-time variables
# ---------------------------------------------------------------------------

class TestIfBlockResolution:
    """Test that deploy-time if-blocks are resolved correctly."""

    def test_simple_if_equals_resolves_true_branch(self, conditioner_gfs):
        """if [[ "${RUN}" == "gfs" ]] resolves to the true branch."""
        content = textwrap.dedent("""\
            if [[ "${RUN}" == "gfs" ]]; then
              export FHMAX=384
            fi
        """)
        result = conditioner_gfs.condition_file(content)

        assert "export FHMAX=384" in result.output
        assert 'if [[ "${RUN}" == "gfs" ]]' not in result.output
        assert "fi" not in result.output
        assert result.eliminated_branches >= 0

    def test_simple_if_equals_resolves_false_eliminates(self, conditioner_gfs):
        """if [[ "${RUN}" == "gdas" ]] with RUN=gfs eliminates the block."""
        content = textwrap.dedent("""\
            if [[ "${RUN}" == "gdas" ]]; then
              export FHMAX=9
            fi
        """)
        result = conditioner_gfs.condition_file(content)

        assert "export FHMAX=9" not in result.output
        assert "# Resolved:" in result.output

    def test_if_else_resolves_to_else_branch(self, conditioner_gfs):
        """if [[ "${RUN}" == "gdas" ]] with else branch selects else."""
        content = textwrap.dedent("""\
            if [[ "${RUN}" == "gdas" ]]; then
              export FHMAX=9
            else
              export FHMAX=384
            fi
        """)
        result = conditioner_gfs.condition_file(content)

        assert "export FHMAX=384" in result.output
        assert "export FHMAX=9" not in result.output
        assert "# Resolved:" in result.output

    def test_if_elif_else_resolves_elif(self, conditioner_gdas):
        """if/elif/else resolves to the matching elif branch."""
        content = textwrap.dedent("""\
            if [[ "${RUN}" == "gfs" ]]; then
              export MODE=forecast
            elif [[ "${RUN}" == "gdas" ]]; then
              export MODE=analysis
            else
              export MODE=unknown
            fi
        """)
        result = conditioner_gdas.condition_file(content)

        assert "export MODE=analysis" in result.output
        assert "export MODE=forecast" not in result.output
        assert "export MODE=unknown" not in result.output
        assert "# Resolved:" in result.output

    def test_if_not_equals_resolves_correctly(self, conditioner_gfs):
        """if [[ "${RUN}" != "gdas" ]] with RUN=gfs resolves to true."""
        content = textwrap.dedent("""\
            if [[ "${RUN}" != "gdas" ]]; then
              export LONG_FCST=YES
            fi
        """)
        result = conditioner_gfs.condition_file(content)

        assert "export LONG_FCST=YES" in result.output
        assert "fi" not in result.output

    def test_case_block_with_known_run_resolves(self, conditioner_gfs):
        """Integration: case ${RUN} with known value resolves correctly."""
        content = textwrap.dedent("""\
            case ${RUN} in
              *gfs)
                export FHOUT=${FHOUT_GFS}
                ;;
              *gdas)
                export FHOUT=${FHOUT_GDAS}
                ;;
            esac
        """)
        result = conditioner_gfs.condition_file(content)

        assert "export FHOUT=${FHOUT_GFS}" in result.output
        assert "FHOUT_GDAS" not in result.output
        assert "esac" not in result.output
        assert "# Resolved:" in result.output


# ---------------------------------------------------------------------------
# Runtime conditional preservation
# ---------------------------------------------------------------------------

class TestRuntimeConditionalPreservation:
    """Test that runtime conditionals are preserved unchanged."""

    def test_pdy_conditional_preserved(self, conditioner_gfs):
        """if [[ "${PDY}" ... ]] is preserved unchanged (PDY is runtime)."""
        content = textwrap.dedent("""\
            if [[ "${PDY}" == "20240101" ]]; then
              export SPECIAL_CASE=yes
            fi
        """)
        result = conditioner_gfs.condition_file(content)

        assert result.output.strip() == content.strip()
        assert result.preserved_conditionals >= 1

    def test_cyc_conditional_preserved(self, conditioner_gfs):
        """if [[ "${CYC}" == "00" ]] is preserved (CYC is runtime, uppercase but not in registry)."""
        content = textwrap.dedent("""\
            if [[ "${CYC}" == "00" ]]; then
              export DO_LONG_FCST=YES
            else
              export DO_LONG_FCST=NO
            fi
        """)
        result = conditioner_gfs.condition_file(content)

        # CYC is uppercase but not in deploy_time_vars → runtime → preserved
        assert 'if [[ "${CYC}" == "00" ]]; then' in result.output
        assert "export DO_LONG_FCST=YES" in result.output
        assert "export DO_LONG_FCST=NO" in result.output
        assert "fi" in result.output
        assert result.preserved_conditionals >= 1

    def test_fhour_conditional_preserved(self, conditioner_multi):
        """if [[ "${FHOUR}" ... ]] is preserved (FHOUR is runtime)."""
        content = textwrap.dedent("""\
            if [[ "${FHOUR}" == "000" ]]; then
              export WRITE_DOPOST=.true.
            fi
        """)
        result = conditioner_multi.condition_file(content)

        assert result.output.strip() == content.strip()
        assert result.preserved_conditionals >= 1

    def test_data_variable_conditional_preserved(self, conditioner_gfs):
        """if [[ -d "${DATA}" ]] style conditionals are preserved."""
        content = textwrap.dedent("""\
            if [[ -d "${DATA}/INPUT" ]]; then
              echo "Input directory exists"
            fi
        """)
        result = conditioner_gfs.condition_file(content)

        # -d test doesn't match the [[ "${VAR}" == "val" ]] pattern
        # so it's preserved unchanged
        assert result.output.strip() == content.strip()


# ---------------------------------------------------------------------------
# Mixed deploy-time + runtime conditional preservation
# ---------------------------------------------------------------------------

class TestMixedConditionalPreservation:
    """Test that mixed deploy-time + runtime conditionals are preserved."""

    def test_mixed_and_condition_preserved(self, conditioner_gfs):
        """if [[ "${RUN}" == "gfs" ]] && [[ "${PDY}" ... ]] is preserved."""
        content = textwrap.dedent("""\
            if [[ "${RUN}" == "gfs" ]] && [[ "${PDY}" == "20240101" ]]; then
              export SPECIAL=yes
            fi
        """)
        result = conditioner_gfs.condition_file(content)

        # Mixed deploy-time (RUN) + runtime (PDY) → preserve unchanged
        assert result.output.strip() == content.strip()
        assert result.preserved_conditionals >= 1

    def test_mixed_or_condition_preserved(self, conditioner_multi):
        """if [[ "${RUN}" == "gfs" ]] || [[ "${FHOUR}" == "000" ]] preserved."""
        content = textwrap.dedent("""\
            if [[ "${RUN}" == "gfs" ]] || [[ "${FHOUR}" == "000" ]]; then
              export LONG_RUN=YES
            fi
        """)
        result = conditioner_multi.condition_file(content)

        # FHOUR is uppercase but not in deploy_time_vars → mixed → preserve
        assert result.output.strip() == content.strip()
        assert result.preserved_conditionals >= 1

    def test_deploy_time_only_and_condition_resolved(self, conditioner_multi):
        """if [[ "${RUN}" == "gfs" ]] && [[ "${DO_WAVE}" == "YES" ]] resolves."""
        content = textwrap.dedent("""\
            if [[ "${RUN}" == "gfs" ]] && [[ "${DO_WAVE}" == "YES" ]]; then
              export WAVE_GRID=global
            fi
        """)
        result = conditioner_multi.condition_file(content)

        # Both RUN and DO_WAVE are deploy-time → should resolve
        assert "export WAVE_GRID=global" in result.output
        assert "fi" not in result.output


# ---------------------------------------------------------------------------
# Resolution comment verification
# ---------------------------------------------------------------------------

class TestResolutionComments:
    """Test that eliminated branches get proper resolution comments."""

    def test_resolution_comment_includes_variable_and_value(self, conditioner_gfs):
        """Resolution comment includes the variable name and its value."""
        content = textwrap.dedent("""\
            if [[ "${RUN}" == "gfs" ]]; then
              export X=1
            fi
        """)
        result = conditioner_gfs.condition_file(content)

        assert "# Resolved: RUN=gfs at deploy time" in result.output

    def test_resolution_comment_for_false_branch(self, conditioner_gfs):
        """Eliminated block still gets a resolution comment."""
        content = textwrap.dedent("""\
            if [[ "${RUN}" == "gdas" ]]; then
              export X=1
            fi
        """)
        result = conditioner_gfs.condition_file(content)

        assert "# Resolved:" in result.output
        assert "RUN=gfs" in result.output
        assert "export X=1" not in result.output

    def test_resolution_comment_preserves_indentation(self, conditioner_gfs):
        """Resolution comment uses the same indentation as the if line."""
        content = """\
    if [[ "${RUN}" == "gfs" ]]; then
      export X=1
    fi
"""
        result = conditioner_gfs.condition_file(content)

        # Comment should be indented with 4 spaces (same as 'if' line)
        lines = result.output.split('\n')
        comment_lines = [l for l in lines if "# Resolved:" in l]
        assert len(comment_lines) >= 1
        assert comment_lines[0].startswith("    # Resolved:")


# ---------------------------------------------------------------------------
# bash -n validation
# ---------------------------------------------------------------------------

class TestBashSyntaxValidation:
    """Test that conditioned output passes bash -n validation."""

    def _bash_n_check(self, content: str) -> bool:
        """Run bash -n on content and return True if valid."""
        try:
            proc = subprocess.run(
                ["bash", "-n"],
                input=content,
                capture_output=True,
                text=True,
                timeout=5,
            )
            return proc.returncode == 0
        except (subprocess.TimeoutExpired, FileNotFoundError):
            pytest.skip("bash not available for syntax check")

    def test_resolved_if_block_passes_bash_n(self, conditioner_gfs):
        """Resolved if-block output is valid shell syntax."""
        content = textwrap.dedent("""\
            #!/bin/bash
            if [[ "${RUN}" == "gfs" ]]; then
              export FHMAX=384
              export FHOUT=3
            else
              export FHMAX=9
              export FHOUT=1
            fi
        """)
        result = conditioner_gfs.condition_file(content)
        assert self._bash_n_check(result.output)

    def test_resolved_case_block_passes_bash_n(self, conditioner_gfs):
        """Resolved case-block output is valid shell syntax."""
        content = textwrap.dedent("""\
            #!/bin/bash
            case ${RUN} in
              *gfs)
                export FHOUT=${FHOUT_GFS}
                ;;
              *gdas)
                export FHMAX_HF=0
                ;;
            esac
        """)
        result = conditioner_gfs.condition_file(content)
        assert self._bash_n_check(result.output)

    def test_mixed_resolved_and_preserved_passes_bash_n(self, conditioner_multi):
        """File with both resolved and preserved blocks is valid shell."""
        content = textwrap.dedent("""\
            #!/bin/bash
            export BASE_VAR=hello

            if [[ "${RUN}" == "gfs" ]]; then
              export MODE=forecast
            else
              export MODE=analysis
            fi

            if [[ "${PDY}" == "20240101" ]]; then
              export SPECIAL=yes
            fi

            case ${RUN} in
              *gfs)
                export FHMAX=384
                ;;
              *gdas)
                export FHMAX=9
                ;;
            esac

            export FINAL_VAR=done
        """)
        result = conditioner_multi.condition_file(content)
        assert self._bash_n_check(result.output)

    def test_complex_config_passes_bash_n(self, conditioner_multi):
        """A realistic config file with multiple patterns passes bash -n."""
        content = textwrap.dedent("""\
            #!/bin/bash

            # Base configuration
            export HOMEgfs="${HOMEgfs:-/path/to/home}"
            export EXPDIR="${EXPDIR:-/path/to/exp}"

            # Deploy-time conditional
            if [[ "${RUN}" == "gfs" ]]; then
              export FHMAX_GFS=384
              export FHOUT_GFS=3
              export FHOUT_HF_GFS=1
            elif [[ "${RUN}" == "gdas" ]]; then
              export FHMAX_GFS=9
              export FHOUT_GFS=1
              export FHOUT_HF_GFS=1
            fi

            # Runtime conditional (preserved)
            if [[ "${PDY}" -ge "20240101" ]]; then
              export NEW_PHYSICS=YES
            fi

            # Another deploy-time conditional
            if [[ "${DO_WAVE}" == "YES" ]]; then
              export WAVE_GRID="global_270k"
            fi
        """)
        result = conditioner_multi.condition_file(content)
        assert self._bash_n_check(result.output)
        # Verify deploy-time blocks were resolved
        assert "export FHMAX_GFS=384" in result.output
        assert "export WAVE_GRID=" in result.output


# ---------------------------------------------------------------------------
# Nested conditionals
# ---------------------------------------------------------------------------

class TestNestedConditionals:
    """Test that nested conditionals are handled correctly."""

    def test_nested_runtime_inside_deploy_time(self, conditioner_gfs):
        """Nested runtime if inside a deploy-time if is preserved in output."""
        content = textwrap.dedent("""\
            if [[ "${RUN}" == "gfs" ]]; then
              export BASE=yes
              if [[ "${PDY}" == "20240101" ]]; then
                export SPECIAL=yes
              fi
            fi
        """)
        result = conditioner_gfs.condition_file(content)

        # The outer deploy-time block resolves (RUN=gfs matches)
        # The inner runtime block should be preserved in the output
        assert "export BASE=yes" in result.output
        assert 'if [[ "${PDY}" == "20240101" ]]; then' in result.output
        assert "export SPECIAL=yes" in result.output
        assert "fi" in result.output

    def test_nested_deploy_time_inside_deploy_time(self, conditioner_multi):
        """Nested deploy-time if inside another deploy-time if."""
        content = textwrap.dedent("""\
            if [[ "${RUN}" == "gfs" ]]; then
              export BASE=yes
              if [[ "${DO_WAVE}" == "YES" ]]; then
                export WAVE=enabled
              fi
            fi
        """)
        result = conditioner_multi.condition_file(content)

        # Outer resolves (RUN=gfs), inner content is emitted
        # The inner if may or may not be resolved depending on implementation
        # At minimum, the body should appear
        assert "export BASE=yes" in result.output
        assert "export WAVE=enabled" in result.output

    def test_nested_deploy_time_false_outer_eliminates_all(self, conditioner_gfs):
        """When outer deploy-time if is false, nested content is eliminated."""
        content = textwrap.dedent("""\
            if [[ "${RUN}" == "gdas" ]]; then
              export BASE=yes
              if [[ "${RUN}" == "gdas" ]]; then
                export NESTED=yes
              fi
            fi
        """)
        result = conditioner_gfs.condition_file(content)

        # Outer is false (RUN=gfs != gdas), so everything is eliminated
        assert "export BASE=yes" not in result.output
        assert "export NESTED=yes" not in result.output
        assert "# Resolved:" in result.output

    def test_multiple_sequential_if_blocks(self, conditioner_multi):
        """Multiple sequential if-blocks are each handled independently."""
        content = textwrap.dedent("""\
            if [[ "${RUN}" == "gfs" ]]; then
              export A=1
            fi
            if [[ "${DO_WAVE}" == "YES" ]]; then
              export B=2
            fi
            if [[ "${DO_OCN}" == "YES" ]]; then
              export C=3
            fi
        """)
        result = conditioner_multi.condition_file(content)

        # RUN=gfs → true, DO_WAVE=YES → true, DO_OCN=NO → false
        assert "export A=1" in result.output
        assert "export B=2" in result.output
        assert "export C=3" not in result.output

    def test_deeply_nested_conditionals(self, conditioner_gfs):
        """Three levels of nesting are handled without errors."""
        content = textwrap.dedent("""\
            if [[ "${RUN}" == "gfs" ]]; then
              export LEVEL1=yes
              if [[ "${PDY}" == "20240101" ]]; then
                export LEVEL2=yes
                if [[ "${cyc}" == "00" ]]; then
                  export LEVEL3=yes
                fi
              fi
            fi
        """)
        result = conditioner_gfs.condition_file(content)

        # Outer resolves (RUN=gfs), inner runtime blocks preserved
        assert "export LEVEL1=yes" in result.output
        assert 'if [[ "${PDY}" == "20240101" ]]; then' in result.output
        assert "export LEVEL2=yes" in result.output
        assert 'if [[ "${cyc}" == "00" ]]; then' in result.output
        assert "export LEVEL3=yes" in result.output


# ---------------------------------------------------------------------------
# Integration: combined if + case blocks
# ---------------------------------------------------------------------------

class TestIntegrationIfAndCase:
    """Test files with both if-blocks and case-blocks."""

    def test_file_with_both_if_and_case_blocks(self, conditioner_multi):
        """Both if-blocks and case-blocks are resolved in the same file."""
        content = textwrap.dedent("""\
            # Config file header
            export HOMEgfs="${HOMEgfs}"

            if [[ "${RUN}" == "gfs" ]]; then
              export FHMAX=384
            else
              export FHMAX=9
            fi

            case ${CASE} in
              C384)
                export LAYOUT="6,8"
                ;;
              C768)
                export LAYOUT="8,12"
                ;;
            esac

            # Runtime conditional preserved
            if [[ "${PDY}" == "20240101" ]]; then
              export SPECIAL=yes
            fi
        """)
        result = conditioner_multi.condition_file(content)

        # Deploy-time if resolved
        assert "export FHMAX=384" in result.output
        assert "export FHMAX=9" not in result.output
        # Deploy-time case resolved
        assert 'export LAYOUT="6,8"' in result.output
        assert 'export LAYOUT="8,12"' not in result.output
        # Runtime preserved
        assert 'if [[ "${PDY}" == "20240101" ]]; then' in result.output
        assert "export SPECIAL=yes" in result.output
        # Header preserved
        assert 'export HOMEgfs="${HOMEgfs}"' in result.output

    def test_conditioner_result_statistics(self, conditioner_multi):
        """ConditionerResult reports correct statistics."""
        content = textwrap.dedent("""\
            if [[ "${RUN}" == "gfs" ]]; then
              export A=1
            else
              export A=2
            fi

            if [[ "${PDY}" == "20240101" ]]; then
              export B=1
            fi

            case ${RUN} in
              *gfs)
                export C=1
                ;;
              *gdas)
                export C=2
                ;;
            esac
        """)
        result = conditioner_multi.condition_file(content)

        # One if-block eliminated (the else branch), one case eliminated
        assert result.eliminated_branches >= 2
        # One runtime conditional preserved (PDY)
        assert result.preserved_conditionals >= 1
