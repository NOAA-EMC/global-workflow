"""Property-based tests for Config_Conditioner correctness properties.

Property 4: Config Conditioner Preserves Runtime Conditionals
    For any config file content containing conditional blocks that test
    runtime variables (PDY, cyc, FHOUR, DATA, etc.), the Config_Conditioner
    output SHALL contain those conditional blocks unchanged (byte-identical).

Property 5: Config Conditioner Evaluates Deploy-Time Conditionals
    For any config file content containing a conditional block that tests
    ONLY deploy-time variables with known values, the Config_Conditioner
    output SHALL contain only the matching branch content (with the
    conditional structure removed) and a comment indicating the resolution.

Property 6: Config Conditioner Output Validity
    For any config file processed by the Config_Conditioner, the output
    SHALL be syntactically valid shell (accepted by bash -n without errors).

Traces to: Design Document - Correctness Properties 4, 5, 6
"""

from __future__ import annotations

import os
import sys

from hypothesis import given, settings, HealthCheck, assume
from hypothesis import strategies as st

sys.path.insert(0, os.path.join(os.path.dirname(__file__), ".."))

from deployment.config_conditioner import ConfigConditioner, ConditionerResult


# ---------------------------------------------------------------------------
# Constants
# ---------------------------------------------------------------------------

# Deploy-time variables from the registry (known at deploy time)
DEPLOY_TIME_VARS = [
    "RUN", "NET", "CASE", "CASE_ENS", "MACHINE", "CDUMP",
    "NMEM_ENS", "APP", "CCPP_SUITE", "DO_COUPLED", "DO_WAVE",
    "DO_OCN", "DO_ICE", "DO_AERO", "REPLAY_ICS",
]

# Runtime variables (only known at job execution time, NOT in registry).
# MUST be uppercase to be recognized by the conditioner's variable regex
# pattern [A-Z_][A-Z0-9_]*. Lowercase vars like "cyc" are not matched by
# the regex and get treated as "no variable found" (constant/deploy-time).
RUNTIME_VARS = [
    "PDY", "CYC", "FHOUR", "DATA", "COMOUT", "ROTDIR",
    "COMIN", "DATAROOT", "CDATE", "GDATE",
]

# Safe values for deploy-time variables (simple alphanumeric, no special chars)
DEPLOY_TIME_VALUES = [
    "gfs", "gdas", "gefs", "C384", "C768", "C96",
    "HERA", "WCOSS2", "ORION", "YES", "NO",
    "ATM", "S2S", "S2SW", "FV3_GFS_v17_p8",
    "0", "20", "80",
]

# Safe values used in runtime conditionals
RUNTIME_VALUES = [
    "20240101", "20230601", "00", "06", "12", "18",
    "000", "024", "120", "384",
]


# ---------------------------------------------------------------------------
# Hypothesis Strategies
# ---------------------------------------------------------------------------

def _runtime_var_strategy():
    """Strategy that picks a runtime variable name."""
    return st.sampled_from(RUNTIME_VARS)


def _deploy_time_var_strategy():
    """Strategy that picks a deploy-time variable name."""
    return st.sampled_from(DEPLOY_TIME_VARS)


def _runtime_value_strategy():
    """Strategy that picks a safe runtime comparison value."""
    return st.sampled_from(RUNTIME_VALUES)


def _deploy_time_value_strategy():
    """Strategy that picks a safe deploy-time comparison value."""
    return st.sampled_from(DEPLOY_TIME_VALUES)


def _operator_strategy():
    """Strategy that picks a bash comparison operator."""
    return st.sampled_from(["==", "!="])


def _simple_export_strategy():
    """Strategy that generates a simple export statement (valid bash)."""
    var_name = st.from_regex(r"[A-Z][A-Z0-9_]{2,8}", fullmatch=True)
    var_value = st.sampled_from([
        "YES", "NO", "1", "0", "hello", "/path/to/thing",
        "${OTHER_VAR}", "some_value", "384", "C384",
    ])
    return st.builds(
        lambda name, val: f"  export {name}={val}",
        var_name, var_value,
    )


@st.composite
def _runtime_if_block(draw):
    """Generate an if-block using a runtime variable (must be preserved).

    Produces syntactically valid bash if-blocks like:
        if [[ "${PDY}" == "20240101" ]]; then
          export SOME_VAR=value
        fi
    """
    var = draw(_runtime_var_strategy())
    value = draw(_runtime_value_strategy())
    op = draw(_operator_strategy())
    body = draw(_simple_export_strategy())
    has_else = draw(st.booleans())

    lines = [
        f'if [[ "${{{var}}}" {op} "{value}" ]]; then',
        body,
    ]
    if has_else:
        else_body = draw(_simple_export_strategy())
        lines.append("else")
        lines.append(else_body)
    lines.append("fi")
    return "\n".join(lines)


@st.composite
def _runtime_case_block(draw):
    """Generate a case-block using a runtime variable (must be preserved).

    Produces syntactically valid bash case-blocks like:
        case ${cyc} in
          00)
            export VAR=val1
            ;;
          12)
            export VAR=val2
            ;;
        esac
    """
    var = draw(_runtime_var_strategy())
    num_branches = draw(st.integers(min_value=1, max_value=3))
    values = draw(
        st.lists(
            _runtime_value_strategy(),
            min_size=num_branches,
            max_size=num_branches,
            unique=True,
        )
    )

    lines = [f"case ${{{var}}} in"]
    for val in values:
        body = draw(_simple_export_strategy())
        lines.append(f"  {val})")
        lines.append(f"  {body}")
        lines.append("    ;;")
    lines.append("esac")
    return "\n".join(lines)


@st.composite
def _deploy_time_if_block(draw, deploy_vars_dict):
    """Generate an if-block using a deploy-time variable (will be evaluated).

    Args:
        deploy_vars_dict: The dict of deploy-time variable -> value used by
            the conditioner.
    """
    var = draw(_deploy_time_var_strategy())
    assume(var in deploy_vars_dict)
    actual_value = deploy_vars_dict[var]

    # Choose whether the condition matches or not
    matches = draw(st.booleans())
    op = "=="

    if matches:
        test_value = actual_value
    else:
        # Pick a value different from the actual
        test_value = draw(_deploy_time_value_strategy())
        assume(test_value != actual_value)

    body_true = draw(_simple_export_strategy())
    has_else = draw(st.booleans())

    lines = [
        f'if [[ "${{{var}}}" {op} "{test_value}" ]]; then',
        body_true,
    ]
    if has_else:
        body_else = draw(_simple_export_strategy())
        lines.append("else")
        lines.append(body_else)
    lines.append("fi")

    return (lines, var, test_value, actual_value, matches, has_else, body_true,
            body_else if has_else else None)


@st.composite
def _deploy_time_case_block(draw, deploy_vars_dict):
    """Generate a case-block using a deploy-time variable (will be evaluated).

    Args:
        deploy_vars_dict: The dict of deploy-time variable -> value used by
            the conditioner.
    """
    var = draw(_deploy_time_var_strategy())
    assume(var in deploy_vars_dict)
    actual_value = deploy_vars_dict[var]

    # Generate branches, ensuring at least one matches
    num_branches = draw(st.integers(min_value=2, max_value=4))
    # Decide which index will be the matching one
    match_idx = draw(st.integers(min_value=0, max_value=num_branches - 1))

    branches = []
    for i in range(num_branches):
        body = draw(_simple_export_strategy())
        if i == match_idx:
            # Use a glob pattern that matches the actual value
            pattern = f"*{actual_value}"
        else:
            val = draw(_deploy_time_value_strategy())
            assume(val != actual_value)
            pattern = f"*{val}"
        branches.append((pattern, body))

    lines = [f"case ${{{var}}} in"]
    for pattern, body in branches:
        lines.append(f"  {pattern})")
        lines.append(f"  {body}")
        lines.append("    ;;")
    lines.append("esac")

    matching_body = branches[match_idx][1]
    return "\n".join(lines), matching_body


@st.composite
def _deploy_time_vars_dict_strategy(draw):
    """Generate a realistic deploy-time vars dict with known values."""
    # Always include RUN since it's the most common
    result = {}
    # Pick a subset of deploy-time vars (at least RUN)
    selected = draw(st.lists(
        st.sampled_from(DEPLOY_TIME_VARS),
        min_size=3,
        max_size=len(DEPLOY_TIME_VARS),
        unique=True,
    ))
    if "RUN" not in selected:
        selected.append("RUN")

    for var in selected:
        result[var] = draw(_deploy_time_value_strategy())

    return result


@st.composite
def _valid_shell_config(draw, deploy_vars_dict):
    """Generate a valid shell config file with mixed conditionals.

    The config may contain:
    - Simple export statements (unconditional)
    - Runtime if-blocks (preserved)
    - Deploy-time if-blocks (evaluated)
    - Runtime case-blocks (preserved)
    - Deploy-time case-blocks (evaluated)

    All generated content is syntactically valid bash.
    """
    parts = ["#!/bin/bash", ""]

    num_sections = draw(st.integers(min_value=1, max_value=5))
    for _ in range(num_sections):
        section_type = draw(st.sampled_from([
            "export", "runtime_if", "deploy_if", "runtime_case", "deploy_case",
        ]))

        if section_type == "export":
            stmt = draw(_simple_export_strategy())
            parts.append(stmt.strip())
        elif section_type == "runtime_if":
            block = draw(_runtime_if_block())
            parts.append(block)
        elif section_type == "deploy_if":
            # Pick a var that's in our dict
            var = draw(st.sampled_from(list(deploy_vars_dict.keys())))
            actual = deploy_vars_dict[var]
            body = draw(_simple_export_strategy())
            matches = draw(st.booleans())
            if matches:
                test_val = actual
            else:
                test_val = draw(_deploy_time_value_strategy())
                assume(test_val != actual)
            block = f'if [[ "${{{var}}}" == "{test_val}" ]]; then\n{body}\nfi'
            parts.append(block)
        elif section_type == "runtime_case":
            block = draw(_runtime_case_block())
            parts.append(block)
        elif section_type == "deploy_case":
            var = draw(st.sampled_from(list(deploy_vars_dict.keys())))
            actual = deploy_vars_dict[var]
            body = draw(_simple_export_strategy())
            block = (
                f"case ${{{var}}} in\n"
                f"  *{actual})\n"
                f"  {body}\n"
                f"    ;;\n"
                f"esac"
            )
            parts.append(block)

        parts.append("")

    return "\n".join(parts)


# ---------------------------------------------------------------------------
# Property 4: Config Conditioner Preserves Runtime Conditionals
# ---------------------------------------------------------------------------

class TestProperty4RuntimePreservation:
    """Property 4: Config Conditioner Preserves Runtime Conditionals.

    For any config file content containing conditional blocks that test
    runtime variables (PDY, cyc, FHOUR, DATA, etc.), the Config_Conditioner
    output SHALL contain those conditional blocks unchanged (byte-identical).

    **Validates: Requirements 5.3, 5.6, 5.7**
    """

    @given(
        runtime_block=_runtime_if_block(),
        deploy_vars=_deploy_time_vars_dict_strategy(),
    )
    @settings(
        max_examples=100,
        deadline=None,
        suppress_health_check=[HealthCheck.too_slow],
    )
    def test_runtime_if_block_preserved_byte_identical(
        self, runtime_block, deploy_vars
    ):
        """Runtime if-blocks are byte-identical in the output.

        **Validates: Requirements 5.3, 5.6, 5.7**
        """
        # Wrap the block in a valid shell script
        content = f"#!/bin/bash\n\n{runtime_block}\n"

        conditioner = ConfigConditioner(deploy_time_vars=deploy_vars)
        result = conditioner.condition_file(content)

        # The runtime block must appear byte-identical in the output
        assert runtime_block in result.output, (
            f"Runtime if-block was modified by conditioner.\n"
            f"Expected block:\n{runtime_block}\n"
            f"Full output:\n{result.output}"
        )
        # Preserved conditionals counter must be >= 1
        assert result.preserved_conditionals >= 1

    @given(
        runtime_block=_runtime_case_block(),
        deploy_vars=_deploy_time_vars_dict_strategy(),
    )
    @settings(
        max_examples=100,
        deadline=None,
        suppress_health_check=[HealthCheck.too_slow],
    )
    def test_runtime_case_block_preserved_byte_identical(
        self, runtime_block, deploy_vars
    ):
        """Runtime case-blocks are byte-identical in the output.

        **Validates: Requirements 5.3, 5.6, 5.7**
        """
        content = f"#!/bin/bash\n\n{runtime_block}\n"

        conditioner = ConfigConditioner(deploy_time_vars=deploy_vars)
        result = conditioner.condition_file(content)

        # The runtime case block must appear byte-identical in the output
        assert runtime_block in result.output, (
            f"Runtime case-block was modified by conditioner.\n"
            f"Expected block:\n{runtime_block}\n"
            f"Full output:\n{result.output}"
        )
        assert result.preserved_conditionals >= 1


# ---------------------------------------------------------------------------
# Property 5: Config Conditioner Evaluates Deploy-Time Conditionals
# ---------------------------------------------------------------------------

class TestProperty5DeployTimeEvaluation:
    """Property 5: Config Conditioner Evaluates Deploy-Time Conditionals.

    For any config file content containing a conditional block that tests
    ONLY deploy-time variables with known values, the Config_Conditioner
    output SHALL contain only the matching branch content (with the
    conditional structure removed) and a comment indicating the resolution.

    **Validates: Requirements 5.1, 5.2, 5.5**
    """

    @given(data=st.data())
    @settings(
        max_examples=100,
        deadline=None,
        suppress_health_check=[HealthCheck.too_slow],
    )
    def test_deploy_time_if_matching_branch_selected(self, data):
        """When condition matches, the true branch body appears in output.

        **Validates: Requirements 5.1, 5.2, 5.5**
        """
        deploy_vars = data.draw(_deploy_time_vars_dict_strategy())
        # Pick a var and value that matches
        var = data.draw(st.sampled_from(list(deploy_vars.keys())))
        actual_value = deploy_vars[var]
        body = data.draw(_simple_export_strategy())

        content = (
            f"#!/bin/bash\n\n"
            f'if [[ "${{{var}}}" == "{actual_value}" ]]; then\n'
            f"{body}\n"
            f"fi\n"
        )

        conditioner = ConfigConditioner(deploy_time_vars=deploy_vars)
        result = conditioner.condition_file(content)

        # The matching branch body must appear in output
        assert body.strip() in result.output, (
            f"Matching branch body not found in output.\n"
            f"Body: {body.strip()}\n"
            f"Output:\n{result.output}"
        )
        # The if/fi structure must be removed
        assert f'if [[ "${{{var}}}" == "{actual_value}" ]]; then' not in result.output
        assert "# Resolved:" in result.output

    @given(data=st.data())
    @settings(
        max_examples=100,
        deadline=None,
        suppress_health_check=[HealthCheck.too_slow],
    )
    def test_deploy_time_if_non_matching_branch_eliminated(self, data):
        """When condition doesn't match, the branch body is eliminated.

        **Validates: Requirements 5.1, 5.2, 5.5**
        """
        deploy_vars = data.draw(_deploy_time_vars_dict_strategy())
        var = data.draw(st.sampled_from(list(deploy_vars.keys())))
        actual_value = deploy_vars[var]

        # Pick a value that does NOT match
        test_value = data.draw(_deploy_time_value_strategy())
        assume(test_value != actual_value)

        body = data.draw(_simple_export_strategy())

        content = (
            f"#!/bin/bash\n\n"
            f'if [[ "${{{var}}}" == "{test_value}" ]]; then\n'
            f"{body}\n"
            f"fi\n"
        )

        conditioner = ConfigConditioner(deploy_time_vars=deploy_vars)
        result = conditioner.condition_file(content)

        # The non-matching branch body must NOT appear in output
        assert body.strip() not in result.output, (
            f"Non-matching branch body found in output.\n"
            f"Body: {body.strip()}\n"
            f"Var: {var}={actual_value}, tested: {test_value}\n"
            f"Output:\n{result.output}"
        )
        # Resolution comment must be present
        assert "# Resolved:" in result.output
        # Eliminated count should be > 0
        assert result.eliminated_branches >= 1

    @given(data=st.data())
    @settings(
        max_examples=100,
        deadline=None,
        suppress_health_check=[HealthCheck.too_slow],
    )
    def test_deploy_time_if_else_selects_correct_branch(self, data):
        """if/else with deploy-time var selects the correct branch.

        **Validates: Requirements 5.1, 5.2, 5.5**
        """
        deploy_vars = data.draw(_deploy_time_vars_dict_strategy())
        var = data.draw(st.sampled_from(list(deploy_vars.keys())))
        actual_value = deploy_vars[var]

        matches = data.draw(st.booleans())
        if matches:
            test_value = actual_value
        else:
            test_value = data.draw(_deploy_time_value_strategy())
            assume(test_value != actual_value)

        true_body = data.draw(_simple_export_strategy())
        else_body = data.draw(_simple_export_strategy())
        # Ensure the two bodies are different so we can distinguish them
        assume(true_body.strip() != else_body.strip())

        content = (
            f"#!/bin/bash\n\n"
            f'if [[ "${{{var}}}" == "{test_value}" ]]; then\n'
            f"{true_body}\n"
            f"else\n"
            f"{else_body}\n"
            f"fi\n"
        )

        conditioner = ConfigConditioner(deploy_time_vars=deploy_vars)
        result = conditioner.condition_file(content)

        if matches:
            # True branch selected
            assert true_body.strip() in result.output
            assert else_body.strip() not in result.output
        else:
            # Else branch selected
            assert else_body.strip() in result.output
            assert true_body.strip() not in result.output

        # Structure removed, comment present
        assert "# Resolved:" in result.output
        assert result.eliminated_branches >= 1

    @given(data=st.data())
    @settings(
        max_examples=100,
        deadline=None,
        suppress_health_check=[HealthCheck.too_slow],
    )
    def test_deploy_time_case_selects_matching_branch(self, data):
        """case block with deploy-time var selects the matching pattern.

        **Validates: Requirements 5.1, 5.2, 5.5**
        """
        deploy_vars = data.draw(_deploy_time_vars_dict_strategy())
        var = data.draw(st.sampled_from(list(deploy_vars.keys())))
        actual_value = deploy_vars[var]

        # Generate a matching body and a non-matching body
        match_body = data.draw(_simple_export_strategy())
        other_body = data.draw(_simple_export_strategy())
        assume(match_body.strip() != other_body.strip())

        # Pick a non-matching value
        other_value = data.draw(_deploy_time_value_strategy())
        assume(other_value != actual_value)

        content = (
            f"#!/bin/bash\n\n"
            f"case ${{{var}}} in\n"
            f"  *{actual_value})\n"
            f"  {match_body}\n"
            f"    ;;\n"
            f"  *{other_value})\n"
            f"  {other_body}\n"
            f"    ;;\n"
            f"esac\n"
        )

        conditioner = ConfigConditioner(deploy_time_vars=deploy_vars)
        result = conditioner.condition_file(content)

        # The matching branch body must appear
        assert match_body.strip() in result.output, (
            f"Matching case branch body not found.\n"
            f"Body: {match_body.strip()}\n"
            f"Var: {var}={actual_value}\n"
            f"Output:\n{result.output}"
        )
        # The non-matching branch body must NOT appear
        assert other_body.strip() not in result.output, (
            f"Non-matching case branch body found in output.\n"
            f"Body: {other_body.strip()}\n"
            f"Output:\n{result.output}"
        )
        # Case structure removed
        assert "esac" not in result.output
        # Resolution comment present
        assert "# Resolved:" in result.output


# ---------------------------------------------------------------------------
# Property 6: Config Conditioner Output Validity
# ---------------------------------------------------------------------------

class TestProperty6OutputValidity:
    """Property 6: Config Conditioner Output Validity.

    For any config file processed by the Config_Conditioner, the output
    SHALL be syntactically valid shell (accepted by bash -n without errors).

    **Validates: Requirements 5.8**
    """

    @given(data=st.data())
    @settings(
        max_examples=100,
        deadline=None,
        suppress_health_check=[HealthCheck.too_slow],
    )
    def test_conditioned_output_passes_bash_n(self, data):
        """All conditioned output is syntactically valid shell.

        **Validates: Requirements 5.8**
        """
        deploy_vars = data.draw(_deploy_time_vars_dict_strategy())
        content = data.draw(_valid_shell_config(deploy_vars))

        conditioner = ConfigConditioner(deploy_time_vars=deploy_vars)
        result = conditioner.condition_file(content)

        assert result.is_valid_shell, (
            f"Conditioned output failed bash -n validation.\n"
            f"Input:\n{content}\n"
            f"Output:\n{result.output}\n"
            f"Syntax error: {conditioner.last_syntax_error}"
        )

    @given(
        runtime_block=_runtime_if_block(),
        deploy_vars=_deploy_time_vars_dict_strategy(),
    )
    @settings(
        max_examples=100,
        deadline=None,
        suppress_health_check=[HealthCheck.too_slow],
    )
    def test_runtime_if_preservation_keeps_valid_shell(
        self, runtime_block, deploy_vars
    ):
        """Preserved runtime if-blocks remain valid shell.

        **Validates: Requirements 5.8**
        """
        content = f"#!/bin/bash\n\n{runtime_block}\n"

        conditioner = ConfigConditioner(deploy_time_vars=deploy_vars)
        result = conditioner.condition_file(content)

        assert result.is_valid_shell, (
            f"Output with preserved runtime block failed bash -n.\n"
            f"Output:\n{result.output}\n"
            f"Syntax error: {conditioner.last_syntax_error}"
        )

    @given(
        runtime_block=_runtime_case_block(),
        deploy_vars=_deploy_time_vars_dict_strategy(),
    )
    @settings(
        max_examples=100,
        deadline=None,
        suppress_health_check=[HealthCheck.too_slow],
    )
    def test_runtime_case_preservation_keeps_valid_shell(
        self, runtime_block, deploy_vars
    ):
        """Preserved runtime case-blocks remain valid shell.

        **Validates: Requirements 5.8**
        """
        content = f"#!/bin/bash\n\n{runtime_block}\n"

        conditioner = ConfigConditioner(deploy_time_vars=deploy_vars)
        result = conditioner.condition_file(content)

        assert result.is_valid_shell, (
            f"Output with preserved runtime case block failed bash -n.\n"
            f"Output:\n{result.output}\n"
            f"Syntax error: {conditioner.last_syntax_error}"
        )
