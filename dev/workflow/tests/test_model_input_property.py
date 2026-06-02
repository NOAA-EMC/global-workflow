"""Property-based tests for Model Input rendering guarantees.

Property 7: Model Input Zero-Token Guarantee
  For any Jinja2 template where every referenced variable has a value in the
  context, the rendered output SHALL contain zero unresolved Jinja2 tokens
  (no '{{', '{%', or '{#' patterns).

Property 8: Model Input Round-Trip Fidelity
  For any rendered model configuration file, the output SHALL be parseable
  by the appropriate format validator with zero errors.

**Validates: Requirements 6.4, 14.1, 14.2, 14.3, 14.4**

Traces to: Design Document - Correctness Properties 7 and 8
"""

from __future__ import annotations

import os
import re
import sys

from hypothesis import given, settings, HealthCheck, assume
from hypothesis import strategies as st

sys.path.insert(0, os.path.join(os.path.dirname(__file__), ".."))

from jinja2 import Environment, StrictUndefined

from deployment.validators.namelist import NamelistValidator
from deployment.validators.mom6_parameter import MOM6ParameterValidator


# ---------------------------------------------------------------------------
# Unresolved token patterns (same as in model_config_renderer.py)
# ---------------------------------------------------------------------------

_UNRESOLVED_TOKEN_PATTERNS = [
    re.compile(r"\{\{"),   # Variable expression
    re.compile(r"\{%"),    # Block/statement tag
    re.compile(r"\{#"),    # Comment tag
]


def _has_unresolved_tokens(content: str) -> bool:
    """Check if content has any unresolved Jinja2 tokens."""
    for pattern in _UNRESOLVED_TOKEN_PATTERNS:
        if pattern.search(content):
            return True
    return False


# ---------------------------------------------------------------------------
# Hypothesis Strategies for Property 7: Zero-Token Guarantee
# ---------------------------------------------------------------------------

# Strategy for valid Jinja2 variable names (identifiers)
_var_name = st.from_regex(r"[a-z][a-z0-9_]{0,9}", fullmatch=True)

# Strategy for simple values (strings that won't introduce Jinja2 tokens)
_simple_value = st.from_regex(r"[A-Za-z0-9_./ -]{1,20}", fullmatch=True)


@st.composite
def _template_with_complete_context(draw):
    """Generate a Jinja2 template string and a complete context providing all variables.

    Strategy:
    - Generate 1-8 variable names
    - Build a template that uses all variables in simple {{ var }} expressions
    - Build a context that provides values for ALL variables
    - Include some static text between template expressions

    This ensures the context is "complete" — every variable in the template
    has a corresponding value in the context.

    Returns:
        Tuple of (template_string, context_dict)
    """
    num_vars = draw(st.integers(min_value=1, max_value=8))
    var_names = draw(
        st.lists(
            _var_name,
            min_size=num_vars,
            max_size=num_vars,
            unique=True,
        )
    )

    # Generate values for each variable
    context = {}
    for var in var_names:
        context[var] = draw(_simple_value)

    # Build a template using these variables with static text between them
    template_parts = []
    for i, var in enumerate(var_names):
        # Add some static text before the variable expression
        static_text = draw(st.from_regex(r"[a-zA-Z0-9_ =\n]{0,15}", fullmatch=True))
        template_parts.append(static_text)
        # Use the variable in a {{ var }} expression
        template_parts.append("{{ " + var + " }}")

    # Add optional trailing static text
    trailing = draw(st.from_regex(r"[a-zA-Z0-9_ =\n]{0,10}", fullmatch=True))
    template_parts.append(trailing)

    template_string = "".join(template_parts)
    return template_string, context


@st.composite
def _template_with_conditionals_and_complete_context(draw):
    """Generate a Jinja2 template with if/for blocks and a complete context.

    Tests that block tags ({%...%}) are also resolved when the context is
    complete. Generates simple if-blocks and for-loops.

    Returns:
        Tuple of (template_string, context_dict)
    """
    var_name_val = draw(_var_name)
    list_var = draw(_var_name.filter(lambda x: x != var_name_val))
    value = draw(_simple_value)

    # Generate a list of items for iteration
    items = draw(st.lists(_simple_value, min_size=1, max_size=4))

    context = {
        var_name_val: value,
        list_var: items,
    }

    # Build template with if-block and for-loop
    template_string = (
        f"{{% if {var_name_val} %}}"
        f"value is {{{{ {var_name_val} }}}}\n"
        f"{{% endif %}}"
        f"{{% for item in {list_var} %}}"
        f"item={{{{ item }}}}\n"
        f"{{% endfor %}}"
    )

    return template_string, context


# ---------------------------------------------------------------------------
# Hypothesis Strategies for Property 8: Round-Trip Fidelity
# ---------------------------------------------------------------------------

# Strategy for valid Fortran namelist group names
_nml_group_name = st.from_regex(r"[a-z][a-z0-9_]{0,12}", fullmatch=True)

# Strategy for valid namelist variable names
_nml_var_name = st.from_regex(r"[a-z][a-z0-9_]{0,10}", fullmatch=True)

# Strategy for Fortran namelist values
_nml_value = st.one_of(
    # Integer values
    st.integers(min_value=-9999, max_value=9999).map(str),
    # Float values
    st.floats(
        min_value=-999.0, max_value=999.0,
        allow_nan=False, allow_infinity=False,
    ).map(lambda f: f"{f:.4f}"),
    # Fortran boolean values
    st.sampled_from([".true.", ".false."]),
    # Quoted string values
    st.from_regex(r"[a-zA-Z0-9_./]{1,12}", fullmatch=True).map(
        lambda s: f"'{s}'"
    ),
)


@st.composite
def _valid_fortran_namelist(draw):
    """Generate valid Fortran namelist content.

    A valid namelist has:
    - One or more &group_name blocks
    - Each block contains variable = value assignments
    - Each block is terminated by /
    - Comments start with !

    Returns:
        String containing valid Fortran namelist content.
    """
    num_groups = draw(st.integers(min_value=1, max_value=4))
    group_names = draw(
        st.lists(
            _nml_group_name,
            min_size=num_groups,
            max_size=num_groups,
            unique=True,
        )
    )

    lines = []
    for group in group_names:
        # Optional comment before group
        if draw(st.booleans()):
            lines.append(f"! {group} configuration")

        lines.append(f"&{group}")

        # Generate 1-5 variable assignments per group
        num_vars = draw(st.integers(min_value=1, max_value=5))
        var_names = draw(
            st.lists(
                _nml_var_name,
                min_size=num_vars,
                max_size=num_vars,
                unique=True,
            )
        )

        for var in var_names:
            val = draw(_nml_value)
            lines.append(f"  {var} = {val}")

        lines.append("/")
        lines.append("")  # Blank line between groups

    return "\n".join(lines)


# Strategy for valid MOM6 parameter names
_mom6_param_name = st.from_regex(r"[A-Z][A-Z0-9_]{0,15}", fullmatch=True)

# Strategy for MOM6 parameter values
_mom6_value = st.one_of(
    # Numeric values
    st.integers(min_value=-9999, max_value=9999).map(str),
    st.floats(
        min_value=-999.0, max_value=999.0,
        allow_nan=False, allow_infinity=False,
    ).map(lambda f: f"{f:.6f}"),
    # Boolean-like values
    st.sampled_from(["True", "False"]),
    # String values (unquoted in MOM6 format)
    st.from_regex(r"[a-zA-Z0-9_./]{1,12}", fullmatch=True),
)


@st.composite
def _valid_mom6_parameters(draw):
    """Generate valid MOM6 parameter file content.

    MOM6 parameter files use:
    - `! section` comment headers
    - `PARAM = VALUE` assignments (uppercase parameter names)
    - Empty/blank lines

    Returns:
        String containing valid MOM6 parameter content.
    """
    num_params = draw(st.integers(min_value=1, max_value=8))
    param_names = draw(
        st.lists(
            _mom6_param_name,
            min_size=num_params,
            max_size=num_params,
            unique=True,
        )
    )

    lines = []
    # Optional section comment at the top
    if draw(st.booleans()):
        lines.append("! === Configuration Parameters ===")
        lines.append("")

    for param in param_names:
        # Optionally add a comment before the parameter
        if draw(st.booleans()):
            lines.append(f"! {param} setting")
        val = draw(_mom6_value)
        lines.append(f"{param} = {val}")

    return "\n".join(lines)


# ---------------------------------------------------------------------------
# Property Test 7: Model Input Zero-Token Guarantee
# ---------------------------------------------------------------------------


@given(data=_template_with_complete_context())
@settings(
    max_examples=100,
    deadline=None,
    suppress_health_check=[HealthCheck.too_slow],
)
def test_zero_token_guarantee_simple(data):
    """Property 7: Zero-Token Guarantee for simple variable expressions.

    **Validates: Requirements 6.4, 14.1**

    For any Jinja2 template where every referenced variable has a value in the
    context, the rendered output SHALL contain zero unresolved Jinja2 tokens.
    """
    template_string, context = data

    # Render the template with a strict Jinja2 environment
    env = Environment(undefined=StrictUndefined)
    template = env.from_string(template_string)
    rendered = template.render(context)

    # The rendered output must contain no unresolved Jinja2 tokens
    assert not _has_unresolved_tokens(rendered), (
        f"Rendered output contains unresolved Jinja2 tokens.\n"
        f"Template: {template_string!r}\n"
        f"Context: {context}\n"
        f"Rendered: {rendered!r}"
    )


@given(data=_template_with_conditionals_and_complete_context())
@settings(
    max_examples=100,
    deadline=None,
    suppress_health_check=[HealthCheck.too_slow],
)
def test_zero_token_guarantee_with_blocks(data):
    """Property 7: Zero-Token Guarantee for block tags (if/for).

    **Validates: Requirements 6.4, 14.1**

    For any Jinja2 template with if/for blocks where the context is complete,
    the rendered output SHALL contain no unresolved block tags ({%...%}) or
    variable expressions ({{...}}).
    """
    template_string, context = data

    # Render the template with a strict Jinja2 environment
    env = Environment(undefined=StrictUndefined)
    template = env.from_string(template_string)
    rendered = template.render(context)

    # The rendered output must contain no unresolved Jinja2 tokens
    assert not _has_unresolved_tokens(rendered), (
        f"Rendered output contains unresolved Jinja2 tokens.\n"
        f"Template: {template_string!r}\n"
        f"Context: {context}\n"
        f"Rendered: {rendered!r}"
    )


# ---------------------------------------------------------------------------
# Property Test 8: Model Input Round-Trip Fidelity
# ---------------------------------------------------------------------------


@given(namelist_content=_valid_fortran_namelist())
@settings(
    max_examples=100,
    deadline=None,
    suppress_health_check=[HealthCheck.too_slow],
)
def test_round_trip_fidelity_namelist(namelist_content):
    """Property 8: Round-Trip Fidelity for Fortran namelists.

    **Validates: Requirements 14.1, 14.2, 14.3, 14.4**

    For any valid Fortran namelist content, the NamelistValidator SHALL
    report zero errors, confirming the content is parseable.
    """
    validator = NamelistValidator()
    errors = validator.validate(namelist_content, "test_input.nml")

    assert errors == [], (
        f"NamelistValidator found errors in valid namelist content.\n"
        f"Errors: {errors}\n"
        f"Content:\n{namelist_content}"
    )


@given(mom6_content=_valid_mom6_parameters())
@settings(
    max_examples=100,
    deadline=None,
    suppress_health_check=[HealthCheck.too_slow],
)
def test_round_trip_fidelity_mom6(mom6_content):
    """Property 8: Round-Trip Fidelity for MOM6 parameter files.

    **Validates: Requirements 14.1, 14.2, 14.3, 14.4**

    For any valid MOM6 parameter file content, the MOM6ParameterValidator
    SHALL report zero errors, confirming the content is parseable.
    """
    validator = MOM6ParameterValidator()
    errors = validator.validate(mom6_content, "test_MOM_input")

    assert errors == [], (
        f"MOM6ParameterValidator found errors in valid MOM6 content.\n"
        f"Errors: {errors}\n"
        f"Content:\n{mom6_content}"
    )


@given(data=_template_with_complete_context())
@settings(
    max_examples=100,
    deadline=None,
    suppress_health_check=[HealthCheck.too_slow],
)
def test_round_trip_fidelity_rendered_namelist(data):
    """Property 8: Round-Trip Fidelity for rendered templates as namelists.

    **Validates: Requirements 14.1, 14.2, 14.3, 14.4**

    Generate a valid Fortran namelist template with Jinja2 variables,
    render it with a complete context, then validate the output with
    NamelistValidator. The rendered output must be parseable.
    """
    _, context = data

    # Build a namelist template that uses the context values
    # Each variable becomes a namelist assignment
    lines = ["&rendered_group"]
    for var_name, value in context.items():
        # Use Jinja2 to inject values into namelist assignments
        lines.append(f"  {var_name} = '{{{{ {var_name} }}}}'")
    lines.append("/")
    template_string = "\n".join(lines)

    # Render with Jinja2
    env = Environment(undefined=StrictUndefined)
    template = env.from_string(template_string)
    rendered = template.render(context)

    # Validate the rendered output as a Fortran namelist
    validator = NamelistValidator()
    errors = validator.validate(rendered, "test_rendered.nml")

    assert errors == [], (
        f"NamelistValidator found errors in rendered namelist.\n"
        f"Errors: {errors}\n"
        f"Template: {template_string!r}\n"
        f"Rendered:\n{rendered}"
    )
