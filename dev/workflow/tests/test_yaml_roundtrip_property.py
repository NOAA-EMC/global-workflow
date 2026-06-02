"""Property-based test: YAML Round-Trip (Property 9 variant for Template_Renderer).

Generates random valid configuration dicts using hypothesis, serializes them
with save_as_yaml(), reads back with yaml.safe_load(), and asserts the loaded
dict equals the original.

**Validates: Requirements 4.9**

Traces to: Design Document - Correctness Property 9 (Template_Renderer variant)
  "parse_yaml(pretty_print(cfg)) returns a tree equal to cfg"
"""

from __future__ import annotations

import os
import sys
import tempfile
from pathlib import Path

import yaml
from hypothesis import given, settings, HealthCheck
from hypothesis import strategies as st

sys.path.insert(0, os.path.join(os.path.dirname(__file__), ".."))

from deployment.template_renderer import save_as_yaml


# ---------------------------------------------------------------------------
# Hypothesis Strategies for generating valid YAML-safe configuration dicts
# ---------------------------------------------------------------------------

# Leaf values that are safe for YAML round-trip via yaml.dump / yaml.safe_load.
# We avoid floats because of precision issues (e.g. 0.1 + 0.2 != 0.3),
# and avoid None because yaml.dump serializes it as "null" which round-trips fine
# but we include it explicitly.
_yaml_safe_scalars = (
    st.text(
        alphabet=st.characters(
            whitelist_categories=("L", "N", "P", "Z"),
            blacklist_characters="\x00",
        ),
        min_size=0,
        max_size=50,
    )
    | st.integers(min_value=-(2**53), max_value=2**53)
    | st.booleans()
    | st.none()
)


def _yaml_safe_values(max_depth: int = 3):
    """Recursive strategy for YAML-safe values: scalars, lists, and dicts.

    Limits depth to avoid excessively deep structures that slow tests.
    """
    if max_depth <= 0:
        return _yaml_safe_scalars

    return st.one_of(
        _yaml_safe_scalars,
        st.lists(
            st.deferred(lambda: _yaml_safe_values(max_depth - 1)),
            min_size=0,
            max_size=5,
        ),
        st.dictionaries(
            keys=st.text(
                alphabet=st.characters(
                    whitelist_categories=("L", "N"),
                    blacklist_characters="\x00",
                ),
                min_size=1,
                max_size=20,
            ),
            values=st.deferred(lambda: _yaml_safe_values(max_depth - 1)),
            min_size=0,
            max_size=5,
        ),
    )


# Strategy for the top-level config: always a non-empty dict
_config_strategy = st.dictionaries(
    keys=st.text(
        alphabet=st.characters(
            whitelist_categories=("L", "N"),
            blacklist_characters="\x00",
        ),
        min_size=1,
        max_size=20,
    ),
    values=_yaml_safe_values(max_depth=3),
    min_size=1,
    max_size=8,
)


# ---------------------------------------------------------------------------
# Property Test: YAML Round-Trip (Property 9 variant for Template_Renderer)
# ---------------------------------------------------------------------------


@given(cfg=_config_strategy)
@settings(
    max_examples=100,
    deadline=None,
    suppress_health_check=[HealthCheck.too_slow],
)
def test_yaml_roundtrip_property(cfg: dict):
    """Property 9 (Template_Renderer variant): parse_yaml(pretty_print(cfg)) == cfg.

    **Validates: Requirements 4.9**

    For any valid configuration dict, serializing with save_as_yaml() and
    reading back with yaml.safe_load() must produce a dict equal to the original.

    Steps:
    1. Generate a random valid configuration dict (nested dicts, lists, strings, ints, bools)
    2. Call save_as_yaml(cfg, path) to serialize to a file
    3. Read back with yaml.safe_load()
    4. Assert the loaded dict equals the original
    """
    with tempfile.TemporaryDirectory() as tmpdir:
        path = Path(tmpdir) / "roundtrip.yaml"

        # Serialize
        save_as_yaml(cfg, path)

        # Read back
        loaded = yaml.safe_load(path.read_text(encoding="utf-8"))

        # Assert round-trip equality
        assert loaded == cfg, (
            f"YAML round-trip failed.\n"
            f"Original: {cfg!r}\n"
            f"Loaded:   {loaded!r}"
        )
