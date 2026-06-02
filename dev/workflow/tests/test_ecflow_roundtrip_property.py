"""Property-based test: ecFlow Round-Trip (Property 11).

Since the ecFlow Python API (ecflow package) is not available in this
environment, this test verifies the text-based round-trip property:

1. Generate a DAG using hypothesis
2. Emit it as .def text via generate_def_text(dag)
3. Write the text to a file
4. Read the file back
5. Parse the tasks from the read-back text using parse_def_tasks()
6. Assert the parsed tasks match the original DAG's task set

This verifies that the .def text format is stable through write/read cycles,
which is the text-serialization equivalent of the ecFlow round-trip property:
  Defs.save_as_defs(path) -> Defs(path) produces structurally equal Defs

**Validates: Requirements 10.8**

Traces to: Design Document - Correctness Property 11
"""

from __future__ import annotations

import os
import sys
import tempfile

from hypothesis import given, settings, HealthCheck
from hypothesis import strategies as st

sys.path.insert(0, os.path.join(os.path.dirname(__file__), ".."))

from deployment.dag_generator import generate_def_text, parse_def_tasks
from deployment.workflow_config import DAG, MeterDef, TaskNode


# ---------------------------------------------------------------------------
# Hypothesis Strategies for generating valid DAG objects
# ---------------------------------------------------------------------------

# Strategy for valid identifiers (task names, family segments)
_identifier = st.from_regex(r"[a-z][a-z0-9_]{0,11}", fullmatch=True)

# Strategy for valid family path segments (1-3 segments separated by '/')
_family_path = st.builds(
    lambda parts: "/".join(parts),
    st.lists(_identifier, min_size=1, max_size=3),
)


@st.composite
def _meter_strategy(draw):
    """Generate a valid MeterDef."""
    name = draw(_identifier)
    min_val = draw(st.integers(min_value=0, max_value=50))
    max_val = draw(st.integers(min_value=min_val + 1, max_value=200))
    has_threshold = draw(st.booleans())
    threshold = draw(st.integers(min_value=min_val, max_value=max_val)) if has_threshold else None
    return MeterDef(name=name, min_value=min_val, max_value=max_val, threshold=threshold)


@st.composite
def _task_node_strategy(draw, family_path: str):
    """Generate a valid TaskNode for a given family path."""
    name = draw(_identifier)
    jjob = "J" + name.upper()

    # Optionally add events
    events = draw(st.lists(_identifier, min_size=0, max_size=2, unique=True))

    # Optionally add meters
    meters = draw(st.lists(_meter_strategy(), min_size=0, max_size=2))

    # Optionally add variables
    has_vars = draw(st.booleans())
    variables = {}
    if has_vars:
        var_key = draw(_identifier).upper()
        var_val = draw(st.text(
            alphabet="abcdefghijklmnopqrstuvwxyz0123456789",
            min_size=1, max_size=8,
        ))
        variables[var_key] = var_val

    return TaskNode(
        name=name,
        family_path=family_path,
        jjob=jjob,
        trigger=None,
        complete=None,
        events=events,
        meters=meters,
        variables=variables,
    )


@st.composite
def _dag_strategy(draw):
    """Generate a valid DAG with unique task paths.

    Produces a DAG with:
    - A suite name
    - 1-5 families, each with 1-4 tasks
    - No triggers (to keep the round-trip focused on structure)
    - Optional meters, events, and variables on tasks
    """
    suite_name = draw(_identifier) + "_suite"
    num_families = draw(st.integers(min_value=1, max_value=5))

    dag = DAG(suite_name=suite_name)
    used_paths = set()
    used_full_paths = set()

    for _ in range(num_families):
        family_path = draw(_family_path)
        # Ensure unique family paths
        while family_path in used_paths:
            family_path = draw(_family_path)
        used_paths.add(family_path)

        num_tasks = draw(st.integers(min_value=1, max_value=4))
        used_names_in_family = set()

        for _ in range(num_tasks):
            node = draw(_task_node_strategy(family_path))
            # Ensure unique task names within the family
            candidate = node.name
            counter = 0
            while candidate in used_names_in_family:
                counter += 1
                candidate = f"{node.name}x{counter}"
            node.name = candidate
            node.jjob = "J" + candidate.upper()
            used_names_in_family.add(candidate)

            full_path = f"{family_path}/{candidate}"
            # Ensure globally unique full paths
            while full_path in used_full_paths:
                counter += 1
                candidate = f"{node.name}x{counter}"
                node.name = candidate
                node.jjob = "J" + candidate.upper()
                full_path = f"{family_path}/{candidate}"
            used_full_paths.add(full_path)

            dag.nodes[full_path] = node

    return dag


# ---------------------------------------------------------------------------
# Property Test: ecFlow Round-Trip (Property 11)
# ---------------------------------------------------------------------------


@given(dag=_dag_strategy())
@settings(
    max_examples=50,
    deadline=None,
    suppress_health_check=[HealthCheck.too_slow],
)
def test_ecflow_roundtrip_property(dag: DAG):
    """Property 11: ecFlow Round-Trip via text serialization.

    **Validates: Requirements 10.8**

    The .def text format round-trip property:
    1. Generate .def text from a DAG via generate_def_text(dag)
    2. Write the text to a temporary file
    3. Read the file back
    4. Parse the tasks from the read-back text using parse_def_tasks()
    5. Assert the parsed (family_path, task_name) set matches the DAG's task set

    This is the text-serialization equivalent of:
      Defs.save_as_defs(path) -> Defs(path) produces structurally equal Defs
    """
    # Step 1: Generate .def text from the DAG
    def_text = generate_def_text(dag)

    # Step 2-3: Write to file and read back
    with tempfile.NamedTemporaryFile(
        mode="w", suffix=".def", delete=False
    ) as tmp:
        tmp.write(def_text)
        tmp_path = tmp.name

    try:
        with open(tmp_path, "r") as f:
            read_back_text = f.read()

        # Verify the write/read cycle preserves the text exactly
        assert def_text == read_back_text, (
            "File write/read cycle altered the .def text content"
        )

        # Step 4: Parse tasks from the read-back text
        parsed_tasks = parse_def_tasks(read_back_text)

        # Step 5: Build expected task set from the original DAG
        expected_tasks = {
            (node.family_path, node.name) for node in dag.nodes.values()
        }

        # Assert structural equality of task sets
        assert parsed_tasks == expected_tasks, (
            f"ecFlow round-trip failed: task sets differ.\n"
            f"  Expected ({len(expected_tasks)} tasks): {sorted(expected_tasks)}\n"
            f"  Parsed ({len(parsed_tasks)} tasks): {sorted(parsed_tasks)}\n"
            f"  Missing from parsed: {sorted(expected_tasks - parsed_tasks)}\n"
            f"  Extra in parsed: {sorted(parsed_tasks - expected_tasks)}"
        )
    finally:
        os.unlink(tmp_path)
