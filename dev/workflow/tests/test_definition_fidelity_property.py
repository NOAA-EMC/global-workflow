"""Property-based test: Definition Fidelity (Property 13).

Generates random valid DAG objects using hypothesis, emits ecFlow .def text
via generate_def_text(), parses the emitted text with parse_def_tasks(), and
asserts the extracted set of (family_path, task_name) pairs equals the set of
TaskNodes in the source DAG.

**Validates: Requirements 10.7**

Traces to: Design Document - Correctness Property 13
  "The set of (family-path, task-name) pairs in the emitted ecFlow Defs
   equals the set of TaskNodes in the source DAG."
"""

from __future__ import annotations

import os
import sys

from hypothesis import given, settings, HealthCheck
from hypothesis import strategies as st

sys.path.insert(0, os.path.join(os.path.dirname(__file__), ".."))

from deployment.workflow_config import DAG, MeterDef, TaskNode
from deployment.dag_generator import generate_def_text, parse_def_tasks


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
def _meter_def_strategy(draw):
    """Generate a valid MeterDef named tuple."""
    name = draw(_identifier)
    min_value = draw(st.integers(min_value=0, max_value=50))
    max_value = draw(st.integers(min_value=min_value + 1, max_value=200))
    has_threshold = draw(st.booleans())
    threshold = (
        draw(st.integers(min_value=min_value, max_value=max_value))
        if has_threshold
        else None
    )
    return MeterDef(
        name=name, min_value=min_value, max_value=max_value, threshold=threshold
    )


@st.composite
def _task_node_strategy(draw, family_path: str):
    """Generate a valid TaskNode for a given family_path."""
    name = draw(_identifier)
    jjob = "J" + name.upper()

    # Optional events
    events = draw(st.lists(_identifier, min_size=0, max_size=2, unique=True))

    # Optional meters
    meters = draw(st.lists(_meter_def_strategy(), min_size=0, max_size=2))

    # Optional variables
    has_vars = draw(st.booleans())
    variables = {}
    if has_vars:
        num_vars = draw(st.integers(min_value=1, max_value=3))
        for _ in range(num_vars):
            var_key = draw(_identifier).upper()
            var_val = draw(
                st.text(
                    alphabet="abcdefghijklmnopqrstuvwxyz0123456789_",
                    min_size=1,
                    max_size=8,
                )
            )
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
        resources={},
    )


@st.composite
def _dag_strategy(draw):
    """Generate a valid DAG object with multiple families and tasks.

    Produces a DAG with:
    - A suite name
    - 1-5 families, each with 1-4 tasks
    - Unique full paths across all tasks
    - No edges (definition fidelity only checks node presence)
    """
    suite_name = draw(_identifier) + "_suite"
    num_families = draw(st.integers(min_value=1, max_value=5))

    nodes = {}
    used_paths = set()

    for _ in range(num_families):
        family_path = draw(_family_path)
        # Ensure unique family paths by appending a suffix if needed
        base_path = family_path
        counter = 0
        while family_path in [n.family_path for n in nodes.values()]:
            counter += 1
            family_path = f"{base_path}/x{counter}"

        num_tasks = draw(st.integers(min_value=1, max_value=4))
        used_names = set()

        for _ in range(num_tasks):
            task = draw(_task_node_strategy(family_path))
            # Ensure unique task names within the family
            base_name = task.name
            candidate = base_name
            name_counter = 0
            while candidate in used_names:
                name_counter += 1
                candidate = f"{base_name}x{name_counter}"

            task = TaskNode(
                name=candidate,
                family_path=family_path,
                jjob="J" + candidate.upper(),
                trigger=task.trigger,
                complete=task.complete,
                events=task.events,
                meters=task.meters,
                variables=task.variables,
                resources=task.resources,
            )
            used_names.add(candidate)

            full_path = f"{family_path}/{candidate}"
            if full_path in used_paths:
                continue
            used_paths.add(full_path)
            nodes[full_path] = task

    return DAG(suite_name=suite_name, nodes=nodes, edges=[])


# ---------------------------------------------------------------------------
# Property Test: Definition Fidelity (Property 13)
# ---------------------------------------------------------------------------


@given(dag=_dag_strategy())
@settings(
    max_examples=200,
    deadline=None,
    suppress_health_check=[HealthCheck.too_slow],
)
def test_definition_fidelity_property(dag: DAG):
    """Property 13: (family_path, task_name) in emitted Defs == TaskNodes in source DAG.

    **Validates: Requirements 10.7**

    Steps:
    1. Generate a random valid DAG object with multiple families and tasks
    2. Call generate_def_text(dag) to emit the ecFlow .def text
    3. Call parse_def_tasks(text) to extract (family_path, task_name) pairs
    4. Assert the extracted set equals {(node.family_path, node.name) for node in dag.nodes.values()}
    """
    # Step 2: Generate the .def text from the DAG
    def_text = generate_def_text(dag)

    # Step 3: Parse the .def text to extract (family_path, task_name) pairs
    parsed_tasks = parse_def_tasks(def_text)

    # Step 4: Build the expected set from the source DAG
    expected_tasks = {
        (node.family_path, node.name) for node in dag.nodes.values()
    }

    # Assert fidelity: emitted tasks == source DAG tasks
    assert parsed_tasks == expected_tasks, (
        "Definition fidelity violated: emitted .def tasks do not match source DAG.\n"
        f"Only in emitted .def (not in DAG): {parsed_tasks - expected_tasks}\n"
        f"Only in source DAG (not in .def): {expected_tasks - parsed_tasks}\n"
        f"Suite: {dag.suite_name}\n"
        f"DAG nodes: {list(dag.nodes.keys())}\n"
        f"Parsed from .def: {sorted(parsed_tasks)}\n"
        f"Expected from DAG: {sorted(expected_tasks)}"
    )
