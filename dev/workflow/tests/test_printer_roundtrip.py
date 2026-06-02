"""Property-based test: Printer Round-Trip (Property 10).

Generates valid in-memory DAG objects using hypothesis, pretty-prints them
to canonical YAML, parses the YAML back into a DAG, and asserts the
resulting DAG is structurally equal to the original.

**Validates: Requirements 10.5**

Traces to: Design Document - Correctness Property 10
  "parse(pretty_print(d)) is structurally equal to d"
"""

from __future__ import annotations

import os
import sys
import tempfile

import hypothesis
from hypothesis import given, settings, HealthCheck
from hypothesis import strategies as st

sys.path.insert(0, os.path.join(os.path.dirname(__file__), ".."))

from deployment.workflow_config import DAG, Edge, MeterDef, TaskNode, parse, pretty_print


# ---------------------------------------------------------------------------
# Hypothesis Strategies for generating valid DAG objects directly
# ---------------------------------------------------------------------------

# Strategy for valid identifiers (task names, family segments, meter names)
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
    # Optionally include a threshold
    has_threshold = draw(st.booleans())
    threshold = draw(st.integers(min_value=min_value, max_value=max_value)) if has_threshold else None
    return MeterDef(name=name, min_value=min_value, max_value=max_value, threshold=threshold)


@st.composite
def _task_node_strategy(draw, family_path: str):
    """Generate a valid TaskNode dataclass instance."""
    name = draw(_identifier)
    jjob = "J" + name.upper()

    # No triggers for generated DAGs (avoids referencing non-existent paths)
    trigger = None
    complete = None

    # Optional events
    events = draw(st.lists(_identifier, min_size=0, max_size=2, unique=True))

    # Optional meters
    meters = draw(st.lists(_meter_def_strategy(), min_size=0, max_size=2))

    # Optional variables (keys are uppercase identifiers, values are simple strings)
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

    # Optional resources (simple key-value pairs)
    has_resources = draw(st.booleans())
    resources = {}
    if has_resources:
        resource_key = draw(st.sampled_from(["walltime", "memory", "nodes", "queue"]))
        resource_val = draw(
            st.text(
                alphabet="abcdefghijklmnopqrstuvwxyz0123456789:",
                min_size=1,
                max_size=10,
            )
        )
        resources[resource_key] = resource_val

    return TaskNode(
        name=name,
        family_path=family_path,
        jjob=jjob,
        trigger=trigger,
        complete=complete,
        events=events,
        meters=meters,
        variables=variables,
        resources=resources,
    )


@st.composite
def _dag_strategy(draw):
    """Generate a valid DAG object with unique task paths.

    Produces a DAG with:
    - A suite name
    - 1-5 families, each with 1-4 tasks
    - No edges (since tasks have no triggers referencing other tasks)
    - Unique full paths across all tasks
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

            # Update the task with the unique name and matching jjob
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
            # Ensure globally unique full paths
            if full_path in used_paths:
                continue
            used_paths.add(full_path)
            nodes[full_path] = task

    # No edges since we don't generate triggers
    edges = []

    return DAG(suite_name=suite_name, nodes=nodes, edges=edges)


# ---------------------------------------------------------------------------
# Structural equality helpers
# ---------------------------------------------------------------------------


def _dag_nodes_equal(dag1: DAG, dag2: DAG) -> bool:
    """Check that two DAGs have structurally equal node sets.

    Compares:
    - Same set of node keys (full paths)
    - For each node: name, family_path, jjob, trigger, complete, events, meters, variables, resources
    """
    if set(dag1.nodes.keys()) != set(dag2.nodes.keys()):
        return False

    for key in dag1.nodes:
        n1 = dag1.nodes[key]
        n2 = dag2.nodes[key]
        if n1.name != n2.name:
            return False
        if n1.family_path != n2.family_path:
            return False
        if n1.jjob != n2.jjob:
            return False
        if n1.trigger != n2.trigger:
            return False
        if n1.complete != n2.complete:
            return False
        if sorted(n1.events) != sorted(n2.events):
            return False
        if n1.meters != n2.meters:
            return False
        if n1.variables != n2.variables:
            return False
        if n1.resources != n2.resources:
            return False

    return True


def _dag_edges_equal(dag1: DAG, dag2: DAG) -> bool:
    """Check that two DAGs have structurally equal edge sets.

    Compares edges as sets of (source, target, kind) tuples.
    """
    def edge_key(e: Edge) -> tuple:
        return (e.source, e.target, e.kind)

    edges1 = sorted(edge_key(e) for e in dag1.edges)
    edges2 = sorted(edge_key(e) for e in dag2.edges)
    return edges1 == edges2


def _dags_structurally_equal(dag1: DAG, dag2: DAG) -> bool:
    """Check that two DAGs are structurally equal.

    Two DAGs are structurally equal if they have:
    - The same suite name
    - The same set of nodes with identical attributes
    - The same set of edges (source, target, kind)
    """
    if dag1.suite_name != dag2.suite_name:
        return False
    if not _dag_nodes_equal(dag1, dag2):
        return False
    if not _dag_edges_equal(dag1, dag2):
        return False
    return True


# ---------------------------------------------------------------------------
# Property Test: Printer Round-Trip (Property 10)
# ---------------------------------------------------------------------------


@given(dag=_dag_strategy())
@settings(
    max_examples=50,
    deadline=None,
    suppress_health_check=[HealthCheck.too_slow],
)
def test_printer_roundtrip_property(dag: DAG):
    """Property 10: parse(pretty_print(d)) is structurally equal to d.

    **Validates: Requirements 10.5**

    Steps:
    1. Generate a valid in-memory DAG object
    2. Call pretty_print() to serialize it to canonical YAML
    3. Write the YAML to a temp file and parse it with parse()
    4. Assert the parsed DAG is structurally equal to the original
    """
    # Step 2: Pretty-print the DAG to canonical YAML
    printed_yaml = pretty_print(dag)

    # Step 3: Write to temp file and parse
    tmp = tempfile.NamedTemporaryFile(mode="w", suffix=".yaml", delete=False)
    try:
        tmp.write(printed_yaml)
        tmp.close()

        parsed_dag = parse(tmp.name)

        # Step 4: Assert structural equality
        assert dag.suite_name == parsed_dag.suite_name, (
            f"Suite names differ: {dag.suite_name!r} vs {parsed_dag.suite_name!r}"
        )
        assert set(dag.nodes.keys()) == set(parsed_dag.nodes.keys()), (
            f"Node sets differ:\n"
            f"  Only in original: {set(dag.nodes.keys()) - set(parsed_dag.nodes.keys())}\n"
            f"  Only in parsed: {set(parsed_dag.nodes.keys()) - set(dag.nodes.keys())}"
        )
        assert _dags_structurally_equal(dag, parsed_dag), (
            "DAGs are not structurally equal after printer round-trip.\n"
            f"Original suite: {dag.suite_name}\n"
            f"Parsed suite: {parsed_dag.suite_name}\n"
            f"Original nodes: {list(dag.nodes.keys())}\n"
            f"Parsed nodes: {list(parsed_dag.nodes.keys())}\n"
            f"Original edges: {[(e.source, e.target, e.kind) for e in dag.edges]}\n"
            f"Parsed edges: {[(e.source, e.target, e.kind) for e in parsed_dag.edges]}"
        )
    finally:
        os.unlink(tmp.name)
