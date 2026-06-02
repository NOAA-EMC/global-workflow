"""Property-based test: Parser Round-Trip (Property 9).

Generates valid Workflow_Configuration YAML structures using hypothesis,
parses them, pretty-prints the resulting DAG, re-parses the pretty-printed
output, and asserts the two DAGs are structurally equal.

**Validates: Requirements 10.4**

Traces to: Design Document - Correctness Property 9
  "pretty_print(parse(f)) parses to a DAG structurally equal to parse(f)"
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
# Hypothesis Strategies for generating valid Workflow_Configuration YAML
# ---------------------------------------------------------------------------

# Strategy for valid identifiers (used in task names, family segments, etc.)
_identifier = st.from_regex(r"[a-z][a-z0-9_]{0,11}", fullmatch=True)

# Strategy for valid family path segments (1-3 segments separated by '/')
_family_path = st.builds(
    lambda parts: "/".join(parts),
    st.lists(_identifier, min_size=1, max_size=3),
)


@st.composite
def _task_strategy(draw, family_path: str):
    """Generate a single valid task definition dict for YAML."""
    name = draw(_identifier)
    jjob = "J" + name.upper()
    # Optionally add a trigger referencing a task in the same family
    has_trigger = draw(st.booleans())
    trigger = "" if not has_trigger else ""
    # Optionally add events
    events = draw(st.lists(_identifier, min_size=0, max_size=2, unique=True))
    # Optionally add meters
    has_meter = draw(st.booleans())
    meters = []
    if has_meter:
        meter_name = draw(_identifier)
        min_val = draw(st.integers(min_value=0, max_value=50))
        max_val = draw(st.integers(min_value=min_val + 1, max_value=200))
        meters.append({"name": meter_name, "min": min_val, "max": max_val})
    # Optionally add variables
    has_vars = draw(st.booleans())
    variables = {}
    if has_vars:
        var_key = draw(_identifier).upper()
        var_val = draw(st.text(alphabet="abcdefghijklmnopqrstuvwxyz0123456789", min_size=1, max_size=8))
        variables[var_key] = var_val

    task_def = {"name": name, "jjob": jjob}
    if trigger:
        task_def["trigger"] = trigger
    if events:
        task_def["events"] = events
    if meters:
        task_def["meters"] = meters
    if variables:
        task_def["variables"] = variables
    return task_def


@st.composite
def _family_strategy(draw):
    """Generate a single valid family definition dict for YAML."""
    path = draw(_family_path)
    # Generate 1-4 tasks per family, ensuring unique names within the family
    num_tasks = draw(st.integers(min_value=1, max_value=4))
    tasks = []
    used_names = set()
    for _ in range(num_tasks):
        task = draw(_task_strategy(path))
        # Ensure unique task names within a family by appending a counter
        base_name = task["name"]
        candidate = base_name
        counter = 0
        while candidate in used_names:
            counter += 1
            candidate = f"{base_name}x{counter}"
        task["name"] = candidate
        used_names.add(candidate)
        task["jjob"] = "J" + task["name"].upper()
        tasks.append(task)
    return {"path": path, "tasks": tasks}


@st.composite
def _workflow_config_yaml(draw):
    """Generate a complete valid Workflow_Configuration YAML string.

    Produces a YAML document with:
    - A suite section with a name
    - 1-5 families, each with 1-4 tasks
    - No triggers between tasks (to avoid referencing non-existent paths)
    - Optional meters, events, and variables on tasks
    """
    suite_name = draw(_identifier) + "_suite"
    num_families = draw(st.integers(min_value=1, max_value=5))

    # Generate families ensuring unique paths AND unique full task paths
    families = []
    used_paths = set()
    used_full_task_paths = set()
    for _ in range(num_families):
        family = draw(_family_strategy())
        # Ensure unique family paths by appending a suffix until unique
        base_path = family["path"]
        candidate_path = base_path
        suffix = 0
        while candidate_path in used_paths:
            suffix += 1
            candidate_path = base_path + f"/s{suffix}"
        family["path"] = candidate_path
        used_paths.add(candidate_path)

        # Ensure no duplicate full task paths across all families
        # by renaming tasks that would collide
        for task in family["tasks"]:
            full_path = f"{family['path']}/{task['name']}"
            base_name = task["name"]
            counter = 0
            while full_path in used_full_task_paths:
                counter += 1
                task["name"] = f"{base_name}y{counter}"
                task["jjob"] = "J" + task["name"].upper()
                full_path = f"{family['path']}/{task['name']}"
            used_full_task_paths.add(full_path)

        families.append(family)

    # Build YAML string manually to ensure valid structure
    lines = []
    lines.append("suite:")
    lines.append(f'  name: "{suite_name}"')
    lines.append("")
    lines.append("families:")

    for family in families:
        lines.append(f'  - path: "{family["path"]}"')
        lines.append("    tasks:")
        for task in family["tasks"]:
            lines.append(f'      - name: "{task["name"]}"')
            lines.append(f'        jjob: "{task["jjob"]}"')
            if "trigger" in task and task["trigger"]:
                lines.append(f'        trigger: "{task["trigger"]}"')
            if "events" in task and task["events"]:
                events_str = ", ".join(f'"{e}"' for e in task["events"])
                lines.append(f"        events: [{events_str}]")
            if "meters" in task and task["meters"]:
                lines.append("        meters:")
                for m in task["meters"]:
                    lines.append(f'          - name: "{m["name"]}"')
                    lines.append(f'            min: {m["min"]}')
                    lines.append(f'            max: {m["max"]}')
            if "variables" in task and task["variables"]:
                lines.append("        variables:")
                for k, v in task["variables"].items():
                    lines.append(f'          {k}: "{v}"')

    return "\n".join(lines) + "\n"


# ---------------------------------------------------------------------------
# Structural equality helpers
# ---------------------------------------------------------------------------


def _dag_nodes_equal(dag1: DAG, dag2: DAG) -> bool:
    """Check that two DAGs have structurally equal node sets.

    Compares:
    - Same set of node keys (full paths)
    - For each node: name, family_path, jjob, trigger, complete, events, meters, variables
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
# Property Test: Parser Round-Trip (Property 9)
# ---------------------------------------------------------------------------


@given(yaml_content=_workflow_config_yaml())
@settings(
    max_examples=50,
    deadline=None,
    suppress_health_check=[HealthCheck.too_slow],
)
def test_parser_roundtrip_property(yaml_content: str):
    """Property 9: pretty_print(parse(f)) parses to a DAG structurally equal to parse(f).

    **Validates: Requirements 10.4**

    Steps:
    1. Generate a valid Workflow_Configuration YAML string
    2. Write it to a temp file and parse it with parse()
    3. Call pretty_print() on the resulting DAG to get canonical YAML
    4. Write the pretty-printed YAML to another temp file and parse again
    5. Assert the two DAGs are structurally equal
    """
    # Step 1-2: Write generated YAML to temp file and parse
    tmp1 = tempfile.NamedTemporaryFile(mode="w", suffix=".yaml", delete=False)
    try:
        tmp1.write(yaml_content)
        tmp1.close()

        dag1 = parse(tmp1.name)

        # Step 3: Pretty-print the DAG back to YAML
        printed_yaml = pretty_print(dag1)

        # Step 4: Write pretty-printed YAML to another temp file and re-parse
        tmp2 = tempfile.NamedTemporaryFile(mode="w", suffix=".yaml", delete=False)
        try:
            tmp2.write(printed_yaml)
            tmp2.close()

            dag2 = parse(tmp2.name)

            # Step 5: Assert structural equality
            assert dag1.suite_name == dag2.suite_name, (
                f"Suite names differ: {dag1.suite_name!r} vs {dag2.suite_name!r}"
            )
            assert set(dag1.nodes.keys()) == set(dag2.nodes.keys()), (
                f"Node sets differ:\n"
                f"  Only in dag1: {set(dag1.nodes.keys()) - set(dag2.nodes.keys())}\n"
                f"  Only in dag2: {set(dag2.nodes.keys()) - set(dag1.nodes.keys())}"
            )
            assert _dags_structurally_equal(dag1, dag2), (
                "DAGs are not structurally equal after round-trip.\n"
                f"dag1 nodes: {list(dag1.nodes.keys())}\n"
                f"dag2 nodes: {list(dag2.nodes.keys())}\n"
                f"dag1 edges: {[(e.source, e.target, e.kind) for e in dag1.edges]}\n"
                f"dag2 edges: {[(e.source, e.target, e.kind) for e in dag2.edges]}"
            )
        finally:
            os.unlink(tmp2.name)
    finally:
        os.unlink(tmp1.name)
