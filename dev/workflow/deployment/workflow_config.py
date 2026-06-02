"""Workflow Configuration Parser and Pretty-Printer.

Defines the in-memory DAG data model (TaskNode, Edge, MeterDef, DAG)
and provides parse/pretty_print functions for Workflow_Configuration YAML.

Traces to: Requirements 2.1, 10.1, 10.2, 10.3, 10.4, 10.5, 10.6
"""

from __future__ import annotations

import re
from collections import OrderedDict
from dataclasses import dataclass, field
from pathlib import Path
from typing import Any, NamedTuple, Optional

import networkx as nx
import yaml


class MeterDef(NamedTuple):
    """Definition of an ecFlow meter on a task node.

    Attributes:
        name: Meter name (e.g. 'forecast_hour').
        min_value: Minimum meter value (inclusive).
        max_value: Maximum meter value (inclusive).
        threshold: Optional threshold for color change in ecFlow GUI.
    """

    name: str
    min_value: int
    max_value: int
    threshold: Optional[int] = None


@dataclass
class Edge:
    """A directed dependency edge in the workflow DAG.

    Attributes:
        source: Fully qualified name of the upstream task (e.g. 'gdas/atmos/prep').
        target: Fully qualified name of the downstream task.
        kind: Type of dependency - one of 'trigger', 'complete', 'event', 'meter'.
        expression: The raw ecFlow trigger/complete expression string.
    """

    source: str
    target: str
    kind: str = "trigger"
    expression: str = ""


@dataclass
class TaskNode:
    """A single ecFlow leaf task in the workflow DAG.

    Identified by (suite, cycle, family_path, name). Corresponds to one
    submitted scheduler job whose entry point is the rendered .ecf script.

    Attributes:
        name: Task name (e.g. 'anal', 'post_f000').
        family_path: Dot-separated or slash-separated family hierarchy
                     (e.g. 'gdas/atmos/analysis').
        jjob: Name of the J-Job script under jobs/ (e.g. 'JGDAS_ATMOS_ANALYSIS').
        trigger: Raw ecFlow trigger expression, or None if no trigger.
        complete: Raw ecFlow complete expression, or None.
        events: List of ecFlow event names declared by this task.
        meters: List of MeterDef named tuples declared by this task.
        variables: Task-level ecFlow variables (key-value pairs).
        resources: Scheduler resource requests (walltime, memory, nodes, etc.).
    """

    name: str
    family_path: str
    jjob: str
    trigger: Optional[str] = None
    complete: Optional[str] = None
    events: list[str] = field(default_factory=list)
    meters: list[MeterDef] = field(default_factory=list)
    variables: dict[str, str] = field(default_factory=dict)
    resources: dict[str, Any] = field(default_factory=dict)

    @property
    def full_path(self) -> str:
        """Return the fully qualified task path (family_path/name)."""
        return f"{self.family_path}/{self.name}"


@dataclass
class DAG:
    """In-memory representation of a workflow Directed Acyclic Graph.

    Contains all task nodes and dependency edges for one Workflow_Configuration.

    Attributes:
        suite_name: Name of the ecFlow suite (e.g. 'gfs_v17').
        nodes: Mapping from fully qualified task path to TaskNode.
        edges: List of directed dependency edges.
    """

    suite_name: str
    nodes: dict[str, TaskNode] = field(default_factory=dict)
    edges: list[Edge] = field(default_factory=list)

    def _to_digraph(self) -> nx.DiGraph:
        """Build a networkx DiGraph from the DAG's nodes and edges.

        Returns:
            A networkx DiGraph with all task nodes as vertices and
            dependency edges as directed edges (source -> target).
        """
        g = nx.DiGraph()
        # Add all nodes to the graph (including those with no edges)
        for node_path in self.nodes:
            g.add_node(node_path)
        # Add edges
        for edge in self.edges:
            g.add_edge(edge.source, edge.target)
        return g

    def validate_acyclic(self) -> None:
        """Validate that the DAG contains no cycles.

        Builds a networkx DiGraph from self.edges and checks for cycles
        using topological sort. If a cycle is detected, identifies the
        cycle path and raises CycleDetectedError.

        Raises:
            CycleDetectedError: If a cycle is found, with the cycle path.
        """
        g = self._to_digraph()

        if not nx.is_directed_acyclic_graph(g):
            # Find one cycle to report
            cycle = nx.find_cycle(g, orientation="original")
            # cycle is a list of (u, v, direction) tuples
            cycle_path = [u for u, v, _ in cycle]
            raise CycleDetectedError(cycle_path)

    def downstream(self, task: str) -> set[str]:
        """Return the set of task names reachable downstream from `task`.

        Uses networkx descendants to find all nodes reachable from `task`
        by following directed edges forward.

        Args:
            task: Fully qualified task path.

        Returns:
            Set of fully qualified task paths reachable via dependency edges.

        Raises:
            KeyError: If `task` is not a node in the DAG.
        """
        g = self._to_digraph()
        if task not in g:
            raise KeyError(f"Task '{task}' is not in the DAG")
        return nx.descendants(g, task)

    def upstream(self, task: str) -> set[str]:
        """Return the set of task names reachable upstream from `task`.

        Uses networkx ancestors to find all nodes that can reach `task`
        by following directed edges backward.

        Args:
            task: Fully qualified task path.

        Returns:
            Set of fully qualified task paths that `task` depends on.

        Raises:
            KeyError: If `task` is not a node in the DAG.
        """
        g = self._to_digraph()
        if task not in g:
            raise KeyError(f"Task '{task}' is not in the DAG")
        return nx.ancestors(g, task)


class CycleDetectedError(Exception):
    """Error raised when a cycle is detected in the workflow DAG.

    Attributes:
        cycle_path: List of node names forming the cycle (last element
                    connects back to the first).
    """

    def __init__(self, cycle_path: list[str]):
        self.cycle_path = cycle_path
        cycle_str = " → ".join(cycle_path + [cycle_path[0]])
        super().__init__(f"Cycle detected: {cycle_str}")


class ParseError(Exception):
    """Error raised when a Workflow_Configuration YAML is malformed.

    Attributes:
        file: Path to the YAML file that caused the error.
        reason: Human-readable description of the problem.
        line: Optional line number where the error was detected.
    """

    def __init__(self, file: str, reason: str, line: Optional[int] = None):
        self.file = file
        self.reason = reason
        self.line = line
        if line is not None:
            msg = f"{file}:{line}: {reason}"
        else:
            msg = f"{file}: {reason}"
        super().__init__(msg)


# ---------------------------------------------------------------------------
# Trigger expression parsing helpers
# ---------------------------------------------------------------------------

# Pattern for "path/to/task == complete" or "path/to/task == active" etc.
_TASK_STATUS_RE = re.compile(
    r"([\w/]+)\s*==\s*(complete|active|aborted|queued|submitted|unknown)"
)

# Pattern for "path/to/task:meter_name ge value"
_METER_RE = re.compile(
    r"([\w/]+):([\w]+)\s+(ge|gt|le|lt|eq|ne)\s+(\d+)"
)


def _resolve_task_path(ref: str, family_path: str) -> str:
    """Resolve a task reference to a fully qualified path.

    If the reference contains a '/', it is treated as an absolute path
    within the suite. Otherwise, it is relative to the current family_path.

    Args:
        ref: The task reference string (e.g. 'anal' or 'gdas/atmos/prep/prep').
        family_path: The family path of the task containing the trigger.

    Returns:
        Fully qualified task path.
    """
    if "/" in ref:
        return ref
    return f"{family_path}/{ref}"


def _parse_trigger_refs(expression: str, family_path: str) -> list[tuple[str, str]]:
    """Extract (source_task_path, kind) pairs from a trigger/complete expression.

    Handles:
      - "path/task == complete" -> (path/task, "trigger")
      - "path/task:meter ge N" -> (path/task, "meter")
      - Boolean compositions with 'and'/'or'

    Args:
        expression: The raw trigger or complete expression string.
        family_path: The family path of the task owning this expression.

    Returns:
        List of (source_full_path, edge_kind) tuples.
    """
    if not expression or not expression.strip():
        return []

    refs: list[tuple[str, str]] = []

    # Find meter references first
    for match in _METER_RE.finditer(expression):
        task_ref = match.group(1)
        resolved = _resolve_task_path(task_ref, family_path)
        refs.append((resolved, "meter"))

    # Find task status references
    for match in _TASK_STATUS_RE.finditer(expression):
        task_ref = match.group(1)
        # Skip if this was already captured as part of a meter expression
        # (meter refs have ':' which won't match _TASK_STATUS_RE)
        resolved = _resolve_task_path(task_ref, family_path)
        refs.append((resolved, "trigger"))

    return refs


def _expand_for_each(task_def: dict) -> list[dict]:
    """Expand a task definition with `for_each` into multiple task definitions.

    The `for_each` key maps a variable name to a list of values. For each value,
    a new task definition is produced with Jinja2-style `{{ var }}` and
    `'%03d' % var` patterns resolved in `name`, `trigger`, and `variables`.

    Currently supports single-variable for_each only.

    Args:
        task_def: A single task definition dict from the YAML.

    Returns:
        List of expanded task definition dicts (without the `for_each` key).
    """
    for_each = task_def.get("for_each")
    if not for_each:
        return [task_def]

    expanded = []
    # Support single variable expansion
    for var_name, values in for_each.items():
        for value in values:
            new_task = dict(task_def)
            # Remove for_each from the expanded copy
            new_task = {k: v for k, v in task_def.items() if k != "for_each"}

            # Expand the task name: resolve Jinja2-like patterns
            name = new_task["name"]
            # Handle {{ '%03d' % var }} pattern
            name = re.sub(
                r"\{\{\s*'%(\d+)d'\s*%\s*" + re.escape(var_name) + r"\s*\}\}",
                lambda m: f"%0{m.group(1)}d" % value,
                name,
            )
            # Handle {{ var }} pattern
            name = re.sub(
                r"\{\{\s*" + re.escape(var_name) + r"\s*\}\}",
                str(value),
                name,
            )
            new_task["name"] = name

            # Expand trigger expression
            if "trigger" in new_task and new_task["trigger"]:
                trigger = new_task["trigger"]
                trigger = re.sub(
                    r"\{\{\s*" + re.escape(var_name) + r"\s*\}\}",
                    str(value),
                    trigger,
                )
                new_task["trigger"] = trigger

            # Expand variables
            if "variables" in new_task and new_task["variables"]:
                new_vars = {}
                for vk, vv in new_task["variables"].items():
                    if isinstance(vv, str):
                        vv = re.sub(
                            r"\{\{\s*" + re.escape(var_name) + r"\s*\}\}",
                            str(value),
                            vv,
                        )
                    new_vars[vk] = vv
                new_task["variables"] = new_vars

            expanded.append(new_task)
        # Only process the first variable (single-variable for_each)
        break

    return expanded


def parse(path: str) -> DAG:
    """Parse a Workflow_Configuration YAML file into an in-memory DAG.

    Reads the YAML file at `path`, validates required keys, expands
    `for_each` parameterized tasks, builds TaskNode objects, and
    constructs dependency edges from trigger/complete expressions.

    Args:
        path: Path to the Workflow_Configuration YAML file.

    Returns:
        A DAG object containing all task nodes and dependency edges.

    Raises:
        ParseError: If the file is missing, malformed, or contains
            invalid configuration. The error includes the file path,
            line number (where possible), and a descriptive reason.
    """
    filepath = Path(path)

    # --- Load YAML ---
    if not filepath.exists():
        raise ParseError(str(filepath), "File not found")

    try:
        with open(filepath, "r") as f:
            raw = yaml.safe_load(f)
    except yaml.YAMLError as e:
        # Extract line number from PyYAML error if available
        line = None
        if hasattr(e, "problem_mark") and e.problem_mark is not None:
            line = e.problem_mark.line + 1  # PyYAML uses 0-based lines
        raise ParseError(str(filepath), f"YAML syntax error: {e}", line) from e

    if raw is None:
        raise ParseError(str(filepath), "File is empty or contains no YAML data")

    if not isinstance(raw, dict):
        raise ParseError(str(filepath), "Top-level YAML must be a mapping")

    # --- Validate required top-level keys ---
    if "suite" not in raw:
        raise ParseError(str(filepath), "Missing required key: 'suite'")

    suite_section = raw["suite"]
    if not isinstance(suite_section, dict):
        raise ParseError(str(filepath), "'suite' must be a mapping")

    if "name" not in suite_section:
        raise ParseError(str(filepath), "Missing required key: 'suite.name'")

    if "families" not in raw:
        raise ParseError(str(filepath), "Missing required key: 'families'")

    families = raw["families"]
    if not isinstance(families, list):
        raise ParseError(str(filepath), "'families' must be a list")

    # --- Build DAG ---
    suite_name = suite_section["name"]
    dag = DAG(suite_name=suite_name)

    # Process each family
    for family_idx, family_def in enumerate(families):
        if not isinstance(family_def, dict):
            raise ParseError(
                str(filepath),
                f"Family at index {family_idx} must be a mapping",
            )

        if "path" not in family_def:
            raise ParseError(
                str(filepath),
                f"Family at index {family_idx} missing required key: 'path'",
            )

        family_path = family_def["path"]

        if "tasks" not in family_def:
            raise ParseError(
                str(filepath),
                f"Family '{family_path}' missing required key: 'tasks'",
            )

        tasks_list = family_def["tasks"]
        if not isinstance(tasks_list, list):
            raise ParseError(
                str(filepath),
                f"Family '{family_path}': 'tasks' must be a list",
            )

        # Process each task in the family
        for task_idx, task_def in enumerate(tasks_list):
            if not isinstance(task_def, dict):
                raise ParseError(
                    str(filepath),
                    f"Family '{family_path}': task at index {task_idx} must be a mapping",
                )

            if "name" not in task_def:
                raise ParseError(
                    str(filepath),
                    f"Family '{family_path}': task at index {task_idx} missing 'name'",
                )

            if "jjob" not in task_def:
                raise ParseError(
                    str(filepath),
                    f"Family '{family_path}': task '{task_def.get('name', '?')}' missing 'jjob'",
                )

            # Expand for_each if present
            expanded_tasks = _expand_for_each(task_def)

            for expanded in expanded_tasks:
                task_name = expanded["name"]
                full_path = f"{family_path}/{task_name}"

                # Parse meters
                meters: list[MeterDef] = []
                for meter_def in expanded.get("meters", []):
                    if isinstance(meter_def, dict):
                        meters.append(
                            MeterDef(
                                name=meter_def["name"],
                                min_value=meter_def.get("min", 0),
                                max_value=meter_def.get("max", 100),
                                threshold=meter_def.get("threshold"),
                            )
                        )

                # Create TaskNode
                node = TaskNode(
                    name=task_name,
                    family_path=family_path,
                    jjob=expanded["jjob"],
                    trigger=expanded.get("trigger") or None,
                    complete=expanded.get("complete") or None,
                    events=expanded.get("events", []),
                    meters=meters,
                    variables=expanded.get("variables", {}),
                    resources=expanded.get("resources", {}),
                )

                if full_path in dag.nodes:
                    raise ParseError(
                        str(filepath),
                        f"Duplicate task path: '{full_path}'",
                    )

                dag.nodes[full_path] = node

    # --- Build edges from trigger/complete expressions ---
    for full_path, node in dag.nodes.items():
        # Process trigger expression
        if node.trigger:
            refs = _parse_trigger_refs(node.trigger, node.family_path)
            for source_path, kind in refs:
                edge = Edge(
                    source=source_path,
                    target=full_path,
                    kind=kind,
                    expression=node.trigger,
                )
                dag.edges.append(edge)

        # Process complete expression
        if node.complete:
            refs = _parse_trigger_refs(node.complete, node.family_path)
            for source_path, kind in refs:
                edge = Edge(
                    source=source_path,
                    target=full_path,
                    kind="complete",
                    expression=node.complete,
                )
                dag.edges.append(edge)

    return dag


# ---------------------------------------------------------------------------
# Pretty-Printer: DAG -> canonical YAML
# ---------------------------------------------------------------------------


def _ordered_dict(*pairs: tuple[str, Any]) -> OrderedDict:
    """Create an OrderedDict from key-value pairs, omitting None values."""
    d: OrderedDict = OrderedDict()
    for key, value in pairs:
        if value is not None:
            d[key] = value
    return d


def _task_node_to_dict(node: TaskNode) -> OrderedDict:
    """Convert a TaskNode to an ordered dict suitable for YAML serialization.

    Produces the canonical task representation with keys in a fixed order:
    name, trigger, jjob, complete, events, meters, variables, resources.

    Args:
        node: The TaskNode to serialize.

    Returns:
        OrderedDict with the task's YAML representation.
    """
    task: OrderedDict = OrderedDict()
    task["name"] = node.name
    # Always include trigger (empty string if None, for round-trip fidelity)
    task["trigger"] = node.trigger if node.trigger else ""
    task["jjob"] = node.jjob

    if node.complete:
        task["complete"] = node.complete

    if node.events:
        task["events"] = list(node.events)

    if node.meters:
        meters_list = []
        for meter in node.meters:
            meter_dict: OrderedDict = OrderedDict()
            meter_dict["name"] = meter.name
            meter_dict["min"] = meter.min_value
            meter_dict["max"] = meter.max_value
            if meter.threshold is not None:
                meter_dict["threshold"] = meter.threshold
            meters_list.append(meter_dict)
        task["meters"] = meters_list

    if node.variables:
        # Sort variables by key for determinism
        task["variables"] = OrderedDict(sorted(node.variables.items()))

    if node.resources:
        # Sort resources by key for determinism
        task["resources"] = OrderedDict(sorted(node.resources.items()))

    return task


def pretty_print(dag: DAG) -> str:
    """Serialize a DAG object to canonical Workflow_Configuration YAML.

    Reconstructs the YAML structure from the in-memory DAG representation.
    Tasks are grouped by family_path, and families are emitted in the order
    they first appear in the nodes dict. Within each family, tasks are
    emitted in insertion order.

    The output is deterministic: given the same DAG input, the function
    produces byte-for-byte identical output across invocations.

    Args:
        dag: The DAG object to serialize.

    Returns:
        A string containing the canonical YAML representation of the
        Workflow_Configuration.

    Traces to: Requirements 10.3, 10.6
    """
    # --- Build the top-level structure ---
    doc: OrderedDict = OrderedDict()

    # Suite section
    doc["suite"] = OrderedDict([("name", dag.suite_name)])

    # --- Group tasks by family_path (preserving insertion order) ---
    families_order: OrderedDict = OrderedDict()
    for full_path, node in dag.nodes.items():
        family_path = node.family_path
        if family_path not in families_order:
            families_order[family_path] = []
        families_order[family_path].append(node)

    # --- Build families list ---
    families_list = []
    for family_path, task_nodes in families_order.items():
        family_dict: OrderedDict = OrderedDict()
        family_dict["path"] = family_path
        family_dict["tasks"] = [_task_node_to_dict(node) for node in task_nodes]
        families_list.append(family_dict)

    doc["families"] = families_list

    # --- Serialize to YAML ---
    # Use a custom Dumper that respects OrderedDict key order and
    # produces clean, deterministic output.
    class _OrderedDumper(yaml.SafeDumper):
        """Custom YAML dumper that preserves OrderedDict key order."""
        pass

    def _represent_ordered_dict(dumper: yaml.SafeDumper, data: OrderedDict) -> Any:
        return dumper.represent_mapping(
            yaml.resolver.BaseResolver.DEFAULT_MAPPING_TAG,
            data.items(),
        )

    def _represent_str(dumper: yaml.SafeDumper, data: str) -> Any:
        # Use double-quoted style for strings containing special chars
        # that might be misinterpreted, otherwise use plain style
        if any(c in data for c in ("\n", "\t", ":", "#", "{", "}", "[", "]", ",", "&", "*", "?", "|", "-", "<", ">", "=", "!", "%", "@", "`")):
            return dumper.represent_scalar(
                "tag:yaml.org,2002:str", data, style='"'
            )
        # Empty strings need quoting
        if data == "":
            return dumper.represent_scalar(
                "tag:yaml.org,2002:str", data, style='"'
            )
        # Strings that look like booleans or numbers need quoting
        if data.lower() in ("true", "false", "yes", "no", "null", "~"):
            return dumper.represent_scalar(
                "tag:yaml.org,2002:str", data, style='"'
            )
        # Check if it looks like a number
        try:
            float(data)
            return dumper.represent_scalar(
                "tag:yaml.org,2002:str", data, style='"'
            )
        except ValueError:
            pass
        return dumper.represent_scalar("tag:yaml.org,2002:str", data)

    _OrderedDumper.add_representer(OrderedDict, _represent_ordered_dict)
    _OrderedDumper.add_representer(str, _represent_str)

    output = yaml.dump(
        doc,
        Dumper=_OrderedDumper,
        default_flow_style=False,
        sort_keys=False,
        allow_unicode=True,
        width=120,
    )

    return output
