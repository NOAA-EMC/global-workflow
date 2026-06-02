"""DAG Generator for ecFlow Suite_Definition emission.

Emits ecFlow `.def` files using the ecFlow definition language text format.
Since the ecFlow Python API (ecflow package) requires the ecFlow server
installation and may not be available in all environments, this module
generates the `.def` file as structured text directly.

The ecFlow definition language is a well-defined text format that ecflow_server
can load via `ecflow_client --load <file>.def`.

Also provides per-task `.ecf` script generation from Jinja2 templates with
platform-specific scheduler directives (PBS for WCOSS2, Slurm for others).

Traces to: Requirements 1.2, 2.1, 2.3, 2.4, 12.5
"""

from __future__ import annotations

import os
from collections import OrderedDict
from dataclasses import dataclass, field
from pathlib import Path
from typing import Any, Optional

from .template_renderer import TemplateRenderer, TemplateRenderError
from .workflow_config import DAG, MeterDef, TaskNode


# ---------------------------------------------------------------------------
# Data structures for inter-cycle and scheduling attributes
# ---------------------------------------------------------------------------


@dataclass
class RepeatDateDef:
    """Definition of an ecFlow repeat date construct for inter-cycle deps.

    Attributes:
        variable: The repeat variable name (e.g. 'YMD').
        start: Start date string (YYYYMMDD or Jinja2 template).
        end: End date string (YYYYMMDD or Jinja2 template).
        step: Step in days.
    """

    variable: str
    start: str
    end: str
    step: int = 1


@dataclass
class CycleDef:
    """Definition of a cycle (repeat + time) from the Workflow_Configuration.

    Attributes:
        name: Cycle name (e.g. 'gdas', 'gfs').
        repeat: Optional RepeatDateDef for date cycling.
        time: Optional time specification (e.g. '00:00 06:00 12:00 18:00').
        cron: Optional cron specification.
    """

    name: str
    repeat: Optional[RepeatDateDef] = None
    time: Optional[str] = None
    cron: Optional[str] = None


@dataclass
class SuiteConfig:
    """Configuration for the ecFlow suite emission.

    Attributes:
        ecf_home: Path to ecFlow home directory.
        ecf_files: Path to ecFlow script files.
        ecf_include: Path to ecFlow include files.
        defaults: Default variables for the suite.
        cycles: List of cycle definitions.
        inter_cycle_deps: List of inter-cycle dependency specifications.
    """

    ecf_home: str = ""
    ecf_files: str = ""
    ecf_include: str = ""
    defaults: dict[str, Any] = field(default_factory=dict)
    cycles: list[CycleDef] = field(default_factory=list)
    inter_cycle_deps: list[dict[str, Any]] = field(default_factory=list)


# ---------------------------------------------------------------------------
# ecFlow .def text generation
# ---------------------------------------------------------------------------


class DefFileWriter:
    """Writes ecFlow suite definition text in the `.def` format.

    Builds a hierarchical text representation of the suite with proper
    indentation, supporting all ecFlow dependency primitives.
    """

    def __init__(self) -> None:
        self._lines: list[str] = []
        self._indent: int = 0

    def _emit(self, text: str) -> None:
        """Emit a line with current indentation."""
        prefix = "  " * self._indent
        self._lines.append(f"{prefix}{text}")

    def _push(self) -> None:
        """Increase indentation level."""
        self._indent += 1

    def _pop(self) -> None:
        """Decrease indentation level."""
        self._indent = max(0, self._indent - 1)

    def get_text(self) -> str:
        """Return the accumulated definition text."""
        return "\n".join(self._lines) + "\n"

    def write_suite_start(self, name: str) -> None:
        """Write suite opening."""
        self._emit(f"suite {name}")
        self._push()

    def write_suite_end(self) -> None:
        """Write suite closing."""
        self._pop()
        self._emit("endsuite")

    def write_family_start(self, name: str) -> None:
        """Write family opening."""
        self._emit(f"family {name}")
        self._push()

    def write_family_end(self) -> None:
        """Write family closing (endfamily)."""
        self._pop()
        self._emit("endfamily")

    def write_task(self, name: str) -> None:
        """Write a task declaration."""
        self._emit(f"task {name}")
        self._push()

    def write_task_end(self) -> None:
        """End a task block (pop indentation)."""
        self._pop()

    def write_trigger(self, expression: str) -> None:
        """Write a trigger expression."""
        self._emit(f"trigger {expression}")

    def write_complete(self, expression: str) -> None:
        """Write a complete expression."""
        self._emit(f"complete {expression}")

    def write_event(self, name: str) -> None:
        """Write an event declaration."""
        self._emit(f"event {name}")

    def write_meter(self, name: str, min_val: int, max_val: int,
                    threshold: Optional[int] = None) -> None:
        """Write a meter declaration."""
        if threshold is not None:
            self._emit(f"meter {name} {min_val} {max_val} {threshold}")
        else:
            self._emit(f"meter {name} {min_val} {max_val}")

    def write_variable(self, name: str, value: str) -> None:
        """Write an ecFlow variable (edit statement)."""
        # Quote value if it contains spaces or special characters
        if " " in value or "'" in value or '"' in value:
            self._emit(f"edit {name} '{value}'")
        else:
            self._emit(f"edit {name} '{value}'")

    def write_repeat_date(self, variable: str, start: str, end: str,
                          step: int = 1) -> None:
        """Write a repeat date construct for inter-cycle dependencies."""
        self._emit(f"repeat date {variable} {start} {end} {step}")

    def write_time(self, time_spec: str) -> None:
        """Write a time dependency.

        Args:
            time_spec: Time in HH:MM format or space-separated list.
        """
        self._emit(f"time {time_spec}")

    def write_date(self, date_spec: str) -> None:
        """Write a date dependency.

        Args:
            date_spec: Date specification (e.g. '1.*.*' for 1st of every month).
        """
        self._emit(f"date {date_spec}")

    def write_cron(self, cron_spec: str) -> None:
        """Write a cron dependency.

        Args:
            cron_spec: Cron specification string.
        """
        self._emit(f"cron {cron_spec}")

    def write_defstatus(self, status: str) -> None:
        """Write a defstatus declaration."""
        self._emit(f"defstatus {status}")

    def write_limit(self, name: str, value: int) -> None:
        """Write a limit declaration."""
        self._emit(f"limit {name} {value}")

    def write_inlimit(self, path: str) -> None:
        """Write an inlimit declaration."""
        self._emit(f"inlimit {path}")

    def write_comment(self, text: str) -> None:
        """Write a comment line."""
        self._emit(f"# {text}")


# ---------------------------------------------------------------------------
# Family tree builder
# ---------------------------------------------------------------------------


def _build_family_tree(
    nodes: dict[str, TaskNode],
) -> OrderedDict[str, Any]:
    """Build a nested family tree from flat task node paths.

    Groups tasks by their family_path components, creating a nested
    dictionary structure that mirrors the ecFlow family hierarchy.

    Args:
        nodes: Mapping from full task path to TaskNode.

    Returns:
        Nested OrderedDict where keys are family names and values are
        either sub-dicts (for nested families) or lists of TaskNodes
        (stored under the special key '__tasks__').
    """
    tree: OrderedDict[str, Any] = OrderedDict()

    for full_path, node in nodes.items():
        # Split family_path into components
        parts = node.family_path.split("/")

        # Navigate/create the nested structure
        current = tree
        for part in parts:
            if part not in current:
                current[part] = OrderedDict()
            current = current[part]

        # Add the task to the leaf family
        if "__tasks__" not in current:
            current["__tasks__"] = []
        current["__tasks__"].append(node)

    return tree


def _emit_family_tree(
    writer: DefFileWriter,
    tree: OrderedDict[str, Any],
    suite_config: Optional[SuiteConfig] = None,
) -> None:
    """Recursively emit the family tree as ecFlow definition text.

    Args:
        writer: The DefFileWriter to emit text to.
        tree: Nested family tree from _build_family_tree.
        suite_config: Optional suite configuration for cycle/time attributes.
    """
    for key, value in tree.items():
        if key == "__tasks__":
            # Emit tasks at this level
            for node in value:
                _emit_task(writer, node)
        else:
            # This is a family node
            writer.write_family_start(key)

            # Check if this family corresponds to a cycle definition
            if suite_config:
                for cycle in suite_config.cycles:
                    if cycle.name == key:
                        # Add repeat date if defined
                        if cycle.repeat:
                            writer.write_repeat_date(
                                cycle.repeat.variable,
                                cycle.repeat.start,
                                cycle.repeat.end,
                                cycle.repeat.step,
                            )
                        # Add time if defined
                        if cycle.time:
                            writer.write_time(cycle.time)
                        # Add cron if defined
                        if cycle.cron:
                            writer.write_cron(cycle.cron)
                        break

            # Recurse into sub-families and tasks
            _emit_family_tree(writer, value, suite_config)
            writer.write_family_end()


def _emit_task(writer: DefFileWriter, node: TaskNode) -> None:
    """Emit a single task node as ecFlow definition text.

    Writes the task declaration followed by its attributes:
    trigger, complete, events, meters, and variables.

    Args:
        writer: The DefFileWriter to emit text to.
        node: The TaskNode to emit.
    """
    writer.write_task(node.name)

    # Trigger expression
    if node.trigger:
        writer.write_trigger(node.trigger)

    # Complete expression
    if node.complete:
        writer.write_complete(node.complete)

    # Events
    for event in node.events:
        writer.write_event(event)

    # Meters
    for meter in node.meters:
        writer.write_meter(
            meter.name, meter.min_value, meter.max_value, meter.threshold
        )

    # Variables (as ecFlow 'edit' statements)
    for var_name, var_value in sorted(node.variables.items()):
        writer.write_variable(var_name, str(var_value))

    writer.write_task_end()


# ---------------------------------------------------------------------------
# Public API
# ---------------------------------------------------------------------------


def parse_suite_config(raw_config: dict[str, Any]) -> SuiteConfig:
    """Parse suite configuration from raw YAML dict.

    Extracts ecf_home, ecf_files, ecf_include, defaults, cycles,
    and inter_cycle_dependencies from the raw parsed YAML.

    Args:
        raw_config: The raw YAML dict (top-level parsed config).

    Returns:
        A SuiteConfig object with all suite-level attributes.
    """
    suite_section = raw_config.get("suite", {})
    config = SuiteConfig(
        ecf_home=suite_section.get("ecf_home", ""),
        ecf_files=suite_section.get("ecf_files", ""),
        ecf_include=suite_section.get("ecf_include", ""),
    )

    # Parse defaults
    config.defaults = raw_config.get("defaults", {})

    # Parse cycles
    for cycle_def in raw_config.get("cycles", []):
        repeat_def = None
        if "repeat" in cycle_def:
            r = cycle_def["repeat"]
            repeat_def = RepeatDateDef(
                variable=r.get("variable", "YMD"),
                start=r.get("start", ""),
                end=r.get("end", ""),
                step=r.get("step", 1),
            )

        cycle = CycleDef(
            name=cycle_def.get("name", ""),
            repeat=repeat_def,
            time=cycle_def.get("time"),
            cron=cycle_def.get("cron"),
        )
        config.cycles.append(cycle)

    # Parse inter-cycle dependencies
    config.inter_cycle_deps = raw_config.get("inter_cycle_dependencies", [])

    return config


def generate_def(
    dag: DAG,
    output_path: str,
    suite_config: Optional[SuiteConfig] = None,
) -> str:
    """Generate an ecFlow `.def` file from a DAG object.

    Builds the suite/family/task hierarchy from the DAG nodes (grouped
    by family_path), adds triggers, events, meters, and variables, and
    writes the `.def` file to the specified output path.

    The generated `.def` file uses the ecFlow definition language text
    format, which can be loaded by ecflow_server via:
        ecflow_client --load <file>.def

    Args:
        dag: The in-memory DAG object containing all task nodes and edges.
        output_path: Path where the `.def` file will be written.
            Typically `<EXPDIR>/ecf/defs/<suite_name>.def`.
        suite_config: Optional suite configuration with cycle definitions,
            defaults, and inter-cycle dependencies. If None, only the
            basic suite/family/task structure is emitted.

    Returns:
        The generated definition text as a string.

    Raises:
        OSError: If the output directory cannot be created or the file
            cannot be written.

    Traces to: Requirements 1.2, 2.1, 2.3, 2.4
    """
    writer = DefFileWriter()

    # --- Suite header ---
    writer.write_suite_start(dag.suite_name)

    # --- Suite-level variables (defaults) ---
    if suite_config:
        # ECF path variables
        if suite_config.ecf_home:
            writer.write_variable("ECF_HOME", suite_config.ecf_home)
        if suite_config.ecf_files:
            writer.write_variable("ECF_FILES", suite_config.ecf_files)
        if suite_config.ecf_include:
            writer.write_variable("ECF_INCLUDE", suite_config.ecf_include)

        # Default variables
        for var_name, var_value in suite_config.defaults.items():
            writer.write_variable(var_name, str(var_value))

    # --- Build and emit family tree ---
    tree = _build_family_tree(dag.nodes)
    _emit_family_tree(writer, tree, suite_config)

    # --- Suite footer ---
    writer.write_suite_end()

    # --- Write to file ---
    output = Path(output_path)
    output.parent.mkdir(parents=True, exist_ok=True)
    def_text = writer.get_text()
    output.write_text(def_text)

    return def_text


def generate_def_text(
    dag: DAG,
    suite_config: Optional[SuiteConfig] = None,
) -> str:
    """Generate ecFlow `.def` text from a DAG without writing to disk.

    This is useful for testing and for the Definition Fidelity property
    (Property 13) where we need to inspect the generated text without
    side effects.

    Args:
        dag: The in-memory DAG object.
        suite_config: Optional suite configuration.

    Returns:
        The generated definition text as a string.
    """
    writer = DefFileWriter()

    # --- Suite header ---
    writer.write_suite_start(dag.suite_name)

    # --- Suite-level variables (defaults) ---
    if suite_config:
        if suite_config.ecf_home:
            writer.write_variable("ECF_HOME", suite_config.ecf_home)
        if suite_config.ecf_files:
            writer.write_variable("ECF_FILES", suite_config.ecf_files)
        if suite_config.ecf_include:
            writer.write_variable("ECF_INCLUDE", suite_config.ecf_include)

        for var_name, var_value in suite_config.defaults.items():
            writer.write_variable(var_name, str(var_value))

    # --- Build and emit family tree ---
    tree = _build_family_tree(dag.nodes)
    _emit_family_tree(writer, tree, suite_config)

    # --- Suite footer ---
    writer.write_suite_end()

    return writer.get_text()


def parse_def_tasks(def_text: str) -> set[tuple[str, str]]:
    """Parse a `.def` file text and extract (family_path, task_name) pairs.

    This is used for the Definition Fidelity property (Property 13) to
    verify that the set of tasks in the emitted definition matches the
    source DAG.

    The parser tracks the current family path by following 'family' and
    'endfamily' keywords, and collects task names from 'task' keywords.

    Args:
        def_text: The ecFlow definition text to parse.

    Returns:
        Set of (family_path, task_name) tuples found in the definition.
    """
    tasks: set[tuple[str, str]] = set()
    family_stack: list[str] = []

    for line in def_text.splitlines():
        stripped = line.strip()

        if stripped.startswith("family "):
            family_name = stripped[len("family "):].strip()
            family_stack.append(family_name)

        elif stripped == "endfamily":
            if family_stack:
                family_stack.pop()

        elif stripped.startswith("task "):
            task_name = stripped[len("task "):].strip()
            family_path = "/".join(family_stack)
            tasks.add((family_path, task_name))

    return tasks


# ---------------------------------------------------------------------------
# Per-task .ecf script generation
# ---------------------------------------------------------------------------

# Platform-specific scheduler directive headers
_PBS_DIRECTIVES = """\
#PBS -N {task_name}
#PBS -j oe
#PBS -q %ECF_JOB_QUEUE%
#PBS -A %ACCOUNT%
#PBS -l walltime=%WALLTIME%
#PBS -l select=%SELECT%
"""

_SLURM_DIRECTIVES = """\
#SBATCH --job-name={task_name}
#SBATCH --output=%ECF_JOBOUT%
#SBATCH --account=%ACCOUNT%
#SBATCH --qos=%QOS%
#SBATCH --time=%WALLTIME%
#SBATCH --nodes=%NODES%
#SBATCH --ntasks=%NTASKS%
"""


def _get_scheduler_directives(platform: str, task_name: str) -> str:
    """Return platform-specific scheduler directives for an ecf script.

    PBS directives are used for WCOSS2; Slurm directives for all other
    supported platforms.

    Args:
        platform: The target platform name (e.g. 'WCOSS2', 'HERA').
        task_name: The task name to embed in the directive header.

    Returns:
        A string containing the scheduler directive block.
    """
    if platform.upper() == "WCOSS2":
        return _PBS_DIRECTIVES.format(task_name=task_name)
    else:
        return _SLURM_DIRECTIVES.format(task_name=task_name)


def generate_ecf_scripts(
    dag: DAG,
    output_dir: str,
    template_path: str,
    platform: str,
) -> list[Path]:
    """Generate per-task .ecf scripts from a DAG using a Jinja2 template.

    For each TaskNode in the DAG, renders the `task.ecf.j2` template with
    the task's context (name, family_path, jjob) and prepends platform-specific
    scheduler directives (PBS for WCOSS2, Slurm for all other platforms).

    The rendered .ecf files are written to:
        <output_dir>/<family_path>/<task_name>.ecf

    Args:
        dag: The in-memory DAG object containing all task nodes.
        output_dir: Base output directory (typically <EXPDIR>/ecf/scripts/).
        template_path: Path to the `task.ecf.j2` Jinja2 template file.
        platform: Target platform name (e.g. 'WCOSS2', 'HERA', 'HERCULES').
            Determines which scheduler directives are prepended.

    Returns:
        List of Path objects for all generated .ecf files.

    Raises:
        TemplateRenderError: If the template cannot be rendered for a task.
        FileNotFoundError: If the template_path does not exist.

    Traces to: Requirements 1.2, 12.5
    """
    template_file = Path(template_path)
    if not template_file.exists():
        raise FileNotFoundError(
            f"Template file not found: {template_path}"
        )

    output_base = Path(output_dir)
    generated_files: list[Path] = []

    # Read the template content once
    template_text = template_file.read_text(encoding="utf-8")

    # Create a TemplateRenderer with the template's parent directory in searchpath
    # so that any {% include %} directives in the template can resolve
    renderer = TemplateRenderer(
        context={},  # Will be overridden per-task via render_string
        searchpath=[str(template_file.parent)],
        strict=True,
    )

    for full_path, node in dag.nodes.items():
        # Build the task context for Jinja2 rendering
        task_context = {
            "task": {
                "name": node.name,
                "family_path": node.family_path,
                "jjob": node.jjob,
            }
        }

        # Update the renderer context for this task
        renderer.context = task_context

        # Render the template
        rendered_body = renderer.render_string(template_text)

        # Get platform-specific scheduler directives
        directives = _get_scheduler_directives(platform, node.name)

        # Combine directives with rendered template body
        ecf_content = directives + rendered_body

        # Determine output path: <output_dir>/<family_path>/<task_name>.ecf
        ecf_path = output_base / node.family_path / f"{node.name}.ecf"

        # Ensure parent directory exists and write the file
        ecf_path.parent.mkdir(parents=True, exist_ok=True)
        ecf_path.write_text(ecf_content, encoding="utf-8")

        generated_files.append(ecf_path)

    return generated_files
