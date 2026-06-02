"""DAG reachability filter for minimal EXPDIR staging.

Performs multi-layer reachability analysis starting from the Workflow_YAML
task definitions and transitively discovering all required artifacts (J-Jobs,
ex-scripts, ush scripts, config files).  Only artifacts in the reachability
set are staged into the sealed EXPDIR.

Traces to: Requirements 1, 2, 3, 4, 9
"""

from __future__ import annotations

import re
from collections import deque
from dataclasses import dataclass, field
from pathlib import Path
from typing import TYPE_CHECKING

if TYPE_CHECKING:
    from .name_resolver import NameResolver, ResolvedName
    from .pipeline import PipelineError as _PipelineError


def _get_pipeline_error():
    """Deferred import of PipelineError to avoid circular dependency."""
    from .pipeline import PipelineError
    return PipelineError


@dataclass(frozen=True)
class DAGReachabilitySet:
    """The complete set of artifacts transitively reachable from the Task_DAG.

    Immutable after computation.  Used by:
    - File_Stager to filter which files to copy
    - Model_Input_Renderer to determine which components need inputs
    - Completeness_Verifier as the expected set
    - Size reduction reporter for statistics

    All sets contain basenames (not full paths) for portability.
    """

    jjobs: frozenset[str]                          # Application_Names (for EXPDIR staging)
    jjob_source_map: dict[str, str] = field(default_factory=dict)  # app_name → source_name
    ex_scripts: frozenset[str] = frozenset()
    ush_scripts: frozenset[str] = frozenset()
    config_files: frozenset[str] = frozenset()
    warnings: tuple[str, ...] = ()

    # Statistics for reporting (Req 9)
    total_available_jjobs: int = 0
    total_available_ex_scripts: int = 0
    total_available_ush_scripts: int = 0
    total_available_configs: int = 0

    @property
    def is_valid(self) -> bool:
        """True if at least one J-Job is present in the reachability set."""
        return len(self.jjobs) > 0

    def contains_jjob(self, name: str) -> bool:
        """Check whether a J-Job is in the reachability set."""
        return name in self.jjobs

    def contains_ex_script(self, name: str) -> bool:
        """Check whether an ex-script is in the reachability set."""
        return name in self.ex_scripts

    def contains_ush_script(self, name: str) -> bool:
        """Check whether a ush script is in the reachability set."""
        return name in self.ush_scripts

    def contains_config(self, name: str) -> bool:
        """Check whether a config file is in the reachability set."""
        return name in self.config_files


# ---------------------------------------------------------------------------
# Config file extraction patterns (Layer 4)
# ---------------------------------------------------------------------------

# Pattern for jjob_header.sh -c "base fcst" invocations in J-Jobs.
# Captures the space-separated list of config basenames from the -c flag.
_JJOB_HEADER_PATTERN = re.compile(
    r'jjob_header\.sh.*-c\s+"(?P<configs>[^"]+)"'
)

# Config files included unconditionally regardless of DAG content.
# These provide foundational variables consumed by all tasks.
_UNCONDITIONAL_CONFIGS: set[str] = {"config.base.j2", "config.base", "config.com"}


# ---------------------------------------------------------------------------
# Ex-script invocation patterns found in J-Job files
# ---------------------------------------------------------------------------

_EX_SCRIPT_PATTERNS = [
    # ${SCRglobal}/exaaaaa.sh or ${SCRmodel}/exaaaaa.sh or ${HOMEglobal/scripts}/ex...
    re.compile(
        r'\$\{(?:SCR\w+|HOMEglobal/scripts)\}/(?P<script>ex[a-z_]+\.(?:sh|py))'
    ),
    # FORECASTSH:= assignment pattern — : "${FORECASTSH:=${SCRglobal}/exglobal_forecast.sh}"
    re.compile(
        r':\s*"\$\{(?:\w+SH):=\$\{(?:SCR\w+)\}/(?P<script>ex[a-z_]+\.(?:sh|py))\}"'
    ),
    # Direct path in variable assignment — export XXXSH="${SCRglobal}/exaaaaa.sh"
    re.compile(
        r'export\s+\w+SH="?\$\{(?:SCR\w+|HOMEglobal/scripts)\}/(?P<script>ex[a-z_]+\.(?:sh|py))"?'
    ),
]


# ---------------------------------------------------------------------------
# Regex patterns for ush script source detection (Layer 3)
# ---------------------------------------------------------------------------

_USH_SOURCE_PATTERNS = [
    # source "${USHglobal}/script_name.sh"
    re.compile(r'source\s+"?\$\{USH(?:\w+)\}/(?P<script>[a-z_][a-z0-9_.]+)"?'),
    # . "${USHglobal}/script_name.sh"  (dot-source)
    re.compile(r'\.\s+"?\$\{USH(?:\w+)\}/(?P<script>[a-z_][a-z0-9_.]+)"?'),
]


# ---------------------------------------------------------------------------
# DAGFilter class — multi-layer reachability analysis
# ---------------------------------------------------------------------------


class DAGFilter:
    """Extracts the DAG_Reachability_Set from a Workflow_YAML.

    Performs multi-layer reachability analysis:
      Layer 1: Workflow_YAML → J-Jobs (Application_Names)
      Layer 1.5: Application_Names → Source_Names (via Name_Resolver)
      Layer 2: Source J-Jobs → ex-scripts
      Layer 3: ex-scripts → ush scripts (transitive)
      Layer 4: Source J-Jobs → config files

    Args:
        dev_root: Path to the dev/ directory.
        workflow_yaml: Parsed workflow configuration dict.
        platform: Target platform for resource file selection.
        name_resolver: Optional NameResolver instance for application-to-source
            name resolution. When None, falls back to direct lookup (backward
            compatibility with shared-named workflows).
    """

    def __init__(
        self,
        dev_root: Path,
        workflow_yaml: dict,
        platform: str,
        name_resolver: NameResolver | None = None,
    ) -> None:
        self.dev_root = dev_root
        self.workflow_yaml = workflow_yaml
        self.platform = platform.upper()
        self._name_resolver = name_resolver
        self._warnings: list[str] = []

    def compute_reachability(self) -> DAGReachabilitySet:
        """Compute the full transitive reachability set.

        Orchestrates all four extraction layers and assembles the final
        immutable DAGReachabilitySet with statistics about total available
        artifacts in the dev/ source tree.

        Returns:
            A frozen DAGReachabilitySet containing all transitively reachable
            artifacts and availability statistics.

        Raises:
            PipelineError: If a referenced J-Job or ex-script is missing.

        Traces to: Requirements 1.1, 2.1, 3.1, 4.1, 9.1, 9.2, 9.3, 9.4
        """
        # Layer 1: Workflow YAML → Application_Names (J-Jobs)
        app_names = self.extract_jjobs_from_yaml()

        # Layer 1.5: Resolve Application_Names → Source_Names via Name_Resolver
        resolved_map = self.resolve_jjobs(app_names)
        source_names = {rn.source_name for rn in resolved_map.values()}
        jjob_source_map = {
            app: rn.source_name for app, rn in resolved_map.items()
        }

        # Layer 2: Source J-Jobs → ex-scripts (uses source_names)
        ex_scripts = self.extract_ex_scripts(source_names)
        # Layer 3: ex-scripts → ush scripts (transitive)
        ush_scripts = self.extract_ush_scripts(ex_scripts)
        # Layer 4: Source J-Jobs → config files (uses source_names)
        config_files = self.extract_config_files(source_names)

        # Compute statistics — total available counts from dev/ directories.
        # Handle missing directories gracefully (count as 0).
        total_jjobs = self._count_dir_entries(self.dev_root / "jobs")
        total_ex = self._count_glob(self.dev_root / "scripts", "ex*.sh")
        total_ush = self._count_glob(self.dev_root / "ush", "*.sh")
        total_configs = self._count_rglob(
            self.dev_root / "parm" / "config", "config.*"
        )

        return DAGReachabilitySet(
            jjobs=frozenset(app_names),
            jjob_source_map=jjob_source_map,
            ex_scripts=frozenset(ex_scripts),
            ush_scripts=frozenset(ush_scripts),
            config_files=frozenset(config_files),
            warnings=tuple(self._warnings),
            total_available_jjobs=total_jjobs,
            total_available_ex_scripts=total_ex,
            total_available_ush_scripts=total_ush,
            total_available_configs=total_configs,
        )

    # ------------------------------------------------------------------
    # Statistics helpers
    # ------------------------------------------------------------------

    @staticmethod
    def _count_dir_entries(directory: Path) -> int:
        """Count all entries (files) in a directory, returning 0 if missing."""
        if not directory.is_dir():
            return 0
        return sum(1 for entry in directory.iterdir() if entry.is_file())

    @staticmethod
    def _count_glob(directory: Path, pattern: str) -> int:
        """Count files matching a glob pattern in a directory."""
        if not directory.is_dir():
            return 0
        return sum(1 for _ in directory.glob(pattern))

    @staticmethod
    def _count_rglob(directory: Path, pattern: str) -> int:
        """Count files matching a recursive glob pattern in a directory."""
        if not directory.is_dir():
            return 0
        return sum(1 for _ in directory.rglob(pattern))

    def extract_jjobs_from_yaml(self) -> set[str]:
        """Layer 1: Extract jjob values from all task definitions.

        Walks ``families[].tasks[].jjob`` in the Workflow_YAML and collects
        the unique set of J-Job names (Application_Names).

        When a Name_Resolver is configured, existence validation is deferred
        to the resolve_jjobs() step. When no resolver is configured (backward
        compat), validates that each name exists directly in ``dev/jobs/``.

        Returns:
            Set of J-Job basenames (Application_Names) referenced by the workflow.

        Raises:
            PipelineError: If a referenced J-Job does not exist in dev/jobs/
                and no Name_Resolver is configured.
        """
        jjobs: set[str] = set()
        for family in self.workflow_yaml.get("families", []):
            for task in family.get("tasks", []):
                jjob = task.get("jjob")
                if jjob:
                    jjobs.add(jjob)

        # When no name_resolver is configured, validate existence directly
        # (backward compatibility: original behavior)
        if self._name_resolver is None:
            for jjob in sorted(jjobs):
                path = self.dev_root / "jobs" / jjob
                if not path.exists():
                    raise _get_pipeline_error()(
                        "dag_filter",
                        f"J-Job '{jjob}' referenced in Workflow_YAML does not "
                        f"exist at {path}",
                    )
        return jjobs

    def resolve_jjobs(self, app_names: set[str]) -> dict[str, "ResolvedName"]:
        """Resolve Application_Names to source files via Name_Resolver.

        If no Name_Resolver is configured, falls back to direct lookup
        (backward compatibility with shared-named workflows). In that case,
        each name is treated as both application AND source name (identity
        mapping).

        Args:
            app_names: Set of Application_Names extracted from Workflow_YAML.

        Returns:
            Dict mapping application_name → ResolvedName. When no resolver
            is configured, each name maps to itself as a passthrough.

        Raises:
            PipelineError: If a name cannot be resolved (when resolver is active)
                or if a name doesn't exist in dev/jobs/ (fallback mode).

        Traces to: Requirements 4.1, 4.2, 4.3, 4.4
        """
        from .name_resolver import ResolvedName

        if self._name_resolver is not None:
            # Use the Name_Resolver for resolution (raises on failure)
            return self._name_resolver.resolve_all(app_names)

        # Fallback: no resolver configured — identity mapping
        # Names are already validated by extract_jjobs_from_yaml() in this path
        result: dict[str, ResolvedName] = {}
        for name in sorted(app_names):
            result[name] = ResolvedName(
                application_name=name,
                source_name=name,
                is_passthrough=True,
            )
        return result

    def extract_ex_scripts(self, jjobs: set[str]) -> set[str]:
        """Layer 2: Parse J-Jobs to find invoked ex-scripts.

        Scans each J-Job file for ex-script invocation patterns and collects
        the set of referenced ex-scripts.  Validates that each referenced
        ex-script exists in ``dev/scripts/``.

        Args:
            jjobs: Set of J-Job basenames (e.g. {"JGLOBAL_FORECAST"}).

        Returns:
            Set of ex-script basenames (e.g. {"exglobal_forecast.sh"}).

        Raises:
            PipelineError: If a referenced ex-script does not exist in
                ``dev/scripts/``.
        """
        ex_scripts: set[str] = set()
        for jjob in sorted(jjobs):
            path = self.dev_root / "jobs" / jjob
            content = path.read_text()
            for pattern in _EX_SCRIPT_PATTERNS:
                for match in pattern.finditer(content):
                    ex_scripts.add(match.group("script"))

        # Validate existence
        for script in sorted(ex_scripts):
            script_path = self.dev_root / "scripts" / script
            if not script_path.exists():
                raise _get_pipeline_error()(
                    "dag_filter",
                    f"Ex-script '{script}' referenced by J-Job does not "
                    f"exist at {script_path}",
                )

        return ex_scripts

    # ------------------------------------------------------------------
    # Layer 3: Transitive ush script resolution
    # ------------------------------------------------------------------

    def extract_ush_scripts(self, ex_scripts: set[str]) -> set[str]:
        """Transitively resolve all ush scripts sourced by ex-scripts.

        Uses BFS with a visited set to compute the transitive closure of
        source dependencies.  Handles circular dependencies gracefully by
        emitting a WARNING without entering an infinite loop.  Missing ush
        scripts emit a WARNING (non-fatal, as they may be conditionally
        sourced).

        Args:
            ex_scripts: Set of ex-script basenames (e.g. {"exglobal_forecast.sh"}).

        Returns:
            Set of ush script basenames transitively reachable from the
            given ex-scripts.

        Traces to: Requirements 3.1, 3.2, 3.3, 3.4, 3.5
        """
        visited: set[str] = set()
        queue: deque[str] = deque()

        # Seed with ush scripts sourced by ex-scripts
        for ex_script in sorted(ex_scripts):
            path = self.dev_root / "scripts" / ex_script
            if not path.exists():
                # ex-script missing is handled by Layer 2; skip here
                continue
            for ush in self._parse_source_refs(path):
                if ush not in visited:
                    queue.append(ush)
                    visited.add(ush)

        # BFS transitive closure
        while queue:
            current = queue.popleft()
            ush_path = self.dev_root / "ush" / current
            if not ush_path.exists():
                self._warnings.append(
                    f"WARNING: Ush script '{current}' referenced but not "
                    f"found at {ush_path} (may be conditionally sourced)"
                )
                continue
            for dep in self._parse_source_refs(ush_path):
                if dep in visited:
                    # Already visited — circular dependency
                    self._warnings.append(
                        f"WARNING: Circular dependency detected: "
                        f"{current} -> {dep}"
                    )
                    continue
                visited.add(dep)
                queue.append(dep)

        return visited

    # ------------------------------------------------------------------
    # Layer 4: Config file extraction
    # ------------------------------------------------------------------

    def extract_config_files(self, jjobs: set[str]) -> set[str]:
        """Layer 4: Parse jjob_header -c flags for config requirements.

        Scans each J-Job file for ``jjob_header.sh -c "base fcst ..."``
        invocations and maps the basenames to actual config files under
        ``dev/parm/config/<app>/``.  Always includes unconditional configs
        (config.base.j2, config.base, config.com) and the platform-specific
        resource file.

        Args:
            jjobs: Set of J-Job basenames (e.g. {"JGLOBAL_FORECAST"}).

        Returns:
            Set of config file basenames required by the DAG-reachable tasks.

        Traces to: Requirements 4.1, 4.2, 4.3, 4.4, 4.5
        """
        configs: set[str] = set(_UNCONDITIONAL_CONFIGS)
        app = self._detect_app()
        config_dir = self.dev_root / "parm" / "config" / app

        for jjob in sorted(jjobs):
            path = self.dev_root / "jobs" / jjob
            try:
                content = path.read_text(encoding="utf-8", errors="replace")
            except OSError:
                continue
            # Find all -c flag matches (a J-Job may have multiple jjob_header
            # invocations, e.g. conditional on ENSMEM)
            for match in _JJOB_HEADER_PATTERN.finditer(content):
                basenames = match.group("configs").split()
                for base in basenames:
                    # Map basename to actual config file (prefer .j2 variant)
                    candidates = [
                        f"config.{base}.j2",
                        f"config.{base}",
                    ]
                    for candidate in candidates:
                        if (config_dir / candidate).exists():
                            configs.add(candidate)
                            break

        # Add platform-specific resource file (Req 4.5)
        platform_resource = f"config.resources.{self.platform}"
        if (config_dir / platform_resource).exists():
            configs.add(platform_resource)
        # Always include the base (non-platform-specific) resources file
        configs.add("config.resources")

        return configs

    # ------------------------------------------------------------------
    # Helpers
    # ------------------------------------------------------------------

    def _detect_app(self) -> str:
        """Determine the app name for config directory lookup.

        Derives the app name from the workflow_yaml context.  The app name
        corresponds to a subdirectory under ``dev/parm/config/`` (e.g. "gfs",
        "gcafs", "gefs", "sfs").

        Strategy:
        1. If the workflow_yaml has a top-level "app" key, use it
        2. Otherwise derive from suite.name prefix (e.g. "gfs_v17" → "gfs")
        3. Validate the derived name matches an existing config directory
        4. Fall back to "gfs" if no match found

        Returns:
            App name string matching a config subdirectory.
        """
        config_base = self.dev_root / "parm" / "config"

        # Strategy 1: explicit "app" key in context (set by pipeline)
        app_value = self.workflow_yaml.get("app")
        if app_value and isinstance(app_value, str):
            # The pipeline sets app = config_path.stem (e.g. "gfs_forecast_only")
            # We need to find the matching config directory prefix
            candidate = self._match_config_dir(app_value, config_base)
            if candidate:
                return candidate

        # Strategy 2: derive from suite.name prefix
        suite = self.workflow_yaml.get("suite", {})
        suite_name = suite.get("name", "") if isinstance(suite, dict) else ""
        if suite_name:
            prefix = suite_name.split("_")[0]
            if (config_base / prefix).is_dir():
                return prefix

        # Strategy 3: derive from NET if available in context
        net = self.workflow_yaml.get("NET")
        if net and isinstance(net, str) and (config_base / net).is_dir():
            return net

        # Fallback
        return "gfs"

    def _match_config_dir(self, app_value: str, config_base: Path) -> str | None:
        """Find the config directory that matches an app value.

        Checks if the app_value itself is a valid directory, or if any
        known config directory is a prefix of the app_value.

        Args:
            app_value: The app identifier (e.g. "gfs_forecast_only" or "gfs").
            config_base: Path to dev/parm/config/.

        Returns:
            Matching directory name or None.
        """
        # Direct match
        if (config_base / app_value).is_dir():
            return app_value

        # Prefix match — find the longest config dir name that is a prefix
        if config_base.is_dir():
            candidates = sorted(
                (d.name for d in config_base.iterdir() if d.is_dir()),
                key=len,
                reverse=True,  # longest first for best match
            )
            for dirname in candidates:
                if app_value.startswith(dirname):
                    return dirname

        return None

    def _parse_source_refs(self, script_path: Path) -> list[str]:
        """Parse a shell script for ush source references.

        Scans the file content against _USH_SOURCE_PATTERNS and returns
        a list of matched ush script basenames.

        Args:
            script_path: Path to the shell script to parse.

        Returns:
            List of ush script basenames referenced via source/dot-source.
        """
        refs: list[str] = []
        try:
            content = script_path.read_text(encoding="utf-8", errors="replace")
        except OSError:
            return refs

        for line in content.splitlines():
            # Skip comment lines
            stripped = line.lstrip()
            if stripped.startswith("#"):
                continue
            for pattern in _USH_SOURCE_PATTERNS:
                match = pattern.search(line)
                if match:
                    refs.append(match.group("script"))
        return refs
