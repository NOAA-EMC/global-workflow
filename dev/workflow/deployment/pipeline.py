"""Deployment pipeline orchestration.

Implements the 8-stage pipeline:
  validate → build context → render templates → stage files →
  generate DAG → EE2 scan → manifest → seal

The `run()` function is the main entry point, invoked by the CLI
(deploy.py) with parsed arguments.

Traces to: Requirements 3.1, 8.1, 8.2
"""

from __future__ import annotations

import enum
import hashlib
import logging
import os
import shutil
import subprocess
import time
from dataclasses import dataclass, field
from datetime import datetime, timezone
from pathlib import Path
from typing import TYPE_CHECKING, Any, Optional

import yaml

from .config_conditioner import ConfigConditioner
from .dag_generator import generate_def, generate_ecf_scripts, parse_suite_config
from .deploy_time_vars import get_deploy_time_values
from .ee2_scanner import run_compliance_scan
from .model_config_renderer import ModelConfigRenderer, RenderedFile
from .platform_conditioner import render_all_platform_conditioned
from .template_renderer import TemplateRenderer, TemplateRenderError
from .validation import _get_installed_version, check_pinned_versions
from .workflow_config import parse as parse_workflow_config

if TYPE_CHECKING:
    from .completeness_verifier import CompletenessVerifier
    from .dag_filter import DAGFilter, DAGReachabilitySet
    from .name_resolver import NameResolver

# Packages whose pinned versions are enforced as a hard precondition
# before any EXPDIR file is written (Req 5.1, 5.2).
_ENFORCED_PACKAGES: tuple[str, ...] = ("wxflow", "uwtools")

logger = logging.getLogger(__name__)


# ---------------------------------------------------------------------------
# Exceptions
# ---------------------------------------------------------------------------


class PipelineError(Exception):
    """Base exception for pipeline errors.

    All pipeline stage failures raise this with a descriptive FATAL ERROR
    message following the design document conventions.
    """

    def __init__(self, stage: str, message: str) -> None:
        self.stage = stage
        self.message = message
        super().__init__(f"FATAL ERROR [{stage}]: {message}")


# ---------------------------------------------------------------------------
# Size Reduction Reporting (Req 9)
# ---------------------------------------------------------------------------


@dataclass
class SizeReductionReport:
    """Statistics comparing filtered vs full deployment (Req 9).

    Captures the count of staged (DAG-reachable) artifacts versus the
    total available in the dev/ source tree for each artifact category.

    Traces to: Requirements 9.1, 9.2, 9.3, 9.4
    """

    staged_jjobs: int
    total_jjobs: int
    staged_ex_scripts: int
    total_ex_scripts: int
    staged_ush_scripts: int
    total_ush_scripts: int
    staged_configs: int
    total_configs: int

    def log(self) -> None:
        """Log the size reduction statistics."""
        logger.info("  DAG Filter Results:")
        logger.info(f"    J-Jobs:      {self.staged_jjobs}/{self.total_jjobs} staged")
        logger.info(f"    Ex-Scripts:  {self.staged_ex_scripts}/{self.total_ex_scripts} staged")
        logger.info(f"    Ush Scripts: {self.staged_ush_scripts}/{self.total_ush_scripts} staged")
        logger.info(f"    Configs:     {self.staged_configs}/{self.total_configs} staged")


# ---------------------------------------------------------------------------
# Pipeline configuration
# ---------------------------------------------------------------------------

# Supported platforms (Req 12.1)
SUPPORTED_PLATFORMS = frozenset({
    "WCOSS2",
    "HERA",
    "HERCULES",
    "ORION",
    "GAEAC6",
    "DERECHO",
    "URSA",
    "AWSPW",
    "AZUREPW",
    "GOOGLEPW",
    "CONTAINER",
})

# Source-to-target mapping for file staging (design table)
# Maps dev/ subdirectories to EXPDIR subdirectories
_STAGE_MAPPING: list[tuple[str, str]] = [
    ("jobs", "jobs"),
    ("scripts", "scripts"),
    ("ush", "ush"),
    ("parm", "parm"),
    ("sorc", "sorc"),
    ("env", "env"),
    ("versions", "versions"),
    ("modulefiles", "modulefiles"),
]

# Directories excluded from staging by default (Req 8.7)
_DEFAULT_EXCLUDES = frozenset({
    "ci",
    "ctests",
    "workflow/tests",
    "workflow/deployment/__pycache__",
})

# Submodule copy manifest: files owned by external submodules that are
# copied verbatim (never templated) into the EXPDIR.
# Each entry maps a source path (relative to project root) to a
# destination path (relative to EXPDIR).
# Traces to: Requirements 13.1, 13.2, 13.3, 13.4, 13.5
SUBMODULE_COPY_MANIFEST: list[tuple[str, str]] = [
    ("sorc/nexus.fd/config/gocart/", "parm/chem/nexus/gocart/"),
    ("sorc/upp.fd/parm/", "parm/post/"),
]

# Submodule sources that may be skipped under SubmodulePolicy.SKIP_OPTIONAL.
# Identified by their source path (relative to project root) as it appears
# in SUBMODULE_COPY_MANIFEST. These submodules contribute non-essential
# inputs (e.g. chemistry/post config) and a non-production EXPDIR can be
# produced without them; a warning is emitted when they are skipped.
_OPTIONAL_SUBMODULE_SOURCES: frozenset[str] = frozenset({
    "sorc/nexus.fd/config/gocart/",
    "sorc/upp.fd/parm/",
})


class SubmodulePolicy(enum.Enum):
    """Resolution policy for missing Submodule_Source files (Req 6.1, 6.2).

    - ``REQUIRE``: the production default. A missing source raises a FATAL
      ``PipelineError`` — submodules must be checked out and fetched.
    - ``FIXTURE``: the verification policy. Missing sources are resolved
      from a documented, byte-stable ``fixture_root`` (the committed
      Submodule_Fixture) so a clean deploy completes without a
      "Submodule source not found" FATAL.
    - ``SKIP_OPTIONAL``: a non-production policy. Entries flagged optional
      (see ``_OPTIONAL_SUBMODULE_SOURCES``) are skipped with a warning when
      their source is missing; non-optional missing sources are still FATAL.
    """

    REQUIRE = "require"
    FIXTURE = "fixture"
    SKIP_OPTIONAL = "skip"

# Template directories to render (Req 4.5)
_TEMPLATE_DIRS = [
    "parm",
    "workflow",
    "ecf",
]

# ---------------------------------------------------------------------------
# Config-file filtering: skip analysis configs not used by the workflow
# ---------------------------------------------------------------------------

# Mapping of config file basename patterns to the J-Job names that require
# them. If none of the listed J-Jobs appear in the workflow YAML's task list,
# the config file is skipped during rendering and staging. This prevents
# analysis-specific configs (e.g. aeroanl) from being deployed into a
# forecast-only EXPDIR that has no aerosol analysis tasks.
#
# Pattern matching: a config file is skipped if its basename starts with any
# key in this dict AND none of the corresponding J-Jobs are active.
_CONFIG_JJOB_REQUIREMENTS: dict[str, list[str]] = {
    "config.aeroanl": [
        "JGDAS_ATMOS_ANALYSIS_AEROANL",
        "JGLOBAL_AERO_ANALYSIS_INITIALIZE",
        "JGLOBAL_AERO_ANALYSIS_VARIATIONAL",
        "JGLOBAL_AERO_ANALYSIS_FINALIZE",
        "JGDAS_AERO_ANALYSIS_GENERATE_BMATRIX",
    ],
    "config.atmanl": [
        "JGDAS_ATMOS_ANALYSIS",
        "JGLOBAL_ATMOS_ANALYSIS_INIT",
        "JGLOBAL_ATMOS_ANALYSIS_VAR",
        "JGLOBAL_ATMOS_ANALYSIS_FINAL",
        "JGLOBAL_ATMOS_ANALYSIS_FV3INC",
    ],
    "config.atmensanl": [
        "JGDAS_ENKF_ANALYSIS",
        "JGLOBAL_ATMENS_ANALYSIS_INIT",
        "JGLOBAL_ATMENS_ANALYSIS_OBS",
        "JGLOBAL_ATMENS_ANALYSIS_SOL",
        "JGLOBAL_ATMENS_ANALYSIS_LETKF",
        "JGLOBAL_ATMENS_ANALYSIS_FV3INC",
        "JGLOBAL_ATMENS_ANALYSIS_FINAL",
    ],
    "config.marineanl": [
        "JGLOBAL_MARINE_ANALYSIS_INIT",
        "JGLOBAL_MARINE_ANALYSIS_VAR",
        "JGLOBAL_MARINE_ANALYSIS_FINAL",
        "JGLOBAL_MARINE_ANALYSIS_CHKPT",
        "JGLOBAL_MARINE_ANALYSIS_ECEN",
        "JGLOBAL_MARINE_ANALYSIS_LETKF",
    ],
    "config.marinebmat": [
        "JGLOBAL_MARINE_BMAT",
        "JGLOBAL_MARINE_BMAT_INIT",
    ],
    "config.snowanl": [
        "JGLOBAL_SNOW_ANALYSIS",
    ],
    "config.esnowanl": [
        "JGDAS_ENKF_SNOW_ANALYSIS",
    ],
}


def _extract_active_jjobs(context: dict[str, Any]) -> set[str]:
    """Extract the set of active J-Job names from the workflow YAML context.

    Walks the ``families[].tasks[].jjob`` entries in the loaded workflow
    configuration to determine which J-Jobs are actually used by this
    workflow. Returns an empty set if the context has no families (in which
    case no filtering is applied — all configs are rendered).
    """
    jjobs: set[str] = set()
    families = context.get("families")
    if not isinstance(families, list):
        return jjobs
    for family in families:
        if not isinstance(family, dict):
            continue
        tasks = family.get("tasks")
        if not isinstance(tasks, list):
            continue
        for task in tasks:
            if not isinstance(task, dict):
                continue
            jjob = task.get("jjob")
            if jjob:
                jjobs.add(str(jjob))
    return jjobs


def _should_skip_config(basename: str, active_jjobs: set[str]) -> bool:
    """Return True if a config file should be skipped based on active J-Jobs.

    A config file is skipped when:
    1. Its basename starts with a pattern in _CONFIG_JJOB_REQUIREMENTS, AND
    2. None of the J-Jobs listed for that pattern are in the active set.

    If active_jjobs is empty (no families in the workflow YAML), no filtering
    is applied (returns False for everything).
    """
    if not active_jjobs:
        return False
    for pattern, required_jjobs in _CONFIG_JJOB_REQUIREMENTS.items():
        if basename.startswith(pattern):
            if not any(jjob in active_jjobs for jjob in required_jjobs):
                return True
    return False


# ---------------------------------------------------------------------------
# Stage implementations
# ---------------------------------------------------------------------------


def _stage_validate(
    config_path: Path,
    platform: str,
    expdir: Path,
    version: str,
    dev_root: Path,
    *,
    enforce_versions: bool = False,
) -> None:
    """Stage 1: Validate inputs.

    Checks:
    - Config file exists
    - Platform is supported
    - Git state is accessible
    - EXPDIR does not already contain a manifest (immutability guard)
    - prefix_registry.yaml exists in the deployment directory
    - wxflow/uwtools pinned versions match requirements.txt; when
      ``enforce_versions`` is True, a package that is not importable is
      treated as a FATAL ERROR (not a warning) so the version guard is a
      hard precondition before any EXPDIR file is written.

    All checks run before the caller creates the EXPDIR, so a FATAL ERROR
    here guarantees no EXPDIR file has been written (Req 5.1, 5.2).

    Traces to: Requirements 3.5, 5.4, 9.5, 5.1, 5.2
    """
    logger.info("Stage 1/8: Validate inputs")

    # Check config file exists
    if not config_path.is_file():
        raise PipelineError(
            "validate",
            f"Configuration file not found: {config_path}",
        )

    # Check platform is supported
    if platform.upper() not in SUPPORTED_PLATFORMS:
        raise PipelineError(
            "validate",
            f"Unsupported platform '{platform}'. "
            f"Supported: {sorted(SUPPORTED_PLATFORMS)}",
        )

    # Check EXPDIR immutability guard (Req 3.5)
    manifest_path = expdir / "manifest.yaml"
    if manifest_path.exists():
        try:
            manifest_data = yaml.safe_load(manifest_path.read_text())
            snapshot_id = manifest_data.get("snapshot_id", "unknown")
        except Exception:
            snapshot_id = "unknown"
        raise PipelineError(
            "validate",
            f"EXPDIR already published with Snapshot_ID {snapshot_id}. "
            f"Cannot overwrite a sealed deployment.",
        )

    # Verify version string is non-empty
    if not version or not version.strip():
        raise PipelineError(
            "validate",
            "Version string must be non-empty (e.g. 'v17.0.0').",
        )

    # Check prefix_registry.yaml exists (Req 5.4)
    prefix_registry_path = Path(__file__).parent / "prefix_registry.yaml"
    if not prefix_registry_path.is_file():
        raise PipelineError(
            "validate",
            f"Prefix registry not found at {prefix_registry_path}",
        )

    # --- wxflow/uwtools version gate (Req 5.1, 5.2) ---
    # This runs before any EXPDIR file is written. A version *mismatch*
    # is always FATAL; a *missing* (not importable) package is FATAL only
    # when enforcing, so the broad test suite can still exercise run()
    # in environments that do not provide every pinned package.
    _check_version_gate(dev_root, enforce_versions=enforce_versions)

    logger.info("  ✓ Inputs validated successfully")


def _check_version_gate(dev_root: Path, *, enforce_versions: bool) -> None:
    """Enforce the wxflow/uwtools version pinning precondition.

    Reads the pinned versions from ``dev_root/workflow/requirements.txt``
    and compares them against the installed package versions via
    ``validation.check_pinned_versions``. A version mismatch is always a
    FATAL ERROR. When ``enforce_versions`` is True, a package that is not
    importable is also a FATAL ERROR identifying the package, the expected
    version, and the found state.

    Args:
        dev_root: Path to the dev/ root (parent of workflow/).
        enforce_versions: When True, treat a not-importable enforced
            package as FATAL; otherwise it is a non-fatal warning.

    Raises:
        PipelineError: If any enforced package is missing (when enforcing)
            or its installed version does not match the pinned version.
    """
    req_path = dev_root / "workflow" / "requirements.txt"

    # When the requirements file is absent (e.g. a minimal verification
    # tree), there is nothing to pin against. Treat that as FATAL only when
    # enforcing; otherwise skip the gate so run() stays usable on trees that
    # do not ship a requirements.txt.
    if not req_path.is_file():
        if enforce_versions:
            raise PipelineError(
                "validate",
                f"FATAL ERROR: Requirements file not found: {req_path}",
            )
        return

    vres = check_pinned_versions(req_path)
    pinned = _parse_pinned_for_packages(req_path, _ENFORCED_PACKAGES)

    # A version *mismatch* (package importable but wrong version) is always
    # FATAL (Req 5.2). check_pinned_versions records mismatches as errors and
    # not-importable packages as warnings.
    errors: list[str] = list(vres.errors)

    if enforce_versions:
        # A *missing* (not importable) enforced package is also FATAL when
        # enforcing (Req 5.1), identifying package/expected/found.
        for pkg in _ENFORCED_PACKAGES:
            if _get_installed_version(pkg) is None:
                expected = pinned.get(pkg, "unknown")
                errors.append(
                    f"FATAL ERROR: required package '{pkg}' is not importable "
                    f"(expected {expected}, found: not installed); "
                    f"deployment refuses to write any EXPDIR file."
                )

    if errors:
        raise PipelineError("validate", "; ".join(errors))


def _parse_pinned_for_packages(
    requirements_path: Path, packages: tuple[str, ...]
) -> dict[str, str]:
    """Return the pinned versions for the named packages from requirements.

    Args:
        requirements_path: Path to the requirements.txt file.
        packages: Package names of interest (matched case-insensitively).

    Returns:
        Dict mapping package name to its pinned version string. Packages
        without an exact (==) pin are omitted.
    """
    from .validation import _parse_pinned_versions

    all_pinned = _parse_pinned_versions(requirements_path)
    return {pkg: all_pinned[pkg] for pkg in packages if pkg in all_pinned}


def _derive_net_run_mode(config_data: dict[str, Any]) -> dict[str, str]:
    """Derive NET, RUN, and MODE from the Workflow_Configuration data.

    Inspects the suite name and cycle definitions to determine:
      - NET: The model network (e.g. 'gfs', 'gefs', 'sfs')
      - RUN: The primary run identifier (e.g. 'gdas', 'gfs')
      - MODE: The workflow mode (e.g. 'cycled', 'forecast-only')

    Args:
        config_data: Parsed YAML dict from the Workflow_Configuration file.

    Returns:
        Dict with keys 'NET', 'RUN', 'MODE'.
    """
    suite = config_data.get("suite", {})
    suite_name = suite.get("name", "")

    # Derive NET from suite name (e.g. 'gfs_v17' -> 'gfs', 'gefs_v13' -> 'gefs')
    net = suite_name.split("_")[0] if suite_name else "gfs"

    # Derive RUN from cycles - use the first cycle name
    cycles = config_data.get("cycles", [])
    if cycles and isinstance(cycles, list) and isinstance(cycles[0], dict):
        run = cycles[0].get("name", net)
    else:
        run = net

    # Derive MODE from inter_cycle_dependencies and suite name
    inter_cycle = config_data.get("inter_cycle_dependencies", [])
    if "fcst_only" in suite_name or "forecast_only" in suite_name:
        mode = "forecast-only"
    elif inter_cycle:
        mode = "cycled"
    else:
        mode = "free-forecast"

    return {
        "NET": net,
        "RUN": run,
        "MODE": mode,
    }


def build_context(
    config_path: Path,
    platform: str,
    version: str,
    expdir: Path,
    comroot: Optional[str] = None,
    repo_root: Optional[Path] = None,
) -> dict[str, Any]:
    """Assemble the deployment-time Jinja2 context dictionary.

    Builds the context dict used by the Template_Renderer to resolve
    Jinja2 templates during deployment. The context includes:
      - PDY, cyc: Shell variable references (${PDY}, ${cyc}) for runtime expansion
      - NET, RUN, MODE: Derived from the Workflow_Configuration
      - MACHINE: The target platform
      - model_ver: The deployment version string
      - EXPDIR: The destination EXPDIR path
      - COMROOT: The COM output root path
      - Git metadata: commit, branch, remote
      - The full configuration tree from the YAML file

    PDY and cyc remain as shell variables because they are resolved at
    runtime by the ecFlow server and the Universal_Wrapper, not at
    deployment time.

    Args:
        config_path: Path to the Workflow_Configuration YAML file.
        platform: Target HPC platform (e.g. 'HERA', 'WCOSS2').
        version: Semantic version string (e.g. 'v17.0.0').
        expdir: Destination EXPDIR path.
        comroot: Optional COM output root path. Defaults to
            '<expdir>/com' if not specified.
        repo_root: Optional path to the git repository root for
            retrieving git metadata. If None, uses config_path's
            parent directory.

    Returns:
        A dict containing all deployment-time context variables for
        Jinja2 template resolution.

    Raises:
        FileNotFoundError: If config_path does not exist.
        yaml.YAMLError: If the configuration file is not valid YAML.

    Traces to: Requirement 4.1 (deployment-time context)
    """
    config_path = Path(config_path)
    expdir = Path(expdir)

    if not config_path.exists():
        raise FileNotFoundError(
            f"Workflow configuration not found: {config_path}"
        )

    # Load the Workflow_Configuration YAML
    with open(config_path, "r") as f:
        config_data = yaml.safe_load(f)

    if config_data is None:
        config_data = {}

    # Derive NET, RUN, MODE from the configuration
    net_run_mode = _derive_net_run_mode(config_data)

    # Determine COMROOT
    if comroot is None:
        comroot = str(expdir / "com")

    # Determine repo root for git metadata
    if repo_root is None:
        repo_root = config_path.parent

    # Retrieve git metadata
    git_commit = _get_git_commit(repo_root)
    git_branch = _get_git_branch(repo_root)
    git_remote = _get_git_remote(repo_root)

    # Derive app name from config filename (e.g. 'gfs_cycled.yaml' -> 'gfs_cycled')
    app = config_path.stem

    # Load platform-specific host variables (e.g. PARTITION_BATCH, QUEUE, etc.)
    # Host configs live at <dev_root>/workflow/hosts/<platform>.yaml or
    # <repo_root>/workflow/hosts/<platform>.yaml (repo_root may be dev_root).
    host_vars: dict[str, Any] = {}
    host_candidates = [
        repo_root / "workflow" / "hosts" / f"{platform.lower()}.yaml",
        repo_root / "hosts" / f"{platform.lower()}.yaml",
    ]
    for host_path in host_candidates:
        if host_path.is_file():
            with open(host_path, "r") as hf:
                loaded = yaml.safe_load(hf)
            if loaded and isinstance(loaded, dict):
                host_vars = loaded
            break

    # Build the context dictionary
    # Merge defaults from config into top-level context so templates
    # can reference them directly (e.g. {{ ECF_TRIES }})
    defaults = config_data.get("defaults", {}) or {}

    context: dict[str, Any] = {
        # Runtime shell variables - preserved for shell expansion
        "PDY": "${PDY}",
        "cyc": "${cyc}",
        # Workflow identity
        "NET": net_run_mode["NET"],
        "RUN": net_run_mode["RUN"],
        "MODE": net_run_mode["MODE"],
        # Platform and version
        "MACHINE": platform.upper(),
        "model_ver": version,
        # Paths
        "EXPDIR": str(expdir),
        "COMROOT": comroot,
        # Application identifier
        "app": app,
        # Git metadata
        "git_commit": git_commit,
        "git_branch": git_branch,
        "git_remote": git_remote,
        # Deployment metadata
        "deploy_timestamp": datetime.now(timezone.utc).isoformat(),
        # Include the full configuration tree for template access
        **config_data,
        # Flatten defaults into top-level context (after config_data
        # so individual defaults override the nested 'defaults' key)
        **defaults,
        # Merge platform host variables (PARTITION_BATCH, QUEUE, etc.)
        # after defaults so host-specific values take precedence
        **host_vars,
    }

    return context


def _stage_build_context(
    config_path: Path,
    platform: str,
    version: str,
    expdir: Path,
    dev_root: Path,
) -> dict[str, Any]:
    """Stage 2: Build deployment-time Jinja2 context.

    Assembles the context dict from:
    - Workflow_Configuration YAML values
    - Platform name
    - Version string
    - Git metadata (commit, branch, remote)
    - Standard deployment variables (PDY, cyc, NET, RUN, MODE, MACHINE,
      model_ver, EXPDIR, COMROOT)

    Delegates to the public build_context() function and logs progress.

    Returns:
        The assembled context dictionary.

    Traces to: Requirement 4.1
    """
    logger.info("Stage 2/8: Build deployment context")

    try:
        context = build_context(
            config_path=config_path,
            platform=platform,
            version=version,
            expdir=expdir,
            repo_root=dev_root,
        )
    except FileNotFoundError as e:
        raise PipelineError("build_context", str(e)) from e
    except yaml.YAMLError as e:
        raise PipelineError(
            "build_context",
            f"Failed to parse configuration YAML: {e}",
        ) from e

    logger.info(f"  ✓ Context built (platform={platform}, version={version})")
    return context


def _load_name_resolver(dev_root: Path) -> "NameResolver":
    """Load PrefixRegistry and instantiate NameResolver.

    Called during Stage 2 (Build Context) to prepare the Name_Resolver
    for use by DAG_Filter and File_Stager in later stages.

    The prefix_registry.yaml is expected in the deployment directory
    (same directory as pipeline.py).

    Args:
        dev_root: Path to the dev/ directory.

    Returns:
        A configured NameResolver instance.

    Raises:
        PipelineError: If prefix_registry.yaml is missing or malformed.

    Traces to: Requirements 2.7, 5.3, 5.4
    """
    from .name_resolver import NameResolver, PrefixRegistry

    registry_path = Path(__file__).parent / "prefix_registry.yaml"
    registry = PrefixRegistry.load(registry_path)
    resolver = NameResolver(dev_root, registry)
    logger.info("  ✓ PrefixRegistry loaded, NameResolver instantiated")
    return resolver


def _stage_render_templates(
    dev_root: Path,
    expdir: Path,
    context: dict[str, Any],
    platform: str,
    reachability_set: Optional[DAGReachabilitySet] = None,
) -> tuple[list[Path], list[RenderedFile]]:
    """Stage 3: Render Jinja2 templates.

    Invokes the TemplateRenderer on all .j2 files under dev/parm/,
    dev/workflow/, and dev/ecf/ (Req 4.5). Then renders UFS model
    configuration templates via ModelConfigRenderer (Req 7.7, 9.5).

    When a reachability_set is provided (DAG filtering enabled), uses
    render_for_dag() to render only model inputs required by DAG-reachable
    tasks. Otherwise uses render_all() for full rendering.

    Model input pre-rendering applies regardless of --dag-filter flag
    (Req 13.3). The reachability_set only controls whether DAG-aware
    filtering is applied to determine which UFS components are active.

    After rendering, verify_no_unresolved_tokens() is called on all
    rendered model inputs to ensure no Jinja2 tokens remain (Req 6.4).

    Skips analysis-specific config templates whose required J-Jobs are
    not present in the workflow YAML (e.g. config.aeroanl.j2 is skipped
    for a forecast-only workflow that has no aerosol analysis tasks).

    Args:
        dev_root: Path to the dev/ directory root.
        expdir: Path to the EXPDIR where rendered files will be placed.
        context: The deployment-time Jinja2 context dictionary.
        platform: Target HPC platform name.
        reachability_set: Optional DAG reachability set. When provided,
            render_for_dag() is used for model inputs; otherwise
            render_all() is used.

    Returns:
        Tuple of (rendered output file paths, model config RenderedFile list).

    Raises:
        PipelineError: If template rendering or model config format
            validation fails (FATAL ERROR).

    Traces to: Requirements 6.1, 6.7, 13.3
    """
    logger.info("Stage 3/8: Render templates")

    # Determine the app name from context (default to 'gfs')
    app = context.get("app", context.get("NET", "gfs"))

    # Extract active J-Jobs for config filtering
    active_jjobs = _extract_active_jjobs(context)

    # Create the renderer with the standard searchpath
    renderer = TemplateRenderer.create(
        context=context,
        dev_root=str(dev_root),
        app=app,
        strict=True,
    )

    rendered_files: list[Path] = []

    # Directories/patterns to skip during general template rendering.
    # The ecFlow task templates (task.ecf.j2, head.h.j2, etc.) are rendered
    # per-task by the DAG_Generator in stage 5, not here.
    # UFS model config templates (parm/ufs/) are rendered by the
    # ModelConfigRenderer in sub-stage 3b when a 'model' context is present.
    skip_patterns = {
        "ecflow/templates",
        "ecflow/include",
        "ufs/",
    }

    for template_dir_name in _TEMPLATE_DIRS:
        src_dir = dev_root / template_dir_name
        if not src_dir.is_dir():
            logger.debug(f"  Skipping {template_dir_name}/ (not found)")
            continue

        # Determine the target directory in EXPDIR
        if template_dir_name == "ecf":
            dst_dir = expdir / "ecf"
        elif template_dir_name == "workflow":
            # Workflow templates that produce ecf definitions go to ecf/
            # Other workflow files stay as workflow metadata
            dst_dir = expdir / "workflow"
        else:
            dst_dir = expdir / template_dir_name

        # Render all .j2 files in this tree, skipping ecFlow templates
        try:
            count = 0
            for src_file in sorted(src_dir.rglob("*.j2")):
                # Check if this file is in a skip pattern directory
                rel_to_src = src_file.relative_to(src_dir)
                should_skip = any(
                    str(rel_to_src).startswith(pattern)
                    for pattern in skip_patterns
                )
                if should_skip:
                    continue

                # Skip templates in other app directories (e.g. config/gcafs/
                # when deploying for app 'gfs'). Only render templates in the
                # active app's config directory or in non-app-specific paths.
                rel_str = str(rel_to_src)
                if rel_str.startswith("config/"):
                    parts = rel_str.split("/")
                    if len(parts) >= 2:
                        template_app_dir = parts[1]
                        # Known app directories that are app-specific
                        config_dir = src_dir / "config"
                        if config_dir.is_dir():
                            app_dirs = {
                                d.name
                                for d in config_dir.iterdir()
                                if d.is_dir()
                            }
                            if template_app_dir in app_dirs and template_app_dir != app:
                                continue

                # Skip analysis-specific configs not used by this workflow
                if _should_skip_config(src_file.stem, active_jjobs):
                    logger.debug(
                        f"  Skipping {rel_to_src} (no matching J-Job in workflow)"
                    )
                    continue

                # Compute destination path (strip .j2 suffix)
                dst_name = rel_to_src.name[:-3]
                dst_file = dst_dir / rel_to_src.parent / dst_name

                renderer.render_file(src_file, dst_file)
                rendered_files.append(dst_file)
                count += 1

            logger.info(
                f"  ✓ Rendered {count} template(s) from {template_dir_name}/"
            )
        except Exception as e:
            raise PipelineError(
                "render_templates",
                f"Template rendering failed in {template_dir_name}/: {e}",
            ) from e

    logger.info(f"  ✓ Total rendered: {len(rendered_files)} file(s)")

    # --- Sub-stage 3b: Model Config Rendering (Req 6.1, 6.7, 7.7, 9.5, 13.3) ---
    # Model input pre-rendering applies regardless of --dag-filter flag.
    # When a reachability_set is available, use render_for_dag() to render
    # only model inputs required by DAG-reachable tasks. Otherwise use
    # render_all() for full rendering.
    model_context = context.get("model")
    model_rendered_files: list[RenderedFile] = []
    if model_context and isinstance(model_context, dict):
        logger.info("  Stage 3b: Render model configuration templates")
        model_renderer = ModelConfigRenderer(dev_root=dev_root)
        try:
            if reachability_set is not None:
                logger.info(
                    "  Using DAG-aware rendering (render_for_dag)"
                )
                model_rendered_files = model_renderer.render_for_dag(
                    model_context=model_context,
                    expdir=expdir,
                    reachability_set=reachability_set,
                )
            else:
                model_rendered_files = model_renderer.render_all(
                    model_context=model_context,
                    expdir=expdir,
                )
            logger.info(
                f"  ✓ Rendered {len(model_rendered_files)} model config file(s)"
            )
        except TemplateRenderError as e:
            raise PipelineError(
                "render_templates",
                f"Model config format validation failed: {e}",
            ) from e

        # Verify no unresolved Jinja2 tokens remain in rendered model
        # inputs (Req 6.4). This check applies regardless of --dag-filter.
        model_renderer.verify_no_unresolved_tokens(model_rendered_files)
        logger.info(
            "  ✓ Zero-token verification passed for model inputs"
        )

        # Add model config rendered paths to the overall rendered list
        for rf in model_rendered_files:
            rendered_files.append(rf.path)

    return rendered_files, model_rendered_files


def _stage_stage_files(
    dev_root: Path,
    expdir: Path,
    allowlist: Optional[list[str]],
    context: Optional[dict[str, Any]] = None,
    *,
    reachability: Optional[DAGReachabilitySet] = None,
) -> list[Path]:
    """Stage 4: Stage non-template files from dev/ to EXPDIR.

    Copies files following the source-to-target mapping defined in the
    design document. Excludes dev/ci/, dev/ctests/ by default unless
    they appear in the allowlist.

    When ``reachability`` is provided (DAG filtering enabled), only files
    in the reachability set are staged. When ``None``, all files are
    staged using full-copy behavior.

    Skips analysis-specific config files whose required J-Jobs are not
    present in the workflow YAML (same filter as Stage 3).

    Uses shutil for file copying (uwtools uw fs copy API equivalent).

    Args:
        dev_root: Path to the dev/ directory.
        expdir: Destination EXPDIR path.
        allowlist: Optional list of dev/ paths to include that are
            normally excluded.
        context: Pipeline context dict for J-Job filtering.
        reachability: When provided, only stage files in the
            DAG_Reachability_Set. When None, stage all files (full mode).

    Returns:
        List of staged file paths in the EXPDIR.

    Traces to: Requirements 8.2, 8.7, 8.8, 9.2, 13.1, 13.2
    """
    logger.info("Stage 4/8: Stage files")

    staged_files: list[Path] = []
    allowlist_set = set(allowlist) if allowlist else set()
    active_jjobs = _extract_active_jjobs(context) if context else set()

    for src_subdir, dst_subdir in _STAGE_MAPPING:
        src_dir = dev_root / src_subdir
        if not src_dir.is_dir():
            logger.debug(f"  Skipping {src_subdir}/ (not found)")
            continue

        dst_dir = expdir / dst_subdir

        # Walk the source directory and copy non-template files
        for src_file in sorted(src_dir.rglob("*")):
            if not src_file.is_file():
                continue

            # Skip .j2 template files (already rendered in stage 3)
            if src_file.suffix == ".j2":
                continue

            # Skip analysis-specific configs not used by this workflow
            if _should_skip_config(src_file.name, active_jjobs):
                continue

            # DAG reachability filter: when enabled, only stage files
            # that are in the reachability set (Req 13.1, 13.2)
            if reachability is not None:
                if src_subdir == "jobs":
                    if not reachability.contains_jjob(src_file.name):
                        continue
                elif src_subdir == "scripts":
                    if not reachability.contains_ex_script(src_file.name):
                        continue
                elif src_subdir == "ush":
                    # Always stage ush/python/ (runtime import dependency,
                    # not discoverable via shell source tracing)
                    rel_to_ush = src_file.relative_to(src_dir)
                    if not str(rel_to_ush).startswith("python"):
                        if not reachability.contains_ush_script(src_file.name):
                            continue
                elif src_subdir == "parm":
                    # For config files under parm/config/, check reachability
                    rel_path_str = str(src_file.relative_to(src_dir))
                    if rel_path_str.startswith("config/"):
                        if not reachability.contains_config(src_file.name):
                            continue

            # Skip __pycache__ and .pyc files
            if "__pycache__" in str(src_file) or src_file.suffix in (".pyc", ".pyo"):
                continue

            # Check exclusions
            rel_to_dev = src_file.relative_to(dev_root)
            rel_parts = rel_to_dev.parts

            # Check if this file is in an excluded directory
            excluded = False
            for exclude in _DEFAULT_EXCLUDES:
                exclude_parts = exclude.split("/")
                if rel_parts[: len(exclude_parts)] == tuple(exclude_parts):
                    # Check if it's in the allowlist
                    dev_path = f"dev/{'/'.join(rel_parts[:len(exclude_parts)])}/"
                    if dev_path not in allowlist_set:
                        excluded = True
                    break

            if excluded:
                continue

            # Compute destination path
            rel_path = src_file.relative_to(src_dir)
            dst_file = dst_dir / rel_path

            # Copy the file
            dst_file.parent.mkdir(parents=True, exist_ok=True)
            shutil.copy2(src_file, dst_file)
            staged_files.append(dst_file)

    logger.info(f"  ✓ Staged {len(staged_files)} file(s)")
    return staged_files


def _stage_submodule_copy(
    project_root: Path,
    expdir: Path,
    *,
    policy: SubmodulePolicy = SubmodulePolicy.REQUIRE,
    fixture_root: Optional[Path] = None,
) -> list[Path]:
    """Stage 4c: Copy submodule-owned files verbatim to EXPDIR.

    Copies files from external submodules (NEXUS, UPP) into the EXPDIR
    without any Jinja2 rendering. Uses shutil.copytree with cp -rp
    semantics (preserve permissions and directory structure).

    These files are owned by upstream submodules and must not be
    templated or modified by the deployment pipeline.

    Missing-source handling is governed by ``policy`` (Req 6.1, 6.2):

    - ``REQUIRE`` (default, production): a missing source raises a FATAL
      ``PipelineError``.
    - ``FIXTURE`` (verification): a missing source is resolved from
      ``fixture_root`` (the committed Submodule_Fixture) using the same
      relative source path. If neither the real source nor the fixture
      exists, a FATAL ``PipelineError`` naming the missing fixture path is
      raised.
    - ``SKIP_OPTIONAL``: a missing source listed in
      ``_OPTIONAL_SUBMODULE_SOURCES`` is skipped with a warning; a missing
      non-optional source is still FATAL.

    Args:
        project_root: Repository root containing the ``sorc/`` submodules.
        expdir: Destination EXPDIR root.
        policy: Resolution policy for missing submodule sources.
        fixture_root: Root of the committed Submodule_Fixture tree, used
            only when ``policy`` is ``FIXTURE``. The fixture mirrors the
            manifest source layout (e.g. ``<fixture_root>/sorc/upp.fd/parm/``).

    Returns:
        List of copied file paths in the EXPDIR.

    Raises:
        PipelineError: If a required source cannot be resolved under the
            active policy (FATAL ERROR).

    Traces to: Requirements 13.1, 13.2, 13.3, 13.4, 13.5, 6.1, 6.2
    """
    logger.info("Stage 4c: Copy submodule files (policy=%s)", policy.value)

    if policy is SubmodulePolicy.FIXTURE and fixture_root is None:
        raise PipelineError(
            "stage_submodule_copy",
            "FATAL ERROR: SubmodulePolicy.FIXTURE requires a fixture_root, "
            "but none was provided.",
        )

    copied_files: list[Path] = []

    for source_rel, dest_rel in SUBMODULE_COPY_MANIFEST:
        src_dir = project_root / source_rel
        dst_dir = expdir / dest_rel

        if not src_dir.exists():
            resolved = _resolve_missing_submodule_source(
                source_rel=source_rel,
                src_dir=src_dir,
                policy=policy,
                fixture_root=fixture_root,
            )
            if resolved is None:
                # SKIP_OPTIONAL skipped an optional entry; move on.
                continue
            src_dir = resolved

        if src_dir.is_file():
            # Single file copy
            dst_dir.parent.mkdir(parents=True, exist_ok=True)
            shutil.copy2(src_dir, dst_dir)
            copied_files.append(dst_dir)
        elif src_dir.is_dir():
            # Directory copy with cp -rp semantics (preserve permissions)
            # Use dirs_exist_ok=True to merge into existing directories
            dst_dir.mkdir(parents=True, exist_ok=True)
            shutil.copytree(
                src_dir,
                dst_dir,
                dirs_exist_ok=True,
                copy_function=shutil.copy2,
            )
            # Collect all copied files for manifest tracking
            for copied_file in sorted(dst_dir.rglob("*")):
                if copied_file.is_file():
                    copied_files.append(copied_file)

    logger.info(f"  ✓ Copied {len(copied_files)} submodule file(s)")
    return copied_files


def _resolve_missing_submodule_source(
    *,
    source_rel: str,
    src_dir: Path,
    policy: SubmodulePolicy,
    fixture_root: Optional[Path],
) -> Optional[Path]:
    """Resolve a missing submodule source according to ``policy``.

    Args:
        source_rel: The manifest source path relative to the project root.
        src_dir: The absolute source path that was found to be missing
            (used to preserve the original REQUIRE-policy FATAL message).
        policy: The active SubmodulePolicy.
        fixture_root: Root of the Submodule_Fixture tree (FIXTURE policy).

    Returns:
        A resolved source ``Path`` to copy from, or ``None`` when the entry
        should be skipped (SKIP_OPTIONAL on an optional entry).

    Raises:
        PipelineError: If the source cannot be resolved under the policy.
    """
    if policy is SubmodulePolicy.FIXTURE:
        # fixture_root is guaranteed non-None by the caller's precondition
        # check, but guard defensively for direct callers.
        if fixture_root is None:
            raise PipelineError(
                "stage_submodule_copy",
                "FATAL ERROR: SubmodulePolicy.FIXTURE requires a fixture_root, "
                "but none was provided.",
            )
        fixture_src = fixture_root / source_rel
        if fixture_src.exists():
            logger.info(
                "  ↪ Resolving '%s' from fixture '%s'", source_rel, fixture_src
            )
            return fixture_src
        raise PipelineError(
            "stage_submodule_copy",
            f"FATAL ERROR: Submodule source not found: "
            f"'{source_rel}' (no checkout and no fixture at '{fixture_src}').",
        )

    if policy is SubmodulePolicy.SKIP_OPTIONAL:
        if source_rel in _OPTIONAL_SUBMODULE_SOURCES:
            logger.warning(
                "  ⚠ Skipping optional submodule source '%s' (not present); "
                "the produced EXPDIR is non-production.",
                source_rel,
            )
            return None
        raise PipelineError(
            "stage_submodule_copy",
            f"Submodule source not found: '{src_dir}'. "
            f"This source is not optional and cannot be skipped.",
        )

    # SubmodulePolicy.REQUIRE (production default): unchanged FATAL behavior.
    raise PipelineError(
        "stage_submodule_copy",
        f"Submodule source not found: '{src_dir}'. "
        f"Ensure the submodule is checked out.",
    )


def _stage_platform_conditioned(
    dev_root: Path,
    expdir: Path,
    platform: str,
    context: dict[str, Any],
    config_path: Path,
) -> None:
    """Stage 4b: Platform-conditioned rendering.

    Renders and stages platform-specific files:
      - env/${PLATFORM}.env
      - parm/config/<app>/config.resources.${PLATFORM}
      - modulefiles/${PLATFORM}/

    Non-platform files (J-Jobs, ex-scripts, ush) are identical across
    platforms and are handled by the general file staging in stage 4.

    Traces to: Requirements 12.2, 12.3
    """
    logger.info("Stage 4b: Platform-conditioned rendering")

    # Determine the app name from context
    app = context.get("app", context.get("NET", "gfs"))

    # Determine the project root (parent of dev/)
    project_root = dev_root.parent

    # Create a renderer for any .j2 template files
    renderer = TemplateRenderer.create(
        context=context,
        dev_root=str(dev_root),
        app=app,
        strict=True,
    )

    # Render all platform-conditioned files
    result = render_all_platform_conditioned(
        project_root=project_root,
        expdir=expdir,
        platform=platform,
        app=app,
        renderer=renderer,
    )

    # Also check for additional app directories that may have
    # platform-specific resources (e.g., gefs, gcafs, sfs)
    parm_config_dir = dev_root / "parm" / "config"
    if parm_config_dir.is_dir():
        for app_dir in sorted(parm_config_dir.iterdir()):
            if not app_dir.is_dir():
                continue
            app_name = app_dir.name
            # Skip the primary app (already handled above)
            if app_name == app:
                continue
            # Check if this app has platform-specific resources
            platform_resource = app_dir / f"config.resources.{platform.upper()}"
            if platform_resource.exists():
                from .platform_conditioner import render_platform_resources
                render_platform_resources(
                    project_root=project_root,
                    expdir=expdir,
                    platform=platform,
                    app=app_name,
                    renderer=renderer,
                )

    logger.info("  ✓ Platform-conditioned rendering complete")


def _stage_condition_configs(expdir: Path, context: dict[str, Any]) -> None:
    """Stage 4c: Condition config files by resolving deploy-time conditionals.

    Instantiates the ConfigConditioner with deploy-time variable values
    extracted from the pipeline context, then processes all config files
    staged in the EXPDIR's parm/config/ directory.  Each config file has
    its deploy-time-known conditionals evaluated and dead branches removed.

    This stage runs ALWAYS regardless of the ``--dag-filter`` flag, per
    Requirement 13.3 — config conditioning applies to both filtered and
    full deployments.

    Args:
        expdir: Path to the staged EXPDIR.
        context: Pipeline context dict (built in Stage 2).

    Raises:
        PipelineError: If a conditioned config file fails bash -n
            syntax validation.

    Traces to: Requirements 5.1, 13.3
    """
    logger.info("Stage 4c: Condition config files (deploy-time resolution)")

    # Extract deploy-time variable values from the pipeline context
    deploy_time_values = get_deploy_time_values(context)

    if not deploy_time_values:
        logger.info("  ⚠ No deploy-time variables resolved from context; skipping conditioning")
        return

    # Instantiate the conditioner with resolved deploy-time values
    conditioner = ConfigConditioner(deploy_time_values)

    # Find all config files in the staged EXPDIR parm/config/ tree
    config_dir = expdir / "parm" / "config"
    if not config_dir.is_dir():
        logger.info("  ⚠ No parm/config/ directory in EXPDIR; skipping conditioning")
        return

    conditioned_count = 0
    total_eliminated = 0
    total_preserved = 0

    for config_file in sorted(config_dir.rglob("*")):
        if not config_file.is_file():
            continue

        # Only condition shell-like config files (skip .yaml, .json, etc.)
        # Config files in global-workflow are typically named config.* without
        # extension, or config.*.j2 (already rendered to config.* by Stage 3)
        if config_file.suffix in (".yaml", ".yml", ".json", ".xml"):
            continue

        content = config_file.read_text()

        # Skip empty files
        if not content.strip():
            continue

        # Condition the file
        result = conditioner.condition_file(content)

        # Check syntax validity (Requirement 5.8)
        if not result.is_valid_shell:
            raise PipelineError(
                "condition_configs",
                f"Conditioned config file failed bash -n validation: "
                f"{config_file.relative_to(expdir)}. "
                f"Syntax error: {conditioner.last_syntax_error}",
            )

        # Write back the conditioned output
        config_file.write_text(result.output)

        conditioned_count += 1
        total_eliminated += result.eliminated_branches
        total_preserved += result.preserved_conditionals

    logger.info(
        f"  ✓ Conditioned {conditioned_count} config file(s): "
        f"{total_eliminated} branch(es) eliminated, "
        f"{total_preserved} runtime conditional(s) preserved"
    )


def _stage_generate_dag(
    config_path: Path,
    expdir: Path,
    platform: str,
    dev_root: Path,
) -> Path:
    """Stage 5: Generate ecFlow DAG (.def and .ecf scripts).

    Parses the Workflow_Configuration, generates the Suite_Definition
    .def file, and renders per-task .ecf scripts.

    Returns:
        Path to the generated .def file.

    Traces to: Requirements 1.2, 2.1
    """
    logger.info("Stage 5/8: Generate DAG")

    # Parse the workflow configuration into a DAG
    dag = parse_workflow_config(str(config_path))

    # Validate the DAG is acyclic
    dag.validate_acyclic()
    logger.info(
        f"  ✓ DAG validated: {len(dag.nodes)} tasks, "
        f"{len(dag.edges)} edges, no cycles"
    )

    # Parse suite configuration for ecFlow emission
    with open(config_path, "r") as f:
        raw_config = yaml.safe_load(f) or {}
    suite_config = parse_suite_config(raw_config)

    # Generate the .def file
    def_dir = expdir / "ecf" / "defs"
    def_path = def_dir / f"{dag.suite_name}.def"
    generate_def(dag, str(def_path), suite_config)
    logger.info(f"  ✓ Generated {def_path.name}")

    # Generate per-task .ecf scripts
    ecf_scripts_dir = expdir / "ecf" / "scripts"
    template_path = dev_root / "workflow" / "ecflow" / "templates" / "task.ecf.j2"

    if template_path.exists():
        ecf_files = generate_ecf_scripts(
            dag,
            str(ecf_scripts_dir),
            str(template_path),
            platform,
        )
        logger.info(f"  ✓ Generated {len(ecf_files)} .ecf script(s)")
    else:
        logger.warning(
            f"  ⚠ Template not found: {template_path}. "
            f"Skipping .ecf script generation."
        )

    # Copy ecFlow include files if they exist
    include_src = dev_root / "workflow" / "ecflow" / "include"
    if include_src.is_dir():
        include_dst = expdir / "ecf" / "include"
        if not include_dst.exists():
            shutil.copytree(include_src, include_dst)
            logger.info("  ✓ Copied ecFlow include files")

    return def_path


def _stage_ee2_scan(expdir: Path) -> None:
    """Stage 6: Run EE2 compliance scan.

    Scans rendered J-Jobs, ex-scripts, and ush scripts for EE2 compliance.
    Raises SystemExit (via run_compliance_scan) on any violation.

    Traces to: Requirements 11.6, 8.6
    """
    logger.info("Stage 6/8: EE2 compliance scan")

    # Only run the scan if the relevant directories exist
    has_scannable = any(
        (expdir / d).is_dir() for d in ("jobs", "scripts", "ush")
    )

    if not has_scannable:
        logger.info("  ⚠ No jobs/, scripts/, or ush/ directories to scan")
        return

    try:
        run_compliance_scan(expdir)
        logger.info("  ✓ EE2 compliance scan passed")
    except SystemExit as e:
        raise PipelineError(
            "ee2_scan",
            str(e),
        ) from e


def _stage_manifest(
    expdir: Path,
    version: str,
    context: dict[str, Any],
) -> Path:
    """Stage 7: Generate manifest.yaml.

    Computes SHA-256 of every file under EXPDIR and writes manifest.yaml
    with Snapshot_ID, git metadata, and per-file hashes.

    Returns:
        Path to the generated manifest.yaml.

    Traces to: Requirements 3.3, 3.6
    """
    logger.info("Stage 7/8: Generate manifest")

    # Compute SHA-256 for every file in the EXPDIR
    file_hashes: dict[str, dict[str, Any]] = {}
    for filepath in sorted(expdir.rglob("*")):
        if not filepath.is_file():
            continue
        # Skip the manifest itself if it somehow exists
        if filepath.name == "manifest.yaml":
            continue

        rel_path = str(filepath.relative_to(expdir))
        sha256 = _compute_sha256(filepath)
        file_size = filepath.stat().st_size
        file_hashes[rel_path] = {
            "sha256": sha256,
            "size": file_size,
        }

    # Build the manifest document
    git_commit = context.get("git_commit", "")
    git_remote = context.get("git_remote", "")
    git_branch = context.get("git_branch", "")

    # Source the resolved installed versions of the pinned packages so the
    # Manifest records the actual environment that produced the EXPDIR
    # (Req 5.5). Falls back to an empty string if a package is not
    # importable (only reachable when version enforcement is disabled).
    wxflow_version = _get_installed_version("wxflow") or ""
    uwtools_version = _get_installed_version("uwtools") or ""

    manifest: dict[str, Any] = {
        "snapshot_id": "",  # Placeholder, computed below
        "git_commit": git_commit,
        "git_remote": git_remote,
        "git_branch": git_branch,
        "deployed_by": _get_current_user(),
        "deployed_on": _get_hostname(),
        "deployed_at": datetime.now(timezone.utc).isoformat(),
        "platform": context.get("MACHINE", ""),
        "wxflow_version": wxflow_version,
        "uwtools_version": uwtools_version,
        "files": file_hashes,
    }

    # Compute Snapshot_ID: <semver>+<sha256_prefix_12> of manifest content
    # First serialize without snapshot_id to compute the hash
    manifest_content = yaml.dump(manifest, sort_keys=False)
    manifest_hash = hashlib.sha256(manifest_content.encode()).hexdigest()[:12]
    snapshot_id = f"{version}+{manifest_hash}"
    manifest["snapshot_id"] = snapshot_id

    # Write manifest.yaml
    manifest_path = expdir / "manifest.yaml"
    manifest_path.write_text(
        yaml.dump(manifest, default_flow_style=False, sort_keys=False),
        encoding="utf-8",
    )

    logger.info(f"  ✓ Manifest generated: Snapshot_ID = {snapshot_id}")
    logger.info(f"  ✓ {len(file_hashes)} file(s) recorded")
    return manifest_path


def _stage_seal(expdir: Path, context: dict[str, Any]) -> None:
    """Stage 8: Seal the EXPDIR.

    Sets all regular files to mode 0444, all directories to mode 0555.
    Writes workflow/provenance.yaml with deployment metadata.

    Traces to: Requirements 3.4, 13.4
    """
    logger.info("Stage 8/8: Seal EXPDIR")

    # Write provenance.yaml before sealing (so it gets sealed too)
    provenance_dir = expdir / "workflow"
    provenance_dir.mkdir(parents=True, exist_ok=True)
    provenance_path = provenance_dir / "provenance.yaml"

    provenance: dict[str, Any] = {
        "git_remote": context.get("git_remote", ""),
        "git_commit": context.get("git_commit", ""),
        "git_branch": context.get("git_branch", ""),
        "deployed_by": _get_current_user(),
        "deployed_on": _get_hostname(),
        "deployed_at": datetime.now(timezone.utc).isoformat(),
        "platform": context.get("MACHINE", ""),
        "version": context.get("model_ver", ""),
        "config": {
            k: v
            for k, v in context.items()
            if k not in ("git_commit", "git_branch", "git_remote")
        },
    }

    provenance_path.write_text(
        yaml.dump(provenance, default_flow_style=False, sort_keys=False),
        encoding="utf-8",
    )
    logger.info("  ✓ Wrote workflow/provenance.yaml")

    # Create empty state.db placeholder
    state_db_path = provenance_dir / "state.db"
    if not state_db_path.exists():
        state_db_path.touch()
        logger.info("  ✓ Created workflow/state.db")

    # Seal: set file permissions to read-only
    for filepath in expdir.rglob("*"):
        if filepath.is_file():
            os.chmod(filepath, 0o444)
        elif filepath.is_dir():
            os.chmod(filepath, 0o555)

    # Seal the EXPDIR root directory itself
    os.chmod(expdir, 0o555)

    logger.info("  ✓ EXPDIR sealed (files=0444, dirs=0555)")


# ---------------------------------------------------------------------------
# DAG filter reporting
# ---------------------------------------------------------------------------


def _log_size_reduction(dev_root: Path, reachability: Any) -> SizeReductionReport:
    """Log size reduction statistics after DAG-filtered staging.

    Builds a SizeReductionReport from the DAGReachabilitySet's staged
    counts (frozenset lengths) and total_available_* fields, then logs
    the results.

    Args:
        dev_root: Path to the dev/ directory (unused currently; the
            totals come from the reachability set which already counted
            them during compute_reachability).
        reachability: A DAGReachabilitySet instance containing both the
            staged artifact sets and total_available_* statistics.

    Returns:
        The SizeReductionReport that was logged.

    Traces to: Requirements 9.1, 9.2, 9.3, 9.4
    """
    report = SizeReductionReport(
        staged_jjobs=len(reachability.jjobs),
        total_jjobs=reachability.total_available_jjobs,
        staged_ex_scripts=len(reachability.ex_scripts),
        total_ex_scripts=reachability.total_available_ex_scripts,
        staged_ush_scripts=len(reachability.ush_scripts),
        total_ush_scripts=reachability.total_available_ush_scripts,
        staged_configs=len(reachability.config_files),
        total_configs=reachability.total_available_configs,
    )
    report.log()
    return report


# ---------------------------------------------------------------------------
# Main pipeline entry point
# ---------------------------------------------------------------------------


def run(
    config: str,
    platform: str,
    expdir: str,
    version: str,
    allowlist: Optional[list[str]] = None,
    dry_run: bool = False,
    enforce_versions: bool = False,
    submodule_policy: SubmodulePolicy = SubmodulePolicy.REQUIRE,
    fixture_root: Optional[str] = None,
    skip_ee2_scan: bool = False,
    dag_filter: bool = False,
) -> dict[str, Any]:
    """Run the 8-stage deployment pipeline.

    Orchestrates the full deployment from dev/ sources to a sealed,
    immutable EXPDIR.

    Args:
        config: Path to the Workflow_Configuration YAML file
            (e.g. dev/parm/workflow/gfs_cycled.yaml).
        platform: Target HPC platform name (e.g. 'HERA', 'WCOSS2').
        expdir: Destination EXPDIR path.
        version: Semantic version string for the Snapshot_ID
            (e.g. 'v17.0.0').
        allowlist: Optional list of dev/ paths to include that are
            normally excluded (e.g. ['dev/ctests/']).
        dry_run: If True, validate without writing any files.
        enforce_versions: If True, treat a not-importable wxflow/uwtools
            package as a FATAL ERROR during Stage 1 validation (hard
            precondition). A version *mismatch* is always FATAL regardless
            of this flag. Defaults to False so the broad test suite can
            exercise run() in environments missing a pinned package; the
            Goal_Realization_Gate runs with this set to True.
        submodule_policy: Resolution policy for missing Submodule_Source
            files (Req 6.1, 6.2). Defaults to ``SubmodulePolicy.REQUIRE``
            (production: missing sources are FATAL). The verification
            harness/Goal_Realization_Gate passes ``SubmodulePolicy.FIXTURE``
            together with ``fixture_root`` to resolve missing sources from
            the committed Submodule_Fixture.
        fixture_root: Root of the committed Submodule_Fixture tree, used
            only when ``submodule_policy`` is ``SubmodulePolicy.FIXTURE``.
        skip_ee2_scan: If True, skip Stage 6 (EE2 compliance scan). Used
            by the Goal_Realization_Gate which performs its own offline EE2
            check (step 5) against the reconciled scanner + baseline.
        dag_filter: If True, enable DAG-filtered staging so only
            artifacts transitively reachable from the Workflow_YAML task
            DAG are staged into the EXPDIR. When False (default), all
            files from dev/ are staged using full-copy behavior.

    Returns:
        A summary dict with keys:
        - snapshot_id: The assigned Snapshot_ID string.
        - expdir: The EXPDIR path.
        - files_rendered: Count of rendered template files.
        - files_staged: Count of staged files.
        - tasks: Count of DAG task nodes.
        - duration_seconds: Total pipeline duration.

    Raises:
        PipelineError: If any stage fails with a FATAL ERROR.
    """
    start_time = time.time()

    config_path = Path(config).resolve()
    expdir_path = Path(expdir).resolve()
    fixture_root_path = Path(fixture_root).resolve() if fixture_root else None

    # Determine the dev/ root (parent of the config file's workflow dir,
    # or inferred from the repository structure)
    dev_root = _find_dev_root(config_path)

    logger.info("=" * 60)
    logger.info("Deployment Pipeline")
    logger.info("=" * 60)
    logger.info(f"  Config:   {config_path}")
    logger.info(f"  Platform: {platform}")
    logger.info(f"  EXPDIR:   {expdir_path}")
    logger.info(f"  Version:  {version}")
    logger.info(f"  Dry-run:  {dry_run}")
    logger.info("=" * 60)

    # --- Stage 1: Validate ---
    _stage_validate(
        config_path,
        platform,
        expdir_path,
        version,
        dev_root,
        enforce_versions=enforce_versions,
    )

    if dry_run:
        logger.info("Dry-run mode: validation passed. No files written.")

        # --- Dry-run name resolution report (Req 7.1, 7.2, 7.3) ---
        # When dag_filter is enabled, load the workflow config, extract jjob
        # names, and run dry-run resolution to show the user how application
        # names map to sources. This provides early feedback on naming errors
        # before a full deployment.
        if dag_filter:
            try:
                with open(config_path, "r") as f:
                    dry_run_config = yaml.safe_load(f) or {}
            except (yaml.YAMLError, OSError) as e:
                raise PipelineError(
                    "dry_run",
                    f"Failed to load workflow config for dry-run: {e}",
                ) from e

            # Extract application names from workflow YAML families/tasks/jjob
            dry_run_jjobs: set[str] = set()
            for family in dry_run_config.get("families", []):
                if not isinstance(family, dict):
                    continue
                for task in family.get("tasks", []):
                    if not isinstance(task, dict):
                        continue
                    jjob = task.get("jjob")
                    if jjob:
                        dry_run_jjobs.add(str(jjob))

            if dry_run_jjobs:
                # Load name resolver and run dry-run resolution
                name_resolver = _load_name_resolver(dev_root)
                report = name_resolver.resolve_all_dry_run(dry_run_jjobs)

                # Print the resolution table
                logger.info("Dry-run name resolution:")
                print(report.format_table())

                # Exit with non-zero status if any names are unresolvable
                if report.unresolvable_count > 0:
                    raise PipelineError(
                        "dry_run",
                        f"Name resolution failed: {report.unresolvable_count} "
                        f"unresolvable name(s) out of {report.total_count} total. "
                        f"Errors: {'; '.join(report.errors)}",
                    )
            else:
                logger.info("  No jjob references found in workflow YAML.")

        duration = time.time() - start_time
        return {
            "snapshot_id": None,
            "expdir": str(expdir_path),
            "files_rendered": 0,
            "files_staged": 0,
            "tasks": 0,
            "duration_seconds": round(duration, 2),
            "dry_run": True,
        }

    # Create the EXPDIR
    expdir_path.mkdir(parents=True, exist_ok=True)

    # --- Stage 2: Build Context ---
    context = _stage_build_context(
        config_path, platform, version, expdir_path, dev_root
    )

    # --- Stage 2 (cont.): Load PrefixRegistry and NameResolver (Req 2.7, 5.3, 5.4) ---
    name_resolver = _load_name_resolver(dev_root)

    # --- Stage 4a (early): DAG Filter (Req 13.1, 13.2, 13.3, 13.4) ---
    # Computed before Stage 3 so the reachability set can inform DAG-aware
    # model input rendering. The DAG filter only needs the workflow YAML
    # and dev/ source tree, both available after Stage 2.
    reachability: Optional[DAGReachabilitySet] = None
    if dag_filter:
        # Deferred import to avoid circular dependency
        # (pipeline -> dag_filter -> pipeline)
        from .dag_filter import DAGFilter

        logger.info("DAG filtering: ENABLED — staging only reachable artifacts")
        dag_filter_obj = DAGFilter(dev_root, context, platform, name_resolver=name_resolver)
        reachability = dag_filter_obj.compute_reachability()
        logger.info(
            f"  ✓ Reachability computed: {len(reachability.jjobs)} J-Jobs, "
            f"{len(reachability.ex_scripts)} ex-scripts, "
            f"{len(reachability.ush_scripts)} ush scripts, "
            f"{len(reachability.config_files)} config files"
        )
        for warning in reachability.warnings:
            logger.warning(f"  ⚠ {warning}")
    else:
        logger.info("DAG filtering: DISABLED — staging all artifacts (full mode)")

    # --- Stage 3: Render Templates (Req 6.1, 6.7, 13.3) ---
    # Model input pre-rendering applies regardless of --dag-filter flag.
    # When reachability is available, render_for_dag() is used for model
    # inputs; otherwise render_all() is used.
    rendered_files, model_rendered_files = _stage_render_templates(
        dev_root, expdir_path, context, platform,
        reachability_set=reachability,
    )

    # --- Stage 4: Stage Files ---
    staged_files = _stage_stage_files(
        dev_root, expdir_path, allowlist, context, reachability=reachability
    )

    # --- Stage 4 (cont.): J-Job rename-on-copy and unconditional artifacts ---
    # When DAG filtering is enabled, use the resolution_map from the
    # DAGReachabilitySet to stage J-Jobs with application-specific renaming.
    # Unconditional artifacts (linking scripts) are staged regardless of
    # --dag-filter flag (Req 9.5).
    from .file_stager import FileStager

    project_root = dev_root.parent
    file_stager = FileStager(
        project_root=project_root, expdir=expdir_path, use_uwtools=False
    )

    if dag_filter and reachability is not None:
        # Build resolution_map from DAGReachabilitySet.jjob_source_map
        # using the NameResolver to get full ResolvedName objects
        resolution_map = name_resolver.resolve_all(reachability.jjobs)
        jjob_staging_result = file_stager.stage_jjobs_with_rename(resolution_map)
        logger.info(
            f"  ✓ Staged {jjob_staging_result.files_copied} J-Job(s) with "
            f"application naming"
        )

    # Stage unconditional artifacts regardless of --dag-filter (Req 9.5)
    unconditional_result = file_stager.stage_unconditional_artifacts()
    logger.info(
        f"  ✓ Staged {unconditional_result.files_copied} unconditional artifact(s)"
    )

    # --- Stage 4c: Submodule Copy (Req 13.3, 13.4, 13.5, 6.1, 6.2) ---
    project_root = dev_root.parent
    submodule_files = _stage_submodule_copy(
        project_root,
        expdir_path,
        policy=submodule_policy,
        fixture_root=fixture_root_path,
    )
    staged_files.extend(submodule_files)

    # --- Stage 4b: Platform-Conditioned Rendering (Req 12.2, 12.3) ---
    _stage_platform_conditioned(
        dev_root, expdir_path, platform, context, config_path
    )

    # --- Stage 4c: Config Conditioning (Req 5.1, 13.3) ---
    # Runs ALWAYS regardless of --dag-filter flag
    _stage_condition_configs(expdir_path, context)

    # --- Stage 4d: Completeness Verification (Req 8.3, 8.4) ---
    if dag_filter:
        logger.info("Stage 4d: Completeness verification")
        # Deferred import to avoid circular dependency
        # (pipeline -> completeness_verifier -> dag_filter -> pipeline)
        from .completeness_verifier import CompletenessVerifier

        verifier = CompletenessVerifier(expdir_path)
        verifier.verify()  # Raises PipelineError on failure
        logger.info("  ✓ Completeness verification passed")

        # Log size reduction statistics (Req 9.1, 9.2, 9.3, 9.4)
        _log_size_reduction(dev_root, reachability)

    # --- Stage 5: Generate DAG ---
    def_path = _stage_generate_dag(config_path, expdir_path, platform, dev_root)

    # --- Stage 6: EE2 Compliance Scan ---
    if not skip_ee2_scan:
        _stage_ee2_scan(expdir_path)
    else:
        logger.info("Stage 6/8: EE2 compliance scan (skipped — gate handles EE2)")

    # --- Stage 7: Generate Manifest ---
    manifest_path = _stage_manifest(expdir_path, version, context)

    # Read back the snapshot_id
    manifest_data = yaml.safe_load(manifest_path.read_text())
    snapshot_id = manifest_data.get("snapshot_id", "")

    # --- Stage 8: Seal EXPDIR ---
    _stage_seal(expdir_path, context)

    # --- Summary ---
    duration = time.time() - start_time
    logger.info("=" * 60)
    logger.info("Deployment Complete")
    logger.info(f"  Snapshot_ID: {snapshot_id}")
    logger.info(f"  Duration:    {duration:.2f}s")
    logger.info("=" * 60)

    return {
        "snapshot_id": snapshot_id,
        "expdir": str(expdir_path),
        "files_rendered": len(rendered_files),
        "files_staged": len(staged_files),
        "tasks": _count_dag_tasks(config_path),
        "duration_seconds": round(duration, 2),
        "dry_run": False,
    }


# ---------------------------------------------------------------------------
# Helper functions
# ---------------------------------------------------------------------------


def _find_dev_root(config_path: Path) -> Path:
    """Find the dev/ root directory from a config file path.

    Walks up from the config file looking for a directory named 'dev'
    that contains expected subdirectories (jobs/, scripts/, ush/).

    Falls back to looking for the repository root (contains .git/).

    Args:
        config_path: Absolute path to the configuration YAML file.

    Returns:
        Path to the dev/ directory.

    Raises:
        PipelineError: If the dev/ root cannot be determined.
    """
    # Walk up from config_path looking for dev/ directory
    current = config_path.parent
    for _ in range(10):  # Limit search depth
        # Check if current is the dev/ directory
        if current.name == "dev" and (current / "jobs").is_dir():
            return current

        # Check if current contains a dev/ subdirectory
        dev_candidate = current / "dev"
        if dev_candidate.is_dir() and (dev_candidate / "jobs").is_dir():
            return dev_candidate

        # Check for .git (repository root)
        if (current / ".git").exists():
            dev_candidate = current / "dev"
            if dev_candidate.is_dir():
                return dev_candidate

        current = current.parent

    raise PipelineError(
        "validate",
        f"Cannot determine dev/ root from config path: {config_path}. "
        f"Ensure the config file is within the repository tree.",
    )


def _get_git_commit(dev_root: Path) -> str:
    """Get the current git commit hash."""
    try:
        result = subprocess.run(
            ["git", "rev-parse", "HEAD"],
            capture_output=True,
            text=True,
            cwd=str(dev_root),
            timeout=10,
        )
        if result.returncode == 0:
            return result.stdout.strip()
    except (subprocess.TimeoutExpired, FileNotFoundError, OSError):
        pass
    return ""


def _get_git_branch(dev_root: Path) -> str:
    """Get the current git branch name."""
    try:
        result = subprocess.run(
            ["git", "rev-parse", "--abbrev-ref", "HEAD"],
            capture_output=True,
            text=True,
            cwd=str(dev_root),
            timeout=10,
        )
        if result.returncode == 0:
            return result.stdout.strip()
    except (subprocess.TimeoutExpired, FileNotFoundError, OSError):
        pass
    return ""


def _get_git_remote(dev_root: Path) -> str:
    """Get the git remote URL (origin)."""
    try:
        result = subprocess.run(
            ["git", "remote", "get-url", "origin"],
            capture_output=True,
            text=True,
            cwd=str(dev_root),
            timeout=10,
        )
        if result.returncode == 0:
            return result.stdout.strip()
    except (subprocess.TimeoutExpired, FileNotFoundError, OSError):
        pass
    return ""


def _get_current_user() -> str:
    """Get the current username."""
    return os.environ.get("USER", os.environ.get("USERNAME", "unknown"))


def _get_hostname() -> str:
    """Get the current hostname."""
    import socket
    try:
        return socket.getfqdn()
    except Exception:
        return os.environ.get("HOSTNAME", "unknown")


def _compute_sha256(filepath: Path) -> str:
    """Compute SHA-256 hash of a file."""
    sha256 = hashlib.sha256()
    with open(filepath, "rb") as f:
        for chunk in iter(lambda: f.read(8192), b""):
            sha256.update(chunk)
    return sha256.hexdigest()


def _count_dag_tasks(config_path: Path) -> int:
    """Count the number of tasks in the DAG from a config file."""
    try:
        dag = parse_workflow_config(str(config_path))
        return len(dag.nodes)
    except Exception:
        return 0
