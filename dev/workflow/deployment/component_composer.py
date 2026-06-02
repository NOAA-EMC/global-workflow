"""Component composition logic for UFS workflow configuration.

Loads component YAML files from dev/parm/components/, filters to active
components, merges model sections and families, resolves cross-component
trigger references, and removes dangling references when components are
excluded.

Traces to: Requirements 10.3, 10.4, 10.7, 10.8, 10.9
"""

from __future__ import annotations

import copy
import logging
import re
from pathlib import Path
from typing import Any

import yaml

logger = logging.getLogger(__name__)


# ---------------------------------------------------------------------------
# Component name to YAML filename mapping
# ---------------------------------------------------------------------------

# Maps the component name used in the `components:` list to the YAML filename
# and the model subsection key within that YAML.
COMPONENT_REGISTRY: dict[str, dict[str, str]] = {
    "atmosphere": {"file": "atmos.yaml", "model_key": "fv3"},
    "ocean": {"file": "ocean.yaml", "model_key": "ocean"},
    "ice": {"file": "ice.yaml", "model_key": "ice"},
    "wave": {"file": "wave.yaml", "model_key": "wave"},
    "aerosol": {"file": "gocart.yaml", "model_key": "aerosol"},
}

# Maps component names to the family path prefixes they own.
# Used for dangling reference detection when a component is excluded.
COMPONENT_FAMILY_PREFIXES: dict[str, list[str]] = {
    "atmosphere": ["gdas/atmos", "gfs/atmos"],
    "ocean": ["gdas/ocean", "gfs/ocean"],
    "ice": ["gdas/ice", "gfs/ice"],
    "wave": ["gdas/wave", "gfs/wave"],
    "aerosol": ["gdas/aerosol", "gfs/aerosol", "gdas/aero", "gfs/aero"],
}


# ---------------------------------------------------------------------------
# Exceptions
# ---------------------------------------------------------------------------


class ComponentCompositionError(Exception):
    """Raised when component composition fails.

    Attributes:
        component: The component name that caused the error.
        reason: Human-readable description of the problem.
    """

    def __init__(self, component: str, reason: str) -> None:
        self.component = component
        self.reason = reason
        super().__init__(f"Component '{component}': {reason}")


# ---------------------------------------------------------------------------
# Component loading
# ---------------------------------------------------------------------------


def load_component_yaml(
    component_name: str,
    components_dir: Path,
) -> dict[str, Any]:
    """Load a single component YAML file.

    Args:
        component_name: The component name (e.g., 'atmosphere', 'ocean').
        components_dir: Path to the directory containing component YAMLs.

    Returns:
        Parsed YAML dict for the component.

    Raises:
        ComponentCompositionError: If the component is unknown or the
            YAML file cannot be loaded.
    """
    if component_name not in COMPONENT_REGISTRY:
        raise ComponentCompositionError(
            component_name,
            f"Unknown component. Supported: {sorted(COMPONENT_REGISTRY.keys())}",
        )

    filename = COMPONENT_REGISTRY[component_name]["file"]
    filepath = components_dir / filename

    if not filepath.exists():
        raise ComponentCompositionError(
            component_name,
            f"Component YAML not found: {filepath}",
        )

    try:
        with open(filepath, "r") as f:
            data = yaml.safe_load(f)
    except yaml.YAMLError as e:
        raise ComponentCompositionError(
            component_name,
            f"Failed to parse YAML: {e}",
        ) from e

    if data is None:
        raise ComponentCompositionError(
            component_name,
            f"Component YAML is empty: {filepath}",
        )

    return data


def load_active_components(
    components_list: list[str],
    components_dir: Path,
) -> dict[str, dict[str, Any]]:
    """Load YAML data for all active components.

    Args:
        components_list: List of active component names from the
            top-level `components:` key.
        components_dir: Path to the directory containing component YAMLs.

    Returns:
        Dict mapping component name to its parsed YAML data.

    Raises:
        ComponentCompositionError: If any component cannot be loaded.
    """
    loaded: dict[str, dict[str, Any]] = {}
    for name in components_list:
        loaded[name] = load_component_yaml(name, components_dir)
    return loaded


# ---------------------------------------------------------------------------
# Model section merge
# ---------------------------------------------------------------------------


def merge_model_sections(
    base_model: dict[str, Any],
    active_components: dict[str, dict[str, Any]],
) -> dict[str, Any]:
    """Merge each active component's model subsection into the base model dict.

    For each active component, extracts its `model.<component_key>` section
    and merges it into the base model dict. The base model's existing values
    for a component key take precedence (explicit overrides component defaults).

    Also derives `model.active_components` from the active component names.

    Args:
        base_model: The top-level `model` section from the Workflow_Configuration.
        active_components: Dict mapping component name to its parsed YAML data.

    Returns:
        Updated model dict with all component model sections merged in.

    Traces to: Requirements 10.3, 10.5, 10.8
    """
    merged = copy.deepcopy(base_model)

    for component_name, component_data in active_components.items():
        model_key = COMPONENT_REGISTRY[component_name]["model_key"]
        component_model = component_data.get("model", {}).get(model_key, {})

        if not component_model:
            continue

        # Merge: component values are defaults, explicit top-level values override
        if model_key not in merged:
            merged[model_key] = {}

        existing = merged[model_key]
        for key, value in component_model.items():
            if key not in existing:
                existing[key] = value

    # Derive active_components list (Requirement 10.8)
    merged["active_components"] = list(active_components.keys())

    return merged


# ---------------------------------------------------------------------------
# Family merge
# ---------------------------------------------------------------------------


def merge_families(
    base_families: list[dict[str, Any]],
    active_components: dict[str, dict[str, Any]],
) -> list[dict[str, Any]]:
    """Merge each active component's families into the top-level families list.

    Appends each component's `families` entries to the base families list.
    Deduplicates by family path — if a family path already exists in the
    base list, the component's version is skipped.

    Args:
        base_families: The top-level `families` list from the
            Workflow_Configuration (may be empty).
        active_components: Dict mapping component name to its parsed YAML data.

    Returns:
        Combined families list with all active component families appended.

    Traces to: Requirements 10.3, 10.6
    """
    merged = list(base_families)
    existing_paths: set[str] = {f["path"] for f in merged if "path" in f}

    for component_name, component_data in active_components.items():
        component_families = component_data.get("families", [])
        if not isinstance(component_families, list):
            logger.warning(
                f"Component '{component_name}': 'families' is not a list, skipping"
            )
            continue

        for family in component_families:
            if not isinstance(family, dict):
                continue
            family_path = family.get("path", "")
            if family_path and family_path not in existing_paths:
                merged.append(copy.deepcopy(family))
                existing_paths.add(family_path)
            elif family_path in existing_paths:
                logger.debug(
                    f"Component '{component_name}': family '{family_path}' "
                    f"already exists in base, skipping"
                )

    return merged


# ---------------------------------------------------------------------------
# Cross-component trigger resolution
# ---------------------------------------------------------------------------

# Pattern matching trigger expressions referencing task paths
# e.g., "gfs/atmos/forecast/fcst == complete" or
#        "gfs/atmos/forecast/fcst:forecast_hour ge 6"
_TRIGGER_PATH_RE = re.compile(
    r"([\w/]+?)(?:\s*==\s*(?:complete|active|aborted|queued|submitted|unknown)"
    r"|:[\w]+\s+(?:ge|gt|le|lt|eq|ne)\s+\d+)"
)


def _extract_trigger_paths(expression: str) -> list[str]:
    """Extract task paths referenced in a trigger expression.

    Args:
        expression: A trigger expression string.

    Returns:
        List of task path strings found in the expression.
    """
    if not expression:
        return []
    return [m.group(1) for m in _TRIGGER_PATH_RE.finditer(expression)]


def _path_belongs_to_component(
    path: str,
    component_name: str,
) -> bool:
    """Check if a task path belongs to a given component.

    Args:
        path: A fully qualified task path (e.g., 'gfs/ocean/post').
        component_name: The component name to check against.

    Returns:
        True if the path belongs to the component.
    """
    prefixes = COMPONENT_FAMILY_PREFIXES.get(component_name, [])
    return any(path.startswith(prefix) for prefix in prefixes)


def _path_belongs_to_excluded_component(
    path: str,
    excluded_components: set[str],
) -> bool:
    """Check if a task path belongs to any excluded component.

    Args:
        path: A fully qualified task path.
        excluded_components: Set of component names that are excluded.

    Returns:
        True if the path belongs to an excluded component.
    """
    for component_name in excluded_components:
        if _path_belongs_to_component(path, component_name):
            return True
    return False


def resolve_triggers(
    families: list[dict[str, Any]],
    excluded_components: set[str],
) -> tuple[list[dict[str, Any]], list[str]]:
    """Resolve cross-component triggers and remove dangling references.

    Iterates through all tasks in the families list. For each task's trigger
    expression, checks if any referenced paths belong to excluded components.
    If so, removes the dangling reference from the trigger expression and
    emits a warning.

    Args:
        families: The merged families list (will be modified in place via
            deep copy).
        excluded_components: Set of component names that are NOT active.

    Returns:
        Tuple of (resolved_families, warnings) where:
        - resolved_families: The families list with dangling triggers removed.
        - warnings: List of warning messages for removed references.

    Traces to: Requirements 10.7, 10.9
    """
    if not excluded_components:
        return families, []

    resolved = copy.deepcopy(families)
    warnings: list[str] = []

    for family in resolved:
        tasks = family.get("tasks", [])
        for task in tasks:
            trigger = task.get("trigger", "")
            if not trigger:
                continue

            # Extract paths from the trigger expression
            referenced_paths = _extract_trigger_paths(trigger)

            # Check each referenced path for dangling references
            dangling_paths: list[str] = []
            for path in referenced_paths:
                if _path_belongs_to_excluded_component(path, excluded_components):
                    dangling_paths.append(path)

            if dangling_paths:
                # Remove dangling references from the trigger expression
                new_trigger = _remove_dangling_refs(trigger, dangling_paths)
                task_path = f"{family.get('path', '?')}/{task.get('name', '?')}"

                for dpath in dangling_paths:
                    warnings.append(
                        f"Removed dangling trigger reference '{dpath}' "
                        f"from task '{task_path}' "
                        f"(component excluded)"
                    )

                task["trigger"] = new_trigger

    return resolved, warnings


def _remove_dangling_refs(
    expression: str,
    dangling_paths: list[str],
) -> str:
    """Remove dangling path references from a trigger expression.

    Handles expressions like:
    - "path/to/task == complete" -> removed entirely
    - "path/to/task:meter ge N" -> removed entirely
    - "expr1 and expr2" -> remaining expression
    - "expr1 or expr2" -> remaining expression

    Args:
        expression: The original trigger expression.
        dangling_paths: List of paths to remove.

    Returns:
        The cleaned trigger expression, or empty string if all refs removed.
    """
    result = expression

    for path in dangling_paths:
        # Remove "path == status" patterns
        result = re.sub(
            r"\s*" + re.escape(path) + r"\s*==\s*\w+\s*",
            " ",
            result,
        )
        # Remove "path:meter op value" patterns
        result = re.sub(
            r"\s*" + re.escape(path) + r":[\w]+\s+(?:ge|gt|le|lt|eq|ne)\s+\d+\s*",
            " ",
            result,
        )

    # Clean up boolean operators left dangling
    # Remove leading/trailing "and"/"or"
    result = re.sub(r"^\s*(?:and|or)\s+", "", result.strip())
    result = re.sub(r"\s+(?:and|or)\s*$", "", result.strip())
    # Remove doubled "and"/"or" (e.g., "expr1 and and expr2")
    result = re.sub(r"\s+(?:and|or)\s+(?:and|or)\s+", " and ", result)

    return result.strip()


# ---------------------------------------------------------------------------
# Main composition function
# ---------------------------------------------------------------------------


def compose_components(
    workflow_config: dict[str, Any],
    components_dir: Path,
) -> dict[str, Any]:
    """Compose component YAMLs into a unified workflow configuration.

    This is the main entry point for component composition. It:
    1. Reads the `components:` list from the workflow config
    2. Loads each active component's YAML
    3. Merges model sections from each component
    4. Merges families from each component
    5. Resolves cross-component triggers
    6. Removes dangling references for excluded components

    Args:
        workflow_config: The top-level Workflow_Configuration dict.
            Must contain a `components:` key listing active components.
        components_dir: Path to the directory containing component YAMLs
            (e.g., dev/parm/components/).

    Returns:
        Updated workflow_config dict with composed components.
        The `model` section will have component subsections merged in,
        and `families` will include all active component families.

    Traces to: Requirements 10.3, 10.4, 10.7, 10.8, 10.9
    """
    result = copy.deepcopy(workflow_config)

    # Step 1: Get active components list
    components_list = result.get("components", [])
    if not components_list:
        logger.warning("No 'components' key found in workflow configuration")
        return result

    # Validate component names
    for name in components_list:
        if name not in COMPONENT_REGISTRY:
            raise ComponentCompositionError(
                name,
                f"Unknown component. Supported: {sorted(COMPONENT_REGISTRY.keys())}",
            )

    # Step 2: Load active component YAMLs
    active_components = load_active_components(components_list, components_dir)

    # Step 3: Determine excluded components
    all_components = set(COMPONENT_REGISTRY.keys())
    excluded_components = all_components - set(components_list)

    # Step 4: Merge model sections
    model_section = result.get("model", {})
    result["model"] = merge_model_sections(model_section, active_components)

    # Step 5: Merge families
    base_families = result.get("families", [])
    if not isinstance(base_families, list):
        base_families = []
    merged_families = merge_families(base_families, active_components)

    # Step 6: Resolve triggers and remove dangling references
    resolved_families, warnings = resolve_triggers(
        merged_families, excluded_components
    )

    # Emit warnings for removed references
    for warning in warnings:
        logger.warning(warning)

    result["families"] = resolved_families

    return result
