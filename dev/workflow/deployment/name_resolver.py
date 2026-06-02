"""Name resolution for application-specific J-Job naming.

Implements the PrefixRegistry and NameResolver that translate
Application_Names (e.g., JGCAFS_FORECAST) in the Workflow_YAML back to
Shared_Source_Names (e.g., JGLOBAL_FORECAST) in dev/jobs/.

Traces to: Requirements 2, 5, 7, 8
"""

from __future__ import annotations

import logging
from dataclasses import dataclass, field
from pathlib import Path
from typing import Any

import yaml

logger = logging.getLogger(__name__)


# ---------------------------------------------------------------------------
# Default registry values (Requirement 5.2)
# ---------------------------------------------------------------------------

_DEFAULT_REGISTRY: dict[str, list[str]] = {
    "JGCAFS_": ["JGLOBAL_"],
    "JGCDAS_": ["JGLOBAL_", "JGDAS_"],
    "JGFS_": ["JGLOBAL_", "JGFS_"],
    "JGDAS_": ["JGLOBAL_", "JGDAS_"],
    "JGEFS_": ["JGLOBAL_", "JGEFS_"],
    "JSFS_": ["JGLOBAL_", "JSFS_"],
}


# ---------------------------------------------------------------------------
# PrefixRegistry
# ---------------------------------------------------------------------------


@dataclass(frozen=True)
class PrefixRegistry:
    """Immutable registry of Application_Prefix -> Shared_Prefix search lists.

    Loaded from prefix_registry.yaml at pipeline initialization.

    Traces to: Requirement 5
    """

    registry: dict[str, list[str]]  # prefix -> ordered search prefixes

    @classmethod
    def load(cls, path: Path) -> "PrefixRegistry":
        """Load registry from YAML file.

        Args:
            path: Path to the prefix_registry.yaml file.

        Returns:
            A PrefixRegistry instance populated from the YAML.

        Raises:
            PipelineError: If the file is missing or has invalid structure.
        """
        from .pipeline import PipelineError

        if not path.is_file():
            raise PipelineError(
                "validate",
                f"Prefix registry not found at {path}",
            )

        try:
            with open(path, "r") as f:
                data: Any = yaml.safe_load(f)
        except yaml.YAMLError as e:
            raise PipelineError(
                "validate",
                f"Failed to parse prefix registry: {e}",
            ) from e

        if not isinstance(data, dict) or "registry" not in data:
            raise PipelineError(
                "validate",
                f"Failed to parse prefix registry: missing 'registry' key in {path}",
            )

        registry = data["registry"]
        if not isinstance(registry, dict):
            raise PipelineError(
                "validate",
                f"Failed to parse prefix registry: 'registry' must be a mapping in {path}",
            )

        # Validate each entry: key must be a string, value must be a list of strings
        for key, value in registry.items():
            if not isinstance(key, str):
                raise PipelineError(
                    "validate",
                    f"Failed to parse prefix registry: key {key!r} is not a string",
                )
            if not isinstance(value, list) or not all(isinstance(v, str) for v in value):
                raise PipelineError(
                    "validate",
                    f"Failed to parse prefix registry: value for '{key}' must be a list of strings",
                )

        return cls(registry=registry)

    @classmethod
    def default(cls) -> "PrefixRegistry":
        """Return the built-in default registry (for tests/fallback).

        Returns:
            A PrefixRegistry with the default mappings from Requirement 5.2.
        """
        return cls(registry=dict(_DEFAULT_REGISTRY))

    def get_search_prefixes(self, app_prefix: str) -> list[str] | None:
        """Return the ordered search list for a prefix, or None if unknown.

        Args:
            app_prefix: The application prefix (e.g., 'JGCAFS_').

        Returns:
            Ordered list of shared prefixes to search, or None if the
            prefix is not registered.
        """
        return self.registry.get(app_prefix)

    def known_prefixes(self) -> frozenset[str]:
        """Return all known application prefixes.

        Returns:
            Frozenset of all registered application prefixes.
        """
        return frozenset(self.registry.keys())


# ---------------------------------------------------------------------------
# ResolvedName
# ---------------------------------------------------------------------------


@dataclass(frozen=True)
class ResolvedName:
    """Result of resolving an Application_Name to a source file.

    Attributes:
        application_name: The name as it appears in Workflow_YAML (e.g., JGCAFS_FORECAST)
        source_name: The file in dev/jobs/ (e.g., JGLOBAL_FORECAST)
        is_passthrough: True if the name was found directly (no prefix resolution)

    Traces to: Requirements 2, 8
    """

    application_name: str
    source_name: str
    is_passthrough: bool


# ---------------------------------------------------------------------------
# DryRunReport
# ---------------------------------------------------------------------------


@dataclass
class DryRunReport:
    """Report from dry-run name resolution.

    Produced by `NameResolver.resolve_all_dry_run()`. Contains resolved
    mappings for successful names and error messages for unresolvable ones.

    Traces to: Requirements 7.1, 7.2, 7.3
    """

    resolved: dict[str, ResolvedName] = field(default_factory=dict)
    errors: list[str] = field(default_factory=list)
    total_count: int = 0
    resolvable_count: int = 0
    unresolvable_count: int = 0

    def format_table(self) -> str:
        """Format as a human-readable table for CLI output.

        Returns:
            A string containing a box-drawing table with application names,
            source names, and resolution status, followed by a summary line.
        """
        # Column headers
        col1_header = "Application_Name"
        col2_header = "Shared_Source_Name"
        col3_header = "Status"

        # Collect all rows (sorted by application name for deterministic output)
        rows: list[tuple[str, str, str]] = []
        for app_name in sorted(self.resolved.keys()):
            rn = self.resolved[app_name]
            rows.append((app_name, rn.source_name, "resolved"))

        # Error entries: extract application name from error message
        # Errors are stored as descriptive strings; we extract what we can
        for error in sorted(self.errors):
            # Try to extract the application name from the error string
            # Error format: "Cannot resolve 'NAME': ..." or "Unknown prefix in application name 'NAME'..."
            app_name = _extract_name_from_error(error)
            rows.append((app_name, "\u2014", "ERROR"))

        # Calculate column widths
        col1_width = max(
            len(col1_header),
            *(len(row[0]) for row in rows) if rows else [0],
        )
        col2_width = max(
            len(col2_header),
            *(len(row[1]) for row in rows) if rows else [0],
        )
        col3_width = max(
            len(col3_header),
            *(len(row[2]) for row in rows) if rows else [0],
        )

        # Build the table
        lines: list[str] = []
        lines.append("Name Resolution Report:")

        # Top border
        lines.append(
            f"\u250c{'─' * (col1_width + 2)}\u252c{'─' * (col2_width + 2)}\u252c{'─' * (col3_width + 2)}\u2510"
        )

        # Header row
        lines.append(
            f"\u2502 {col1_header:<{col1_width}} \u2502 {col2_header:<{col2_width}} \u2502 {col3_header:<{col3_width}} \u2502"
        )

        # Header separator
        lines.append(
            f"\u251c{'─' * (col1_width + 2)}\u253c{'─' * (col2_width + 2)}\u253c{'─' * (col3_width + 2)}\u2524"
        )

        # Data rows
        for app_name, source_name, status in rows:
            lines.append(
                f"\u2502 {app_name:<{col1_width}} \u2502 {source_name:<{col2_width}} \u2502 {status:<{col3_width}} \u2502"
            )

        # Bottom border
        lines.append(
            f"\u2514{'─' * (col1_width + 2)}\u2534{'─' * (col2_width + 2)}\u2534{'─' * (col3_width + 2)}\u2518"
        )

        # Summary line
        lines.append(
            f"Summary: {self.resolvable_count} resolvable, "
            f"{self.unresolvable_count} unresolvable ({self.total_count} total)"
        )

        return "\n".join(lines)


def _extract_name_from_error(error: str) -> str:
    """Extract the application name from an error message.

    Handles error formats:
    - "Cannot resolve 'NAME': ..."
    - "Unknown prefix in application name 'NAME'. ..."
    """
    import re

    # Try to match quoted name
    match = re.search(r"'([^']+)'", error)
    if match:
        return match.group(1)
    return "UNKNOWN"


# ---------------------------------------------------------------------------
# NameResolver
# ---------------------------------------------------------------------------


class NameResolver:
    """Resolves Application_Names to Shared_Source_Names in dev/jobs/.

    Args:
        dev_root: Path to the dev/ directory.
        registry: PrefixRegistry instance defining search orders.

    Traces to: Requirements 2, 4, 7, 8
    """

    def __init__(self, dev_root: Path, registry: PrefixRegistry) -> None:
        self._dev_root = dev_root
        self._registry = registry
        self._jobs_dir = dev_root / "jobs"

    def resolve(self, application_name: str) -> ResolvedName:
        """Resolve a single Application_Name to its source file.

        Resolution algorithm:
        1. If application_name exists directly in dev/jobs/ → pass-through
        2. Identify the Application_Prefix from the registry (longest match)
        3. Strip prefix to get suffix, search Shared_Prefixes in registry order
        4. Direct fallback: check if application_name itself exists
        5. If no match: raise PipelineError (FATAL)

        Returns:
            ResolvedName with application_name, source_name, and passthrough flag.

        Raises:
            PipelineError: If no source file can be found.
        """
        from .pipeline import PipelineError

        # Step 1: Direct check — if the file already exists, pass through
        if (self._jobs_dir / application_name).exists():
            return ResolvedName(
                application_name=application_name,
                source_name=application_name,
                is_passthrough=True,
            )

        # Step 2: Prefix identification — find longest matching prefix
        app_prefix: str | None = None
        suffix: str = ""
        for prefix in sorted(self._registry.known_prefixes(), key=len, reverse=True):
            if application_name.startswith(prefix):
                app_prefix = prefix
                suffix = application_name[len(prefix):]
                break

        if app_prefix is None:
            raise PipelineError(
                "name_resolution",
                f"Unknown prefix in application name '{application_name}'. "
                f"Known prefixes: {sorted(self._registry.known_prefixes())}",
            )

        # Step 3: Ordered search through shared prefixes
        search_prefixes = self._registry.get_search_prefixes(app_prefix)
        candidates: list[str] = []
        if search_prefixes:
            for shared_prefix in search_prefixes:
                candidate = shared_prefix + suffix
                candidates.append(candidate)
                if (self._jobs_dir / candidate).exists():
                    return ResolvedName(
                        application_name=application_name,
                        source_name=candidate,
                        is_passthrough=False,
                    )

        # Step 4: Direct fallback — check application_name again
        # (This handles the case where the file was not found in step 1
        # but may exist now, or handles edge cases in the algorithm)
        if (self._jobs_dir / application_name).exists():
            return ResolvedName(
                application_name=application_name,
                source_name=application_name,
                is_passthrough=True,
            )

        # Step 5: FATAL error — no source file found
        candidates.append(application_name)
        raise PipelineError(
            "name_resolution",
            f"Cannot resolve '{application_name}': "
            f"searched [{', '.join(candidates)}] in dev/jobs/",
        )

    def resolve_all(self, application_names: set[str]) -> dict[str, ResolvedName]:
        """Resolve a batch of Application_Names.

        Returns a dict mapping application_name → ResolvedName.
        Raises PipelineError on the first unresolvable name (production mode).

        Args:
            application_names: Set of Application_Names to resolve.

        Returns:
            Dict mapping each application name to its ResolvedName.

        Raises:
            PipelineError: On the first name that cannot be resolved.

        Traces to: Requirement 2.5
        """
        results: dict[str, ResolvedName] = {}
        for name in sorted(application_names):
            results[name] = self.resolve(name)
        return results

    def resolve_all_dry_run(self, application_names: set[str]) -> DryRunReport:
        """Resolve all names, accumulating errors instead of raising.

        Unlike `resolve_all()`, this method does not raise on unresolvable
        names. Instead, it catches PipelineError for each name and accumulates
        errors in the returned report.

        Args:
            application_names: Set of Application_Names to resolve.

        Returns:
            DryRunReport with resolved mappings, errors, and counts.

        Traces to: Requirements 7.1, 7.2, 7.3
        """
        from .pipeline import PipelineError

        resolved: dict[str, ResolvedName] = {}
        errors: list[str] = []

        for name in sorted(application_names):
            try:
                resolved[name] = self.resolve(name)
            except PipelineError as e:
                errors.append(e.message)

        total_count = len(application_names)
        resolvable_count = len(resolved)
        unresolvable_count = len(errors)

        return DryRunReport(
            resolved=resolved,
            errors=errors,
            total_count=total_count,
            resolvable_count=resolvable_count,
            unresolvable_count=unresolvable_count,
        )