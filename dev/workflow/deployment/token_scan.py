"""Token_Scan — unresolved templating-token scanner.

Searches for unresolved templating tokens in two scopes:

1. **Rendered EXPDIR artifacts** (:func:`scan_rendered_expdir`): a sealed
   EXPDIR must contain *no* atparse tokens (``@[VAR]``) and *no* Jinja2 tokens
   (``{{``, ``{%``, ``{#``) in any rendered file. The Atparse_Exemption_Registry
   does **not** exempt EXPDIR artifacts — sealed files are always fully
   rendered (Req 7.5, 9).

2. **Repository runtime sources** (:func:`scan_repo_runtime`): retained runtime
   scripts and config files may contain ``@[VAR]`` atparse tokens *only* if
   their repo-relative path is recorded in the Atparse_Exemption_Registry
   (``dev/parm/atparse_exemptions.yaml``); any other ``@[...]`` is a violation
   (Req 2.6, 3.3). Additionally, ``ush/forecast_postdet.sh`` must not ``source``
   any ``parsing_namelists_*.sh`` script (Req 1.5).

A registry entry whose file no longer contains ``@[...]`` produces a *warning*
(``stale_exemptions``) but does NOT fail the scan (Req 3.4).

Traces to parent: Req 4.6, Req 8, Property 14.
"""

from __future__ import annotations

import re
from dataclasses import dataclass, field
from pathlib import Path
from typing import Iterable, Optional

import yaml

# ---------------------------------------------------------------------------
# Token patterns (Design Component 3)
# ---------------------------------------------------------------------------

#: Runtime atparse token, e.g. ``@[VAR]`` / ``@[my_var]``.
ATPARSE_PATTERN = re.compile(r"@\[[A-Za-z_][A-Za-z0-9_]*\]")

#: Jinja2 token openers that must never survive into a rendered artifact.
#: The ``{#`` pattern uses a negative lookbehind to avoid matching shell
#: parameter expansion ``${#...}`` (string/array length syntax).
JINJA_PATTERNS = (re.compile(r"\{\{"), re.compile(r"\{%"), re.compile(r"(?<!\$)\{#"))

#: ``source ... parsing_namelists_<COMPONENT>.sh`` (any superseded component).
_PARSING_SOURCE_PATTERN = re.compile(
    r"source\s+.*?(parsing_namelists_[A-Za-z0-9_]+\.sh)"
)

# ---------------------------------------------------------------------------
# Scan scoping
# ---------------------------------------------------------------------------

#: Default repository runtime trees scanned for atparse tokens. The deploy-time
#: ``dev/`` tree (Jinja2 ``.j2`` templates), specs, and tooling are intentionally
#: excluded — only the production runtime sources are subject to the
#: "no runtime atparse outside the registry" rule.
RUNTIME_SCAN_DIRS = ("ush", "scripts", "jobs", "parm")

#: Directory names skipped anywhere they appear during a walk.
_EXCLUDED_DIR_NAMES = {
    ".git",
    "__pycache__",
    ".venv",
    ".hypothesis",
    ".pytest_cache",
    ".mypy_cache",
    "node_modules",
}

#: EXPDIR-relative path prefixes that are NOT rendered templates and may
#: legitimately contain Jinja2-like syntax (ecFlow variable references that
#: ecFlow resolves at runtime, verbatim-staged config, generated metadata).
#: Mirrors the exclusions used by tests/test_no_unresolved_tokens.py.
#:
#: J-Jobs (``jobs/``) are staged verbatim from ``dev/jobs/`` and contain
#: commented-out Jinja2 conditional compilation markers (``#{% if ... %}``).
#: The ``parm/`` tree includes the exemption registry itself.
EXPDIR_EXCLUDED_PREFIXES = (
    "parm/workflow/",      # Workflow_Configuration source (staged verbatim)
    "parm/atparse_exemptions",  # Exemption registry (documents @[VAR] pattern)
    "parm/components/",    # Workflow config components (contain Jinja2 by design)
    "parm/config/gcafs/yaml/",  # GCAFS YAML configs (contain Jinja2 includes)
    "ecf/defs/",           # ecFlow .def files (contain ecFlow vars)
    "workflow/provenance",  # provenance metadata (raw config values)
    "manifest.yaml",       # deployment manifest (generated metadata)
    "jobs/",               # J-Jobs (staged verbatim, contain {%} markers)
)


@dataclass
class TokenScanResult:
    """Outcome of a Token_Scan pass.

    Attributes:
        atparse_violations: ``(path, lineno, token)`` for disallowed ``@[...]``.
        jinja_violations: ``(path, lineno, token)`` for ``{{`` / ``{%`` / ``{#``.
        stale_exemptions: registry paths that no longer contain ``@[...]``
            (warning only — they do NOT fail the scan, per Req 3.4).
        parsing_source_violations: ``(script, sourced_name)`` for any
            ``source ... parsing_namelists_*.sh`` reference.
    """

    atparse_violations: list[tuple[str, int, str]] = field(default_factory=list)
    jinja_violations: list[tuple[str, int, str]] = field(default_factory=list)
    stale_exemptions: list[str] = field(default_factory=list)
    parsing_source_violations: list[tuple[str, str]] = field(default_factory=list)

    @property
    def passed(self) -> bool:
        """True iff there are no hard violations.

        ``stale_exemptions`` are warnings only (Req 3.4) and do NOT affect the
        pass/fail outcome.
        """
        return not (
            self.atparse_violations
            or self.jinja_violations
            or self.parsing_source_violations
        )

    def format_report(self) -> str:
        """Render a human-readable summary of violations and warnings."""
        lines: list[str] = []
        for path, lineno, token in self.atparse_violations:
            lines.append(
                f"FATAL ERROR: unresolved atparse token '{token}' in "
                f"{path}:{lineno} (not in Atparse_Exemption_Registry)"
            )
        for path, lineno, token in self.jinja_violations:
            lines.append(
                f"FATAL ERROR: unresolved Jinja2 token '{token}' in "
                f"{path}:{lineno}"
            )
        for script, sourced in self.parsing_source_violations:
            lines.append(
                f"FATAL ERROR: {script} sources runtime templating script "
                f"'{sourced}' (must consume pre-rendered config via cpreq)"
            )
        for entry in self.stale_exemptions:
            lines.append(
                f"WARNING: stale Atparse_Exemption_Registry entry '{entry}' "
                f"no longer contains any @[...] tokens; remove it."
            )
        return "\n".join(lines)


def load_exemptions(registry_path: Path) -> set[str]:
    """Load the set of exempt repo-relative paths from the registry YAML.

    The registry format is a top-level ``exemptions:`` list of
    ``{path: <repo-relative>, justification: <str>}`` entries (Req 3.1).

    Args:
        registry_path: Path to ``dev/parm/atparse_exemptions.yaml``.

    Returns:
        The set of repo-relative paths permitted to retain runtime ``@[...]``.
        Returns an empty set if the file is missing or has no entries.
    """
    registry_path = Path(registry_path)
    if not registry_path.is_file():
        return set()

    data = yaml.safe_load(registry_path.read_text(encoding="utf-8")) or {}
    entries = data.get("exemptions") or []

    exempt: set[str] = set()
    for entry in entries:
        if isinstance(entry, dict) and entry.get("path"):
            exempt.add(str(entry["path"]).strip())
    return exempt


def _read_text_safe(path: Path) -> Optional[str]:
    """Read a file as UTF-8 text, returning None for binary/unreadable files.

    Skips binary files gracefully (Req: binary-safe scanning of a real EXPDIR
    or repo): a NUL byte in the first chunk marks the file as binary.
    """
    try:
        raw = path.read_bytes()
    except (OSError, IOError):
        return None
    if b"\x00" in raw:
        return None
    try:
        return raw.decode("utf-8")
    except UnicodeDecodeError:
        # Fall back to a lossy decode so ASCII tokens are still detectable.
        return raw.decode("utf-8", errors="replace")


def _iter_files(root: Path, scan_dirs: Optional[Iterable[str]] = None):
    """Yield regular files under ``root``, skipping excluded directories.

    If ``scan_dirs`` is given, only those top-level subdirectories of ``root``
    are walked; otherwise the entire tree under ``root`` is walked.
    """
    if scan_dirs is None:
        roots = [root]
    else:
        roots = [root / d for d in scan_dirs]

    for base in roots:
        if not base.exists():
            continue
        if base.is_file():
            yield base
            continue
        for path in sorted(base.rglob("*")):
            if not path.is_file():
                continue
            if any(part in _EXCLUDED_DIR_NAMES for part in path.parts):
                continue
            yield path


def _find_tokens(content: str, pattern: re.Pattern) -> list[tuple[int, str]]:
    """Return ``(lineno, matched_text)`` for every match, 1-indexed by line."""
    found: list[tuple[int, str]] = []
    for lineno, line in enumerate(content.splitlines(), start=1):
        for match in pattern.finditer(line):
            found.append((lineno, match.group(0)))
    return found


def _is_expdir_excluded(rel_path: str) -> bool:
    """True if an EXPDIR-relative path is a non-rendered metadata file.

    Also excludes ``.j2`` files (Jinja2 templates staged from submodules that
    are not rendered during deployment — they are consumed at runtime by the
    Template_Renderer or model config tools).
    """
    if rel_path.endswith(".j2"):
        return True
    return any(
        rel_path.startswith(prefix) or rel_path == prefix
        for prefix in EXPDIR_EXCLUDED_PREFIXES
    )


def scan_rendered_expdir(expdir: Path) -> TokenScanResult:
    """Scan a sealed EXPDIR for any unresolved templating tokens.

    A rendered EXPDIR file must contain no ``@[...]`` atparse tokens and no
    ``{{`` / ``{%`` / ``{#`` Jinja2 tokens (Req 7.5, 9). The
    Atparse_Exemption_Registry does NOT exempt EXPDIR artifacts — sealed files
    are always fully rendered.

    Non-rendered metadata files (see :data:`EXPDIR_EXCLUDED_PREFIXES`) and
    binary files are skipped.

    Args:
        expdir: Path to the EXPDIR root.

    Returns:
        A :class:`TokenScanResult`. ``parsing_source_violations`` and
        ``stale_exemptions`` are always empty for an EXPDIR scan.
    """
    result = TokenScanResult()
    expdir = Path(expdir)

    for path in _iter_files(expdir):
        rel_path = path.relative_to(expdir).as_posix()
        if _is_expdir_excluded(rel_path):
            continue
        content = _read_text_safe(path)
        if content is None:
            continue

        for lineno, token in _find_tokens(content, ATPARSE_PATTERN):
            result.atparse_violations.append((rel_path, lineno, token))
        for pattern in JINJA_PATTERNS:
            for lineno, token in _find_tokens(content, pattern):
                result.jinja_violations.append((rel_path, lineno, token))

    return result


def scan_repo_runtime(
    repo_root: Path,
    registry: set[str],
    scan_dirs: Optional[Iterable[str]] = None,
) -> TokenScanResult:
    """Scan retained repository runtime sources for disallowed atparse usage.

    Rules:
      - A runtime file containing ``@[...]`` passes ONLY if its repo-relative
        path is in ``registry``; otherwise every token is a violation (Req 3.3).
      - A registry entry whose file contains no ``@[...]`` is reported as a
        stale exemption warning (Req 3.4) — it does NOT fail the scan.
      - ``ush/forecast_postdet.sh`` must not ``source`` any
        ``parsing_namelists_*.sh`` script (Req 1.5).

    Args:
        repo_root: Repository root.
        registry: Set of exempt repo-relative paths (from
            :func:`load_exemptions`).
        scan_dirs: Optional override of the runtime subdirectories to walk.
            Defaults to :data:`RUNTIME_SCAN_DIRS`.

    Returns:
        A :class:`TokenScanResult`. ``jinja_violations`` is always empty for a
        repo-runtime scan (Jinja2 templates legitimately live under ``dev/``).
    """
    result = TokenScanResult()
    repo_root = Path(repo_root)
    dirs = tuple(scan_dirs) if scan_dirs is not None else RUNTIME_SCAN_DIRS

    # Track which registry paths actually contained @[...] tokens so we can
    # report the remainder as stale exemptions.
    exemptions_with_tokens: set[str] = set()

    for path in _iter_files(repo_root, dirs):
        rel_path = path.relative_to(repo_root).as_posix()
        content = _read_text_safe(path)
        if content is None:
            continue

        atparse_hits = _find_tokens(content, ATPARSE_PATTERN)
        if atparse_hits:
            if rel_path in registry:
                exemptions_with_tokens.add(rel_path)
            else:
                for lineno, token in atparse_hits:
                    result.atparse_violations.append((rel_path, lineno, token))

    # Stale-exemption detection: a registry entry that exists but no longer
    # carries any @[...] tokens (or whose file is absent) is stale (Req 3.4).
    for exempt_path in sorted(registry):
        if exempt_path not in exemptions_with_tokens:
            result.stale_exemptions.append(exempt_path)

    # forecast_postdet.sh must not source any parsing_namelists_*.sh (Req 1.5).
    forecast_postdet = repo_root / "ush" / "forecast_postdet.sh"
    if forecast_postdet.is_file():
        result.parsing_source_violations.extend(
            _scan_parsing_sources(forecast_postdet, repo_root)
        )

    return result


def _scan_parsing_sources(
    script_path: Path, repo_root: Path
) -> list[tuple[str, str]]:
    """Return ``(script_rel, sourced_name)`` for parsing_namelists sources.

    Comment lines (first non-whitespace char ``#``) are ignored so that
    documentation referencing the removed scripts by name is not flagged.
    """
    content = _read_text_safe(script_path)
    if content is None:
        return []

    try:
        script_rel = script_path.relative_to(repo_root).as_posix()
    except ValueError:
        script_rel = script_path.as_posix()

    violations: list[tuple[str, str]] = []
    for line in content.splitlines():
        if line.lstrip().startswith("#"):
            continue
        match = _PARSING_SOURCE_PATTERN.search(line)
        if match:
            violations.append((script_rel, match.group(1)))
    return violations
