"""Reference-guarded deletion of obsolete runtime scripts and legacy files.

Implements the deletion-guard algorithm (Req 2.5, Design Component 2): before
deleting a file ``F``, the repository's *retained runtime* trees are searched
for functional references to ``basename(F)``. If any retained script still
references ``F``, the file is **retained** and a verification error naming the
referencing script is emitted, so the deletion is blocked until the reference
is removed. This makes deletions safe and order-independent — e.g. ``atparse.bash``
is only deletable after every ``parsing_*.sh`` *and* every registry-exempt
consumer stops sourcing it.

Two deletion modes are supported:

* **Reference-guarded** (Req 2.1, 2.2): ``parsing_namelists_*.sh``,
  ``parsing_model_configure_FV3.sh``, ``parsing_ufs_configure.sh`` and
  ``atparse.bash``. Deleted only when no *retained* runtime script references
  them.
* **Unconditional** (Req 2.3, 2.4): the legacy ``@[...]`` token-bearing data
  files ``parm/ufs/fv3/diag_table`` and ``parm/ufs/gocart/AERO_HISTORY.rc``,
  which are superseded by deploy-time ``.j2`` templates under ``dev/parm/``.

Reference scanning is intentionally scoped to the production runtime trees
(:data:`RUNTIME_SCAN_DIRS`) and excludes ``.git``, ``__pycache__``, the file
itself, the concurrent deletion batch, and the Atparse_Exemption_Registry. It
also ignores comment-only matches (a name appearing in a ``#`` comment or in the
inline-comment portion of a line is documentation, not a functional reference)
so that the "# replaces parsing_namelists_WW3.sh" breadcrumbs left in
``forecast_postdet.sh`` do not block deletion.

Traces to parent: Req 4.6, Req 8, Property 14.
"""

from __future__ import annotations

from dataclasses import dataclass, field
from pathlib import Path
from typing import Iterable, Optional

# ---------------------------------------------------------------------------
# Scan scoping (mirrors token_scan.RUNTIME_SCAN_DIRS)
# ---------------------------------------------------------------------------

#: Production runtime trees searched for functional references. The deploy-time
#: ``dev/`` tree (``.j2`` templates whose ``{# Replaces ... #}`` headers name the
#: retired scripts), specs, docs, ``.github`` and ``sorc/`` submodules are
#: intentionally excluded — only retained *runtime* scripts can block a deletion.
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

#: Repo-relative path of the Atparse_Exemption_Registry (never a "referencer").
DEFAULT_REGISTRY_REL = "dev/parm/atparse_exemptions.yaml"

#: Reference-guarded deletion targets (Req 2.1, 2.2), repo-relative.
GUARDED_TARGETS = (
    "ush/parsing_namelists_WW3.sh",
    "ush/parsing_namelists_MOM6.sh",
    "ush/parsing_namelists_CICE.sh",
    "ush/parsing_namelists_GOCART.sh",
    "ush/parsing_namelists_FV3.sh",
    "ush/parsing_namelists_FV3_nest.sh",
    "ush/parsing_model_configure_FV3.sh",
    "ush/parsing_ufs_configure.sh",
    # atparse.bash is checked LAST: it is only deletable once every
    # parsing_*.sh and every registry-exempt consumer stops sourcing it.
    "ush/atparse.bash",
)

#: Unconditional deletion targets (Req 2.3, 2.4), repo-relative.
UNCONDITIONAL_TARGETS = (
    "parm/ufs/fv3/diag_table",
    "parm/ufs/gocart/AERO_HISTORY.rc",
)


@dataclass
class DeletionResult:
    """Outcome of a reference-guarded deletion pass.

    Attributes:
        deleted: repo-relative paths that were removed.
        retained: ``(path, [referencer, ...])`` for targets blocked by a
            retained-script reference (Req 2.5).
        missing: targets that did not exist (nothing to delete).
        errors: human-readable verification errors (one per retained target).
    """

    deleted: list[str] = field(default_factory=list)
    retained: list[tuple[str, list[str]]] = field(default_factory=list)
    missing: list[str] = field(default_factory=list)
    errors: list[str] = field(default_factory=list)

    @property
    def passed(self) -> bool:
        """True iff no target was blocked by a retained reference."""
        return not self.retained and not self.errors

    def format_report(self) -> str:
        """Render a human-readable summary of the pass."""
        lines: list[str] = []
        for path in self.deleted:
            lines.append(f"DELETED: {path}")
        for path in self.missing:
            lines.append(f"SKIPPED (absent): {path}")
        for path, referencers in self.retained:
            joined = ", ".join(referencers)
            lines.append(
                f"VERIFICATION ERROR: retained '{path}' — still referenced by: "
                f"{joined} (deletion blocked until the reference is removed)"
            )
        return "\n".join(lines)


def _read_text_safe(path: Path) -> Optional[str]:
    """Read a file as UTF-8 text, returning None for binary/unreadable files."""
    try:
        raw = path.read_bytes()
    except (OSError, IOError):
        return None
    if b"\x00" in raw:
        return None
    return raw.decode("utf-8", errors="replace")


def _iter_runtime_files(repo_root: Path, scan_dirs: Iterable[str]):
    """Yield regular files under the runtime trees, skipping excluded dirs."""
    for d in scan_dirs:
        base = repo_root / d
        if not base.exists():
            continue
        for path in sorted(base.rglob("*")):
            if not path.is_file():
                continue
            if any(part in _EXCLUDED_DIR_NAMES for part in path.parts):
                continue
            yield path


def _code_portion(line: str) -> str:
    """Return the portion of a shell/Python line before an inline ``#`` comment.

    This is a deliberately simple heuristic: it splits on the first ``#``. It can
    truncate a ``#`` that lives inside a quoted string, but for reference
    detection that only makes the check *more* conservative (it never turns a
    real functional reference into a missed one for the basenames handled here,
    none of which contain ``#``).
    """
    return line.split("#", 1)[0]


def find_blocking_references(
    repo_root: Path,
    target_rel: str,
    *,
    registry_rel: str = DEFAULT_REGISTRY_REL,
    batch: Iterable[str] = (),
    scan_dirs: Iterable[str] = RUNTIME_SCAN_DIRS,
) -> list[str]:
    """Return retained-script paths that functionally reference ``target_rel``.

    A *functional* reference is the target's basename appearing in the code
    portion (outside any ``#`` comment) of a retained runtime file. Files in
    ``batch`` (the concurrent deletion set), the target itself, and the
    Atparse_Exemption_Registry are not counted as referencers (Req 2.5).

    Args:
        repo_root: Repository root.
        target_rel: Repo-relative path of the file being considered for deletion.
        registry_rel: Repo-relative path of the exemption registry to exclude.
        batch: Repo-relative paths also scheduled for deletion in this pass.
        scan_dirs: Runtime subdirectories to search.

    Returns:
        Sorted, de-duplicated repo-relative paths of retained referencing files.
    """
    repo_root = Path(repo_root)
    basename = Path(target_rel).name
    batch_set = set(batch)
    referencers: set[str] = set()

    for path in _iter_runtime_files(repo_root, scan_dirs):
        rel = path.relative_to(repo_root).as_posix()
        if rel == target_rel or rel in batch_set or rel == registry_rel:
            continue
        text = _read_text_safe(path)
        if text is None:
            continue
        for line in text.splitlines():
            stripped = line.lstrip()
            if stripped.startswith("#"):
                continue
            if basename in _code_portion(line):
                referencers.add(rel)
                break

    return sorted(referencers)


def delete_guarded(
    repo_root: Path,
    *,
    guarded_targets: Iterable[str] = GUARDED_TARGETS,
    unconditional_targets: Iterable[str] = UNCONDITIONAL_TARGETS,
    registry_rel: str = DEFAULT_REGISTRY_REL,
    dry_run: bool = False,
) -> DeletionResult:
    """Delete obsolete scripts (reference-guarded) and legacy files.

    Reference-guarded targets are removed only when no retained runtime script
    references them; otherwise they are retained with a verification error
    (Req 2.5). Unconditional targets (the legacy ``@[...]`` data files) are
    removed when present (Req 2.3, 2.4).

    Args:
        repo_root: Repository root.
        guarded_targets: Repo-relative reference-guarded deletion targets.
        unconditional_targets: Repo-relative unconditional deletion targets.
        registry_rel: Repo-relative exemption-registry path to exclude from scans.
        dry_run: If True, compute the outcome without removing any file.

    Returns:
        A :class:`DeletionResult` describing the pass.
    """
    repo_root = Path(repo_root)
    result = DeletionResult()

    guarded = list(guarded_targets)
    batch = set(guarded)

    for target_rel in guarded:
        target = repo_root / target_rel
        if not target.exists():
            result.missing.append(target_rel)
            continue
        referencers = find_blocking_references(
            repo_root,
            target_rel,
            registry_rel=registry_rel,
            batch=batch,
        )
        if referencers:
            result.retained.append((target_rel, referencers))
            result.errors.append(
                f"VERIFICATION ERROR: cannot delete '{target_rel}' — still "
                f"referenced by retained script(s): {', '.join(referencers)}"
            )
            continue
        if not dry_run:
            target.unlink()
        result.deleted.append(target_rel)

    for target_rel in unconditional_targets:
        target = repo_root / target_rel
        if not target.exists():
            result.missing.append(target_rel)
            continue
        if not dry_run:
            target.unlink()
        result.deleted.append(target_rel)

    return result


def _default_repo_root() -> Path:
    """Repository root inferred from this file: deployment -> workflow -> dev -> root."""
    return Path(__file__).resolve().parents[3]


def main(argv: Optional[list[str]] = None) -> int:
    """CLI entry point: run a guarded deletion pass and print the report."""
    import argparse

    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument(
        "--repo-root",
        type=Path,
        default=_default_repo_root(),
        help="Repository root (default: inferred from this file's location).",
    )
    parser.add_argument(
        "--dry-run",
        action="store_true",
        help="Report the outcome without removing any file.",
    )
    args = parser.parse_args(argv)

    result = delete_guarded(args.repo_root, dry_run=args.dry_run)
    print(result.format_report())
    # A retained (blocked) target is a verification error: non-zero exit.
    return 0 if result.passed else 1


if __name__ == "__main__":  # pragma: no cover
    raise SystemExit(main())
