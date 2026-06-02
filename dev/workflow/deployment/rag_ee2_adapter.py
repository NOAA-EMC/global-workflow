"""RAG-backed EE2 compliance adapter (Design Component 8, Req 10).

The authoritative EE2 judge for this feature is the agentcore MCP RAG server's
EE2 tooling, backed by the official NCEP WCOSS EE2 v11 standards with the
Phase 2 SME-corrected patterns (``set -eu`` / ``set -e`` are NOT required;
``err_chk`` / ``err_exit`` / ``cpreq`` / ``cpfs`` are the correct patterns).

**Hard availability constraint.** The agentcore MCP RAG server is reachable
*only inside the development environment* — never in CI or any other
environment. EE2 authority is therefore split into two phases:

1. **Development-time authoring (RAG live).** A developer constructs a
   :class:`RagEE2Client` that wraps the live MCP RAG tools and calls
   :func:`run_rag_ee2_scan` on the created/modified scripts. The authoritative
   per-file / per-category verdict is persisted by :func:`record_baseline` as a
   committed **EE2_Baseline_Recording** (JSON) under
   ``dev/workflow/tests/fixtures/ee2/``.
2. **CI / gate time (offline, RAG absent).** The Goal_Realization_Gate never
   calls the RAG server. It runs the reconciled in-repo
   :mod:`deployment.ee2_scanner` and uses :func:`check_against_baseline` to
   confirm the offline scanner still reproduces the committed authoritative
   verdict.

This module is import-safe and offline-safe: it has **no** dependency on the
MCP RAG server. The only RAG coupling is through the injected
:class:`RagEE2Client`, which is implemented exclusively in the development
environment. Nothing here imports or calls the RAG server at module import or
gate time.

Traces to parent: Req 11.6. This spec: Req 10.1, 10.2, 10.3, 10.4, 10.6, 10.7.
"""

from __future__ import annotations

import json
import re
import subprocess
from dataclasses import dataclass, field
from pathlib import Path
from typing import Iterable, Optional, Protocol, runtime_checkable

# ---------------------------------------------------------------------------
# Category sets (Design Component 8)
# ---------------------------------------------------------------------------

#: Categories passed to the RAG ``scan_repository_compliance`` tool (Req 10.1).
SCAN_CATEGORIES = [
    "error_handling",
    "environment_variables",
    "file_naming",
    "shebang_compliance",
    "production_utilities",
]

#: Categories passed to the RAG ``extract_code_for_analysis`` tool — the checks
#: the standard scan does not auto-evaluate (Req 10.2).
EXTRACT_CATEGORIES = [
    "output_file_naming",
    "shebang_compliance",
    "env_var_validation",
]

#: Categories the in-repo :mod:`deployment.ee2_scanner` actually evaluates.
#: ``check_against_baseline`` compares only these overlapping scan categories,
#: because ``production_utilities`` and the extract categories are RAG-only.
SCANNER_CATEGORIES = [
    "error_handling",
    "environment_variables",
    "file_naming",
    "shebang_compliance",
]

#: Schema version of the EE2_Baseline_Recording JSON artifact.
BASELINE_SCHEMA_VERSION = "1.0"

#: Provenance string recorded in every baseline (the authoritative source).
BASELINE_AUTHORITY = (
    "agentcore MCP RAG EE2 v11 (Phase 2 SME-corrected patterns: "
    "err_chk/err_exit/cpreq/cpfs correct; set -e / set -eu not required)"
)


# ---------------------------------------------------------------------------
# Changed-file derivation (Design Component 8)
# ---------------------------------------------------------------------------

#: Basename pattern for J-Jobs (uppercase, starts with J, no extension).
_JJOB_RE = re.compile(r"^J[A-Z][A-Z0-9_]*$")

#: Basename pattern for ex-scripts (``ex*.sh`` / ``ex*.py``).
_EXSCRIPT_RE = re.compile(r"^ex.*\.(sh|py)$")


def _is_ee2_relevant(rel_path: str) -> bool:
    """True if a repo-relative path is in scope for the RAG EE2 scan.

    Scope (Design Component 8): ``*.sh``, J-Jobs (``J[A-Z_]*``), ex-scripts
    (``ex*.sh`` / ``ex*.py``), and anything under a ``ush/`` tree.
    """
    posix = rel_path.replace("\\", "/")
    name = posix.rsplit("/", 1)[-1]
    if posix.endswith(".sh"):
        return True
    if _JJOB_RE.match(name):
        return True
    if _EXSCRIPT_RE.match(name):
        return True
    if posix == "ush" or posix.startswith("ush/") or "/ush/" in posix:
        return True
    return False


def derive_changed_files(
    repo_root: Path,
    base_ref: Optional[str] = None,
    *,
    include_untracked: bool = True,
) -> list[str]:
    """Derive the EE2-relevant changed-file set from ``git diff --name-only``.

    Returns repo-relative POSIX paths for changed files filtered to ``*.sh``,
    J-Jobs, ex-scripts, and ``ush/`` scripts (Design Component 8). When
    ``base_ref`` is given the diff is taken against it (e.g. the feature merge
    base); otherwise the working-tree diff against ``HEAD`` is used.

    Files that no longer exist on disk (pure deletions) are dropped — the scan
    targets created/modified scripts.

    Args:
        repo_root: Repository root.
        base_ref: Optional git ref to diff against (default: ``HEAD``).
        include_untracked: Also include new untracked files (``git status``).

    Returns:
        Sorted, de-duplicated repo-relative paths in EE2 scope that exist on
        disk. Returns an empty list if git is unavailable.
    """
    repo_root = Path(repo_root)
    candidates: set[str] = set()

    diff_cmd = ["git", "diff", "--name-only"]
    if base_ref:
        diff_cmd.append(base_ref)
    candidates.update(_run_git(diff_cmd, repo_root))

    if include_untracked:
        candidates.update(
            _run_git(
                ["git", "ls-files", "--others", "--exclude-standard"], repo_root
            )
        )

    relevant: set[str] = set()
    for rel in candidates:
        if not rel:
            continue
        if not _is_ee2_relevant(rel):
            continue
        if not (repo_root / rel).is_file():
            continue  # pure deletion — nothing to scan
        relevant.add(rel)

    return sorted(relevant)


def _run_git(cmd: list[str], cwd: Path) -> list[str]:
    """Run a git command, returning stdout lines (empty list on failure)."""
    try:
        proc = subprocess.run(
            cmd,
            cwd=str(cwd),
            capture_output=True,
            text=True,
            check=False,
        )
    except (OSError, ValueError):
        return []
    if proc.returncode != 0:
        return []
    return [line.strip() for line in proc.stdout.splitlines() if line.strip()]


# ---------------------------------------------------------------------------
# RAG client protocol + result model
# ---------------------------------------------------------------------------


@runtime_checkable
class RagEE2Client(Protocol):
    """Protocol for the live RAG EE2 tooling — implemented only in the dev env.

    A concrete implementation wraps the agentcore MCP RAG tools
    ``scan_repository_compliance`` and ``extract_code_for_analysis`` and returns
    their parsed JSON payloads. It is constructed and used **only** in the
    development environment; CI and unit tests never instantiate it.

    ``files`` is a list of ``{"name": str, "content": str, "path": str}`` dicts.
    """

    def scan_repository_compliance(
        self, files: list[dict], categories: list[str]
    ) -> dict:
        """Return the parsed ``scan_repository_compliance`` JSON payload."""
        ...

    def extract_code_for_analysis(
        self, files: list[dict], categories: list[str]
    ) -> dict:
        """Return the parsed ``extract_code_for_analysis`` findings payload."""
        ...


@dataclass
class RagEE2Result:
    """Authoritative RAG EE2 verdict for a set of scanned files.

    Attributes:
        files_with_issues: Count of files the scan flagged (Req 10.1).
        issues_by_category: ``category -> [issue dict, ...]`` from the scan;
            each issue dict carries at least a ``file`` key when present.
        extract_findings: ``extract_category -> [finding dict, ...]`` from
            ``extract_code_for_analysis`` (Req 10.2); empty lists mean every
            finding was resolved or carried an explicit written justification.
        scanned_files: Repo-relative paths that were submitted to the scan.
    """

    files_with_issues: int
    issues_by_category: dict[str, list[dict]] = field(default_factory=dict)
    extract_findings: dict[str, list[dict]] = field(default_factory=dict)
    scanned_files: list[str] = field(default_factory=list)

    @property
    def passed(self) -> bool:
        """True iff the scan found no issues and no unresolved extract finding."""
        return self.files_with_issues == 0 and not any(
            self.extract_findings.values()
        )

    def per_file_verdict(self) -> dict[str, dict]:
        """Build a ``file -> {scan: {...}, extract: {...}}`` verdict map.

        For every scanned file, each scan category is marked ``"clean"`` unless
        an issue in ``issues_by_category`` names that file, and each extract
        category is marked ``"clean"`` unless ``extract_findings`` names it.
        """
        flagged_scan: dict[str, set[str]] = {}
        for category, issues in self.issues_by_category.items():
            for issue in issues or []:
                fname = _issue_file(issue)
                if fname:
                    flagged_scan.setdefault(fname, set()).add(category)

        flagged_extract: dict[str, set[str]] = {}
        for category, findings in self.extract_findings.items():
            for finding in findings or []:
                fname = _issue_file(finding)
                if fname:
                    flagged_extract.setdefault(fname, set()).add(category)

        verdict: dict[str, dict] = {}
        for rel in self.scanned_files:
            base = rel.replace("\\", "/").rsplit("/", 1)[-1]
            scan_flags = flagged_scan.get(rel, set()) | flagged_scan.get(base, set())
            extract_flags = (
                flagged_extract.get(rel, set()) | flagged_extract.get(base, set())
            )
            verdict[rel] = {
                "scan": {
                    cat: ("issue" if cat in scan_flags else "clean")
                    for cat in SCAN_CATEGORIES
                },
                "extract": {
                    cat: ("issue" if cat in extract_flags else "clean")
                    for cat in EXTRACT_CATEGORIES
                },
            }
        return verdict


def _issue_file(issue: dict) -> Optional[str]:
    """Best-effort extraction of a file path from a RAG issue/finding dict."""
    if not isinstance(issue, dict):
        return None
    for key in ("file", "path", "filename", "name"):
        value = issue.get(key)
        if value:
            return str(value).replace("\\", "/")
    return None


def _file_payload(repo_root: Path, rel_path: str) -> dict:
    """Build a ``{name, content, path}`` payload for a repo-relative file."""
    abs_path = Path(repo_root) / rel_path
    content = abs_path.read_text(encoding="utf-8", errors="replace")
    return {
        "name": Path(rel_path).name,
        "content": content,
        "path": rel_path,
    }


def run_rag_ee2_scan(
    client: RagEE2Client,
    changed_files: Iterable[Path | str],
    *,
    repo_root: Optional[Path] = None,
    scan_categories: Optional[list[str]] = None,
    extract_categories: Optional[list[str]] = None,
) -> RagEE2Result:
    """DEV-ONLY: scan created/modified scripts with the live RAG EE2 tooling.

    Runs ``scan_repository_compliance`` over :data:`SCAN_CATEGORIES` and
    ``extract_code_for_analysis`` over :data:`EXTRACT_CATEGORIES` (Req 10.1,
    10.2), returning the authoritative :class:`RagEE2Result`.

    Args:
        client: A live :class:`RagEE2Client` (dev environment only).
        changed_files: Repo-relative (or absolute) paths to scan.
        repo_root: Repository root used to resolve relative paths and read
            content. Defaults to the inferred repo root.
        scan_categories: Override of the scan categories (default
            :data:`SCAN_CATEGORIES`).
        extract_categories: Override of the extract categories (default
            :data:`EXTRACT_CATEGORIES`).

    Returns:
        The authoritative :class:`RagEE2Result`.
    """
    root = Path(repo_root) if repo_root is not None else _default_repo_root()
    scats = list(scan_categories) if scan_categories is not None else list(SCAN_CATEGORIES)
    ecats = (
        list(extract_categories)
        if extract_categories is not None
        else list(EXTRACT_CATEGORIES)
    )

    rels: list[str] = []
    payloads: list[dict] = []
    for entry in changed_files:
        rel = _as_rel(entry, root)
        rels.append(rel)
        payloads.append(_file_payload(root, rel))

    scan_resp = client.scan_repository_compliance(payloads, scats) or {}
    extract_resp = client.extract_code_for_analysis(payloads, ecats) or {}

    stats = scan_resp.get("statistics", {}) or {}
    files_with_issues = int(stats.get("files_with_issues", 0) or 0)
    issues_by_category = {
        str(k): list(v or [])
        for k, v in (scan_resp.get("issues_by_category", {}) or {}).items()
    }

    extract_findings = _parse_extract_findings(extract_resp, ecats)

    return RagEE2Result(
        files_with_issues=files_with_issues,
        issues_by_category=issues_by_category,
        extract_findings=extract_findings,
        scanned_files=rels,
    )


def _parse_extract_findings(
    extract_resp: dict, categories: list[str]
) -> dict[str, list[dict]]:
    """Normalize an extract response into ``category -> [finding dict, ...]``.

    Accepts either an explicit ``findings`` / ``extract_findings`` mapping or a
    flat ``{category: [...]}`` payload. Every requested category is represented
    (empty list when there is no finding), so :attr:`RagEE2Result.passed` is
    well-defined.
    """
    findings_map = (
        extract_resp.get("extract_findings")
        or extract_resp.get("findings")
        or extract_resp.get("issues_by_category")
        or {}
    )
    result: dict[str, list[dict]] = {}
    for cat in categories:
        value = findings_map.get(cat, []) if isinstance(findings_map, dict) else []
        result[cat] = list(value or [])
    return result


def _as_rel(entry: Path | str, repo_root: Path) -> str:
    """Return a repo-relative POSIX path for a path-like entry."""
    p = Path(entry)
    if p.is_absolute():
        try:
            return p.relative_to(repo_root).as_posix()
        except ValueError:
            return p.as_posix()
    return p.as_posix()


# ---------------------------------------------------------------------------
# Baseline recording + offline reconciliation
# ---------------------------------------------------------------------------


def record_baseline(
    result: RagEE2Result,
    out_dir: Path,
    *,
    name: str = "ee2_baseline.json",
) -> Path:
    """Persist a :class:`RagEE2Result` as a committed EE2_Baseline_Recording.

    The recording is deterministic (sorted keys, no volatile timestamp) so it
    is reproducible offline by CI and by unit tests without a live RAG
    connection (Req 10.3).

    Args:
        result: The authoritative RAG verdict.
        out_dir: Directory to write the recording into (created if absent).
        name: Recording filename.

    Returns:
        The path to the written recording.
    """
    out_dir = Path(out_dir)
    out_dir.mkdir(parents=True, exist_ok=True)
    out_path = out_dir / name

    recording = {
        "schema_version": BASELINE_SCHEMA_VERSION,
        "authority": BASELINE_AUTHORITY,
        "scan_categories": list(SCAN_CATEGORIES),
        "extract_categories": list(EXTRACT_CATEGORIES),
        "scanner_categories": list(SCANNER_CATEGORIES),
        "files_with_issues": result.files_with_issues,
        "passed": result.passed,
        "issues_by_category": result.issues_by_category,
        "extract_findings": result.extract_findings,
        "files": result.per_file_verdict(),
    }
    out_path.write_text(
        json.dumps(recording, indent=2, sort_keys=True) + "\n",
        encoding="utf-8",
    )
    return out_path


def load_baseline(baseline_path: Path) -> dict:
    """Load an EE2_Baseline_Recording JSON artifact."""
    return json.loads(Path(baseline_path).read_text(encoding="utf-8"))


def check_against_baseline(scanner_result, baseline_path: Path) -> list[str]:
    """OFFLINE: confirm the reconciled scanner reproduces the RAG baseline.

    Compares the in-repo :mod:`deployment.ee2_scanner` ``ScanResult`` against
    the committed EE2_Baseline_Recording over the overlapping scan categories
    (:data:`SCANNER_CATEGORIES`). ``production_utilities`` and the extract
    categories are RAG-only and are not compared (the offline scanner does not
    evaluate them).

    A divergence is reported when, for a baseline file and a comparable
    category:

    * the baseline says ``clean`` but the scanner flags a violation
      (a false positive the scanner must be reconciled away — Req 10.4), or
    * the baseline says ``issue`` but the scanner reports none
      (a false negative — Req 10.4).

    Args:
        scanner_result: An ``ee2_scanner.ScanResult`` (anything exposing a
            ``violations`` iterable of objects with ``category`` and ``file``).
        baseline_path: Path to the committed EE2_Baseline_Recording.

    Returns:
        A list of human-readable divergence messages. Empty means the offline
        scanner faithfully reproduces the authoritative RAG verdict (Req 10.6).
    """
    baseline = load_baseline(baseline_path)
    files = baseline.get("files", {}) or {}

    # Build the set of (basename, category) pairs the scanner flagged.
    scanner_flags: set[tuple[str, str]] = set()
    for violation in getattr(scanner_result, "violations", []) or []:
        category = getattr(violation, "category", None)
        vfile = getattr(violation, "file", "") or ""
        base = str(vfile).replace("\\", "/").rsplit("/", 1)[-1]
        if category:
            scanner_flags.add((base, category))

    divergences: list[str] = []
    for rel, verdict in sorted(files.items()):
        base = rel.replace("\\", "/").rsplit("/", 1)[-1]
        scan_verdict = (verdict or {}).get("scan", {}) or {}
        for category in SCANNER_CATEGORIES:
            baseline_state = scan_verdict.get(category)
            if baseline_state is None:
                continue
            scanner_flagged = (base, category) in scanner_flags
            if baseline_state == "clean" and scanner_flagged:
                divergences.append(
                    f"DIVERGENCE: ee2_scanner flags [{category}] on '{rel}', "
                    f"but the RAG baseline records it clean — reconcile the "
                    f"scanner to drop this false positive (Req 10.4)."
                )
            elif baseline_state == "issue" and not scanner_flagged:
                divergences.append(
                    f"DIVERGENCE: RAG baseline records an issue [{category}] on "
                    f"'{rel}', but ee2_scanner reports none — the offline "
                    f"scanner misses an authoritative finding (Req 10.4)."
                )

    return divergences


def _default_repo_root() -> Path:
    """Repository root inferred from this file: deployment -> workflow -> dev -> root."""
    return Path(__file__).resolve().parents[3]
