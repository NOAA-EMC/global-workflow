"""Rocoto decommission static guard check (Design Component 4, Req 4.4).

The Rocoto orchestration engine has been decommissioned in favour of an
ecFlow-only policy (parent Req 1.4 / Req 14.3). ``dev/workflow/setup_workflow.py``
must therefore contain **no** reachable Rocoto code path: the ``rocoto``
subparser, ``rocoto_xml_factory``, and every rocoto-conditioned branch are
removed. A small *deprecation guard* is deliberately retained so that any
attempt to invoke a Rocoto path fails fast with a FATAL ERROR referencing the
ecFlow-only policy (Req 4.3, 4.5):

* :class:`setup_workflow.RocotoDecommissionedError`
* :func:`setup_workflow.rocoto_deprecation_guard`
* :func:`setup_workflow._check_for_rocoto_invocation`

This module implements the *structural* static scan required by Req 4.4. It does
**not** merely count occurrences of the term ``rocoto``. Instead it passes only
when every residual case-insensitive ``rocoto`` occurrence belongs to the
documented deprecation-guard structure, and it fails when a single lone
occurrence sits outside that structure.

Structural-guard recognition
-----------------------------
An occurrence is recognised as part of the documented guard when **any** of the
following hold:

1. It lies within the source span of a guard symbol's definition — a ``class``
   or ``def`` whose name is in :data:`GUARD_ALLOWLIST_SYMBOLS` — including that
   definition's docstring and body (this is the "guard cluster": the class /
   function name together with its FATAL-ERROR message).
2. It lies within the span of a *call* to one of the guard symbols (or the
   short comment block immediately preceding that call), i.e. the wiring that
   makes the guard reachable.
3. The line directly references one of the :data:`GUARD_ALLOWLIST_SYMBOLS`.
4. The line matches a :data:`GUARD_DOC_PATTERNS` documented allowlist pattern —
   a decommission/deprecation notice (e.g. the module docstring).
5. The line is a *comment* that documents a guard symbol referenced within the
   next :data:`_COMMENT_ANCHOR_WINDOW` lines (e.g. an explanatory
   ``# Check for Rocoto invocation ...`` comment sitting just above the
   ``_check_for_rocoto_invocation(...)`` call wiring).

Any case-insensitive ``rocoto`` occurrence not covered by one of the above is a
lone, non-guard occurrence and is reported as a violation. Because allowances
3-5 either require a guard-symbol reference, match a decommission notice, or
apply only to comment lines, a lone *reachable* Rocoto code path cannot be
masked.
"""

from __future__ import annotations

import ast
import re
from pathlib import Path

#: The guard symbols permitted to retain the term "rocoto" in
#: ``setup_workflow.py``. These collectively form the documented deprecation
#: guard (Design Component 4).
GUARD_ALLOWLIST_SYMBOLS = {
    "RocotoDecommissionedError",
    "rocoto_deprecation_guard",
    "_check_for_rocoto_invocation",
}

#: Documented allowlist patterns for decommission/deprecation *prose* (module
#: and class docstrings, the FATAL-ERROR message) that legitimately name Rocoto
#: while explaining that it is gone. These are intentionally specific to a
#: decommission notice so they cannot mask a stray reachable reference.
GUARD_DOC_PATTERNS = (
    re.compile(r"rocoto\s+has\s+been\s+decommissioned", re.IGNORECASE),
    re.compile(r"rocoto\s+is\s+decommissioned", re.IGNORECASE),
    re.compile(r"decommissioned\s+rocoto", re.IGNORECASE),
)

#: Case-insensitive matcher for the bare term.
_ROCOTO_PATTERN = re.compile(r"rocoto", re.IGNORECASE)

#: How many lines below a documenting comment to look for a guard-symbol
#: reference. A short window keeps recognition tight: a comment is only treated
#: as guard documentation when a guard symbol is wired up immediately below it.
_COMMENT_ANCHOR_WINDOW = 4


def _call_symbol_name(node: ast.Call) -> str | None:
    """Return the called symbol's bare name for a :class:`ast.Call`.

    Handles both ``name(...)`` and ``obj.name(...)`` call shapes.
    """
    func = node.func
    if isinstance(func, ast.Name):
        return func.id
    if isinstance(func, ast.Attribute):
        return func.attr
    return None


def _preceding_comment_lines(lines: list[str], call_lineno: int) -> set[int]:
    """Return 1-indexed line numbers of the comment block above ``call_lineno``.

    Walks upward from the line before the call, collecting consecutive comment
    or blank lines, so an explanatory ``# Check for Rocoto invocation ...``
    comment that documents the guard call is treated as part of the guard.
    """
    covered: set[int] = set()
    idx = call_lineno - 1  # line above the call (1-indexed)
    while idx >= 1:
        stripped = lines[idx - 1].strip()
        if stripped.startswith("#"):
            covered.add(idx)
            idx -= 1
        else:
            break
    return covered


def _build_guard_line_coverage(tree: ast.AST, lines: list[str]) -> set[int]:
    """Compute the set of 1-indexed lines belonging to guard structures.

    Covers guard symbol *definitions* (class/function, including decorators and
    bodies) and *calls* to guard symbols (plus their preceding comment block).
    """
    covered: set[int] = set()

    for node in ast.walk(tree):
        # Guard symbol definitions: class RocotoDecommissionedError, def
        # rocoto_deprecation_guard, def _check_for_rocoto_invocation.
        if isinstance(node, (ast.FunctionDef, ast.AsyncFunctionDef, ast.ClassDef)):
            if node.name in GUARD_ALLOWLIST_SYMBOLS:
                start = node.lineno
                for dec in getattr(node, "decorator_list", []):
                    start = min(start, dec.lineno)
                end = getattr(node, "end_lineno", node.lineno)
                covered.update(range(start, end + 1))

        # Calls to a guard symbol (the wiring that makes the guard reachable).
        elif isinstance(node, ast.Call):
            name = _call_symbol_name(node)
            if name in GUARD_ALLOWLIST_SYMBOLS:
                start = node.lineno
                end = getattr(node, "end_lineno", node.lineno)
                covered.update(range(start, end + 1))
                covered |= _preceding_comment_lines(lines, start)

    return covered


def _line_references_guard_symbol(line: str) -> bool:
    """True if the line names any guard symbol from :data:`GUARD_ALLOWLIST_SYMBOLS`."""
    return any(symbol in line for symbol in GUARD_ALLOWLIST_SYMBOLS)


def _line_matches_doc_pattern(line: str) -> bool:
    """True if the line matches a documented decommission/deprecation pattern."""
    return any(pattern.search(line) for pattern in GUARD_DOC_PATTERNS)


def _is_guard_documenting_comment(lines: list[str], lineno: int) -> bool:
    """True if a comment line documents a guard symbol wired up just below it.

    A ``rocoto``-mentioning comment is recognised as guard documentation only
    when (a) the line is itself a comment and (b) one of the
    :data:`GUARD_ALLOWLIST_SYMBOLS` is referenced within the next
    :data:`_COMMENT_ANCHOR_WINDOW` lines. This lets an explanatory comment that
    sits directly above the guard-call wiring count as part of the guard, while
    keeping reachable, non-comment code strict.
    """
    line = lines[lineno - 1]
    if not line.lstrip().startswith("#"):
        return False
    end = min(len(lines), lineno + _COMMENT_ANCHOR_WINDOW)
    for idx in range(lineno, end):  # lines below the comment (1-indexed slice)
        if _line_references_guard_symbol(lines[idx]):
            return True
    return False


def check_setup_workflow_rocoto_free(path: Path) -> list[str]:
    """Statically verify ``setup_workflow.py`` is free of non-guard Rocoto refs.

    Implements the structural scan of Req 4.4: the scan passes only when every
    residual case-insensitive ``rocoto`` occurrence belongs to the documented
    deprecation-guard structure (see the module docstring), and fails on a lone
    occurrence outside the guard.

    Args:
        path: Path to ``dev/workflow/setup_workflow.py`` (or any module to
            check).

    Returns:
        A list of human-readable violation messages. An empty list means the
        file is clean: it contains no Rocoto references at all, or every
        reference is part of the documented guard structure.
    """
    path = Path(path)
    violations: list[str] = []

    try:
        source = path.read_text(encoding="utf-8")
    except (OSError, IOError) as exc:
        return [f"FATAL ERROR: cannot read {path}: {exc}"]

    lines = source.splitlines()

    try:
        tree = ast.parse(source, filename=str(path))
    except SyntaxError as exc:
        return [
            f"FATAL ERROR: cannot parse {path} for rocoto-guard analysis: {exc}"
        ]

    guard_lines = _build_guard_line_coverage(tree, lines)

    for lineno, line in enumerate(lines, start=1):
        if not _ROCOTO_PATTERN.search(line):
            continue
        if lineno in guard_lines:
            continue
        if _line_references_guard_symbol(line):
            continue
        if _line_matches_doc_pattern(line):
            continue
        if _is_guard_documenting_comment(lines, lineno):
            continue
        violations.append(
            f"FATAL ERROR: lone 'rocoto' reference outside the documented "
            f"deprecation guard at {path.name}:{lineno}: {line.strip()!r} "
            f"(ecFlow-only policy: Rocoto is decommissioned per Req 1.4 / 14.3)"
        )

    return violations
