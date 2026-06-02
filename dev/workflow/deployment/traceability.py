"""Traceability_Matrix loading and completion-claim reconciliation (Req 8).

This module backs the reconciliation half of the Goal_Realization_Gate
(Design Component 7). It is intentionally free of any live-RAG or network
dependency so it can run offline in CI and be unit-tested in isolation; the
gate (task 9) imports the public functions here:

* :func:`load_traceability_matrix` — parse ``traceability_matrix.yaml`` into a
  structured :class:`TraceabilityMatrix` (Req 8.1, 8.2).
* :func:`find_unmapped_parent_items` — emit a verification error for any parent
  requirement (R1-R14) or Property (1-14) that has no proving test, proceeding
  regardless (Req 8.4).
* :func:`reconcile_completed_tasks` — for each parent ``tasks.md`` task marked
  complete, assert at least one mapped proving test passes, recording mismatches
  (Req 8.5, 8.6). Mismatches are reported, never fatal to the overall gate.

The matrix maps every parent requirement and Property to its proving test(s)
plus a ``status`` (``pending`` | ``pass`` | ``fail`` | ``unmapped``). Parent task
completion claims are read from the parent spec's ``tasks.md``; each completed
task is reconciled to the matrix via the parent requirement numbers it cites
(``_Requirements: 3.8_`` / ``**Validates: Requirements 4.6**``) and any
``**Property N: ...**`` markers it declares.

Traces to: design Component 7; parent spec immutable-dag-workflow-modernization
(all parent requirements and Properties 1-14).
"""

from __future__ import annotations

import re
from dataclasses import dataclass, field
from pathlib import Path
from typing import Mapping, Optional

import yaml

# ---------------------------------------------------------------------------
# Status vocabulary (Design "Data Models": status in {pending,pass,fail,unmapped})
# ---------------------------------------------------------------------------

STATUS_PENDING = "pending"
STATUS_PASS = "pass"
STATUS_FAIL = "fail"
STATUS_UNMAPPED = "unmapped"

VALID_STATUSES = frozenset(
    {STATUS_PENDING, STATUS_PASS, STATUS_FAIL, STATUS_UNMAPPED}
)

#: Parent spec defines 14 requirements (R1-R14) and 14 correctness Properties.
PARENT_REQUIREMENT_KEYS = tuple(f"R{n}" for n in range(1, 15))
PARENT_PROPERTY_NUMBERS = tuple(range(1, 15))

# ---------------------------------------------------------------------------
# Default artifact locations (resolved relative to this module)
# deployment/ -> workflow/ -> dev/ -> <repo root>
# ---------------------------------------------------------------------------

_WORKFLOW_DIR = Path(__file__).resolve().parents[1]
_REPO_ROOT = Path(__file__).resolve().parents[3]

#: Default Traceability_Matrix path (``dev/workflow/traceability_matrix.yaml``).
DEFAULT_MATRIX_PATH = _WORKFLOW_DIR / "traceability_matrix.yaml"

#: Default parent ``tasks.md`` whose completion claims are reconciled.
DEFAULT_PARENT_TASKS_PATH = (
    _REPO_ROOT
    / ".kiro"
    / "specs"
    / "immutable-dag-workflow-modernization"
    / "tasks.md"
)


# ---------------------------------------------------------------------------
# Data models
# ---------------------------------------------------------------------------


@dataclass
class MatrixItem:
    """A single parent requirement or Property entry in the matrix.

    Attributes:
        key: Item key — ``"R1".."R14"`` for requirements, ``"1".."14"`` for
            Properties (string form for uniform reporting).
        label: Human-readable requirement title or Property name.
        tests: Proving-test identifiers (pytest paths relative to
            ``dev/workflow/``; ``file.py::node`` pins a single node).
        status: One of :data:`VALID_STATUSES`.
    """

    key: str
    label: str
    tests: list[str] = field(default_factory=list)
    status: str = STATUS_PENDING

    @property
    def is_mapped(self) -> bool:
        """True iff at least one proving test is recorded for this item."""
        return bool(self.tests)


@dataclass
class TraceabilityMatrix:
    """Parsed Traceability_Matrix (Req 8.1).

    Attributes:
        properties: Property number (1-14) -> :class:`MatrixItem`.
        requirements: Requirement key (``"R1".."R14"``) -> :class:`MatrixItem`.
        source_path: Path the matrix was loaded from (for messages).
    """

    properties: dict[int, MatrixItem] = field(default_factory=dict)
    requirements: dict[str, MatrixItem] = field(default_factory=dict)
    source_path: Optional[Path] = None

    def property_item(self, number: int) -> Optional[MatrixItem]:
        """Return the Property item for ``number`` or None if absent."""
        return self.properties.get(number)

    def requirement_item(self, key: str) -> Optional[MatrixItem]:
        """Return the requirement item for ``key`` (e.g. ``"R3"``) or None."""
        return self.requirements.get(key)

    def all_tests(self) -> set[str]:
        """Return the union of every proving test referenced by the matrix."""
        tests: set[str] = set()
        for item in self.properties.values():
            tests.update(item.tests)
        for item in self.requirements.values():
            tests.update(item.tests)
        return tests


@dataclass
class ParentTask:
    """A parsed parent ``tasks.md`` task entry.

    Attributes:
        task_id: Dotted task number (e.g. ``"8"`` or ``"8.1"``).
        title: Task title text (first line after the checkbox).
        completed: True iff the checkbox mark is ``x`` / ``X``.
        requirement_numbers: Parent requirement *major* numbers cited by the
            task (e.g. ``{3, 8}`` from ``_Requirements: 3.8, 8.6_``), mapped to
            ``R#`` keys for matrix lookup.
        property_numbers: Property numbers declared via ``**Property N: ...**``.
    """

    task_id: str
    title: str
    completed: bool
    requirement_numbers: set[int] = field(default_factory=set)
    property_numbers: set[int] = field(default_factory=set)

    def mapped_item_keys(self) -> tuple[set[str], set[int]]:
        """Return ``(requirement_keys, property_numbers)`` for matrix lookup."""
        req_keys = {f"R{n}" for n in self.requirement_numbers}
        return req_keys, set(self.property_numbers)

    @property
    def has_mapping(self) -> bool:
        """True iff the task cites any parent requirement or Property."""
        return bool(self.requirement_numbers or self.property_numbers)


# ---------------------------------------------------------------------------
# Matrix loading (Req 8.1, 8.2)
# ---------------------------------------------------------------------------


class TraceabilityMatrixError(ValueError):
    """Raised when the Traceability_Matrix YAML is missing or malformed."""


def _coerce_tests(raw) -> list[str]:
    """Normalize a YAML ``tests`` value into a list of stripped strings."""
    if raw is None:
        return []
    if isinstance(raw, str):
        return [raw.strip()] if raw.strip() else []
    if isinstance(raw, (list, tuple)):
        return [str(t).strip() for t in raw if str(t).strip()]
    raise TraceabilityMatrixError(
        f"'tests' must be a string or list, got {type(raw).__name__}"
    )


def _coerce_status(raw) -> str:
    """Validate and normalize a status value, defaulting to ``pending``."""
    if raw is None:
        return STATUS_PENDING
    status = str(raw).strip().lower()
    if status not in VALID_STATUSES:
        raise TraceabilityMatrixError(
            f"invalid status {raw!r}; expected one of {sorted(VALID_STATUSES)}"
        )
    return status


def load_traceability_matrix(
    path: Path | str = DEFAULT_MATRIX_PATH,
) -> TraceabilityMatrix:
    """Load and parse the Traceability_Matrix YAML (Req 8.1, 8.2).

    Args:
        path: Path to ``traceability_matrix.yaml``. Defaults to
            :data:`DEFAULT_MATRIX_PATH`.

    Returns:
        A :class:`TraceabilityMatrix` with parsed ``properties`` and
        ``requirements`` items.

    Raises:
        TraceabilityMatrixError: if the file is missing, unreadable, not a
            mapping, or an entry is malformed.
    """
    path = Path(path)
    if not path.is_file():
        raise TraceabilityMatrixError(f"Traceability_Matrix not found: {path}")

    try:
        data = yaml.safe_load(path.read_text(encoding="utf-8"))
    except yaml.YAMLError as exc:  # pragma: no cover - defensive
        raise TraceabilityMatrixError(f"cannot parse {path}: {exc}") from exc

    if not isinstance(data, Mapping):
        raise TraceabilityMatrixError(
            f"{path}: top-level document must be a mapping"
        )

    matrix = TraceabilityMatrix(source_path=path)

    # Properties: keys 1..N
    props = data.get("properties") or {}
    if not isinstance(props, Mapping):
        raise TraceabilityMatrixError(f"{path}: 'properties' must be a mapping")
    for raw_num, entry in props.items():
        try:
            number = int(raw_num)
        except (TypeError, ValueError) as exc:
            raise TraceabilityMatrixError(
                f"{path}: property key {raw_num!r} is not an integer"
            ) from exc
        entry = entry or {}
        if not isinstance(entry, Mapping):
            raise TraceabilityMatrixError(
                f"{path}: property {number} entry must be a mapping"
            )
        matrix.properties[number] = MatrixItem(
            key=str(number),
            label=str(entry.get("name", f"Property {number}")),
            tests=_coerce_tests(entry.get("tests")),
            status=_coerce_status(entry.get("status")),
        )

    # Requirements: keys R1..RN
    reqs = data.get("requirements") or {}
    if not isinstance(reqs, Mapping):
        raise TraceabilityMatrixError(f"{path}: 'requirements' must be a mapping")
    for raw_key, entry in reqs.items():
        key = str(raw_key).strip()
        entry = entry or {}
        if not isinstance(entry, Mapping):
            raise TraceabilityMatrixError(
                f"{path}: requirement {key} entry must be a mapping"
            )
        matrix.requirements[key] = MatrixItem(
            key=key,
            label=str(entry.get("title", key)),
            tests=_coerce_tests(entry.get("tests")),
            status=_coerce_status(entry.get("status")),
        )

    return matrix


# ---------------------------------------------------------------------------
# Unmapped-item detection (Req 8.4)
# ---------------------------------------------------------------------------


def find_unmapped_parent_items(
    matrix: TraceabilityMatrix,
    *,
    requirement_keys: tuple[str, ...] = PARENT_REQUIREMENT_KEYS,
    property_numbers: tuple[int, ...] = PARENT_PROPERTY_NUMBERS,
) -> list[str]:
    """Return verification errors for parent items lacking a proving test.

    Implements Req 8.4: if a parent requirement or Property has no proving test
    in the Traceability_Matrix, the reconciliation check emits a verification
    error identifying the unmapped parent item. Detection is what matters —
    callers proceed regardless (the gate does not abort on these).

    A parent item is *unmapped* when it is absent from the matrix entirely, when
    its ``tests`` list is empty, or when its status is explicitly ``unmapped``.

    Args:
        matrix: The loaded :class:`TraceabilityMatrix`.
        requirement_keys: Expected parent requirement keys (default R1-R14).
        property_numbers: Expected parent Property numbers (default 1-14).

    Returns:
        A list of human-readable verification-error messages (empty == every
        expected parent item is mapped).
    """
    errors: list[str] = []

    for number in property_numbers:
        item = matrix.property_item(number)
        if item is None:
            errors.append(
                f"VERIFICATION ERROR: parent Property {number} is not present "
                f"in the Traceability_Matrix (no proving test mapped)."
            )
        elif not item.is_mapped or item.status == STATUS_UNMAPPED:
            errors.append(
                f"VERIFICATION ERROR: parent Property {number} "
                f"({item.label!r}) has no proving test in the "
                f"Traceability_Matrix."
            )

    for key in requirement_keys:
        item = matrix.requirement_item(key)
        if item is None:
            errors.append(
                f"VERIFICATION ERROR: parent requirement {key} is not present "
                f"in the Traceability_Matrix (no proving test mapped)."
            )
        elif not item.is_mapped or item.status == STATUS_UNMAPPED:
            errors.append(
                f"VERIFICATION ERROR: parent requirement {key} "
                f"({item.label!r}) has no proving test in the "
                f"Traceability_Matrix."
            )

    return errors


# ---------------------------------------------------------------------------
# Parent tasks.md parsing
# ---------------------------------------------------------------------------

#: Markdown checkbox line, e.g. ``- [x] 8.1 Add the version-gate unit test``.
_CHECKBOX_RE = re.compile(
    r"^\s*-\s*\[(?P<mark>[ xX~\-])\]\s*(?P<rest>.*)$"
)

#: Leading dotted task id + title, e.g. ``8.1 Add the ...`` / ``8. Build ...``.
_TASK_ID_RE = re.compile(r"^(?P<id>\d+(?:\.\d+)*)\.?\s+(?P<title>.*)$")

#: ``_Requirements: 3.8, 8.6_`` reference line.
_REQ_LINE_RE = re.compile(r"_Requirements?:\s*(?P<refs>[^_]+)_")

#: ``**Validates: Requirements 4.6**`` reference (property-test tasks).
_VALIDATES_RE = re.compile(
    r"\*\*Validates:\s*Requirements?\s*(?P<refs>[^*]+)\*\*", re.IGNORECASE
)

#: ``**Property 9: Parser Round-Trip**`` declaration.
_PROPERTY_RE = re.compile(r"\*\*Property\s+(?P<num>\d+)", re.IGNORECASE)

#: A single requirement reference token like ``3.8`` or ``10`` — capture major.
_REQ_REF_RE = re.compile(r"(?P<major>\d+)(?:\.\d+)*")


def _extract_requirement_majors(refs_text: str) -> set[int]:
    """Return the set of major requirement numbers from a refs fragment.

    ``"3.8, 8.6, 10.4"`` -> ``{3, 8, 10}``.
    """
    majors: set[int] = set()
    for token in re.split(r"[,\s]+", refs_text.strip()):
        if not token:
            continue
        match = _REQ_REF_RE.match(token)
        if match:
            majors.add(int(match.group("major")))
    return majors


def parse_parent_tasks(
    tasks_path: Path | str = DEFAULT_PARENT_TASKS_PATH,
) -> list[ParentTask]:
    """Parse the parent ``tasks.md`` into a list of :class:`ParentTask`.

    Each checkbox line begins a task; the lines that follow (until the next
    checkbox) are the task's body, from which requirement and Property
    references are harvested.

    Args:
        tasks_path: Path to the parent spec ``tasks.md``.

    Returns:
        Every parsed task (completed and not).

    Raises:
        TraceabilityMatrixError: if the file cannot be read.
    """
    tasks_path = Path(tasks_path)
    try:
        text = tasks_path.read_text(encoding="utf-8")
    except OSError as exc:
        raise TraceabilityMatrixError(
            f"cannot read parent tasks file {tasks_path}: {exc}"
        ) from exc

    tasks: list[ParentTask] = []
    current: Optional[ParentTask] = None

    def _harvest(task: ParentTask, line: str) -> None:
        """Pull requirement/Property references out of a body line."""
        for match in _REQ_LINE_RE.finditer(line):
            task.requirement_numbers |= _extract_requirement_majors(
                match.group("refs")
            )
        for match in _VALIDATES_RE.finditer(line):
            task.requirement_numbers |= _extract_requirement_majors(
                match.group("refs")
            )
        for match in _PROPERTY_RE.finditer(line):
            task.property_numbers.add(int(match.group("num")))

    for line in text.splitlines():
        checkbox = _CHECKBOX_RE.match(line)
        if checkbox:
            rest = checkbox.group("rest").strip()
            id_match = _TASK_ID_RE.match(rest)
            if id_match:
                task_id = id_match.group("id")
                title = id_match.group("title").strip()
            else:
                # A checkbox without a dotted id (rare); use the text as title.
                task_id = ""
                title = rest
            current = ParentTask(
                task_id=task_id,
                title=title,
                completed=checkbox.group("mark") in ("x", "X"),
            )
            tasks.append(current)
            # The checkbox line itself can carry inline references.
            _harvest(current, rest)
        elif current is not None:
            _harvest(current, line)

    return tasks


# ---------------------------------------------------------------------------
# Completion-claim reconciliation (Req 8.5, 8.6)
# ---------------------------------------------------------------------------


def _test_passed(
    test_id: str,
    item_status: str,
    test_results: Optional[Mapping[str, bool]],
) -> bool:
    """Decide whether a single proving test counts as passing.

    Two modes:
      * ``test_results`` given (the gate path): look the test up by exact id,
        then by file-part (``file.py`` from ``file.py::node``), then by any
        result key sharing the same file-part. Unknown tests count as failing.
      * ``test_results`` is None (offline/static path): the test inherits its
        matrix item's status, so it passes iff ``item_status == 'pass'``.
    """
    if test_results is None:
        return item_status == STATUS_PASS

    if test_id in test_results:
        return bool(test_results[test_id])

    file_part = test_id.split("::", 1)[0]
    if file_part in test_results:
        return bool(test_results[file_part])

    # Fall back to matching any recorded node within the same file.
    for key, passed in test_results.items():
        if key.split("::", 1)[0] == file_part:
            if passed:
                return True
    return False


def reconcile_completed_tasks(
    matrix: TraceabilityMatrix,
    parent_tasks_path: Path | str = DEFAULT_PARENT_TASKS_PATH,
    test_results: Optional[Mapping[str, bool]] = None,
) -> list[str]:
    """Reconcile parent completion claims against proving-test pass status.

    Implements Req 8.5 / 8.6: every parent ``tasks.md`` task marked complete
    must be backed by at least one passing proving test recorded in the
    Traceability_Matrix. A completed task whose mapped proving tests are not
    passing yields a recorded *mismatch* message identifying the task and its
    non-passing test(s). Mismatches are non-fatal to the overall gate (the gate
    records them but ``GateResult.realized`` is governed elsewhere).

    Tasks that cite no parent requirement or Property (e.g. checkpoint tasks)
    are not reconcilable to a specific proving test and are skipped.

    Args:
        matrix: The loaded :class:`TraceabilityMatrix`.
        parent_tasks_path: Path to the parent spec ``tasks.md``.
        test_results: Optional mapping of proving-test id -> passed. When
            supplied (the gate path), pass/fail is taken from these results;
            when omitted, pass/fail is derived from the matrix item statuses.

    Returns:
        A list of mismatch messages (empty == every completed, mapped parent
        task is backed by a passing proving test).
    """
    mismatches: list[str] = []
    tasks = parse_parent_tasks(parent_tasks_path)

    for task in tasks:
        if not task.completed or not task.has_mapping:
            continue

        req_keys, prop_numbers = task.mapped_item_keys()

        # Collect (test_id, passed) across every mapped matrix item.
        evidence: list[tuple[str, bool]] = []
        for key in sorted(req_keys):
            item = matrix.requirement_item(key)
            if item is None:
                continue
            for test_id in item.tests:
                evidence.append(
                    (test_id, _test_passed(test_id, item.status, test_results))
                )
        for number in sorted(prop_numbers):
            item = matrix.property_item(number)
            if item is None:
                continue
            for test_id in item.tests:
                evidence.append(
                    (test_id, _test_passed(test_id, item.status, test_results))
                )

        label = f"task {task.task_id} ({task.title!r})" if task.task_id else (
            f"task {task.title!r}"
        )

        if not evidence:
            # The task cites parent items, but those items carry no proving
            # test in the matrix — it cannot be backed (relates to Req 8.4).
            mismatches.append(
                f"RECONCILIATION MISMATCH: completed parent {label} cites "
                f"parent items {sorted(req_keys) + sorted(prop_numbers)} that "
                f"have no proving test in the Traceability_Matrix."
            )
            continue

        if not any(passed for _, passed in evidence):
            non_passing = sorted({test_id for test_id, passed in evidence if not passed})
            mismatches.append(
                f"RECONCILIATION MISMATCH: completed parent {label} has no "
                f"passing proving test; non-passing test(s): {non_passing}"
            )

    return mismatches
