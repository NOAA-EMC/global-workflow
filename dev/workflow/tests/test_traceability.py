"""Unit tests for the Traceability_Matrix loader and reconciliation (Req 8).

Covers Design Component 7 (Traceability_Matrix + reconciliation), exercising
the functions the Goal_Realization_Gate (task 9) consumes:

- ``load_traceability_matrix`` parses properties/requirements + status (Req 8.1, 8.2);
- ``find_unmapped_parent_items`` emits a verification error for any unmapped
  parent requirement (R1-R14) or Property (1-14) and proceeds (Req 8.4);
- ``parse_parent_tasks`` harvests completion marks + requirement/Property refs;
- ``reconcile_completed_tasks`` records a mismatch when a completed parent task
  has no passing proving test, and is non-fatal (Req 8.5, 8.6).

Also asserts the committed ``traceability_matrix.yaml`` maps every parent
requirement and Property (Req 8.1) so the gate starts from a fully-mapped state.

**Validates: Requirements 8.1, 8.2, 8.3, 8.4, 8.5, 8.6**

Traces to: Design Document - Component 7 (Traceability_Matrix, reconciliation);
parent spec immutable-dag-workflow-modernization (R1-R14, Properties 1-14).
"""

from __future__ import annotations

import os
import sys
from pathlib import Path

import pytest
import yaml

sys.path.insert(0, os.path.join(os.path.dirname(__file__), ".."))

from deployment.traceability import (  # noqa: E402
    DEFAULT_MATRIX_PATH,
    PARENT_PROPERTY_NUMBERS,
    PARENT_REQUIREMENT_KEYS,
    STATUS_PASS,
    STATUS_PENDING,
    TraceabilityMatrix,
    TraceabilityMatrixError,
    find_unmapped_parent_items,
    load_traceability_matrix,
    parse_parent_tasks,
    reconcile_completed_tasks,
)

# Repo root: tests -> workflow -> dev -> <repo root>
REPO_ROOT = Path(__file__).resolve().parents[3]


# ---------------------------------------------------------------------------
# Helpers
# ---------------------------------------------------------------------------


def _write(path: Path, text: str) -> Path:
    """Write text to ``path`` creating parent dirs; return the path."""
    path.parent.mkdir(parents=True, exist_ok=True)
    path.write_text(text, encoding="utf-8")
    return path


def _full_matrix_yaml(
    *,
    prop_status: str = STATUS_PENDING,
    req_status: str = STATUS_PENDING,
) -> str:
    """Build a complete matrix (Properties 1-14, R1-R14) with one test each."""
    props = {
        n: {
            "name": f"Property {n}",
            "tests": [f"tests/test_property_{n}.py"],
            "status": prop_status,
        }
        for n in PARENT_PROPERTY_NUMBERS
    }
    reqs = {
        key: {
            "title": f"Requirement {key}",
            "tests": [f"tests/test_req_{key.lower()}.py"],
            "status": req_status,
        }
        for key in PARENT_REQUIREMENT_KEYS
    }
    return yaml.dump({"properties": props, "requirements": reqs}, sort_keys=False)


# ---------------------------------------------------------------------------
# load_traceability_matrix (Req 8.1, 8.2)
# ---------------------------------------------------------------------------


class TestLoadTraceabilityMatrix:
    """Loading and validation of the Traceability_Matrix YAML."""

    def test_loads_properties_and_requirements(self, tmp_path):
        """A well-formed matrix parses into property/requirement items."""
        path = _write(tmp_path / "matrix.yaml", _full_matrix_yaml())
        matrix = load_traceability_matrix(path)

        assert set(matrix.properties) == set(PARENT_PROPERTY_NUMBERS)
        assert set(matrix.requirements) == set(PARENT_REQUIREMENT_KEYS)
        assert matrix.property_item(1).tests == ["tests/test_property_1.py"]
        assert matrix.requirement_item("R1").tests == ["tests/test_req_r1.py"]
        assert matrix.property_item(1).status == STATUS_PENDING

    def test_status_defaults_to_pending(self, tmp_path):
        """An entry with no status defaults to 'pending'."""
        path = _write(
            tmp_path / "m.yaml",
            yaml.dump(
                {
                    "properties": {1: {"name": "P1", "tests": ["t.py"]}},
                    "requirements": {"R1": {"title": "R1", "tests": ["t.py"]}},
                }
            ),
        )
        matrix = load_traceability_matrix(path)
        assert matrix.property_item(1).status == STATUS_PENDING
        assert matrix.requirement_item("R1").status == STATUS_PENDING

    def test_missing_file_raises(self, tmp_path):
        """A missing matrix file raises a descriptive error."""
        with pytest.raises(TraceabilityMatrixError, match="not found"):
            load_traceability_matrix(tmp_path / "nope.yaml")

    def test_invalid_status_raises(self, tmp_path):
        """An unknown status value is rejected."""
        path = _write(
            tmp_path / "m.yaml",
            yaml.dump(
                {"properties": {1: {"tests": ["t.py"], "status": "green"}}}
            ),
        )
        with pytest.raises(TraceabilityMatrixError, match="invalid status"):
            load_traceability_matrix(path)

    def test_single_test_string_is_coerced_to_list(self, tmp_path):
        """A scalar 'tests' value is normalized into a one-element list."""
        path = _write(
            tmp_path / "m.yaml",
            yaml.dump({"properties": {1: {"tests": "tests/only.py"}}}),
        )
        matrix = load_traceability_matrix(path)
        assert matrix.property_item(1).tests == ["tests/only.py"]


# ---------------------------------------------------------------------------
# find_unmapped_parent_items (Req 8.4)
# ---------------------------------------------------------------------------


class TestFindUnmappedParentItems:
    """Unmapped-item detection emits verification errors but does not abort."""

    def test_fully_mapped_matrix_has_no_errors(self, tmp_path):
        """A complete matrix yields zero unmapped-item errors."""
        path = _write(tmp_path / "m.yaml", _full_matrix_yaml())
        matrix = load_traceability_matrix(path)
        assert find_unmapped_parent_items(matrix) == []

    def test_missing_property_is_reported(self):
        """A parent Property absent from the matrix is reported."""
        matrix = TraceabilityMatrix()  # empty
        errors = find_unmapped_parent_items(
            matrix, requirement_keys=(), property_numbers=(7,)
        )
        assert len(errors) == 1
        assert "Property 7" in errors[0]

    def test_property_with_empty_tests_is_reported(self, tmp_path):
        """A Property present but with no tests counts as unmapped."""
        path = _write(
            tmp_path / "m.yaml",
            yaml.dump({"properties": {3: {"name": "P3", "tests": []}}}),
        )
        matrix = load_traceability_matrix(path)
        errors = find_unmapped_parent_items(
            matrix, requirement_keys=(), property_numbers=(3,)
        )
        assert len(errors) == 1
        assert "Property 3" in errors[0]

    def test_missing_requirement_is_reported(self):
        """A parent requirement absent from the matrix is reported."""
        matrix = TraceabilityMatrix()
        errors = find_unmapped_parent_items(
            matrix, requirement_keys=("R9",), property_numbers=()
        )
        assert len(errors) == 1
        assert "R9" in errors[0]


# ---------------------------------------------------------------------------
# parse_parent_tasks
# ---------------------------------------------------------------------------


SAMPLE_TASKS_MD = """\
# Implementation Plan

- [x] 1. Implement determinism
  - Deploy twice and compare manifests.
  - _Requirements: 3.8, 9.4_

- [x] 1.1 Write property test (Property 1)
  - **Property 1: Deployment Determinism**
  - **Validates: Requirements 3.8**

- [ ] 2. Not done yet
  - _Requirements: 7.6_

- [x] 3. Checkpoint - ensure all tests pass
  - No requirement references here.
"""


class TestParseParentTasks:
    """Parsing completion marks and requirement/Property references."""

    def test_parses_marks_and_refs(self, tmp_path):
        """Completed/incomplete marks and references are harvested."""
        path = _write(tmp_path / "tasks.md", SAMPLE_TASKS_MD)
        tasks = parse_parent_tasks(path)

        by_id = {t.task_id: t for t in tasks}
        assert by_id["1"].completed is True
        assert by_id["1"].requirement_numbers == {3, 9}
        assert by_id["1.1"].completed is True
        assert by_id["1.1"].property_numbers == {1}
        assert by_id["1.1"].requirement_numbers == {3}
        assert by_id["2"].completed is False
        assert by_id["2"].requirement_numbers == {7}
        # The checkpoint task cites nothing -> not reconcilable.
        assert by_id["3"].has_mapping is False

    def test_missing_tasks_file_raises(self, tmp_path):
        """A missing parent tasks file raises a descriptive error."""
        with pytest.raises(TraceabilityMatrixError, match="cannot read"):
            parse_parent_tasks(tmp_path / "absent.md")


# ---------------------------------------------------------------------------
# reconcile_completed_tasks (Req 8.5, 8.6)
# ---------------------------------------------------------------------------


class TestReconcileCompletedTasks:
    """Completion-claim reconciliation against proving-test pass status."""

    def _matrix(self, tmp_path) -> TraceabilityMatrix:
        path = _write(tmp_path / "m.yaml", _full_matrix_yaml())
        return load_traceability_matrix(path)

    def _tasks(self, tmp_path) -> Path:
        return _write(tmp_path / "tasks.md", SAMPLE_TASKS_MD)

    def test_all_passing_results_yield_no_mismatch(self, tmp_path):
        """When every mapped proving test passes, no mismatch is recorded."""
        matrix = self._matrix(tmp_path)
        tasks = self._tasks(tmp_path)
        results = {t: True for t in matrix.all_tests()}
        assert reconcile_completed_tasks(matrix, tasks, test_results=results) == []

    def test_failing_results_record_mismatch(self, tmp_path):
        """A completed task with all proving tests failing yields a mismatch."""
        matrix = self._matrix(tmp_path)
        tasks = self._tasks(tmp_path)
        # Fail R3 + R9 (backing completed task 1) and Property 1 + R3 (task 1.1).
        results = {t: True for t in matrix.all_tests()}
        for key in ("R3", "R9"):
            for t in matrix.requirement_item(key).tests:
                results[t] = False
        for t in matrix.property_item(1).tests:
            results[t] = False
        mismatches = reconcile_completed_tasks(matrix, tasks, test_results=results)

        assert any("task 1 " in m or "task 1 (" in m for m in mismatches)
        assert any("task 1.1" in m for m in mismatches)
        # The incomplete task 2 must NOT appear (only completed tasks count).
        assert not any("task 2 " in m for m in mismatches)

    def test_incomplete_task_is_not_reconciled(self, tmp_path):
        """Tasks that are not marked complete are skipped entirely."""
        matrix = self._matrix(tmp_path)
        tasks = self._tasks(tmp_path)
        # All tests fail; only completed tasks should generate mismatches.
        results = {t: False for t in matrix.all_tests()}
        mismatches = reconcile_completed_tasks(matrix, tasks, test_results=results)
        assert not any("Not done yet" in m for m in mismatches)

    def test_checkpoint_task_without_refs_is_skipped(self, tmp_path):
        """A completed task citing no parent item produces no mismatch."""
        matrix = self._matrix(tmp_path)
        tasks = self._tasks(tmp_path)
        results = {t: False for t in matrix.all_tests()}
        mismatches = reconcile_completed_tasks(matrix, tasks, test_results=results)
        assert not any("Checkpoint" in m for m in mismatches)

    def test_offline_status_mode_uses_matrix_status(self, tmp_path):
        """Without test_results, pass/fail derives from matrix item status."""
        # All-pass statuses -> completed mapped tasks are backed.
        pass_path = _write(
            tmp_path / "m_pass.yaml",
            _full_matrix_yaml(prop_status=STATUS_PASS, req_status=STATUS_PASS),
        )
        matrix_pass = load_traceability_matrix(pass_path)
        tasks = self._tasks(tmp_path)
        assert reconcile_completed_tasks(matrix_pass, tasks) == []

        # All-pending statuses -> nothing passes -> completed tasks mismatch.
        pending_matrix = self._matrix(tmp_path)
        mismatches = reconcile_completed_tasks(pending_matrix, tasks)
        assert any("task 1 " in m or "task 1 (" in m for m in mismatches)


# ---------------------------------------------------------------------------
# Committed matrix sanity (Req 8.1)
# ---------------------------------------------------------------------------


class TestCommittedMatrix:
    """The repository's traceability_matrix.yaml is complete and well-formed."""

    def test_default_matrix_maps_every_parent_item(self):
        """The committed matrix maps all Properties 1-14 and R1-R14 (Req 8.1)."""
        matrix = load_traceability_matrix(DEFAULT_MATRIX_PATH)
        assert set(matrix.properties) == set(PARENT_PROPERTY_NUMBERS)
        assert set(matrix.requirements) == set(PARENT_REQUIREMENT_KEYS)
        # No unmapped items in the committed matrix.
        assert find_unmapped_parent_items(matrix) == []

    def test_default_matrix_tests_exist_on_disk(self):
        """Every proving test path referenced by the matrix exists."""
        matrix = load_traceability_matrix(DEFAULT_MATRIX_PATH)
        workflow_dir = REPO_ROOT / "dev" / "workflow"
        missing = []
        for test_id in sorted(matrix.all_tests()):
            file_part = test_id.split("::", 1)[0]
            if not (workflow_dir / file_part).is_file():
                missing.append(file_part)
        assert missing == [], f"matrix references non-existent tests: {missing}"
