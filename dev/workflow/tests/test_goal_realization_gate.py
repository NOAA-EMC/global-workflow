"""Unit tests for the Goal_Realization_Gate (Design Component 7, task 9.1).

These tests are **offline / CI-safe**: they NEVER perform a live deploy and
NEVER call the agentcore MCP RAG server (Req 10.6). The two heavy steps
(deploy + full pytest run) are injected via ``deploy_fn`` / ``run_suite_fn`` so
the orchestrator, the :class:`GateResult.realized` truth table, and the
traceability reconciliation are exercised deterministically.

Coverage:

* :class:`goal_realization_gate.GateResult` ``realized`` **truth table** —
  including the ``rag_ee2_passed`` term (Req 7.1, 7.7).
* Reconciliation detecting **unmapped parent items** (Req 8.4) and
  **completed-task / failing-test mismatches** (Req 8.6) via
  :func:`goal_realization_gate.run_reconciliation`.
* Supporting deterministic helpers: JUnit parsing (failures / runtime errors /
  collection errors), Property evaluation, and an end-to-end orchestrator run
  on a fully isolated temp tree.

**Validates: Requirements 7.1, 7.7, 8.4, 8.6**

Traces to: Design Document - Component 7 (Goal_Realization_Gate, Verification_
Report, Traceability_Matrix reconciliation).
"""

from __future__ import annotations

import json
import os
import sys
from pathlib import Path

import pytest
import yaml

sys.path.insert(0, os.path.join(os.path.dirname(__file__), ".."))

import goal_realization_gate as gate  # noqa: E402
from deployment.traceability import (  # noqa: E402
    PARENT_PROPERTY_NUMBERS,
    PARENT_REQUIREMENT_KEYS,
    STATUS_PASS,
    STATUS_PENDING,
    load_traceability_matrix,
)


# ---------------------------------------------------------------------------
# Helpers
# ---------------------------------------------------------------------------


def _write(path: Path, text: str) -> Path:
    """Write text to ``path`` creating parent dirs; return the path."""
    path.parent.mkdir(parents=True, exist_ok=True)
    path.write_text(text, encoding="utf-8")
    return path


def _green_result() -> gate.GateResult:
    """Build a fully-green GateResult (every realized condition satisfied)."""
    return gate.GateResult(
        properties={n: True for n in range(1, 15)},
        suite_failed=0,
        suite_errors=0,
        collection_errors=0,
        token_scan_passed=True,
        ee2_passed=True,
        rag_ee2_passed=True,
        unmapped_parent_items=[],
        task_test_mismatches=[],
    )


def _junit_xml(testcases: list[dict]) -> str:
    """Render a minimal pytest-style JUnit XML from testcase dicts.

    Each dict: ``{file, name, status}`` where status in
    {"passed", "failed", "error", "collection_error", "skipped"}.
    """
    rows: list[str] = []
    for tc in testcases:
        file_attr = tc.get("file", "")
        name = tc.get("name", "")
        classname = tc.get("classname", file_attr.replace("/", ".")[:-3] if file_attr else "")
        status = tc.get("status", "passed")
        attrs = f'classname="{classname}" name="{name}" file="{file_attr}"'
        if status == "passed":
            rows.append(f"<testcase {attrs}/>")
        elif status == "failed":
            rows.append(f'<testcase {attrs}><failure message="boom">x</failure></testcase>')
        elif status == "error":
            rows.append(f'<testcase {attrs}><error message="runtime boom">x</error></testcase>')
        elif status == "collection_error":
            rows.append(
                f'<testcase {attrs}><error message="collection failure: import error">'
                f"x</error></testcase>"
            )
        elif status == "skipped":
            rows.append(f"<testcase {attrs}><skipped/></testcase>")
    body = "".join(rows)
    return f'<?xml version="1.0"?><testsuites><testsuite name="pytest">{body}</testsuite></testsuites>'


def _all_properties_passing_testcases() -> list[dict]:
    """A passing testcase for every PROPERTY_TESTS proving test."""
    cases: list[dict] = []
    for tests in gate.PROPERTY_TESTS.values():
        for test_id in tests:
            file_part, _, node = test_id.partition("::")
            cases.append(
                {
                    "file": file_part,
                    "name": node or "test_module",
                    "status": "passed",
                }
            )
    return cases


def _full_matrix_yaml(*, prop_status=STATUS_PENDING, req_status=STATUS_PENDING) -> str:
    """A complete matrix (Properties 1-14, R1-R14) with one test each."""
    props = {
        n: {"name": f"Property {n}", "tests": [f"tests/test_property_{n}.py"], "status": prop_status}
        for n in PARENT_PROPERTY_NUMBERS
    }
    reqs = {
        key: {"title": key, "tests": [f"tests/test_req_{key.lower()}.py"], "status": req_status}
        for key in PARENT_REQUIREMENT_KEYS
    }
    return yaml.dump({"properties": props, "requirements": reqs}, sort_keys=False)


# ===========================================================================
# GateResult.realized truth table (Req 7.1, 7.7)
# ===========================================================================


class TestGateResultRealizedTruthTable:
    """Every realized condition is necessary; the all-green case is sufficient."""

    def test_all_green_is_realized(self):
        """The fully-green GateResult is realized (sufficiency)."""
        assert _green_result().realized is True

    @pytest.mark.parametrize(
        "mutate, label",
        [
            (lambda r: r.properties.__setitem__(7, False), "one property fails"),
            (lambda r: setattr(r, "suite_failed", 1), "a suite test failed"),
            (lambda r: setattr(r, "suite_errors", 1), "a suite test errored"),
            (lambda r: setattr(r, "collection_errors", 1), "a collection error"),
            (lambda r: setattr(r, "token_scan_passed", False), "token scan failed"),
            (lambda r: setattr(r, "ee2_passed", False), "in-repo EE2 failed"),
            (lambda r: setattr(r, "rag_ee2_passed", False), "offline RAG EE2 failed"),
        ],
    )
    def test_each_condition_is_necessary(self, mutate, label):
        """Flipping any single realized condition to bad makes realized False."""
        result = _green_result()
        mutate(result)
        assert result.realized is False, f"realized should be False when {label}"

    def test_rag_ee2_term_is_independently_necessary(self):
        """rag_ee2_passed (Req 10.6) is its own gate term in the truth table.

        With every *other* condition green, a False ``rag_ee2_passed`` alone
        must force a non-passing gate — proving the offline RAG-reconciled EE2
        check is part of the realized rule, not a no-op.
        """
        result = _green_result()
        assert result.realized is True
        result.rag_ee2_passed = False
        assert result.realized is False
        result.rag_ee2_passed = True
        assert result.realized is True

    def test_reconciliation_findings_do_not_affect_realized(self):
        """Unmapped items / task mismatches are reported but non-fatal (Req 8.4, 8.6)."""
        result = _green_result()
        result.unmapped_parent_items = ["VERIFICATION ERROR: parent Property 2 ..."]
        result.task_test_mismatches = ["RECONCILIATION MISMATCH: completed parent task 3 ..."]
        assert result.realized is True

    def test_missing_a_property_fails_all_properties_pass(self):
        """A GateResult with only 13 properties recorded is not realized (Req 7.2)."""
        result = _green_result()
        del result.properties[14]
        assert result.all_properties_pass is False
        assert result.realized is False

    def test_summary_reflects_realized_and_terms(self):
        """to_summary captures realized plus every per-Property and gate term."""
        result = _green_result()
        result.suite_failed = 2
        summary = result.to_summary()
        assert summary["realized"] is False
        assert summary["suite_failed"] == 2
        assert summary["rag_ee2"] is True
        assert set(summary["properties"]) == {str(n) for n in range(1, 15)}
        assert all(summary["properties"][str(n)] for n in range(1, 15))


# ===========================================================================
# JUnit parsing + Property evaluation (Req 7.1, 7.2, 7.3)
# ===========================================================================


class TestJUnitParsing:
    """Per-testcase classification of failures, errors, and collection errors."""

    def test_counts_failures_errors_and_collection_errors(self, tmp_path):
        xml = _junit_xml(
            [
                {"file": "tests/test_a.py", "name": "test_ok", "status": "passed"},
                {"file": "tests/test_a.py", "name": "test_bad", "status": "failed"},
                {"file": "tests/test_b.py", "name": "test_err", "status": "error"},
                {"file": "tests/test_c.py", "name": "test_skip", "status": "skipped"},
                {"file": "tests/test_d.py", "name": "", "status": "collection_error"},
            ]
        )
        report = gate.parse_junit_report(_write(tmp_path / "r.xml", xml))
        assert report.failed == 1
        assert report.errors == 1
        assert report.collection_errors == 1
        assert report.skipped == 1
        # A file with a failure does not pass; a clean file passes.
        assert report.file_results["tests/test_a.py"] is False
        assert report.test_id_passed("tests/test_a.py::test_ok") is True
        assert report.test_id_passed("tests/test_a.py::test_bad") is False

    def test_missing_report_raises(self, tmp_path):
        with pytest.raises(gate.GateError, match="not found"):
            gate.parse_junit_report(tmp_path / "absent.xml")

    def test_parametrized_node_matches_base_name(self, tmp_path):
        """A parametrized test node satisfies its un-parametrized proving-test id."""
        xml = _junit_xml(
            [
                {"file": "tests/test_p.py", "name": "test_prop[v17.0.0]", "status": "passed"},
            ]
        )
        report = gate.parse_junit_report(_write(tmp_path / "r.xml", xml))
        assert report.test_id_passed("tests/test_p.py::test_prop") is True


class TestEvaluateProperties:
    """Property pass status derives from the JUnit report (Req 7.1, 7.2)."""

    def test_all_properties_pass_when_all_proving_tests_pass(self, tmp_path):
        xml = _junit_xml(_all_properties_passing_testcases())
        report = gate.parse_junit_report(_write(tmp_path / "r.xml", xml))
        props = gate.evaluate_properties(report)
        assert set(props) == set(range(1, 15))
        assert all(props.values())

    def test_property_fails_when_a_proving_test_fails(self, tmp_path):
        cases = _all_properties_passing_testcases()
        # Property 5 maps to two tests; failing one must fail the property.
        for tc in cases:
            if tc["file"] == "tests/test_atomicity_property.py":
                tc["status"] = "failed"
        xml = _junit_xml(cases)
        report = gate.parse_junit_report(_write(tmp_path / "r.xml", xml))
        props = gate.evaluate_properties(report)
        assert props[5] is False
        assert props[1] is True

    def test_property_fails_when_proving_test_absent(self, tmp_path):
        """A Property whose proving test never ran counts as failing."""
        report = gate.parse_junit_report(_write(tmp_path / "r.xml", _junit_xml([])))
        props = gate.evaluate_properties(report)
        assert all(v is False for v in props.values())


# ===========================================================================
# Reconciliation: unmapped items + completed-task/failing-test mismatch
# (Req 8.4, 8.6)
# ===========================================================================


SAMPLE_TASKS_MD = """\
# Implementation Plan

- [x] 1. Implement determinism
  - _Requirements: 3.8, 9.4_

- [x] 1.1 Write property test (Property 1)
  - **Property 1: Deployment Determinism**
  - **Validates: Requirements 3.8**

- [ ] 2. Not done yet
  - _Requirements: 7.6_
"""


class TestReconciliation:
    """run_reconciliation surfaces unmapped items and completion mismatches."""

    def test_full_matrix_all_pass_has_no_findings(self, tmp_path):
        matrix_path = _write(
            tmp_path / "m.yaml",
            _full_matrix_yaml(prop_status=STATUS_PASS, req_status=STATUS_PASS),
        )
        tasks = _write(tmp_path / "tasks.md", SAMPLE_TASKS_MD)
        matrix = load_traceability_matrix(matrix_path)
        unmapped, mismatches = gate.run_reconciliation(matrix, tasks)
        assert unmapped == []
        assert mismatches == []

    def test_detects_unmapped_parent_items(self, tmp_path):
        """A matrix missing a Property's proving test yields an unmapped error (Req 8.4)."""
        data = yaml.safe_load(_full_matrix_yaml())
        # Strip the proving test for Property 2 and requirement R9.
        data["properties"][2]["tests"] = []
        data["requirements"]["R9"]["tests"] = []
        matrix_path = _write(tmp_path / "m.yaml", yaml.dump(data, sort_keys=False))
        tasks = _write(tmp_path / "tasks.md", SAMPLE_TASKS_MD)

        matrix = load_traceability_matrix(matrix_path)
        unmapped, _ = gate.run_reconciliation(matrix, tasks)
        assert any("Property 2" in m for m in unmapped)
        assert any("R9" in m for m in unmapped)

    def test_detects_completed_task_failing_test_mismatch(self, tmp_path):
        """A completed task whose proving tests all fail is recorded (Req 8.6)."""
        matrix_path = _write(tmp_path / "m.yaml", _full_matrix_yaml())
        tasks = _write(tmp_path / "tasks.md", SAMPLE_TASKS_MD)
        matrix = load_traceability_matrix(matrix_path)

        # Fail the tests backing completed tasks 1 (R3, R9) and 1.1 (Property 1, R3).
        results = {t: True for t in matrix.all_tests()}
        for key in ("R3", "R9"):
            for t in matrix.requirement_item(key).tests:
                results[t] = False
        for t in matrix.property_item(1).tests:
            results[t] = False

        _, mismatches = gate.run_reconciliation(matrix, tasks, test_results=results)
        assert any("task 1 " in m or "task 1 (" in m for m in mismatches)
        assert any("task 1.1" in m for m in mismatches)
        # The incomplete task 2 must NOT be reconciled.
        assert not any("task 2 " in m for m in mismatches)

    def test_junit_test_results_feeds_reconciliation(self, tmp_path):
        """junit_test_results maps matrix tests to JUnit pass status (Req 8.3)."""
        # Matrix whose tests are real proving-test ids present in the JUnit.
        data = {
            "properties": {
                1: {
                    "name": "Deployment Determinism",
                    "tests": [
                        "tests/test_deployment_determinism.py::test_deployment_determinism_property"
                    ],
                    "status": STATUS_PENDING,
                }
            },
            "requirements": {
                "R1": {"title": "R1", "tests": ["tests/test_manifest_integrity_property.py"]}
            },
        }
        matrix_path = _write(tmp_path / "m.yaml", yaml.dump(data, sort_keys=False))
        matrix = load_traceability_matrix(matrix_path)

        xml = _junit_xml(
            [
                {
                    "file": "tests/test_deployment_determinism.py",
                    "name": "test_deployment_determinism_property",
                    "status": "passed",
                },
                {
                    "file": "tests/test_manifest_integrity_property.py",
                    "name": "test_manifest",
                    "status": "failed",
                },
            ]
        )
        report = gate.parse_junit_report(_write(tmp_path / "r.xml", xml))
        results = gate.junit_test_results(report, matrix)
        assert results[
            "tests/test_deployment_determinism.py::test_deployment_determinism_property"
        ] is True
        assert results["tests/test_manifest_integrity_property.py"] is False


# ===========================================================================
# Offline EE2 (Req 9, 10.6) — never calls the RAG server
# ===========================================================================


class TestOfflineEE2:
    """The offline EE2 step consumes the committed baseline, never the RAG."""

    def test_clean_expdir_and_matching_baseline_pass(self, tmp_path):
        """A clean EXPDIR + a clean baselined script yields both EE2 terms True."""
        # A clean EXPDIR: one EE2-compliant ush script (cpreq pattern, valid shebang).
        expdir = tmp_path / "EXPDIR"
        _write(
            expdir / "ush" / "stage.sh",
            "#!/bin/bash\n"
            'if [[ ! -f "${EXPDIR}/x" ]]; then\n'
            '    echo "FATAL ERROR: missing x"\n'
            "    exit 1\n"
            "fi\n"
            'cpreq "${EXPDIR}/x" "${DATA}/x"\n',
        )

        # A repo with one baselined, EE2-clean script and a matching baseline.
        repo_root = tmp_path / "repo"
        _write(repo_root / "ush" / "forecast_postdet.sh", "#!/bin/bash\necho hi\n")
        baseline = {
            "passed": True,
            "scanner_categories": list(gate.SCANNER_CATEGORIES),
            "files": {
                "ush/forecast_postdet.sh": {
                    "scan": {c: "clean" for c in gate.SCANNER_CATEGORIES}
                }
            },
        }
        baseline_path = _write(
            tmp_path / "baseline.json", json.dumps(baseline)
        )

        outcome = gate.run_offline_ee2(expdir, baseline_path=baseline_path, repo_root=repo_root)
        assert outcome.ee2_passed is True
        assert outcome.rag_ee2_passed is True
        assert outcome.baseline_divergences == []

    def test_baseline_divergence_fails_rag_ee2(self, tmp_path):
        """A scanner false-positive vs a clean baseline fails rag_ee2 (Req 10.6)."""
        expdir = tmp_path / "EXPDIR"
        _write(expdir / "ush" / "ok.sh", "#!/bin/bash\necho ok\n")

        # Baselined script that the scanner WILL flag (captures err but no err_chk),
        # while the baseline records error_handling clean -> divergence.
        repo_root = tmp_path / "repo"
        _write(
            repo_root / "ush" / "forecast_postdet.sh",
            "#!/bin/bash\n${SOMEEXE} arg\nerr=$?\n",
        )
        baseline = {
            "passed": True,
            "scanner_categories": list(gate.SCANNER_CATEGORIES),
            "files": {
                "ush/forecast_postdet.sh": {
                    "scan": {c: "clean" for c in gate.SCANNER_CATEGORIES}
                }
            },
        }
        baseline_path = _write(tmp_path / "baseline.json", json.dumps(baseline))

        outcome = gate.run_offline_ee2(expdir, baseline_path=baseline_path, repo_root=repo_root)
        assert outcome.rag_ee2_passed is False
        assert outcome.baseline_divergences  # at least one divergence recorded


# ===========================================================================
# Orchestrator end-to-end with injected deploy + suite (no live deploy/RAG)
# ===========================================================================


def _isolated_repo(tmp_path: Path) -> Path:
    """A minimal clean repo root: token-free, EE2-clean runtime sources."""
    repo = tmp_path / "repo"
    _write(repo / "ush" / "util.sh", "#!/bin/bash\n# clean helper\necho hello\n")
    # No forecast_postdet.sh -> no parsing-source scan; no atparse tokens anywhere.
    return repo


def _clean_baseline(tmp_path: Path, repo: Path) -> Path:
    """A passing baseline for a clean script that exists in ``repo``."""
    _write(repo / "ush" / "forecast_postdet.sh", "#!/bin/bash\necho hi\n")
    baseline = {
        "passed": True,
        "scanner_categories": list(gate.SCANNER_CATEGORIES),
        "files": {
            "ush/forecast_postdet.sh": {
                "scan": {c: "clean" for c in gate.SCANNER_CATEGORIES}
            }
        },
    }
    return _write(tmp_path / "baseline.json", json.dumps(baseline))


class TestRunGateOrchestration:
    """End-to-end orchestration with the heavy steps injected (deterministic)."""

    def _make_deploy_fn(self):
        """A deploy_fn stub that creates a clean, token-free, EE2-clean EXPDIR."""

        def _deploy(*, config, platform, version, expdir, fixture_root):
            expdir = Path(expdir)
            _write(
                expdir / "ush" / "stage.sh",
                "#!/bin/bash\n"
                'if [[ ! -f "${EXPDIR}/x" ]]; then\n'
                '    echo "FATAL ERROR: missing"\n'
                "    exit 1\n"
                "fi\n"
                'cpreq "${EXPDIR}/x" "${DATA}/x"\n',
            )
            return {"snapshot_id": "v17.0.0+deadbeef", "expdir": str(expdir)}

        return _deploy

    def _common_kwargs(self, tmp_path):
        repo = _isolated_repo(tmp_path)
        registry = _write(tmp_path / "registry.yaml", yaml.dump({"exemptions": []}))
        baseline = _clean_baseline(tmp_path, repo)
        matrix = _write(
            tmp_path / "matrix.yaml",
            _full_matrix_yaml(prop_status=STATUS_PASS, req_status=STATUS_PASS),
        )
        tasks = _write(tmp_path / "tasks.md", "- [ ] 1. nothing done\n  - _Requirements: 1.1_\n")
        return {
            "repo_root": repo,
            "registry_path": registry,
            "baseline_path": baseline,
            "matrix_path": matrix,
            "parent_tasks_path": tasks,
            "report_dir": tmp_path / "report",
            "fixture_root": tmp_path / "fixtures",
            "require_environment": False,
        }

    def test_all_green_run_is_realized(self, tmp_path):
        """Injected clean deploy + all-passing suite -> realized, report emitted."""
        kwargs = self._common_kwargs(tmp_path)

        def _run_suite(workflow_dir, junit_path):
            Path(junit_path).parent.mkdir(parents=True, exist_ok=True)
            Path(junit_path).write_text(
                _junit_xml(_all_properties_passing_testcases()), encoding="utf-8"
            )
            return 0

        result = gate.run_gate(
            deploy_fn=self._make_deploy_fn(),
            run_suite_fn=_run_suite,
            **kwargs,
        )

        assert result.realized is True
        assert result.all_properties_pass is True
        assert result.token_scan_passed is True
        assert result.ee2_passed is True
        assert result.rag_ee2_passed is True
        # Verification_Report artifacts emitted (Req 7.6).
        assert (kwargs["report_dir"] / gate.VERIFICATION_REPORT_XML).is_file()
        summary = kwargs["report_dir"] / gate.VERIFICATION_SUMMARY_JSON
        assert summary.is_file()

    def test_suite_failure_makes_gate_non_passing(self, tmp_path):
        """A single failing suite test forces realized False (Req 7.3, 7.7)."""
        kwargs = self._common_kwargs(tmp_path)

        def _run_suite(workflow_dir, junit_path):
            cases = _all_properties_passing_testcases()
            cases.append(
                {"file": "tests/test_unrelated.py", "name": "test_x", "status": "failed"}
            )
            Path(junit_path).parent.mkdir(parents=True, exist_ok=True)
            Path(junit_path).write_text(_junit_xml(cases), encoding="utf-8")
            return 1

        result = gate.run_gate(
            deploy_fn=self._make_deploy_fn(),
            run_suite_fn=_run_suite,
            **kwargs,
        )
        assert result.suite_failed == 1
        assert result.realized is False

    def test_missing_environment_raises_when_required(self, tmp_path):
        """With require_environment=True and a missing pkg, the gate aborts (Req 5.3)."""
        kwargs = self._common_kwargs(tmp_path)
        kwargs["require_environment"] = True

        def _boom_deploy(**_):  # must never be reached
            raise AssertionError("deploy must not run when environment is missing")

        def _boom_suite(*_):  # must never be reached
            raise AssertionError("suite must not run when environment is missing")

        # Force a missing package regardless of the real environment.
        original = gate.assert_verification_environment
        gate.assert_verification_environment = lambda *a, **k: ["uwtools"]
        try:
            with pytest.raises(gate.GateError, match="missing required"):
                gate.run_gate(
                    deploy_fn=_boom_deploy, run_suite_fn=_boom_suite, **kwargs
                )
        finally:
            gate.assert_verification_environment = original


# ===========================================================================
# Offline-safety guarantee (Req 10.6): the gate never imports the RAG server
# ===========================================================================


def test_gate_module_has_no_rag_server_dependency():
    """The committed gate is import-safe with no MCP / RAG-server import (Req 10.6).

    The module may *document* the RAG server in prose and may import the
    *offline* :mod:`deployment.rag_ee2_adapter` (whose own test proves it has no
    RAG-server dependency), but it must not ``import`` any live MCP /
    agentcore RAG-server client — its only EE2 authority is the committed,
    offline EE2_Baseline_Recording.
    """
    source = Path(gate.__file__).read_text(encoding="utf-8")
    import_lines = [
        line.strip()
        for line in source.splitlines()
        if line.strip().startswith(("import ", "from "))
    ]
    joined = "\n".join(import_lines).lower()
    assert "mcp" not in joined
    assert "agentcore" not in joined
    # The only permitted "rag" import is the offline adapter module itself.
    rag_imports = [line for line in import_lines if "rag" in line.lower()]
    assert all("rag_ee2_adapter" in line for line in rag_imports), rag_imports
