"""Unit tests for the RAG-backed EE2 adapter and the reconciled scanner.

These tests are **offline / CI-safe**: they NEVER call the live agentcore MCP
RAG server (Req 10.6). They exercise:

* :class:`deployment.rag_ee2_adapter.RagEE2Result` ``passed`` semantics
  (Req 10.1, 10.2);
* :func:`deployment.rag_ee2_adapter.derive_changed_files` changed-file
  derivation filtered to ``*.sh`` / J-Jobs / ex-scripts / ``ush/`` (Component 8);
* :func:`deployment.rag_ee2_adapter.run_rag_ee2_scan` against a *fake* in-memory
  client (a stand-in for the dev-only live client) and
  :func:`deployment.rag_ee2_adapter.record_baseline` round-tripping;
* :func:`deployment.rag_ee2_adapter.check_against_baseline` matching the
  reconciled :mod:`deployment.ee2_scanner` output against the committed
  EE2_Baseline_Recording (Req 10.4, 10.6), including detection of a scanner
  false positive (a divergence) when the scanner is *not* reconciled.

**Validates: Requirements 10.4, 10.6** (and exercises 10.1, 10.2, 10.3).

Traces to: Design Document - Component 8 (RAG_EE2_Compliance_Scan Adapter).
"""

from __future__ import annotations

import os
import subprocess
import sys
from pathlib import Path

import pytest

sys.path.insert(0, os.path.join(os.path.dirname(__file__), ".."))

from deployment import ee2_scanner
from deployment.ee2_scanner import ScanResult, scan_file
from deployment.rag_ee2_adapter import (
    EXTRACT_CATEGORIES,
    SCAN_CATEGORIES,
    SCANNER_CATEGORIES,
    RagEE2Client,
    RagEE2Result,
    check_against_baseline,
    derive_changed_files,
    load_baseline,
    record_baseline,
    run_rag_ee2_scan,
)

# Committed EE2_Baseline_Recording produced in the dev env from the live RAG.
FIXTURE_DIR = Path(__file__).resolve().parent / "fixtures" / "ee2"
FORECAST_BASELINE = FIXTURE_DIR / "forecast_postdet_baseline.json"


# ---------------------------------------------------------------------------
# Helpers
# ---------------------------------------------------------------------------


def _write(path: Path, text: str) -> Path:
    path.parent.mkdir(parents=True, exist_ok=True)
    path.write_text(text, encoding="utf-8")
    return path


class _FakeRagClient:
    """In-memory stand-in for the dev-only live RAG client.

    Returns canned ``scan_repository_compliance`` / ``extract_code_for_analysis``
    payloads so the adapter can be exercised offline without the RAG server.
    """

    def __init__(self, scan_payload: dict, extract_payload: dict):
        self._scan = scan_payload
        self._extract = extract_payload
        self.scan_calls: list[tuple[int, list[str]]] = []
        self.extract_calls: list[tuple[int, list[str]]] = []

    def scan_repository_compliance(self, files, categories):
        self.scan_calls.append((len(files), list(categories)))
        return self._scan

    def extract_code_for_analysis(self, files, categories):
        self.extract_calls.append((len(files), list(categories)))
        return self._extract


# A clean staging block mirroring the reconciled forecast_postdet.sh pattern.
CLEAN_CPREQ_BLOCK = """\
#! /usr/bin/env bash
WW3_nml() {
    echo "SUB ${FUNCNAME[0]}: Copying pre-rendered WW3 config from EXPDIR"
    if [[ ! -f "${EXPDIR}/parm/ufs/wave/ww3_shel.nml" ]]; then
        echo "FATAL ERROR: Pre-rendered ww3_shel.nml not found"
        exit 1
    fi
    cpreq "${EXPDIR}/parm/ufs/wave/ww3_shel.nml" "${DATA}/ww3_shel.nml"
}
"""


# ---------------------------------------------------------------------------
# RagEE2Result.passed semantics (Req 10.1, 10.2)
# ---------------------------------------------------------------------------


class TestRagEE2ResultPassed:
    def test_clean_result_passes(self):
        """No scan issues and no extract findings -> passed."""
        result = RagEE2Result(
            files_with_issues=0,
            issues_by_category={},
            extract_findings={c: [] for c in EXTRACT_CATEGORIES},
            scanned_files=["ush/forecast_postdet.sh"],
        )
        assert result.passed

    def test_scan_issue_fails(self):
        """A flagged file in any scan category fails the result (Req 10.1)."""
        result = RagEE2Result(
            files_with_issues=1,
            issues_by_category={
                "error_handling": [{"file": "ush/x.sh", "desc": "missing err_chk"}]
            },
            extract_findings={c: [] for c in EXTRACT_CATEGORIES},
            scanned_files=["ush/x.sh"],
        )
        assert not result.passed

    def test_extract_finding_fails(self):
        """An unresolved extract finding fails the result (Req 10.2)."""
        result = RagEE2Result(
            files_with_issues=0,
            issues_by_category={},
            extract_findings={
                "output_file_naming": [{"file": "scripts/exgfs_post.sh"}],
                "shebang_compliance": [],
                "env_var_validation": [],
            },
            scanned_files=["scripts/exgfs_post.sh"],
        )
        assert not result.passed

    def test_per_file_verdict_marks_flagged_category(self):
        """per_file_verdict marks the flagged category 'issue', rest 'clean'."""
        result = RagEE2Result(
            files_with_issues=1,
            issues_by_category={
                "shebang_compliance": [{"file": "ush/forecast_postdet.sh"}]
            },
            extract_findings={c: [] for c in EXTRACT_CATEGORIES},
            scanned_files=["ush/forecast_postdet.sh"],
        )
        verdict = result.per_file_verdict()
        scan = verdict["ush/forecast_postdet.sh"]["scan"]
        assert scan["shebang_compliance"] == "issue"
        assert scan["error_handling"] == "clean"


# ---------------------------------------------------------------------------
# Changed-file derivation (Design Component 8)
# ---------------------------------------------------------------------------


class TestDeriveChangedFiles:
    @staticmethod
    def _init_repo(root: Path) -> None:
        subprocess.run(["git", "init", "-q"], cwd=root, check=True)
        subprocess.run(
            ["git", "config", "user.email", "t@example.com"], cwd=root, check=True
        )
        subprocess.run(["git", "config", "user.name", "t"], cwd=root, check=True)

    def test_filters_to_ee2_relevant_paths(self, tmp_path):
        """Only *.sh, J-Jobs, ex-scripts, and ush/ files are returned."""
        self._init_repo(tmp_path)
        # Committed baseline files.
        _write(tmp_path / "README.md", "# repo\n")
        _write(tmp_path / "ush" / "forecast_postdet.sh", "#!/bin/bash\necho v1\n")
        subprocess.run(["git", "add", "-A"], cwd=tmp_path, check=True)
        subprocess.run(["git", "commit", "-qm", "init"], cwd=tmp_path, check=True)

        # Modify a ush script (relevant) and a doc (irrelevant).
        _write(tmp_path / "ush" / "forecast_postdet.sh", "#!/bin/bash\necho v2\n")
        _write(tmp_path / "README.md", "# repo edited\n")
        # New untracked files: one relevant J-Job, one relevant ex-script, one not.
        _write(tmp_path / "jobs" / "JGFS_FORECAST", "#!/bin/bash\necho job\n")
        _write(tmp_path / "scripts" / "exgfs_post.sh", "#!/bin/bash\necho ex\n")
        _write(tmp_path / "docs" / "notes.txt", "notes\n")

        changed = derive_changed_files(tmp_path)
        assert "ush/forecast_postdet.sh" in changed
        assert "jobs/JGFS_FORECAST" in changed
        assert "scripts/exgfs_post.sh" in changed
        assert "README.md" not in changed
        assert "docs/notes.txt" not in changed

    def test_pure_deletion_dropped(self, tmp_path):
        """A deleted file (no longer on disk) is not in the scan set."""
        self._init_repo(tmp_path)
        _write(tmp_path / "ush" / "parsing_namelists_WW3.sh", "#!/bin/bash\n")
        subprocess.run(["git", "add", "-A"], cwd=tmp_path, check=True)
        subprocess.run(["git", "commit", "-qm", "init"], cwd=tmp_path, check=True)
        (tmp_path / "ush" / "parsing_namelists_WW3.sh").unlink()

        changed = derive_changed_files(tmp_path)
        assert "ush/parsing_namelists_WW3.sh" not in changed

    def test_no_git_returns_empty(self, tmp_path):
        """A directory that is not a git repo yields an empty list (no crash)."""
        assert derive_changed_files(tmp_path) == []


# ---------------------------------------------------------------------------
# run_rag_ee2_scan + record_baseline round-trip (dev-time shape, offline)
# ---------------------------------------------------------------------------


class TestRunScanAndRecordBaseline:
    def test_clean_scan_records_passing_baseline(self, tmp_path):
        """A clean fake-RAG scan yields a passing, reproducible baseline."""
        repo = tmp_path / "repo"
        _write(repo / "ush" / "forecast_postdet.sh", CLEAN_CPREQ_BLOCK)

        client: RagEE2Client = _FakeRagClient(
            scan_payload={"statistics": {"files_with_issues": 0}, "issues_by_category": {}},
            extract_payload={"extract_findings": {c: [] for c in EXTRACT_CATEGORIES}},
        )
        result = run_rag_ee2_scan(
            client, ["ush/forecast_postdet.sh"], repo_root=repo
        )
        assert result.passed
        # The adapter must request all five scan + three extract categories.
        assert client.scan_calls == [(1, SCAN_CATEGORIES)]
        assert client.extract_calls == [(1, EXTRACT_CATEGORIES)]

        out = record_baseline(result, repo / "fixtures", name="b.json")
        reloaded = load_baseline(out)
        assert reloaded["passed"] is True
        assert reloaded["files"]["ush/forecast_postdet.sh"]["scan"][
            "error_handling"
        ] == "clean"

    def test_record_baseline_is_deterministic(self, tmp_path):
        """Two recordings of the same result are byte-identical (Req 10.3)."""
        result = RagEE2Result(
            files_with_issues=0,
            issues_by_category={},
            extract_findings={c: [] for c in EXTRACT_CATEGORIES},
            scanned_files=["ush/forecast_postdet.sh"],
        )
        a = record_baseline(result, tmp_path / "a", name="b.json").read_bytes()
        b = record_baseline(result, tmp_path / "b", name="b.json").read_bytes()
        assert a == b


# ---------------------------------------------------------------------------
# check_against_baseline vs the reconciled scanner (Req 10.4, 10.6)
# ---------------------------------------------------------------------------


class TestCheckAgainstBaseline:
    def test_committed_baseline_exists_and_passes(self):
        """The committed forecast_postdet baseline is present and PASS."""
        assert FORECAST_BASELINE.is_file()
        baseline = load_baseline(FORECAST_BASELINE)
        assert baseline["passed"] is True
        assert baseline["scan_categories"] == SCAN_CATEGORIES
        assert baseline["scanner_categories"] == SCANNER_CATEGORIES

    def test_reconciled_scanner_matches_committed_baseline(self):
        """The reconciled ee2_scanner reproduces the RAG verdict (no divergence).

        Scans the real modified ``ush/forecast_postdet.sh`` with the in-repo
        scanner and confirms it matches the committed authoritative baseline —
        i.e. the cpreq staging blocks are NOT flagged (Req 10.4, 10.6).
        """
        repo_root = Path(__file__).resolve().parents[3]
        target = repo_root / "ush" / "forecast_postdet.sh"
        assert target.is_file(), target

        scanner_result = scan_file(target, categories=SCANNER_CATEGORIES)
        divergences = check_against_baseline(scanner_result, FORECAST_BASELINE)
        assert divergences == [], divergences

    def test_clean_scanresult_matches_clean_baseline(self, tmp_path):
        """An empty ScanResult matches an all-clean baseline."""
        result = RagEE2Result(
            files_with_issues=0,
            issues_by_category={},
            extract_findings={c: [] for c in EXTRACT_CATEGORIES},
            scanned_files=["ush/forecast_postdet.sh"],
        )
        baseline_path = record_baseline(result, tmp_path, name="b.json")
        divergences = check_against_baseline(ScanResult(), baseline_path)
        assert divergences == []

    def test_false_positive_is_detected_as_divergence(self, tmp_path):
        """A scanner false positive (clean baseline, scanner flags) diverges.

        This is exactly the condition the task-7.1 reconciliation removes: if
        the scanner flagged error_handling on a file the RAG records clean,
        check_against_baseline must surface it (Req 10.4).
        """
        result = RagEE2Result(
            files_with_issues=0,
            issues_by_category={},
            extract_findings={c: [] for c in EXTRACT_CATEGORIES},
            scanned_files=["ush/forecast_postdet.sh"],
        )
        baseline_path = record_baseline(result, tmp_path, name="b.json")

        # A scanner result that (incorrectly) flags error_handling.
        bad = ScanResult()
        bad.add("error_handling", "ush/forecast_postdet.sh", "missing err_chk")
        divergences = check_against_baseline(bad, baseline_path)
        assert len(divergences) == 1
        assert "error_handling" in divergences[0]
        assert "false positive" in divergences[0]

    def test_false_negative_is_detected_as_divergence(self, tmp_path):
        """A baseline issue the scanner misses is reported as a divergence."""
        result = RagEE2Result(
            files_with_issues=1,
            issues_by_category={
                "file_naming": [{"file": "scripts/BAD_NAME.sh"}]
            },
            extract_findings={c: [] for c in EXTRACT_CATEGORIES},
            scanned_files=["scripts/BAD_NAME.sh"],
        )
        baseline_path = record_baseline(result, tmp_path, name="b.json")
        # Scanner reports nothing -> misses the authoritative finding.
        divergences = check_against_baseline(ScanResult(), baseline_path)
        assert any("file_naming" in d for d in divergences)

    def test_reconciled_scanner_does_not_flag_cpreq_block(self, tmp_path):
        """Direct check: the reconciled scanner leaves a cpreq block clean.

        Guards the reconciliation itself (Req 10.4): the SME-corrected pattern
        (cpreq + pre-flight FATAL ERROR + exit, no set -e) must be clean.
        """
        target = _write(tmp_path / "ush" / "stage.sh", CLEAN_CPREQ_BLOCK)
        result = scan_file(target, categories=["error_handling"])
        assert result.passed, [v.format() for v in result.violations]


# ---------------------------------------------------------------------------
# Offline safety: the adapter never imports/calls the live RAG server
# ---------------------------------------------------------------------------


def test_adapter_module_has_no_rag_server_dependency():
    """The committed adapter is import-safe with no MCP/RAG server import.

    The adapter may *document* the agentcore RAG server in prose, but it must
    not ``import`` any MCP / RAG-server client at module scope — its only RAG
    coupling is the injected :class:`RagEE2Client` protocol (Req 10.6).
    """
    import deployment.rag_ee2_adapter as adapter

    source = Path(adapter.__file__).read_text(encoding="utf-8")
    import_lines = [
        line.strip()
        for line in source.splitlines()
        if line.strip().startswith(("import ", "from "))
    ]
    joined = "\n".join(import_lines).lower()
    assert "mcp" not in joined
    assert "agentcore" not in joined
    assert "rag" not in joined
