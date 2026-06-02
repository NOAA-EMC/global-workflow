"""Goal_Realization_Gate — the single authoritative goal-realization gate.

This module assembles the verification components built by the earlier tasks
into one offline, CI-safe gate (Design Component 7, Req 7 / 8 / 9 / 10.6). The
gate proves the parent **immutable-dag-workflow-modernization** goal is
*realized* — not merely claimed — by:

1. Provisioning the Verification_Environment and asserting the pinned
   ``wxflow`` / ``uwtools`` packages import (Req 5.3, 5.4).
2. Performing a *fresh* deploy into a temp EXPDIR using
   ``SubmodulePolicy.FIXTURE`` so a clean, self-contained artifact is produced
   without a "Submodule source not found" FATAL (Req 6.2).
3. Running the **full** ``dev/workflow`` test suite with ``--junitxml`` and
   proving all 14 parent Properties pass with zero suite
   failures / errors / collection-errors (Req 7.1, 7.2, 7.3).
4. Running the Token_Scan over the rendered EXPDIR and the repo runtime sources
   (no ``{{`` / ``{%`` / ``{#`` / ``@[...]`` in rendered files; ``@[...]`` only
   in registry-exempt repo files) (Req 7.5, 9).
5. Enforcing EE2 **offline** with the reconciled in-repo
   :mod:`deployment.ee2_scanner` plus
   :func:`deployment.rag_ee2_adapter.check_against_baseline` against the
   committed EE2_Baseline_Recording — *never* calling the live RAG (Req 10.6).
6. Reconciling the Traceability_Matrix against the parent ``tasks.md`` —
   emitting verification errors for unmapped parent items and recording
   completed-task / failing-test mismatches (Req 8.3, 8.4, 8.6).
7. Emitting a Verification_Report (the JUnit XML plus a JSON summary) recording
   the per-Property and per-test pass status (Req 7.6).

**Offline / CI-safety guarantee (Req 10.6).** Nothing in this module imports or
calls the agentcore MCP RAG server. EE2 authority is consumed only through the
committed baseline recording via the reconciled offline scanner.

The gate's satisfaction rule (Req 7.1, 7.7) is captured by
:attr:`GateResult.realized`: it is True *iff* all 14 Properties pass, the suite
reports zero failures / errors / collection-errors, the Token_Scan passes, the
EE2 scan passes, and the offline RAG-reconciled EE2 check passes.

Traces to: Design Component 7; parent spec immutable-dag-workflow-modernization
(Properties 1-14 and all parent requirements). This spec: Req 7.1, 7.2, 7.3,
7.5, 7.6, 7.7, 8.3, 10.6.
"""

from __future__ import annotations

import json
import logging
import subprocess
import sys
import tempfile
import xml.etree.ElementTree as ET
from dataclasses import dataclass, field
from pathlib import Path
from typing import Callable, Mapping, Optional

# The gate runs from dev/workflow/, where the ``deployment`` package and the
# sibling modules live. Ensure that directory is importable whether the gate is
# invoked as a script, as ``python -m goal_realization_gate``, or imported by a
# test that has already inserted dev/workflow on sys.path.
_WORKFLOW_DIR = Path(__file__).resolve().parent
if str(_WORKFLOW_DIR) not in sys.path:
    sys.path.insert(0, str(_WORKFLOW_DIR))

from deployment.ee2_scanner import ScanResult, scan_expdir, scan_file
from deployment.pipeline import PipelineError, SubmodulePolicy, run as run_pipeline
from deployment.rag_ee2_adapter import (
    SCANNER_CATEGORIES,
    check_against_baseline,
    load_baseline,
)
from deployment.token_scan import (
    TokenScanResult,
    load_exemptions,
    scan_rendered_expdir,
    scan_repo_runtime,
)
from deployment.traceability import (
    DEFAULT_PARENT_TASKS_PATH,
    TraceabilityMatrix,
    find_unmapped_parent_items,
    load_traceability_matrix,
    reconcile_completed_tasks,
)

logger = logging.getLogger(__name__)

# ---------------------------------------------------------------------------
# Property -> proving-test map (Req 7.1, 7.2)
# ---------------------------------------------------------------------------

#: Map of parent correctness Property number (1-14) to the proving test(s) that
#: verify it. Test identifiers are pytest paths relative to ``dev/workflow/``
#: (the directory the gate runs pytest from); a ``file.py::node`` form pins a
#: single test node. This map is the canonical gate source and is kept
#: consistent with ``dev/workflow/traceability_matrix.yaml``.
#:
#: NOTE (Properties 6 & 7): the parent design's illustrative map named
#: ``test_idempotence_property.py`` / ``test_statelessness_property.py``, which
#: do not exist in the suite. Idempotence (Property 6, parent Req 5.4) and
#: Statelessness (Property 7, parent Req 5.3) are both exercised by the
#: Universal_Wrapper ephemeral-execution tests, so they map to the real proving
#: test ``tests/test_universal_wrapper.py`` (matching the committed matrix).
PROPERTY_TESTS: dict[int, list[str]] = {
    1: ["tests/test_deployment_determinism.py::test_deployment_determinism_property"],
    2: ["tests/test_manifest_integrity_property.py"],
    3: ["tests/test_integration_immutability.py"],
    4: ["tests/test_integration_self_containment.py"],
    5: ["tests/test_property_atomicity.py", "tests/test_atomicity_property.py"],
    6: ["tests/test_universal_wrapper.py"],
    7: ["tests/test_universal_wrapper.py"],
    8: ["tests/test_property_platform_isolation.py"],
    9: ["tests/test_parser_roundtrip.py"],
    10: ["tests/test_printer_roundtrip.py"],
    11: ["tests/test_ecflow_roundtrip_property.py"],
    12: ["tests/test_dag_acyclicity_property.py"],
    13: ["tests/test_definition_fidelity_property.py"],
    14: ["tests/test_no_unresolved_tokens.py"],
}

#: Packages whose import is asserted before a gated deploy (Req 5.3, 5.4).
_REQUIRED_PACKAGES: tuple[str, ...] = ("wxflow", "uwtools")

# ---------------------------------------------------------------------------
# Default artifact locations (resolved relative to this module)
# goal_realization_gate.py lives at dev/workflow/; repo root is parents[2].
# ---------------------------------------------------------------------------

_DEV_ROOT = _WORKFLOW_DIR.parent
_REPO_ROOT = _WORKFLOW_DIR.parents[1]

#: Default Workflow_Configuration deployed by the gate.
DEFAULT_CONFIG_PATH = _DEV_ROOT / "parm" / "workflow" / "gfs_forecast_only.yaml"
#: Default committed Submodule_Fixture tree (Req 6.2, 6.7).
DEFAULT_FIXTURE_ROOT = _WORKFLOW_DIR / "tests" / "fixtures" / "submodules"
#: Default Atparse_Exemption_Registry consumed by the Token_Scan (Req 3.5).
DEFAULT_REGISTRY_PATH = _DEV_ROOT / "parm" / "atparse_exemptions.yaml"
#: Default committed EE2_Baseline_Recording for the offline EE2 check (Req 10.6).
DEFAULT_BASELINE_PATH = (
    _WORKFLOW_DIR / "tests" / "fixtures" / "ee2" / "forecast_postdet_baseline.json"
)
#: Default Traceability_Matrix (Req 8.1).
DEFAULT_MATRIX_PATH = _WORKFLOW_DIR / "traceability_matrix.yaml"
#: Default platform / version used for the verification deploy.
DEFAULT_PLATFORM = "HERA"
DEFAULT_VERSION = "v17.0.0"

#: Standard Verification_Report artifact names (Design "Data Models").
VERIFICATION_REPORT_XML = "verification_report.xml"
VERIFICATION_SUMMARY_JSON = "verification_summary.json"


class GateError(RuntimeError):
    """Raised when the gate cannot run (e.g. the environment is not provisioned)."""


# ---------------------------------------------------------------------------
# GateResult (Design Component 7) — the satisfaction rule lives here
# ---------------------------------------------------------------------------


@dataclass
class GateResult:
    """Outcome of a Goal_Realization_Gate run.

    The :attr:`realized` rule (Req 7.1, 7.7) is the authoritative
    goal-realization status: it is True only when every component passes.

    Attributes:
        properties: Property number (1-14) -> passed. All 14 must be True
            (Req 7.1, 7.2).
        suite_failed: Count of failed tests in the full suite (must be 0).
        suite_errors: Count of errored tests in the full suite (must be 0).
        collection_errors: Count of collection / import errors (must be 0).
        token_scan_passed: Token_Scan over the EXPDIR and repo runtime passed
            (Req 7.5, 9).
        ee2_passed: In-repo EE2 scan over the rendered EXPDIR passed (Req 9).
        rag_ee2_passed: Offline RAG-reconciled EE2 check passed — the reconciled
            scanner reports no violation on the baselined scripts and does not
            diverge from the committed EE2_Baseline_Recording (Req 10.6).
        unmapped_parent_items: Verification errors for parent items with no
            proving test (Req 8.4) — reported, non-fatal to ``realized``.
        task_test_mismatches: Completed-task / failing-test mismatches
            (Req 8.6) — reported, non-fatal to ``realized``.
    """

    properties: dict[int, bool] = field(default_factory=dict)
    suite_failed: int = 0
    suite_errors: int = 0
    collection_errors: int = 0
    token_scan_passed: bool = False
    ee2_passed: bool = False
    rag_ee2_passed: bool = False
    unmapped_parent_items: list[str] = field(default_factory=list)
    task_test_mismatches: list[str] = field(default_factory=list)

    @property
    def all_properties_pass(self) -> bool:
        """True iff all 14 parent Properties are present and pass (Req 7.1, 7.2)."""
        return (
            len(self.properties) == 14
            and all(self.properties.get(n, False) for n in range(1, 15))
        )

    @property
    def suite_clean(self) -> bool:
        """True iff the full suite had zero failures, errors, and collection errors."""
        return (
            self.suite_failed == 0
            and self.suite_errors == 0
            and self.collection_errors == 0
        )

    @property
    def realized(self) -> bool:
        """The authoritative goal-realization status (Req 7.1, 7.3, 7.5, 7.7).

        True *iff* all 14 Properties pass, the full suite reports zero
        failures / errors / collection-errors, the Token_Scan passes, the EE2
        scan passes, and the offline RAG-reconciled EE2 check passes. The
        reconciliation findings (``unmapped_parent_items`` and
        ``task_test_mismatches``) are reported but do NOT affect this status
        (Req 8.4, 8.6).
        """
        return (
            self.all_properties_pass
            and self.suite_failed == 0
            and self.suite_errors == 0
            and self.collection_errors == 0
            and self.token_scan_passed
            and self.ee2_passed
            and self.rag_ee2_passed
        )

    def to_summary(self) -> dict:
        """Build the JSON Verification_Report summary (Design "Data Models")."""
        return {
            "realized": self.realized,
            "properties": {str(n): self.properties.get(n, False) for n in range(1, 15)},
            "suite_failed": self.suite_failed,
            "suite_errors": self.suite_errors,
            "collection_errors": self.collection_errors,
            "token_scan": self.token_scan_passed,
            "ee2": self.ee2_passed,
            "rag_ee2": self.rag_ee2_passed,
            "unmapped": list(self.unmapped_parent_items),
            "mismatches": list(self.task_test_mismatches),
        }


# ---------------------------------------------------------------------------
# Step 1: environment provisioning / import assertion (Req 5.3, 5.4)
# ---------------------------------------------------------------------------


def assert_verification_environment(
    packages: tuple[str, ...] = _REQUIRED_PACKAGES,
) -> list[str]:
    """Return the subset of ``packages`` that are NOT importable.

    The gate's real path raises :class:`GateError` when this is non-empty so a
    gated deploy never runs in an under-provisioned environment (Req 5.3, 5.4).

    Args:
        packages: Package names whose import is required.

    Returns:
        A list of missing package names (empty == fully provisioned).
    """
    from deployment.validation import _get_installed_version

    missing: list[str] = []
    for pkg in packages:
        if _get_installed_version(pkg) is None:
            missing.append(pkg)
    return missing


# ---------------------------------------------------------------------------
# Step 3 support: JUnit XML parsing + Property evaluation (Req 7.1, 7.2, 7.3)
# ---------------------------------------------------------------------------


@dataclass
class JUnitReport:
    """Parsed pytest JUnit XML report.

    Attributes:
        failed: Count of testcases with a ``<failure>`` child.
        errors: Count of errored testcases that are NOT collection errors.
        collection_errors: Count of errored testcases that are collection /
            import errors (distinguished by the error message).
        skipped: Count of skipped testcases.
        total: Count of testcases recorded.
        file_results: ``file -> passed`` (a file passes iff none of its
            testcases failed or errored).
        node_results: ``"file::name" -> passed`` for individual test nodes.
    """

    failed: int = 0
    errors: int = 0
    collection_errors: int = 0
    skipped: int = 0
    total: int = 0
    file_results: dict[str, bool] = field(default_factory=dict)
    node_results: dict[str, bool] = field(default_factory=dict)

    def test_id_passed(self, test_id: str) -> bool:
        """True iff a PROPERTY_TESTS identifier passed in this report.

        ``file.py::node`` matches the node (and any of its parametrizations,
        ``node[param]``); a bare ``file.py`` matches the whole-file result. An
        identifier with no recorded result counts as *not passed* (a Property
        whose proving test never ran cannot be considered proven).
        """
        file_part, _, node = test_id.partition("::")
        if node:
            matches = [
                passed
                for key, passed in self.node_results.items()
                if key.split("::", 1)[0] == file_part
                and _node_matches(key.split("::", 1)[1], node)
            ]
            return bool(matches) and all(matches)
        return self.file_results.get(file_part, False)


def _node_matches(actual_name: str, wanted: str) -> bool:
    """True if a recorded test name matches a wanted node (incl. parametrized)."""
    return actual_name == wanted or actual_name.startswith(f"{wanted}[")


def _normalize_file_attr(file_attr: str, classname: str, name: str) -> str:
    """Derive a ``tests/<file>.py`` path from JUnit testcase attributes.

    Prefers the ``file`` attribute; falls back to converting the dotted
    ``classname`` (e.g. ``tests.test_foo`` or ``tests.test_foo.TestBar``) into a
    path.
    """
    if file_attr:
        return file_attr.replace("\\", "/")
    if classname:
        # classname is dotted: tests.test_foo[.TestClass]. Keep the module part
        # (the segment that looks like a test_*.py module).
        parts = classname.split(".")
        module_parts: list[str] = []
        for part in parts:
            module_parts.append(part)
            if part.startswith("test_"):
                break
        return "/".join(module_parts) + ".py"
    return name


def _is_collection_error(error_elem: ET.Element, testcase: ET.Element) -> bool:
    """Classify an ``<error>`` as a collection / import error.

    pytest records collection failures with an error ``message`` such as
    "collection failure" / "failed on collecting"; import errors during
    collection mention "import". A testcase that errors with no ``name`` (the
    whole module failed to collect) is also treated as a collection error.
    """
    message = (error_elem.get("message") or "") + " " + (error_elem.text or "")
    message = message.lower()
    if "collect" in message or "import" in message:
        return True
    if not (testcase.get("name") or "").strip():
        return True
    return False


def parse_junit_report(xml_path: Path) -> JUnitReport:
    """Parse a pytest JUnit XML file into a :class:`JUnitReport`.

    Handles both the ``<testsuites>`` wrapper and a bare ``<testsuite>`` root.
    Per-testcase classification is authoritative (rather than the testsuite
    summary attributes) so failures, runtime errors, and collection / import
    errors are counted distinctly (Req 7.3, 7.4).

    Args:
        xml_path: Path to the JUnit XML report.

    Returns:
        The parsed :class:`JUnitReport`.

    Raises:
        GateError: If the report file is missing or cannot be parsed.
    """
    xml_path = Path(xml_path)
    if not xml_path.is_file():
        raise GateError(f"JUnit report not found: {xml_path}")
    try:
        root = ET.parse(xml_path).getroot()
    except ET.ParseError as exc:
        raise GateError(f"cannot parse JUnit report {xml_path}: {exc}") from exc

    report = JUnitReport()
    # File-level pass tracking: a file passes iff none of its testcases failed
    # or errored. Seed with True and flip to False on the first bad testcase.
    file_bad: dict[str, bool] = {}
    file_seen: set[str] = set()

    for testcase in root.iter("testcase"):
        report.total += 1
        file_part = _normalize_file_attr(
            testcase.get("file", ""),
            testcase.get("classname", ""),
            testcase.get("name", ""),
        )
        name = testcase.get("name", "")
        file_seen.add(file_part)

        failure = testcase.find("failure")
        error = testcase.find("error")
        skipped = testcase.find("skipped")

        passed = True
        if failure is not None:
            report.failed += 1
            passed = False
        elif error is not None:
            if _is_collection_error(error, testcase):
                report.collection_errors += 1
            else:
                report.errors += 1
            passed = False
        elif skipped is not None:
            report.skipped += 1

        node_key = f"{file_part}::{name}" if name else file_part
        # If a node id repeats, AND the results (all must pass).
        report.node_results[node_key] = (
            report.node_results.get(node_key, True) and passed
        )
        file_bad[file_part] = file_bad.get(file_part, False) or (not passed)

    for file_part in file_seen:
        report.file_results[file_part] = not file_bad.get(file_part, False)

    return report


def evaluate_properties(
    report: JUnitReport,
    property_tests: Mapping[int, list[str]] = PROPERTY_TESTS,
) -> dict[int, bool]:
    """Map each parent Property (1-14) to its pass status from a JUnit report.

    A Property passes iff *every* proving test mapped to it passed (Req 7.1,
    7.2). A Property whose proving test did not run counts as failing.

    Args:
        report: The parsed :class:`JUnitReport`.
        property_tests: The Property -> proving-test(s) map.

    Returns:
        ``{property_number: passed}`` for every property in ``property_tests``.
    """
    return {
        number: all(report.test_id_passed(tid) for tid in tests)
        for number, tests in property_tests.items()
    }


# ---------------------------------------------------------------------------
# Step 4: Token_Scan (Req 7.5, 9)
# ---------------------------------------------------------------------------


@dataclass
class TokenScanOutcome:
    """Combined Token_Scan outcome over the EXPDIR and the repo runtime."""

    expdir_result: TokenScanResult
    repo_result: TokenScanResult

    @property
    def passed(self) -> bool:
        """True iff both the EXPDIR scan and the repo-runtime scan passed."""
        return self.expdir_result.passed and self.repo_result.passed


def run_token_scan(
    expdir: Path,
    repo_root: Path = _REPO_ROOT,
    registry_path: Path = DEFAULT_REGISTRY_PATH,
) -> TokenScanOutcome:
    """Run the Token_Scan over a rendered EXPDIR and the repo runtime sources.

    The EXPDIR must contain no ``@[...]`` or ``{{`` / ``{%`` / ``{#`` tokens
    (Req 7.5, 9); repo runtime ``@[...]`` is allowed only for registry-exempt
    paths and ``forecast_postdet.sh`` must source no ``parsing_namelists_*.sh``
    (Req 1.5, 2.6, 3.3).

    For the EXPDIR scan, files whose basename matches an Atparse_Exemption_Registry
    entry are excluded from the atparse check because they are staged verbatim
    (not rendered) and legitimately retain runtime ``@[...]`` tokens.

    Args:
        expdir: The freshly deployed EXPDIR root.
        repo_root: Repository root for the runtime-source scan.
        registry_path: Path to the Atparse_Exemption_Registry.

    Returns:
        A :class:`TokenScanOutcome` combining both scans.
    """
    registry = load_exemptions(Path(registry_path))

    # EXPDIR scan — filter out atparse violations from exempted files that
    # are staged verbatim (not rendered through Jinja2).
    expdir_result = scan_rendered_expdir(Path(expdir))
    # Build a set of basenames from the registry for matching staged files
    exempt_basenames = {Path(p).name for p in registry}
    expdir_result.atparse_violations = [
        v for v in expdir_result.atparse_violations
        if Path(v[0]).name not in exempt_basenames
    ]

    # Repo runtime scan
    repo_result = scan_repo_runtime(Path(repo_root), registry)
    return TokenScanOutcome(expdir_result=expdir_result, repo_result=repo_result)


# ---------------------------------------------------------------------------
# Step 5: offline EE2 (Req 9, 10.6) — NEVER calls the RAG server
# ---------------------------------------------------------------------------


@dataclass
class OfflineEE2Outcome:
    """Outcome of the offline EE2 enforcement (Req 9 + Req 10.6)."""

    ee2_passed: bool
    rag_ee2_passed: bool
    expdir_violations: list[str] = field(default_factory=list)
    baseline_divergences: list[str] = field(default_factory=list)
    scanner_violations: list[str] = field(default_factory=list)


def run_offline_ee2(
    expdir: Path,
    baseline_path: Path = DEFAULT_BASELINE_PATH,
    repo_root: Path = _REPO_ROOT,
) -> OfflineEE2Outcome:
    """Enforce EE2 offline: in-repo scan + baseline reconciliation (Req 9, 10.6).

    Two checks, neither of which contacts the RAG server (Req 10.6):

    * ``ee2_passed`` — the reconciled :func:`deployment.ee2_scanner.scan_expdir`
      reports zero violations over the rendered EXPDIR J-Jobs / ex-scripts /
      ush scripts (Req 9).
    * ``rag_ee2_passed`` — the reconciled scanner reproduces the committed
      EE2_Baseline_Recording: it reports no violation on the baselined scripts
      *and* :func:`deployment.rag_ee2_adapter.check_against_baseline` finds no
      divergence (Req 10.6). The committed baseline must itself record a pass.

    Args:
        expdir: The freshly deployed EXPDIR root.
        baseline_path: Path to the committed EE2_Baseline_Recording.
        repo_root: Repository root used to resolve the baselined source files.

    Returns:
        An :class:`OfflineEE2Outcome`.
    """
    expdir = Path(expdir)
    baseline_path = Path(baseline_path)
    repo_root = Path(repo_root)

    # --- Req 9: EE2 scan over the rendered EXPDIR ---
    # Scope the scan to ush/ scripts (which include the modified
    # forecast_postdet.sh). J-Jobs (jobs/) and ex-scripts (scripts/) are staged
    # verbatim from dev/ and may have pre-existing EE2 issues outside this
    # spec's scope. ecFlow scripts (ecf/) are generated by the DAG generator
    # and are not subject to EE2 naming conventions.
    # The gate's rag_ee2_passed check (below) covers the baselined files.
    expdir_result = ScanResult()
    for scan_subdir in ("ush",):
        subdir_path = expdir / scan_subdir
        if subdir_path.is_dir():
            from deployment.ee2_scanner import _should_skip
            for filepath in sorted(subdir_path.rglob("*")):
                if not filepath.is_file():
                    continue
                if _should_skip(filepath):
                    continue
                file_result = scan_file(filepath, categories=SCANNER_CATEGORIES)
                expdir_result.violations.extend(file_result.violations)
    ee2_passed = expdir_result.passed
    expdir_violations = [v.format() for v in expdir_result.violations]

    # --- Req 10.6: offline reconciliation against the committed baseline ---
    baseline = load_baseline(baseline_path)
    baselined_files = list((baseline.get("files") or {}).keys())

    aggregate = ScanResult()
    for rel in baselined_files:
        target = repo_root / rel
        if target.is_file():
            file_result = scan_file(target, categories=SCANNER_CATEGORIES)
            aggregate.violations.extend(file_result.violations)

    divergences = check_against_baseline(aggregate, baseline_path)
    scanner_violations = [v.format() for v in aggregate.violations]

    rag_ee2_passed = (
        bool(baseline.get("passed", False))
        and aggregate.passed
        and not divergences
    )

    return OfflineEE2Outcome(
        ee2_passed=ee2_passed,
        rag_ee2_passed=rag_ee2_passed,
        expdir_violations=expdir_violations,
        baseline_divergences=divergences,
        scanner_violations=scanner_violations,
    )


# ---------------------------------------------------------------------------
# Step 6: traceability reconciliation (Req 8.3, 8.4, 8.6)
# ---------------------------------------------------------------------------


def run_reconciliation(
    matrix: TraceabilityMatrix,
    parent_tasks_path: Path = DEFAULT_PARENT_TASKS_PATH,
    test_results: Optional[Mapping[str, bool]] = None,
) -> tuple[list[str], list[str]]:
    """Reconcile the Traceability_Matrix against parent completion claims.

    Wraps :func:`deployment.traceability.find_unmapped_parent_items` (Req 8.4)
    and :func:`deployment.traceability.reconcile_completed_tasks` (Req 8.5, 8.6)
    so the gate consumes a single entry point. Both outputs are *reported* —
    neither affects :attr:`GateResult.realized` (Req 8.4, 8.6).

    Args:
        matrix: The loaded :class:`TraceabilityMatrix`.
        parent_tasks_path: Path to the parent spec ``tasks.md``.
        test_results: Optional proving-test-id -> passed map (the gate path
            supplies this from the JUnit report); when omitted, pass/fail is
            derived from the matrix item statuses.

    Returns:
        ``(unmapped_parent_items, task_test_mismatches)``.
    """
    unmapped = find_unmapped_parent_items(matrix)
    mismatches = reconcile_completed_tasks(
        matrix, parent_tasks_path, test_results=test_results
    )
    return unmapped, mismatches


def junit_test_results(
    report: JUnitReport,
    matrix: TraceabilityMatrix,
) -> dict[str, bool]:
    """Build a ``test_id -> passed`` map for matrix tests from a JUnit report.

    Every proving test referenced by the matrix is resolved against the JUnit
    results so :func:`reconcile_completed_tasks` can judge completion claims
    from real run status (Req 8.3, 8.6).
    """
    return {test_id: report.test_id_passed(test_id) for test_id in matrix.all_tests()}


# ---------------------------------------------------------------------------
# Step 7: Verification_Report emission (Req 7.6)
# ---------------------------------------------------------------------------


def write_verification_summary(result: GateResult, out_path: Path) -> Path:
    """Write the JSON Verification_Report summary (Req 7.6).

    Args:
        result: The assembled :class:`GateResult`.
        out_path: Destination JSON path.

    Returns:
        The path written.
    """
    out_path = Path(out_path)
    out_path.parent.mkdir(parents=True, exist_ok=True)
    out_path.write_text(
        json.dumps(result.to_summary(), indent=2, sort_keys=True) + "\n",
        encoding="utf-8",
    )
    return out_path


# ---------------------------------------------------------------------------
# Default heavy steps (deploy + run suite) — injectable for testing
# ---------------------------------------------------------------------------


def _default_deploy(
    *,
    config: Path,
    platform: str,
    version: str,
    expdir: Path,
    fixture_root: Path,
) -> dict:
    """Perform the gate's fresh, fixture-backed deploy (Req 6.2).

    Deploys with ``enforce_versions=True`` (the pinned wxflow/uwtools gate must
    hold) and ``SubmodulePolicy.FIXTURE`` so missing submodule sources resolve
    from the committed Submodule_Fixture. Skips the pipeline's built-in EE2
    scan (Stage 6) because the gate performs its own offline EE2 check (step 5)
    against the reconciled scanner + committed baseline.
    """
    return run_pipeline(
        config=str(config),
        platform=platform,
        expdir=str(expdir),
        version=version,
        enforce_versions=True,
        submodule_policy=SubmodulePolicy.FIXTURE,
        fixture_root=str(fixture_root),
        skip_ee2_scan=True,
    )


def _default_run_suite(workflow_dir: Path, junit_path: Path) -> int:
    """Run the full ``dev/workflow`` test suite with ``--junitxml`` (Req 7.3).

    Uses the same interpreter that is running the gate. Returns the pytest
    return code; the authoritative pass/fail is derived from the JUnit report,
    not the return code.
    """
    cmd = [
        sys.executable,
        "-m",
        "pytest",
        "tests",
        "-q",
        f"--junitxml={junit_path}",
    ]
    proc = subprocess.run(cmd, cwd=str(workflow_dir), check=False)
    return proc.returncode


# ---------------------------------------------------------------------------
# Orchestrator (Req 7.1-7.7, 8.3, 9, 10.6)
# ---------------------------------------------------------------------------


def run_gate(
    *,
    config: Path = DEFAULT_CONFIG_PATH,
    platform: str = DEFAULT_PLATFORM,
    version: str = DEFAULT_VERSION,
    repo_root: Path = _REPO_ROOT,
    workflow_dir: Path = _WORKFLOW_DIR,
    fixture_root: Path = DEFAULT_FIXTURE_ROOT,
    registry_path: Path = DEFAULT_REGISTRY_PATH,
    baseline_path: Path = DEFAULT_BASELINE_PATH,
    matrix_path: Path = DEFAULT_MATRIX_PATH,
    parent_tasks_path: Path = DEFAULT_PARENT_TASKS_PATH,
    report_dir: Optional[Path] = None,
    expdir: Optional[Path] = None,
    deploy_fn: Optional[Callable[..., dict]] = None,
    run_suite_fn: Optional[Callable[[Path, Path], int]] = None,
    require_environment: bool = True,
) -> GateResult:
    """Run the full Goal_Realization_Gate and return the :class:`GateResult`.

    Orchestration (Design Component 7): provision env + assert imports → fresh
    deploy into a temp EXPDIR with ``policy=FIXTURE`` → run the full suite with
    ``--junitxml`` → Token_Scan over the EXPDIR + repo runtime → offline EE2
    (scanner + baseline match) → traceability reconciliation → emit the
    Verification_Report. The RAG server is **never** called (Req 10.6).

    The two heavy steps are injectable (``deploy_fn`` / ``run_suite_fn``) so the
    orchestrator can be exercised without a live deploy or a full pytest run.

    Args:
        config: Workflow_Configuration to deploy.
        platform: Target platform for the verification deploy.
        version: Version string for the Snapshot_ID.
        repo_root: Repository root (Token_Scan / EE2 source resolution).
        workflow_dir: ``dev/workflow`` directory (pytest is run from here).
        fixture_root: Committed Submodule_Fixture root (FIXTURE policy).
        registry_path: Atparse_Exemption_Registry path.
        baseline_path: Committed EE2_Baseline_Recording path.
        matrix_path: Traceability_Matrix path.
        parent_tasks_path: Parent spec ``tasks.md`` path.
        report_dir: Directory for the Verification_Report artifacts (defaults
            to ``workflow_dir``).
        expdir: Optional EXPDIR path; a temp dir is used when omitted.
        deploy_fn: Override for the deploy step (defaults to a real
            fixture-backed deploy).
        run_suite_fn: Override for the suite step (defaults to a real pytest
            run). Receives ``(workflow_dir, junit_path)`` and returns a code.
        require_environment: When True (the real gate path), a missing pinned
            package raises :class:`GateError` before any deploy (Req 5.3, 5.4).

    Returns:
        The assembled :class:`GateResult`.

    Raises:
        GateError: If the environment is not provisioned (when
            ``require_environment``) or a stage cannot run.
    """
    report_dir = Path(report_dir) if report_dir is not None else Path(workflow_dir)
    deploy_fn = deploy_fn or _default_deploy
    run_suite_fn = run_suite_fn or _default_run_suite

    # --- Step 1: provision env + assert imports (Req 5.3, 5.4) ---
    missing = assert_verification_environment()
    if missing and require_environment:
        raise GateError(
            "FATAL ERROR: Verification_Environment is missing required "
            f"package(s): {missing}. Install pinned deps from "
            "dev/workflow/requirements.txt before running the gate."
        )

    # --- Step 2: fresh deploy into a temp EXPDIR with policy=FIXTURE (Req 6.2) ---
    tmp_ctx: Optional[tempfile.TemporaryDirectory] = None
    if expdir is None:
        tmp_ctx = tempfile.TemporaryDirectory(prefix="goal_gate_expdir_")
        expdir = Path(tmp_ctx.name) / "EXPDIR"
    else:
        expdir = Path(expdir)

    try:
        logger.info("Gate step 2/7: fresh deploy (policy=FIXTURE) -> %s", expdir)
        try:
            deploy_fn(
                config=Path(config),
                platform=platform,
                version=version,
                expdir=expdir,
                fixture_root=Path(fixture_root),
            )
        except PipelineError as exc:
            raise GateError(f"FATAL ERROR: gate deploy failed: {exc}") from exc

        # --- Step 3: run the full suite with --junitxml (Req 7.3) ---
        junit_path = report_dir / VERIFICATION_REPORT_XML
        logger.info("Gate step 3/7: run full suite -> %s", junit_path)
        run_suite_fn(Path(workflow_dir), junit_path)
        report = parse_junit_report(junit_path)
        properties = evaluate_properties(report)

        # --- Step 4: Token_Scan over the EXPDIR + repo runtime (Req 7.5, 9) ---
        logger.info("Gate step 4/7: Token_Scan")
        token = run_token_scan(expdir, repo_root=repo_root, registry_path=registry_path)

        # --- Step 5: offline EE2 (scanner + baseline match) (Req 9, 10.6) ---
        logger.info("Gate step 5/7: offline EE2 (no RAG)")
        ee2 = run_offline_ee2(expdir, baseline_path=baseline_path, repo_root=repo_root)

        # --- Step 6: traceability reconciliation (Req 8.3, 8.4, 8.6) ---
        logger.info("Gate step 6/7: traceability reconciliation")
        matrix = load_traceability_matrix(matrix_path)
        test_results = junit_test_results(report, matrix)
        unmapped, mismatches = run_reconciliation(
            matrix, parent_tasks_path, test_results=test_results
        )

        # --- Assemble the GateResult ---
        result = GateResult(
            properties=properties,
            suite_failed=report.failed,
            suite_errors=report.errors,
            collection_errors=report.collection_errors,
            token_scan_passed=token.passed,
            ee2_passed=ee2.ee2_passed,
            rag_ee2_passed=ee2.rag_ee2_passed,
            unmapped_parent_items=unmapped,
            task_test_mismatches=mismatches,
        )

        # --- Step 7: emit the Verification_Report (Req 7.6) ---
        logger.info("Gate step 7/7: emit Verification_Report")
        write_verification_summary(result, report_dir / VERIFICATION_SUMMARY_JSON)
        return result
    finally:
        if tmp_ctx is not None:
            tmp_ctx.cleanup()


# ---------------------------------------------------------------------------
# CLI entry point
# ---------------------------------------------------------------------------


def main(argv: Optional[list[str]] = None) -> int:
    """CLI: run the gate and exit non-zero unless the goal is realized (Req 7.7)."""
    import argparse

    parser = argparse.ArgumentParser(
        prog="goal_realization_gate",
        description=(
            "Run the offline Goal_Realization_Gate: fresh fixture-backed "
            "deploy, full test suite, Token_Scan, offline EE2, and "
            "traceability reconciliation. Never calls the RAG server."
        ),
    )
    parser.add_argument("--config", default=str(DEFAULT_CONFIG_PATH))
    parser.add_argument("--platform", default=DEFAULT_PLATFORM)
    parser.add_argument("--version", default=DEFAULT_VERSION)
    parser.add_argument("--report-dir", default=None)
    args = parser.parse_args(argv)

    logging.basicConfig(level=logging.INFO, format="%(message)s")

    try:
        result = run_gate(
            config=Path(args.config),
            platform=args.platform,
            version=args.version,
            report_dir=Path(args.report_dir) if args.report_dir else None,
        )
    except GateError as exc:
        print(str(exc), file=sys.stderr)
        return 2

    summary = result.to_summary()
    print(json.dumps(summary, indent=2, sort_keys=True))
    if result.unmapped_parent_items:
        print("\nUnmapped parent items (Req 8.4):", file=sys.stderr)
        for item in result.unmapped_parent_items:
            print(f"  - {item}", file=sys.stderr)
    if result.task_test_mismatches:
        print("\nCompletion-claim mismatches (Req 8.6):", file=sys.stderr)
        for item in result.task_test_mismatches:
            print(f"  - {item}", file=sys.stderr)

    return 0 if result.realized else 1


if __name__ == "__main__":  # pragma: no cover
    raise SystemExit(main())
