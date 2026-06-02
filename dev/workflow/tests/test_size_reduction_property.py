"""Property-based test: Size Reduction Accuracy (Property 13).

Generates random staged file sets and total available counts, then verifies
that the SizeReductionReport accurately reflects the actual reachability set
contents — staged counts equal frozenset lengths, total counts equal the
total_available fields, and staged counts are always <= total counts.

**Validates: Requirements 9.1, 9.2, 9.3, 9.4**

Traces to: Design Document - Correctness Property 13
  "Reported counts match actual file counts."
"""

from __future__ import annotations

import os
import sys
from pathlib import Path
from unittest.mock import MagicMock

from hypothesis import given, settings, HealthCheck
from hypothesis import strategies as st

sys.path.insert(0, os.path.join(os.path.dirname(__file__), ".."))

from deployment.dag_filter import DAGReachabilitySet
from deployment.pipeline import SizeReductionReport, _log_size_reduction


# ---------------------------------------------------------------------------
# Hypothesis Strategies
# ---------------------------------------------------------------------------

# Strategy for generating file name sets (e.g. J-Job names, script names)
_file_names = st.frozensets(
    st.text(
        alphabet=st.characters(whitelist_categories=("L", "N", "P")),
        min_size=1,
        max_size=30,
    ),
    min_size=0,
    max_size=50,
)


@st.composite
def _reachability_sets(draw):
    """Generate a random DAGReachabilitySet with consistent counts.

    Ensures total_available_* counts are always >= len of the respective
    staged frozensets, mirroring real-world behavior where staged is a
    subset of available.
    """
    jjobs = draw(_file_names)
    ex_scripts = draw(_file_names)
    ush_scripts = draw(_file_names)
    config_files = draw(_file_names)

    # Total available must be >= staged count
    total_jjobs = draw(st.integers(min_value=len(jjobs), max_value=len(jjobs) + 200))
    total_ex = draw(st.integers(min_value=len(ex_scripts), max_value=len(ex_scripts) + 200))
    total_ush = draw(st.integers(min_value=len(ush_scripts), max_value=len(ush_scripts) + 200))
    total_configs = draw(st.integers(min_value=len(config_files), max_value=len(config_files) + 200))

    reachability = DAGReachabilitySet(
        jjobs=jjobs,
        ex_scripts=ex_scripts,
        ush_scripts=ush_scripts,
        config_files=config_files,
        warnings=(),
        total_available_jjobs=total_jjobs,
        total_available_ex_scripts=total_ex,
        total_available_ush_scripts=total_ush,
        total_available_configs=total_configs,
    )

    return reachability


# ---------------------------------------------------------------------------
# Property Test: Size Reduction Accuracy (Property 13)
# ---------------------------------------------------------------------------


@given(reachability=_reachability_sets())
@settings(
    max_examples=100,
    deadline=None,
    suppress_health_check=[HealthCheck.too_slow],
)
def test_size_reduction_staged_counts_match_set_lengths(reachability):
    """Property 13a: Staged counts in report equal actual frozenset lengths.

    **Validates: Requirements 9.1, 9.2, 9.3, 9.4**

    For any random DAGReachabilitySet, calling _log_size_reduction must
    produce a SizeReductionReport where staged_* fields equal the length
    of the corresponding frozensets in the reachability set.
    """
    report = _log_size_reduction(Path("/tmp/fake_dev"), reachability)

    assert report.staged_jjobs == len(reachability.jjobs), (
        f"staged_jjobs={report.staged_jjobs} != len(jjobs)={len(reachability.jjobs)}"
    )
    assert report.staged_ex_scripts == len(reachability.ex_scripts), (
        f"staged_ex_scripts={report.staged_ex_scripts} != "
        f"len(ex_scripts)={len(reachability.ex_scripts)}"
    )
    assert report.staged_ush_scripts == len(reachability.ush_scripts), (
        f"staged_ush_scripts={report.staged_ush_scripts} != "
        f"len(ush_scripts)={len(reachability.ush_scripts)}"
    )
    assert report.staged_configs == len(reachability.config_files), (
        f"staged_configs={report.staged_configs} != "
        f"len(config_files)={len(reachability.config_files)}"
    )


@given(reachability=_reachability_sets())
@settings(
    max_examples=100,
    deadline=None,
    suppress_health_check=[HealthCheck.too_slow],
)
def test_size_reduction_total_counts_match_available(reachability):
    """Property 13b: Total counts in report equal total_available fields.

    **Validates: Requirements 9.1, 9.2, 9.3, 9.4**

    For any random DAGReachabilitySet, the SizeReductionReport total_*
    fields must equal the corresponding total_available_* fields.
    """
    report = _log_size_reduction(Path("/tmp/fake_dev"), reachability)

    assert report.total_jjobs == reachability.total_available_jjobs, (
        f"total_jjobs={report.total_jjobs} != "
        f"total_available_jjobs={reachability.total_available_jjobs}"
    )
    assert report.total_ex_scripts == reachability.total_available_ex_scripts, (
        f"total_ex_scripts={report.total_ex_scripts} != "
        f"total_available_ex_scripts={reachability.total_available_ex_scripts}"
    )
    assert report.total_ush_scripts == reachability.total_available_ush_scripts, (
        f"total_ush_scripts={report.total_ush_scripts} != "
        f"total_available_ush_scripts={reachability.total_available_ush_scripts}"
    )
    assert report.total_configs == reachability.total_available_configs, (
        f"total_configs={report.total_configs} != "
        f"total_available_configs={reachability.total_available_configs}"
    )


@given(reachability=_reachability_sets())
@settings(
    max_examples=100,
    deadline=None,
    suppress_health_check=[HealthCheck.too_slow],
)
def test_size_reduction_staged_never_exceeds_total(reachability):
    """Property 13c: Staged counts are always <= total counts.

    **Validates: Requirements 9.1, 9.2, 9.3, 9.4**

    For any random DAGReachabilitySet where totals >= staged (as
    guaranteed by the strategy), the report must reflect this invariant:
    staged_* <= total_* for all categories.
    """
    report = _log_size_reduction(Path("/tmp/fake_dev"), reachability)

    assert report.staged_jjobs <= report.total_jjobs, (
        f"staged_jjobs={report.staged_jjobs} > total_jjobs={report.total_jjobs}"
    )
    assert report.staged_ex_scripts <= report.total_ex_scripts, (
        f"staged_ex_scripts={report.staged_ex_scripts} > "
        f"total_ex_scripts={report.total_ex_scripts}"
    )
    assert report.staged_ush_scripts <= report.total_ush_scripts, (
        f"staged_ush_scripts={report.staged_ush_scripts} > "
        f"total_ush_scripts={report.total_ush_scripts}"
    )
    assert report.staged_configs <= report.total_configs, (
        f"staged_configs={report.staged_configs} > "
        f"total_configs={report.total_configs}"
    )


@given(reachability=_reachability_sets())
@settings(
    max_examples=100,
    deadline=None,
    suppress_health_check=[HealthCheck.too_slow],
)
def test_size_reduction_report_is_consistent(reachability):
    """Property 13d: The SizeReductionReport is internally consistent.

    **Validates: Requirements 9.1, 9.2, 9.3, 9.4**

    For any random DAGReachabilitySet, all report fields must be
    non-negative and the report instance must be a SizeReductionReport.
    """
    report = _log_size_reduction(Path("/tmp/fake_dev"), reachability)

    assert isinstance(report, SizeReductionReport)

    # All counts must be non-negative
    assert report.staged_jjobs >= 0
    assert report.staged_ex_scripts >= 0
    assert report.staged_ush_scripts >= 0
    assert report.staged_configs >= 0
    assert report.total_jjobs >= 0
    assert report.total_ex_scripts >= 0
    assert report.total_ush_scripts >= 0
    assert report.total_configs >= 0
