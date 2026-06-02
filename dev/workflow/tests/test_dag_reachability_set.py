"""Unit tests for the DAGReachabilitySet dataclass.

Tests construction, immutability (frozen=True), the is_valid property,
contains_* helper methods, and statistics fields.

Validates: Requirements 1.1, 2.1, 3.1, 4.1, 9.1
"""

from __future__ import annotations

import os
import sys

import pytest

sys.path.insert(0, os.path.join(os.path.dirname(__file__), ".."))

from deployment.dag_filter import DAGReachabilitySet


# ---------------------------------------------------------------------------
# Fixtures
# ---------------------------------------------------------------------------


@pytest.fixture
def sample_reachability_set() -> DAGReachabilitySet:
    """A typical reachability set with multiple artifacts."""
    return DAGReachabilitySet(
        jjobs=frozenset({"JGLOBAL_FORECAST", "JGFS_ATMOS_POST"}),
        ex_scripts=frozenset({"exglobal_forecast.sh", "exgfs_atmos_post.sh"}),
        ush_scripts=frozenset({"forecast_predet.sh", "forecast_det.sh", "forecast_postdet.sh"}),
        config_files=frozenset({"config.base.j2", "config.fcst.j2", "config.com"}),
        warnings=("WARNING: ush script 'optional.sh' not found",),
        total_available_jjobs=92,
        total_available_ex_scripts=50,
        total_available_ush_scripts=120,
        total_available_configs=30,
    )


@pytest.fixture
def empty_reachability_set() -> DAGReachabilitySet:
    """An empty reachability set (no jjobs)."""
    return DAGReachabilitySet(
        jjobs=frozenset(),
        ex_scripts=frozenset(),
        ush_scripts=frozenset(),
        config_files=frozenset(),
        warnings=(),
    )


# ---------------------------------------------------------------------------
# Tests for construction and field types
# ---------------------------------------------------------------------------


class TestDAGReachabilitySetConstruction:
    """Tests for DAGReachabilitySet construction and field types."""

    def test_fields_are_frozensets(self, sample_reachability_set: DAGReachabilitySet):
        """jjobs, ex_scripts, ush_scripts, config_files are frozenset."""
        rs = sample_reachability_set
        assert isinstance(rs.jjobs, frozenset)
        assert isinstance(rs.ex_scripts, frozenset)
        assert isinstance(rs.ush_scripts, frozenset)
        assert isinstance(rs.config_files, frozenset)

    def test_warnings_is_tuple(self, sample_reachability_set: DAGReachabilitySet):
        """warnings field is a tuple."""
        assert isinstance(sample_reachability_set.warnings, tuple)

    def test_statistics_default_to_zero(self):
        """Statistics fields default to 0 when not provided."""
        rs = DAGReachabilitySet(
            jjobs=frozenset({"JGLOBAL_FORECAST"}),
            ex_scripts=frozenset(),
            ush_scripts=frozenset(),
            config_files=frozenset(),
            warnings=(),
        )
        assert rs.total_available_jjobs == 0
        assert rs.total_available_ex_scripts == 0
        assert rs.total_available_ush_scripts == 0
        assert rs.total_available_configs == 0

    def test_statistics_accept_custom_values(self, sample_reachability_set: DAGReachabilitySet):
        """Statistics fields store provided values."""
        rs = sample_reachability_set
        assert rs.total_available_jjobs == 92
        assert rs.total_available_ex_scripts == 50
        assert rs.total_available_ush_scripts == 120
        assert rs.total_available_configs == 30


# ---------------------------------------------------------------------------
# Tests for immutability (frozen=True)
# ---------------------------------------------------------------------------


class TestDAGReachabilitySetImmutability:
    """Tests that the dataclass is frozen (immutable after creation)."""

    def test_cannot_reassign_jjobs(self, sample_reachability_set: DAGReachabilitySet):
        """Assigning to jjobs raises an error."""
        with pytest.raises(AttributeError):
            sample_reachability_set.jjobs = frozenset()  # type: ignore[misc]

    def test_cannot_reassign_warnings(self, sample_reachability_set: DAGReachabilitySet):
        """Assigning to warnings raises an error."""
        with pytest.raises(AttributeError):
            sample_reachability_set.warnings = ()  # type: ignore[misc]

    def test_cannot_reassign_statistics(self, sample_reachability_set: DAGReachabilitySet):
        """Assigning to statistics fields raises an error."""
        with pytest.raises(AttributeError):
            sample_reachability_set.total_available_jjobs = 999  # type: ignore[misc]


# ---------------------------------------------------------------------------
# Tests for is_valid property
# ---------------------------------------------------------------------------


class TestDAGReachabilitySetIsValid:
    """Tests for the is_valid property."""

    def test_valid_when_jjobs_present(self, sample_reachability_set: DAGReachabilitySet):
        """is_valid returns True when at least one J-Job is present."""
        assert sample_reachability_set.is_valid is True

    def test_invalid_when_no_jjobs(self, empty_reachability_set: DAGReachabilitySet):
        """is_valid returns False when jjobs is empty."""
        assert empty_reachability_set.is_valid is False

    def test_valid_with_single_jjob(self):
        """is_valid returns True with exactly one J-Job."""
        rs = DAGReachabilitySet(
            jjobs=frozenset({"JGLOBAL_FORECAST"}),
            ex_scripts=frozenset(),
            ush_scripts=frozenset(),
            config_files=frozenset(),
            warnings=(),
        )
        assert rs.is_valid is True


# ---------------------------------------------------------------------------
# Tests for contains_* helper methods
# ---------------------------------------------------------------------------


class TestDAGReachabilitySetContains:
    """Tests for the contains_* helper methods."""

    def test_contains_jjob_present(self, sample_reachability_set: DAGReachabilitySet):
        """contains_jjob returns True for a present J-Job."""
        assert sample_reachability_set.contains_jjob("JGLOBAL_FORECAST") is True

    def test_contains_jjob_absent(self, sample_reachability_set: DAGReachabilitySet):
        """contains_jjob returns False for an absent J-Job."""
        assert sample_reachability_set.contains_jjob("JGFS_WAVE_POST") is False

    def test_contains_ex_script_present(self, sample_reachability_set: DAGReachabilitySet):
        """contains_ex_script returns True for a present ex-script."""
        assert sample_reachability_set.contains_ex_script("exglobal_forecast.sh") is True

    def test_contains_ex_script_absent(self, sample_reachability_set: DAGReachabilitySet):
        """contains_ex_script returns False for an absent ex-script."""
        assert sample_reachability_set.contains_ex_script("exgfs_wave.sh") is False

    def test_contains_ush_script_present(self, sample_reachability_set: DAGReachabilitySet):
        """contains_ush_script returns True for a present ush script."""
        assert sample_reachability_set.contains_ush_script("forecast_predet.sh") is True

    def test_contains_ush_script_absent(self, sample_reachability_set: DAGReachabilitySet):
        """contains_ush_script returns False for an absent ush script."""
        assert sample_reachability_set.contains_ush_script("nonexistent.sh") is False

    def test_contains_config_present(self, sample_reachability_set: DAGReachabilitySet):
        """contains_config returns True for a present config file."""
        assert sample_reachability_set.contains_config("config.base.j2") is True

    def test_contains_config_absent(self, sample_reachability_set: DAGReachabilitySet):
        """contains_config returns False for an absent config file."""
        assert sample_reachability_set.contains_config("config.wave.j2") is False
