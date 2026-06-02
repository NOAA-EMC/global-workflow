"""Unit tests for SizeReductionReport and _log_size_reduction helper.

Validates that the size reduction reporting correctly extracts counts
from a DAGReachabilitySet and logs them.

Traces to: Requirements 9.1, 9.2, 9.3, 9.4
"""

from __future__ import annotations

import logging
import os
import sys
from pathlib import Path
from unittest.mock import MagicMock

import pytest

sys.path.insert(0, os.path.join(os.path.dirname(__file__), ".."))

from deployment.pipeline import SizeReductionReport, _log_size_reduction


class TestSizeReductionReport:
    """Tests for the SizeReductionReport dataclass."""

    def test_dataclass_fields(self):
        """Report stores staged and total counts for all four categories."""
        report = SizeReductionReport(
            staged_jjobs=5,
            total_jjobs=92,
            staged_ex_scripts=3,
            total_ex_scripts=45,
            staged_ush_scripts=10,
            total_ush_scripts=80,
            staged_configs=8,
            total_configs=30,
        )
        assert report.staged_jjobs == 5
        assert report.total_jjobs == 92
        assert report.staged_ex_scripts == 3
        assert report.total_ex_scripts == 45
        assert report.staged_ush_scripts == 10
        assert report.total_ush_scripts == 80
        assert report.staged_configs == 8
        assert report.total_configs == 30

    def test_log_outputs_all_categories(self, caplog):
        """log() emits INFO lines for all four artifact categories."""
        report = SizeReductionReport(
            staged_jjobs=2,
            total_jjobs=10,
            staged_ex_scripts=1,
            total_ex_scripts=8,
            staged_ush_scripts=4,
            total_ush_scripts=20,
            staged_configs=3,
            total_configs=15,
        )
        with caplog.at_level(logging.INFO):
            report.log()

        log_text = caplog.text
        assert "DAG Filter Results:" in log_text
        assert "2/10" in log_text  # J-Jobs
        assert "1/8" in log_text   # Ex-Scripts
        assert "4/20" in log_text  # Ush Scripts
        assert "3/15" in log_text  # Configs

    def test_log_zero_counts(self, caplog):
        """log() handles zero staged counts gracefully."""
        report = SizeReductionReport(
            staged_jjobs=0,
            total_jjobs=50,
            staged_ex_scripts=0,
            total_ex_scripts=30,
            staged_ush_scripts=0,
            total_ush_scripts=40,
            staged_configs=0,
            total_configs=20,
        )
        with caplog.at_level(logging.INFO):
            report.log()

        log_text = caplog.text
        assert "0/50" in log_text
        assert "0/30" in log_text
        assert "0/40" in log_text
        assert "0/20" in log_text


class TestLogSizeReduction:
    """Tests for the _log_size_reduction helper function."""

    def _make_reachability(
        self,
        jjobs=frozenset(),
        ex_scripts=frozenset(),
        ush_scripts=frozenset(),
        config_files=frozenset(),
        total_jjobs=0,
        total_ex_scripts=0,
        total_ush_scripts=0,
        total_configs=0,
    ):
        """Create a mock DAGReachabilitySet with the given fields."""
        mock = MagicMock()
        mock.jjobs = jjobs
        mock.ex_scripts = ex_scripts
        mock.ush_scripts = ush_scripts
        mock.config_files = config_files
        mock.total_available_jjobs = total_jjobs
        mock.total_available_ex_scripts = total_ex_scripts
        mock.total_available_ush_scripts = total_ush_scripts
        mock.total_available_configs = total_configs
        return mock

    def test_returns_report(self, tmp_path):
        """_log_size_reduction returns a SizeReductionReport instance."""
        reachability = self._make_reachability(
            jjobs=frozenset({"JGLOBAL_FORECAST", "JGFS_ATMOS_POST"}),
            ex_scripts=frozenset({"exglobal_forecast.sh"}),
            ush_scripts=frozenset({"forecast_predet.sh", "forecast_det.sh"}),
            config_files=frozenset({"config.base.j2", "config.fcst.j2"}),
            total_jjobs=92,
            total_ex_scripts=45,
            total_ush_scripts=80,
            total_configs=30,
        )
        report = _log_size_reduction(tmp_path, reachability)

        assert isinstance(report, SizeReductionReport)
        assert report.staged_jjobs == 2
        assert report.total_jjobs == 92
        assert report.staged_ex_scripts == 1
        assert report.total_ex_scripts == 45
        assert report.staged_ush_scripts == 2
        assert report.total_ush_scripts == 80
        assert report.staged_configs == 2
        assert report.total_configs == 30

    def test_logs_reduction_stats(self, tmp_path, caplog):
        """_log_size_reduction logs the reduction statistics."""
        reachability = self._make_reachability(
            jjobs=frozenset({"JGLOBAL_FORECAST"}),
            ex_scripts=frozenset({"exglobal_forecast.sh"}),
            ush_scripts=frozenset({"forecast_predet.sh"}),
            config_files=frozenset({"config.base.j2"}),
            total_jjobs=50,
            total_ex_scripts=25,
            total_ush_scripts=40,
            total_configs=15,
        )
        with caplog.at_level(logging.INFO):
            _log_size_reduction(tmp_path, reachability)

        log_text = caplog.text
        assert "1/50" in log_text
        assert "1/25" in log_text
        assert "1/40" in log_text
        assert "1/15" in log_text

    def test_empty_reachability_set(self, tmp_path, caplog):
        """_log_size_reduction handles empty reachability sets."""
        reachability = self._make_reachability(
            jjobs=frozenset(),
            ex_scripts=frozenset(),
            ush_scripts=frozenset(),
            config_files=frozenset(),
            total_jjobs=92,
            total_ex_scripts=45,
            total_ush_scripts=80,
            total_configs=30,
        )
        with caplog.at_level(logging.INFO):
            report = _log_size_reduction(tmp_path, reachability)

        assert report.staged_jjobs == 0
        assert report.staged_ex_scripts == 0
        assert report.staged_ush_scripts == 0
        assert report.staged_configs == 0
