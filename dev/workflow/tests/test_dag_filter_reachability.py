"""Unit tests for DAGFilter.compute_reachability() orchestrator.

Tests the full pipeline wiring of all four layers, statistics population,
and the returned frozen DAGReachabilitySet.

Validates: Requirements 1.1, 2.1, 3.1, 4.1, 9.1, 9.2, 9.3, 9.4
"""

from __future__ import annotations

import os
import sys
from pathlib import Path

import pytest

sys.path.insert(0, os.path.join(os.path.dirname(__file__), ".."))

from deployment.dag_filter import DAGFilter, DAGReachabilitySet
from deployment.pipeline import PipelineError


# ---------------------------------------------------------------------------
# Fixtures
# ---------------------------------------------------------------------------


@pytest.fixture
def full_dev_root(tmp_path: Path) -> Path:
    """Create a complete dev/ directory structure for end-to-end testing."""
    # Create jobs directory with multiple J-Jobs
    jobs_dir = tmp_path / "jobs"
    jobs_dir.mkdir()
    (jobs_dir / "JGLOBAL_FORECAST").write_text(
        '#!/bin/bash\n'
        'source "${HOMEglobal}/ush/jjob_header.sh" -e "fcst" -c "base fcst"\n'
        ': "${FORECASTSH:=${SCRglobal}/exglobal_forecast.sh}"\n'
    )
    (jobs_dir / "JGFS_ATMOS_POST").write_text(
        '#!/bin/bash\n'
        'source "${HOMEglobal}/ush/jjob_header.sh" -e "upp" -c "base upp"\n'
        '${SCRglobal}/exgfs_atmos_post.sh\n'
    )
    # Extra J-Job NOT referenced by workflow (should not appear in result)
    (jobs_dir / "JGDAS_ATMOS_ANALYSIS").write_text(
        '#!/bin/bash\n'
        'source "${HOMEglobal}/ush/jjob_header.sh" -e "anal" -c "base anal"\n'
        '${SCRglobal}/exgdas_atmos_analysis.sh\n'
    )

    # Create scripts directory with ex-scripts
    scripts_dir = tmp_path / "scripts"
    scripts_dir.mkdir()
    (scripts_dir / "exglobal_forecast.sh").write_text(
        '#!/bin/bash\n'
        'source "${USHglobal}/forecast_predet.sh"\n'
        'source "${USHglobal}/forecast_det.sh"\n'
    )
    (scripts_dir / "exgfs_atmos_post.sh").write_text(
        '#!/bin/bash\n'
        'source "${USHglobal}/atmos_post.sh"\n'
    )
    # Extra ex-script NOT referenced (should not appear in result)
    (scripts_dir / "exgdas_atmos_analysis.sh").write_text(
        '#!/bin/bash\n'
        'source "${USHglobal}/analysis_helper.sh"\n'
    )

    # Create ush directory with scripts
    ush_dir = tmp_path / "ush"
    ush_dir.mkdir()
    (ush_dir / "forecast_predet.sh").write_text(
        '#!/bin/bash\n'
        'source "${USHglobal}/common_utils.sh"\n'
    )
    (ush_dir / "forecast_det.sh").write_text("#!/bin/bash\n")
    (ush_dir / "common_utils.sh").write_text("#!/bin/bash\n")
    (ush_dir / "atmos_post.sh").write_text("#!/bin/bash\n")
    # Extra ush scripts NOT referenced
    (ush_dir / "analysis_helper.sh").write_text("#!/bin/bash\n")
    (ush_dir / "unused_helper.sh").write_text("#!/bin/bash\n")

    # Create config directory structure
    config_dir = tmp_path / "parm" / "config" / "gfs"
    config_dir.mkdir(parents=True)
    (config_dir / "config.base.j2").write_text("# base\n")
    (config_dir / "config.base").write_text("# base plain\n")
    (config_dir / "config.com").write_text("# common\n")
    (config_dir / "config.fcst.j2").write_text("# forecast\n")
    (config_dir / "config.upp").write_text("# UPP\n")
    (config_dir / "config.anal").write_text("# analysis\n")
    (config_dir / "config.resources").write_text("# resources\n")
    (config_dir / "config.resources.HERA").write_text("# HERA resources\n")

    return tmp_path


@pytest.fixture
def workflow_yaml_two_tasks() -> dict:
    """Workflow YAML referencing two J-Jobs (forecast + post)."""
    return {
        "suite": {"name": "gfs_v17"},
        "families": [
            {
                "name": "forecast",
                "tasks": [
                    {"name": "forecast", "jjob": "JGLOBAL_FORECAST"},
                ],
            },
            {
                "name": "post",
                "tasks": [
                    {"name": "atmos_post", "jjob": "JGFS_ATMOS_POST"},
                ],
            },
        ],
    }


@pytest.fixture
def workflow_yaml_single_task() -> dict:
    """Workflow YAML referencing only the forecast J-Job."""
    return {
        "suite": {"name": "gfs_v17"},
        "families": [
            {
                "name": "forecast",
                "tasks": [
                    {"name": "forecast", "jjob": "JGLOBAL_FORECAST"},
                ],
            },
        ],
    }


# ---------------------------------------------------------------------------
# Tests for compute_reachability
# ---------------------------------------------------------------------------


class TestComputeReachability:
    """Tests for DAGFilter.compute_reachability() orchestrator."""

    def test_returns_dag_reachability_set(
        self, full_dev_root: Path, workflow_yaml_two_tasks: dict
    ):
        """Returns a DAGReachabilitySet instance."""
        dag = DAGFilter(full_dev_root, workflow_yaml_two_tasks, "hera")
        result = dag.compute_reachability()
        assert isinstance(result, DAGReachabilitySet)

    def test_jjobs_are_frozenset(
        self, full_dev_root: Path, workflow_yaml_two_tasks: dict
    ):
        """Result jjobs field is a frozenset."""
        dag = DAGFilter(full_dev_root, workflow_yaml_two_tasks, "hera")
        result = dag.compute_reachability()
        assert isinstance(result.jjobs, frozenset)

    def test_ex_scripts_are_frozenset(
        self, full_dev_root: Path, workflow_yaml_two_tasks: dict
    ):
        """Result ex_scripts field is a frozenset."""
        dag = DAGFilter(full_dev_root, workflow_yaml_two_tasks, "hera")
        result = dag.compute_reachability()
        assert isinstance(result.ex_scripts, frozenset)

    def test_ush_scripts_are_frozenset(
        self, full_dev_root: Path, workflow_yaml_two_tasks: dict
    ):
        """Result ush_scripts field is a frozenset."""
        dag = DAGFilter(full_dev_root, workflow_yaml_two_tasks, "hera")
        result = dag.compute_reachability()
        assert isinstance(result.ush_scripts, frozenset)

    def test_config_files_are_frozenset(
        self, full_dev_root: Path, workflow_yaml_two_tasks: dict
    ):
        """Result config_files field is a frozenset."""
        dag = DAGFilter(full_dev_root, workflow_yaml_two_tasks, "hera")
        result = dag.compute_reachability()
        assert isinstance(result.config_files, frozenset)

    def test_warnings_are_tuple(
        self, full_dev_root: Path, workflow_yaml_two_tasks: dict
    ):
        """Result warnings field is a tuple."""
        dag = DAGFilter(full_dev_root, workflow_yaml_two_tasks, "hera")
        result = dag.compute_reachability()
        assert isinstance(result.warnings, tuple)

    def test_extracts_referenced_jjobs(
        self, full_dev_root: Path, workflow_yaml_two_tasks: dict
    ):
        """Extracts only J-Jobs referenced in the workflow YAML."""
        dag = DAGFilter(full_dev_root, workflow_yaml_two_tasks, "hera")
        result = dag.compute_reachability()
        assert result.jjobs == frozenset({"JGLOBAL_FORECAST", "JGFS_ATMOS_POST"})
        # Unreferenced J-Job should NOT be in the result
        assert "JGDAS_ATMOS_ANALYSIS" not in result.jjobs

    def test_extracts_ex_scripts_from_jjobs(
        self, full_dev_root: Path, workflow_yaml_two_tasks: dict
    ):
        """Extracts ex-scripts invoked by the reachable J-Jobs."""
        dag = DAGFilter(full_dev_root, workflow_yaml_two_tasks, "hera")
        result = dag.compute_reachability()
        assert "exglobal_forecast.sh" in result.ex_scripts
        assert "exgfs_atmos_post.sh" in result.ex_scripts
        # Unreferenced ex-script should NOT be in the result
        assert "exgdas_atmos_analysis.sh" not in result.ex_scripts

    def test_extracts_ush_scripts_transitively(
        self, full_dev_root: Path, workflow_yaml_two_tasks: dict
    ):
        """Extracts ush scripts transitively from ex-scripts."""
        dag = DAGFilter(full_dev_root, workflow_yaml_two_tasks, "hera")
        result = dag.compute_reachability()
        # forecast_predet.sh sources common_utils.sh (transitive)
        assert "forecast_predet.sh" in result.ush_scripts
        assert "forecast_det.sh" in result.ush_scripts
        assert "common_utils.sh" in result.ush_scripts
        assert "atmos_post.sh" in result.ush_scripts
        # Unreferenced ush scripts should NOT be in the result
        assert "analysis_helper.sh" not in result.ush_scripts
        assert "unused_helper.sh" not in result.ush_scripts

    def test_extracts_config_files(
        self, full_dev_root: Path, workflow_yaml_two_tasks: dict
    ):
        """Extracts config files from jjob_header -c flags."""
        dag = DAGFilter(full_dev_root, workflow_yaml_two_tasks, "hera")
        result = dag.compute_reachability()
        # Unconditional configs always present
        assert "config.base.j2" in result.config_files
        assert "config.com" in result.config_files
        # From JGLOBAL_FORECAST: -c "base fcst"
        assert "config.fcst.j2" in result.config_files
        # From JGFS_ATMOS_POST: -c "base upp"
        assert "config.upp" in result.config_files
        # Platform resource
        assert "config.resources.HERA" in result.config_files
        assert "config.resources" in result.config_files

    def test_unreferenced_configs_excluded(
        self, full_dev_root: Path, workflow_yaml_single_task: dict
    ):
        """Config files not referenced by reachable J-Jobs are excluded."""
        dag = DAGFilter(full_dev_root, workflow_yaml_single_task, "hera")
        result = dag.compute_reachability()
        # Only forecast task → no "upp" config
        assert "config.upp" not in result.config_files
        # "anal" config also not included
        assert "config.anal" not in result.config_files

    def test_statistics_total_jjobs(
        self, full_dev_root: Path, workflow_yaml_two_tasks: dict
    ):
        """Statistics count total available J-Jobs in dev/jobs/."""
        dag = DAGFilter(full_dev_root, workflow_yaml_two_tasks, "hera")
        result = dag.compute_reachability()
        # 3 J-Job files in the fixture
        assert result.total_available_jjobs == 3

    def test_statistics_total_ex_scripts(
        self, full_dev_root: Path, workflow_yaml_two_tasks: dict
    ):
        """Statistics count total available ex-scripts in dev/scripts/."""
        dag = DAGFilter(full_dev_root, workflow_yaml_two_tasks, "hera")
        result = dag.compute_reachability()
        # 3 ex*.sh files in the fixture
        assert result.total_available_ex_scripts == 3

    def test_statistics_total_ush_scripts(
        self, full_dev_root: Path, workflow_yaml_two_tasks: dict
    ):
        """Statistics count total available ush scripts in dev/ush/."""
        dag = DAGFilter(full_dev_root, workflow_yaml_two_tasks, "hera")
        result = dag.compute_reachability()
        # 6 .sh files in ush/ directory
        assert result.total_available_ush_scripts == 6

    def test_statistics_total_configs(
        self, full_dev_root: Path, workflow_yaml_two_tasks: dict
    ):
        """Statistics count total available config files recursively."""
        dag = DAGFilter(full_dev_root, workflow_yaml_two_tasks, "hera")
        result = dag.compute_reachability()
        # 8 config.* files in parm/config/ (recursive)
        assert result.total_available_configs == 8

    def test_is_valid_with_jjobs(
        self, full_dev_root: Path, workflow_yaml_two_tasks: dict
    ):
        """Result is_valid is True when jjobs are present."""
        dag = DAGFilter(full_dev_root, workflow_yaml_two_tasks, "hera")
        result = dag.compute_reachability()
        assert result.is_valid is True

    def test_result_is_immutable(
        self, full_dev_root: Path, workflow_yaml_two_tasks: dict
    ):
        """DAGReachabilitySet is frozen (immutable dataclass)."""
        dag = DAGFilter(full_dev_root, workflow_yaml_two_tasks, "hera")
        result = dag.compute_reachability()
        with pytest.raises(Exception):
            result.jjobs = frozenset()  # type: ignore[misc]

    def test_raises_on_missing_jjob(self, full_dev_root: Path):
        """Raises PipelineError when a referenced J-Job doesn't exist."""
        yaml_data = {
            "suite": {"name": "gfs_v17"},
            "families": [
                {
                    "name": "family",
                    "tasks": [{"name": "t", "jjob": "JNONEXISTENT"}],
                },
            ],
        }
        dag = DAGFilter(full_dev_root, yaml_data, "hera")
        with pytest.raises(PipelineError) as exc_info:
            dag.compute_reachability()
        assert "JNONEXISTENT" in str(exc_info.value)

    def test_raises_on_missing_ex_script(self, full_dev_root: Path):
        """Raises PipelineError when a referenced ex-script doesn't exist."""
        # Create a J-Job that references a non-existent ex-script
        jobs_dir = full_dev_root / "jobs"
        (jobs_dir / "JBAD_SCRIPT_REF").write_text(
            '#!/bin/bash\n'
            '${SCRglobal}/exnonexistent_script.sh\n'
        )
        yaml_data = {
            "suite": {"name": "gfs_v17"},
            "families": [
                {
                    "name": "family",
                    "tasks": [{"name": "t", "jjob": "JBAD_SCRIPT_REF"}],
                },
            ],
        }
        dag = DAGFilter(full_dev_root, yaml_data, "hera")
        with pytest.raises(PipelineError) as exc_info:
            dag.compute_reachability()
        assert "exnonexistent_script.sh" in str(exc_info.value)

    def test_missing_directories_return_zero_stats(self, tmp_path: Path):
        """Missing dev/ subdirectories result in 0 statistics."""
        # Minimal setup: only jobs dir with one J-Job (no scripts, ush, config)
        jobs_dir = tmp_path / "jobs"
        jobs_dir.mkdir()
        (jobs_dir / "JMINIMAL").write_text("#!/bin/bash\necho minimal\n")

        yaml_data = {
            "families": [
                {
                    "name": "family",
                    "tasks": [{"name": "t", "jjob": "JMINIMAL"}],
                },
            ],
        }
        dag = DAGFilter(tmp_path, yaml_data, "hera")
        result = dag.compute_reachability()
        # No scripts/ directory → 0 ex-scripts available
        assert result.total_available_ex_scripts == 0
        # No ush/ directory → 0 ush scripts available
        assert result.total_available_ush_scripts == 0
        # No parm/config/ directory → 0 configs available
        assert result.total_available_configs == 0
        # jobs/ has 1 file
        assert result.total_available_jjobs == 1

    def test_empty_workflow_returns_empty_sets(self, full_dev_root: Path):
        """Empty workflow YAML returns empty reachability sets."""
        yaml_data: dict = {"families": []}
        dag = DAGFilter(full_dev_root, yaml_data, "hera")
        result = dag.compute_reachability()
        assert result.jjobs == frozenset()
        assert result.ex_scripts == frozenset()
        assert result.ush_scripts == frozenset()
        # Config files still include unconditional ones
        assert "config.base.j2" in result.config_files
        assert "config.com" in result.config_files

    def test_contains_helper_methods(
        self, full_dev_root: Path, workflow_yaml_two_tasks: dict
    ):
        """contains_* helper methods work on the result."""
        dag = DAGFilter(full_dev_root, workflow_yaml_two_tasks, "hera")
        result = dag.compute_reachability()
        assert result.contains_jjob("JGLOBAL_FORECAST") is True
        assert result.contains_jjob("JNONEXISTENT") is False
        assert result.contains_ex_script("exglobal_forecast.sh") is True
        assert result.contains_ush_script("forecast_predet.sh") is True
        assert result.contains_config("config.fcst.j2") is True
