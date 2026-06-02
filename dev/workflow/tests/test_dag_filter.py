"""Unit tests for the DAGFilter class — Layers 1, 2, and 4.

Tests the constructor, extract_jjobs_from_yaml() (Layer 1),
extract_ex_scripts() (Layer 2), extract_config_files() (Layer 4),
and multi-layer integration scenarios.

Validates: Requirements 1.1–1.5, 2.1–2.4, 3.1–3.5, 4.1–4.5
"""

from __future__ import annotations

import os
import sys
from pathlib import Path

import pytest

sys.path.insert(0, os.path.join(os.path.dirname(__file__), ".."))

from deployment.dag_filter import DAGFilter
from deployment.pipeline import PipelineError


# ---------------------------------------------------------------------------
# Fixtures
# ---------------------------------------------------------------------------


@pytest.fixture
def tmp_dev_root(tmp_path: Path) -> Path:
    """Create a minimal dev/ directory structure with some J-Jobs."""
    jobs_dir = tmp_path / "jobs"
    jobs_dir.mkdir()
    # Create some J-Job files
    (jobs_dir / "JGLOBAL_FORECAST").write_text("#!/bin/bash\necho forecast\n")
    (jobs_dir / "JGFS_ATMOS_POST").write_text("#!/bin/bash\necho post\n")
    (jobs_dir / "JGDAS_ATMOS_ANALYSIS").write_text("#!/bin/bash\necho analysis\n")
    return tmp_path


@pytest.fixture
def simple_workflow_yaml() -> dict:
    """A minimal workflow YAML with two families and three tasks."""
    return {
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
                    {"name": "analysis", "jjob": "JGDAS_ATMOS_ANALYSIS"},
                ],
            },
        ]
    }


@pytest.fixture
def empty_workflow_yaml() -> dict:
    """A workflow YAML with no families."""
    return {}


@pytest.fixture
def workflow_yaml_no_jjob() -> dict:
    """A workflow YAML with tasks that have no jjob field."""
    return {
        "families": [
            {
                "name": "prep",
                "tasks": [
                    {"name": "prep_task"},  # no jjob field
                    {"name": "another_task", "jjob": None},  # jjob is None
                ],
            },
        ]
    }


# ---------------------------------------------------------------------------
# Tests for __init__
# ---------------------------------------------------------------------------


class TestDAGFilterInit:
    """Tests for DAGFilter constructor."""

    def test_stores_dev_root(self, tmp_dev_root: Path, simple_workflow_yaml: dict):
        """Constructor stores dev_root as provided."""
        dag = DAGFilter(tmp_dev_root, simple_workflow_yaml, "hera")
        assert dag.dev_root == tmp_dev_root

    def test_stores_workflow_yaml(self, tmp_dev_root: Path, simple_workflow_yaml: dict):
        """Constructor stores workflow_yaml dict."""
        dag = DAGFilter(tmp_dev_root, simple_workflow_yaml, "hera")
        assert dag.workflow_yaml is simple_workflow_yaml

    def test_platform_uppercased(self, tmp_dev_root: Path, simple_workflow_yaml: dict):
        """Constructor uppercases the platform string."""
        dag = DAGFilter(tmp_dev_root, simple_workflow_yaml, "hera")
        assert dag.platform == "HERA"

    def test_platform_already_upper(self, tmp_dev_root: Path, simple_workflow_yaml: dict):
        """Constructor handles already-uppercase platform."""
        dag = DAGFilter(tmp_dev_root, simple_workflow_yaml, "WCOSS2")
        assert dag.platform == "WCOSS2"

    def test_warnings_initialized_empty(self, tmp_dev_root: Path, simple_workflow_yaml: dict):
        """Constructor initializes _warnings as empty list."""
        dag = DAGFilter(tmp_dev_root, simple_workflow_yaml, "hera")
        assert dag._warnings == []


# ---------------------------------------------------------------------------
# Tests for extract_jjobs_from_yaml
# ---------------------------------------------------------------------------


class TestExtractJjobsFromYaml:
    """Tests for DAGFilter.extract_jjobs_from_yaml()."""

    def test_extracts_all_referenced_jjobs(
        self, tmp_dev_root: Path, simple_workflow_yaml: dict
    ):
        """Extracts all jjob values from families[].tasks[].jjob."""
        dag = DAGFilter(tmp_dev_root, simple_workflow_yaml, "hera")
        result = dag.extract_jjobs_from_yaml()
        assert result == {"JGLOBAL_FORECAST", "JGFS_ATMOS_POST", "JGDAS_ATMOS_ANALYSIS"}

    def test_returns_set_type(self, tmp_dev_root: Path, simple_workflow_yaml: dict):
        """Returns a set of strings."""
        dag = DAGFilter(tmp_dev_root, simple_workflow_yaml, "hera")
        result = dag.extract_jjobs_from_yaml()
        assert isinstance(result, set)

    def test_empty_workflow_returns_empty_set(
        self, tmp_dev_root: Path, empty_workflow_yaml: dict
    ):
        """Empty workflow YAML (no families) returns empty set."""
        dag = DAGFilter(tmp_dev_root, empty_workflow_yaml, "hera")
        result = dag.extract_jjobs_from_yaml()
        assert result == set()

    def test_tasks_without_jjob_skipped(
        self, tmp_dev_root: Path, workflow_yaml_no_jjob: dict
    ):
        """Tasks with no jjob field or jjob=None are skipped."""
        dag = DAGFilter(tmp_dev_root, workflow_yaml_no_jjob, "hera")
        result = dag.extract_jjobs_from_yaml()
        assert result == set()

    def test_deduplicates_jjobs(self, tmp_dev_root: Path):
        """Same jjob referenced by multiple tasks appears once."""
        yaml_data = {
            "families": [
                {
                    "name": "family1",
                    "tasks": [
                        {"name": "task1", "jjob": "JGLOBAL_FORECAST"},
                        {"name": "task2", "jjob": "JGLOBAL_FORECAST"},
                    ],
                },
            ]
        }
        dag = DAGFilter(tmp_dev_root, yaml_data, "hera")
        result = dag.extract_jjobs_from_yaml()
        assert result == {"JGLOBAL_FORECAST"}

    def test_raises_pipeline_error_for_missing_jjob(self, tmp_dev_root: Path):
        """Raises PipelineError when a referenced J-Job doesn't exist in dev/jobs/."""
        yaml_data = {
            "families": [
                {
                    "name": "family1",
                    "tasks": [
                        {"name": "task1", "jjob": "JGLOBAL_FORECAST"},
                        {"name": "task2", "jjob": "JNONEXISTENT_JOB"},
                    ],
                },
            ]
        }
        dag = DAGFilter(tmp_dev_root, yaml_data, "hera")
        with pytest.raises(PipelineError) as exc_info:
            dag.extract_jjobs_from_yaml()
        assert "JNONEXISTENT_JOB" in str(exc_info.value)
        assert "dag_filter" == exc_info.value.stage

    def test_error_message_includes_path(self, tmp_dev_root: Path):
        """PipelineError message includes the expected file path."""
        yaml_data = {
            "families": [
                {
                    "name": "family1",
                    "tasks": [{"name": "task1", "jjob": "JMISSING_JOB"}],
                },
            ]
        }
        dag = DAGFilter(tmp_dev_root, yaml_data, "hera")
        with pytest.raises(PipelineError) as exc_info:
            dag.extract_jjobs_from_yaml()
        expected_path = str(tmp_dev_root / "jobs" / "JMISSING_JOB")
        assert expected_path in str(exc_info.value)

    def test_multiple_families_traversed(self, tmp_dev_root: Path):
        """All families are traversed, not just the first."""
        yaml_data = {
            "families": [
                {
                    "name": "family1",
                    "tasks": [{"name": "t1", "jjob": "JGLOBAL_FORECAST"}],
                },
                {
                    "name": "family2",
                    "tasks": [{"name": "t2", "jjob": "JGFS_ATMOS_POST"}],
                },
                {
                    "name": "family3",
                    "tasks": [{"name": "t3", "jjob": "JGDAS_ATMOS_ANALYSIS"}],
                },
            ]
        }
        dag = DAGFilter(tmp_dev_root, yaml_data, "hera")
        result = dag.extract_jjobs_from_yaml()
        assert len(result) == 3

    def test_families_with_empty_tasks(self, tmp_dev_root: Path):
        """Families with empty tasks list are handled gracefully."""
        yaml_data = {
            "families": [
                {"name": "empty_family", "tasks": []},
                {
                    "name": "real_family",
                    "tasks": [{"name": "t1", "jjob": "JGLOBAL_FORECAST"}],
                },
            ]
        }
        dag = DAGFilter(tmp_dev_root, yaml_data, "hera")
        result = dag.extract_jjobs_from_yaml()
        assert result == {"JGLOBAL_FORECAST"}


# ---------------------------------------------------------------------------
# Tests for extract_config_files (Layer 4)
# ---------------------------------------------------------------------------

from deployment.dag_filter import _JJOB_HEADER_PATTERN, _UNCONDITIONAL_CONFIGS


@pytest.fixture
def config_dev_root(tmp_path: Path) -> Path:
    """Create a dev/ directory with jobs and config files for Layer 4 tests."""
    # Create jobs directory
    jobs_dir = tmp_path / "jobs"
    jobs_dir.mkdir()

    # Create a J-Job with jjob_header.sh -c pattern
    (jobs_dir / "JGLOBAL_FORECAST").write_text(
        '#!/bin/bash\n'
        'source "${HOMEglobal}/ush/jjob_header.sh" -e "fcst" -c "base fcst"\n'
    )
    (jobs_dir / "JGLOBAL_ATMOS_UPP").write_text(
        '#!/bin/bash\n'
        'source "${HOMEglobal}/ush/jjob_header.sh" -e "upp" -c "base upp"\n'
    )
    # J-Job with multiple jjob_header invocations (conditional)
    (jobs_dir / "JGLOBAL_FORECAST_ENS").write_text(
        '#!/bin/bash\n'
        'if [[ 10#${ENSMEM:--1} -ge 0 ]]; then\n'
        '    source "${HOMEglobal}/ush/jjob_header.sh" -e "efcs" -c "base fcst efcs"\n'
        'else\n'
        '    source "${HOMEglobal}/ush/jjob_header.sh" -e "fcst" -c "base fcst"\n'
        'fi\n'
    )
    # J-Job with no -c flag
    (jobs_dir / "JGLOBAL_CLEANUP").write_text(
        '#!/bin/bash\n'
        'echo "no jjob_header here"\n'
    )

    # Create config directory structure (gfs app)
    config_dir = tmp_path / "parm" / "config" / "gfs"
    config_dir.mkdir(parents=True)
    (config_dir / "config.base.j2").write_text("# base config\n")
    (config_dir / "config.base").write_text("# base config plain\n")
    (config_dir / "config.com").write_text("# common config\n")
    (config_dir / "config.fcst.j2").write_text("# forecast config\n")
    (config_dir / "config.upp").write_text("# UPP config\n")
    (config_dir / "config.efcs").write_text("# ensemble forecast config\n")
    (config_dir / "config.resources").write_text("# base resources\n")
    (config_dir / "config.resources.HERA").write_text("# HERA resources\n")
    (config_dir / "config.resources.WCOSS2").write_text("# WCOSS2 resources\n")

    return tmp_path


@pytest.fixture
def gfs_workflow_yaml() -> dict:
    """Workflow YAML with suite name indicating gfs app."""
    return {
        "suite": {"name": "gfs_v17_fcst_only"},
        "families": [
            {
                "name": "forecast",
                "tasks": [
                    {"name": "fcst", "jjob": "JGLOBAL_FORECAST"},
                ],
            },
        ],
    }


class TestExtractConfigFiles:
    """Tests for DAGFilter.extract_config_files() — Layer 4."""

    def test_unconditional_configs_always_included(
        self, config_dev_root: Path, gfs_workflow_yaml: dict
    ):
        """config.base.j2, config.base, and config.com are always included."""
        dag = DAGFilter(config_dev_root, gfs_workflow_yaml, "hera")
        result = dag.extract_config_files(set())  # empty jjobs
        assert "config.base.j2" in result
        assert "config.base" in result
        assert "config.com" in result

    def test_extracts_configs_from_jjob_header_c_flag(
        self, config_dev_root: Path, gfs_workflow_yaml: dict
    ):
        """Extracts config basenames from jjob_header.sh -c flag."""
        dag = DAGFilter(config_dev_root, gfs_workflow_yaml, "hera")
        result = dag.extract_config_files({"JGLOBAL_FORECAST"})
        # -c "base fcst" → config.base.j2 (prefers .j2) and config.fcst.j2
        assert "config.base.j2" in result
        assert "config.fcst.j2" in result

    def test_prefers_j2_variant(
        self, config_dev_root: Path, gfs_workflow_yaml: dict
    ):
        """When both config.X.j2 and config.X exist, prefers .j2."""
        dag = DAGFilter(config_dev_root, gfs_workflow_yaml, "hera")
        result = dag.extract_config_files({"JGLOBAL_FORECAST"})
        # "base" maps to config.base.j2 (not config.base) because .j2 is checked first
        assert "config.base.j2" in result

    def test_falls_back_to_plain_config(
        self, config_dev_root: Path, gfs_workflow_yaml: dict
    ):
        """Falls back to config.X when config.X.j2 doesn't exist."""
        dag = DAGFilter(config_dev_root, gfs_workflow_yaml, "hera")
        result = dag.extract_config_files({"JGLOBAL_ATMOS_UPP"})
        # "upp" → config.upp (no .j2 variant exists)
        assert "config.upp" in result

    def test_multiple_jjob_header_invocations(
        self, config_dev_root: Path, gfs_workflow_yaml: dict
    ):
        """Handles J-Jobs with multiple jjob_header.sh invocations."""
        dag = DAGFilter(config_dev_root, gfs_workflow_yaml, "hera")
        result = dag.extract_config_files({"JGLOBAL_FORECAST_ENS"})
        # Two invocations: -c "base fcst efcs" and -c "base fcst"
        assert "config.fcst.j2" in result
        assert "config.efcs" in result

    def test_jjob_without_c_flag_contributes_nothing(
        self, config_dev_root: Path, gfs_workflow_yaml: dict
    ):
        """J-Jobs without jjob_header -c flag don't add extra configs."""
        dag = DAGFilter(config_dev_root, gfs_workflow_yaml, "hera")
        result = dag.extract_config_files({"JGLOBAL_CLEANUP"})
        # Only unconditional configs + resources
        expected_minimum = _UNCONDITIONAL_CONFIGS | {"config.resources"}
        assert result.issuperset(expected_minimum)
        # Should not have any extra configs beyond unconditional + resources + platform
        extra = result - expected_minimum - {"config.resources.HERA"}
        assert extra == set()

    def test_platform_specific_resource_included(
        self, config_dev_root: Path, gfs_workflow_yaml: dict
    ):
        """Platform-specific resource file is included when it exists."""
        dag = DAGFilter(config_dev_root, gfs_workflow_yaml, "hera")
        result = dag.extract_config_files({"JGLOBAL_FORECAST"})
        assert "config.resources.HERA" in result
        assert "config.resources" in result

    def test_platform_resource_case_insensitive_input(
        self, config_dev_root: Path, gfs_workflow_yaml: dict
    ):
        """Platform is uppercased for resource file lookup."""
        dag = DAGFilter(config_dev_root, gfs_workflow_yaml, "hera")
        result = dag.extract_config_files({"JGLOBAL_FORECAST"})
        # Platform "hera" → uppercased to "HERA" → config.resources.HERA
        assert "config.resources.HERA" in result

    def test_missing_platform_resource_not_included(
        self, config_dev_root: Path, gfs_workflow_yaml: dict
    ):
        """Platform resource file not included if it doesn't exist."""
        dag = DAGFilter(config_dev_root, gfs_workflow_yaml, "DERECHO")
        # config.resources.DERECHO doesn't exist in fixture
        # (we didn't create it in config_dev_root)
        # But wait — let me check: we only created HERA and WCOSS2
        result = dag.extract_config_files({"JGLOBAL_FORECAST"})
        assert "config.resources.DERECHO" not in result
        # Base resources always included
        assert "config.resources" in result

    def test_nonexistent_config_basename_skipped(
        self, config_dev_root: Path, gfs_workflow_yaml: dict
    ):
        """Config basenames that don't map to existing files are skipped."""
        # Create a J-Job referencing a config that doesn't exist
        jobs_dir = config_dev_root / "jobs"
        (jobs_dir / "JGLOBAL_SPECIAL").write_text(
            '#!/bin/bash\n'
            'source "${HOMEglobal}/ush/jjob_header.sh" -e "special" -c "base nonexistent"\n'
        )
        dag = DAGFilter(config_dev_root, gfs_workflow_yaml, "hera")
        result = dag.extract_config_files({"JGLOBAL_SPECIAL"})
        # "nonexistent" has no config.nonexistent.j2 or config.nonexistent
        assert "config.nonexistent.j2" not in result
        assert "config.nonexistent" not in result
        # But "base" still resolves
        assert "config.base.j2" in result


class TestDetectApp:
    """Tests for DAGFilter._detect_app() helper."""

    def test_detects_from_suite_name(self, config_dev_root: Path):
        """Detects app from suite.name prefix."""
        yaml_data = {"suite": {"name": "gfs_v17_fcst_only"}}
        dag = DAGFilter(config_dev_root, yaml_data, "hera")
        assert dag._detect_app() == "gfs"

    def test_detects_from_app_key(self, config_dev_root: Path):
        """Detects app from explicit 'app' key in workflow_yaml."""
        yaml_data = {"app": "gfs_forecast_only"}
        dag = DAGFilter(config_dev_root, yaml_data, "hera")
        assert dag._detect_app() == "gfs"

    def test_detects_from_net_key(self, config_dev_root: Path):
        """Detects app from NET key when suite is missing."""
        yaml_data = {"NET": "gfs"}
        dag = DAGFilter(config_dev_root, yaml_data, "hera")
        assert dag._detect_app() == "gfs"

    def test_fallback_to_gfs(self, tmp_path: Path):
        """Falls back to 'gfs' when no app can be determined."""
        # No config directories at all
        (tmp_path / "parm" / "config").mkdir(parents=True)
        yaml_data = {}
        dag = DAGFilter(tmp_path, yaml_data, "hera")
        assert dag._detect_app() == "gfs"

    def test_direct_app_match(self, config_dev_root: Path):
        """Direct app name matching a config directory."""
        yaml_data = {"app": "gfs"}
        dag = DAGFilter(config_dev_root, yaml_data, "hera")
        assert dag._detect_app() == "gfs"


class TestJjobHeaderPattern:
    """Tests for the _JJOB_HEADER_PATTERN regex."""

    def test_matches_simple_c_flag(self):
        """Matches jjob_header.sh -c 'base fcst' pattern."""
        line = 'source "${HOMEglobal}/ush/jjob_header.sh" -e "fcst" -c "base fcst"'
        match = _JJOB_HEADER_PATTERN.search(line)
        assert match is not None
        assert match.group("configs") == "base fcst"

    def test_matches_multiple_configs(self):
        """Matches multiple config basenames in -c flag."""
        line = 'source "${HOMEglobal}/ush/jjob_header.sh" -e "efcs" -c "base fcst efcs"'
        match = _JJOB_HEADER_PATTERN.search(line)
        assert match is not None
        assert match.group("configs") == "base fcst efcs"

    def test_no_match_without_c_flag(self):
        """Does not match when -c flag is absent."""
        line = 'source "${HOMEglobal}/ush/jjob_header.sh" -e "fcst"'
        match = _JJOB_HEADER_PATTERN.search(line)
        assert match is None

    def test_matches_with_different_spacing(self):
        """Matches with various spacing around -c."""
        line = 'jjob_header.sh  -e "test"  -c "base wave"'
        match = _JJOB_HEADER_PATTERN.search(line)
        assert match is not None
        assert match.group("configs") == "base wave"


# ---------------------------------------------------------------------------
# Tests for extract_ex_scripts (Layer 2)
# ---------------------------------------------------------------------------

from deployment.dag_filter import _EX_SCRIPT_PATTERNS


@pytest.fixture
def ex_script_dev_root(tmp_path: Path) -> Path:
    """Create a dev/ directory with J-Jobs containing various ex-script patterns."""
    jobs_dir = tmp_path / "jobs"
    jobs_dir.mkdir()
    scripts_dir = tmp_path / "scripts"
    scripts_dir.mkdir()

    # J-Job using the FORECASTSH:= assignment pattern
    (jobs_dir / "JGLOBAL_FORECAST").write_text(
        '#!/bin/bash\n'
        ': "${FORECASTSH:=${SCRglobal}/exglobal_forecast.sh}"\n'
        '"${FORECASTSH}" && true\n'
    )
    # J-Job using direct ${SCRglobal}/ex... invocation
    (jobs_dir / "JGFS_ATMOS_POST").write_text(
        '#!/bin/bash\n'
        '${SCRglobal}/exgfs_atmos_post.sh\n'
    )
    # J-Job using export pattern
    (jobs_dir / "JGFS_WAVE_INIT").write_text(
        '#!/bin/bash\n'
        'export WAVESH="${SCRglobal}/exgfs_wave_init.sh"\n'
    )
    # J-Job with no ex-script reference
    (jobs_dir / "JGLOBAL_CLEANUP").write_text(
        '#!/bin/bash\n'
        'echo "cleanup only"\n'
    )
    # J-Job with multiple ex-script references (conditional)
    (jobs_dir / "JGLOBAL_MULTI").write_text(
        '#!/bin/bash\n'
        ': "${MAINSH:=${SCRglobal}/exglobal_main.sh}"\n'
        '${SCRglobal}/exglobal_helper.sh\n'
    )

    # Create corresponding ex-scripts
    (scripts_dir / "exglobal_forecast.sh").write_text("#!/bin/bash\n")
    (scripts_dir / "exgfs_atmos_post.sh").write_text("#!/bin/bash\n")
    (scripts_dir / "exgfs_wave_init.sh").write_text("#!/bin/bash\n")
    (scripts_dir / "exglobal_main.sh").write_text("#!/bin/bash\n")
    (scripts_dir / "exglobal_helper.sh").write_text("#!/bin/bash\n")

    return tmp_path


class TestExScriptPatterns:
    """Tests for the _EX_SCRIPT_PATTERNS regex list."""

    def test_matches_scr_global_pattern(self):
        """Matches ${SCRglobal}/exaaaaa.sh pattern."""
        line = '${SCRglobal}/exglobal_forecast.sh'
        matches = []
        for pattern in _EX_SCRIPT_PATTERNS:
            for m in pattern.finditer(line):
                matches.append(m.group("script"))
        assert "exglobal_forecast.sh" in matches

    def test_matches_forecastsh_assignment(self):
        """Matches : "${FORECASTSH:=${SCRglobal}/exglobal_forecast.sh}" pattern."""
        line = ': "${FORECASTSH:=${SCRglobal}/exglobal_forecast.sh}"'
        matches = []
        for pattern in _EX_SCRIPT_PATTERNS:
            for m in pattern.finditer(line):
                matches.append(m.group("script"))
        assert "exglobal_forecast.sh" in matches

    def test_matches_export_pattern(self):
        """Matches export XXXSH="${SCRglobal}/exaaaaa.sh" pattern."""
        line = 'export WAVESH="${SCRglobal}/exgfs_wave_init.sh"'
        matches = []
        for pattern in _EX_SCRIPT_PATTERNS:
            for m in pattern.finditer(line):
                matches.append(m.group("script"))
        assert "exgfs_wave_init.sh" in matches

    def test_matches_scr_model_variant(self):
        """Matches ${SCRgfs}/exgfs_something.sh pattern."""
        line = '${SCRgfs}/exgfs_atmos_post.sh'
        matches = []
        for pattern in _EX_SCRIPT_PATTERNS:
            for m in pattern.finditer(line):
                matches.append(m.group("script"))
        assert "exgfs_atmos_post.sh" in matches

    def test_matches_python_ex_script(self):
        """Matches ex-scripts with .py extension."""
        line = '${SCRglobal}/exglobal_archive.py'
        matches = []
        for pattern in _EX_SCRIPT_PATTERNS:
            for m in pattern.finditer(line):
                matches.append(m.group("script"))
        assert "exglobal_archive.py" in matches

    def test_no_match_for_non_ex_script(self):
        """Does not match scripts that don't start with 'ex'."""
        line = '${SCRglobal}/forecast_predet.sh'
        matches = []
        for pattern in _EX_SCRIPT_PATTERNS:
            for m in pattern.finditer(line):
                matches.append(m.group("script"))
        assert matches == []

    def test_no_match_for_comment_line(self):
        """Regex still matches in comments (filtering is done at parse level)."""
        # Note: The regex itself matches; comment filtering is done by the caller
        line = '# ${SCRglobal}/exglobal_forecast.sh'
        matches = []
        for pattern in _EX_SCRIPT_PATTERNS:
            for m in pattern.finditer(line):
                matches.append(m.group("script"))
        # Regex matches even in comments — that's expected behavior
        assert "exglobal_forecast.sh" in matches


class TestExtractExScripts:
    """Tests for DAGFilter.extract_ex_scripts() — Layer 2."""

    def test_extracts_from_forecastsh_pattern(self, ex_script_dev_root: Path):
        """Extracts ex-script from FORECASTSH:= assignment pattern."""
        yaml_data = {"families": []}
        dag = DAGFilter(ex_script_dev_root, yaml_data, "hera")
        result = dag.extract_ex_scripts({"JGLOBAL_FORECAST"})
        assert "exglobal_forecast.sh" in result

    def test_extracts_from_direct_invocation(self, ex_script_dev_root: Path):
        """Extracts ex-script from direct ${SCRglobal}/ex... invocation."""
        yaml_data = {"families": []}
        dag = DAGFilter(ex_script_dev_root, yaml_data, "hera")
        result = dag.extract_ex_scripts({"JGFS_ATMOS_POST"})
        assert "exgfs_atmos_post.sh" in result

    def test_extracts_from_export_pattern(self, ex_script_dev_root: Path):
        """Extracts ex-script from export XXXSH= pattern."""
        yaml_data = {"families": []}
        dag = DAGFilter(ex_script_dev_root, yaml_data, "hera")
        result = dag.extract_ex_scripts({"JGFS_WAVE_INIT"})
        assert "exgfs_wave_init.sh" in result

    def test_jjob_with_no_ex_script_returns_empty(self, ex_script_dev_root: Path):
        """J-Job with no ex-script pattern returns empty set."""
        yaml_data = {"families": []}
        dag = DAGFilter(ex_script_dev_root, yaml_data, "hera")
        result = dag.extract_ex_scripts({"JGLOBAL_CLEANUP"})
        assert result == set()

    def test_multiple_ex_scripts_from_single_jjob(self, ex_script_dev_root: Path):
        """Extracts multiple ex-scripts from a single J-Job."""
        yaml_data = {"families": []}
        dag = DAGFilter(ex_script_dev_root, yaml_data, "hera")
        result = dag.extract_ex_scripts({"JGLOBAL_MULTI"})
        assert "exglobal_main.sh" in result
        assert "exglobal_helper.sh" in result

    def test_multiple_jjobs_combined(self, ex_script_dev_root: Path):
        """Extracts ex-scripts from multiple J-Jobs into a single set."""
        yaml_data = {"families": []}
        dag = DAGFilter(ex_script_dev_root, yaml_data, "hera")
        result = dag.extract_ex_scripts({"JGLOBAL_FORECAST", "JGFS_ATMOS_POST"})
        assert "exglobal_forecast.sh" in result
        assert "exgfs_atmos_post.sh" in result

    def test_deduplicates_ex_scripts(self, ex_script_dev_root: Path):
        """Same ex-script referenced by multiple J-Jobs appears once."""
        # Create two J-Jobs referencing the same ex-script
        jobs_dir = ex_script_dev_root / "jobs"
        (jobs_dir / "JGLOBAL_FORECAST_V2").write_text(
            '#!/bin/bash\n'
            '${SCRglobal}/exglobal_forecast.sh\n'
        )
        yaml_data = {"families": []}
        dag = DAGFilter(ex_script_dev_root, yaml_data, "hera")
        result = dag.extract_ex_scripts({"JGLOBAL_FORECAST", "JGLOBAL_FORECAST_V2"})
        # Should appear only once
        assert result == {"exglobal_forecast.sh"}

    def test_returns_set_type(self, ex_script_dev_root: Path):
        """Returns a set of strings."""
        yaml_data = {"families": []}
        dag = DAGFilter(ex_script_dev_root, yaml_data, "hera")
        result = dag.extract_ex_scripts({"JGLOBAL_FORECAST"})
        assert isinstance(result, set)

    def test_empty_jjobs_returns_empty_set(self, ex_script_dev_root: Path):
        """Empty jjobs input returns empty set."""
        yaml_data = {"families": []}
        dag = DAGFilter(ex_script_dev_root, yaml_data, "hera")
        result = dag.extract_ex_scripts(set())
        assert result == set()

    def test_raises_pipeline_error_for_missing_ex_script(self, tmp_path: Path):
        """Raises PipelineError when referenced ex-script doesn't exist."""
        jobs_dir = tmp_path / "jobs"
        jobs_dir.mkdir()
        scripts_dir = tmp_path / "scripts"
        scripts_dir.mkdir()
        # J-Job references an ex-script that doesn't exist
        (jobs_dir / "JBAD_REF").write_text(
            '#!/bin/bash\n'
            '${SCRglobal}/exnonexistent_script.sh\n'
        )
        yaml_data = {"families": []}
        dag = DAGFilter(tmp_path, yaml_data, "hera")
        with pytest.raises(PipelineError) as exc_info:
            dag.extract_ex_scripts({"JBAD_REF"})
        assert "exnonexistent_script.sh" in str(exc_info.value)
        assert "dag_filter" == exc_info.value.stage

    def test_error_message_includes_path(self, tmp_path: Path):
        """PipelineError message includes the expected file path."""
        jobs_dir = tmp_path / "jobs"
        jobs_dir.mkdir()
        scripts_dir = tmp_path / "scripts"
        scripts_dir.mkdir()
        (jobs_dir / "JBAD_REF").write_text(
            '#!/bin/bash\n'
            '${SCRglobal}/exmissing.sh\n'
        )
        yaml_data = {"families": []}
        dag = DAGFilter(tmp_path, yaml_data, "hera")
        with pytest.raises(PipelineError) as exc_info:
            dag.extract_ex_scripts({"JBAD_REF"})
        expected_path = str(tmp_path / "scripts" / "exmissing.sh")
        assert expected_path in str(exc_info.value)


# ---------------------------------------------------------------------------
# Integration tests — multi-layer scenarios
# ---------------------------------------------------------------------------


@pytest.fixture
def integration_dev_root(tmp_path: Path) -> Path:
    """Create a complete dev/ directory for multi-layer integration tests."""
    # Jobs
    jobs_dir = tmp_path / "jobs"
    jobs_dir.mkdir()
    (jobs_dir / "JGLOBAL_FORECAST").write_text(
        '#!/bin/bash\n'
        'source "${HOMEglobal}/ush/jjob_header.sh" -e "fcst" -c "base fcst"\n'
        ': "${FORECASTSH:=${SCRglobal}/exglobal_forecast.sh}"\n'
        '"${FORECASTSH}" && true\n'
    )
    (jobs_dir / "JGFS_ATMOS_POST").write_text(
        '#!/bin/bash\n'
        'source "${HOMEglobal}/ush/jjob_header.sh" -e "upp" -c "base upp"\n'
        '${SCRglobal}/exgfs_atmos_post.sh\n'
    )
    # Unreferenced J-Job
    (jobs_dir / "JGDAS_ATMOS_ANALYSIS").write_text(
        '#!/bin/bash\n'
        'source "${HOMEglobal}/ush/jjob_header.sh" -e "anal" -c "base anal"\n'
        '${SCRglobal}/exgdas_atmos_analysis.sh\n'
    )

    # Scripts
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
    (scripts_dir / "exgdas_atmos_analysis.sh").write_text(
        '#!/bin/bash\n'
        'source "${USHglobal}/analysis_helper.sh"\n'
    )

    # Ush scripts (with transitive deps)
    ush_dir = tmp_path / "ush"
    ush_dir.mkdir()
    (ush_dir / "forecast_predet.sh").write_text(
        '#!/bin/bash\n'
        'source "${USHglobal}/common_utils.sh"\n'
    )
    (ush_dir / "forecast_det.sh").write_text("#!/bin/bash\n")
    (ush_dir / "common_utils.sh").write_text("#!/bin/bash\n")
    (ush_dir / "atmos_post.sh").write_text("#!/bin/bash\n")
    (ush_dir / "analysis_helper.sh").write_text("#!/bin/bash\n")
    (ush_dir / "unused_script.sh").write_text("#!/bin/bash\n")

    # Config files
    config_dir = tmp_path / "parm" / "config" / "gfs"
    config_dir.mkdir(parents=True)
    (config_dir / "config.base.j2").write_text("# base\n")
    (config_dir / "config.base").write_text("# base plain\n")
    (config_dir / "config.com").write_text("# common\n")
    (config_dir / "config.fcst.j2").write_text("# forecast\n")
    (config_dir / "config.upp").write_text("# UPP\n")
    (config_dir / "config.anal").write_text("# analysis\n")
    (config_dir / "config.resources").write_text("# resources\n")
    (config_dir / "config.resources.HERA").write_text("# HERA\n")

    return tmp_path


class TestDAGFilterIntegration:
    """Integration tests exercising multiple DAG_Filter layers together.

    These tests verify that the full compute_reachability() pipeline
    correctly chains Layer 1 → Layer 2 → Layer 3 → Layer 4 and produces
    a coherent, minimal reachability set.

    Validates: Requirements 1.1–1.5, 2.1–2.4, 3.1–3.5, 4.1–4.5
    """

    def test_full_reachability_includes_only_referenced_artifacts(
        self, integration_dev_root: Path
    ):
        """Full pipeline includes only artifacts reachable from workflow YAML."""
        yaml_data = {
            "suite": {"name": "gfs_v17"},
            "families": [
                {
                    "name": "forecast",
                    "tasks": [{"name": "fcst", "jjob": "JGLOBAL_FORECAST"}],
                },
            ],
        }
        dag = DAGFilter(integration_dev_root, yaml_data, "hera")
        result = dag.compute_reachability()

        # Layer 1: Only referenced J-Job
        assert result.jjobs == frozenset({"JGLOBAL_FORECAST"})
        assert "JGDAS_ATMOS_ANALYSIS" not in result.jjobs

        # Layer 2: Only ex-script from referenced J-Job
        assert "exglobal_forecast.sh" in result.ex_scripts
        assert "exgdas_atmos_analysis.sh" not in result.ex_scripts

        # Layer 3: Only ush scripts transitively from reachable ex-scripts
        assert "forecast_predet.sh" in result.ush_scripts
        assert "forecast_det.sh" in result.ush_scripts
        assert "common_utils.sh" in result.ush_scripts  # transitive
        assert "analysis_helper.sh" not in result.ush_scripts
        assert "unused_script.sh" not in result.ush_scripts

        # Layer 4: Only configs from referenced J-Jobs + unconditional
        assert "config.fcst.j2" in result.config_files
        assert "config.base.j2" in result.config_files
        assert "config.com" in result.config_files
        assert "config.anal" not in result.config_files

    def test_adding_task_expands_reachability(self, integration_dev_root: Path):
        """Adding a task to the workflow expands the reachability set."""
        yaml_one_task = {
            "suite": {"name": "gfs_v17"},
            "families": [
                {
                    "name": "forecast",
                    "tasks": [{"name": "fcst", "jjob": "JGLOBAL_FORECAST"}],
                },
            ],
        }
        yaml_two_tasks = {
            "suite": {"name": "gfs_v17"},
            "families": [
                {
                    "name": "forecast",
                    "tasks": [{"name": "fcst", "jjob": "JGLOBAL_FORECAST"}],
                },
                {
                    "name": "post",
                    "tasks": [{"name": "post", "jjob": "JGFS_ATMOS_POST"}],
                },
            ],
        }

        dag1 = DAGFilter(integration_dev_root, yaml_one_task, "hera")
        result1 = dag1.compute_reachability()

        dag2 = DAGFilter(integration_dev_root, yaml_two_tasks, "hera")
        result2 = dag2.compute_reachability()

        # Two-task result is a superset of one-task result
        assert result1.jjobs.issubset(result2.jjobs)
        assert result1.ex_scripts.issubset(result2.ex_scripts)
        assert result1.ush_scripts.issubset(result2.ush_scripts)

        # Two-task result has additional artifacts
        assert "JGFS_ATMOS_POST" in result2.jjobs
        assert "exgfs_atmos_post.sh" in result2.ex_scripts
        assert "atmos_post.sh" in result2.ush_scripts
        assert "config.upp" in result2.config_files

    def test_config_base_always_present_regardless_of_tasks(
        self, integration_dev_root: Path
    ):
        """config.base.j2 and config.com are always in the reachability set."""
        # Even with an empty workflow (no tasks)
        yaml_data = {"suite": {"name": "gfs_v17"}, "families": []}
        dag = DAGFilter(integration_dev_root, yaml_data, "hera")
        result = dag.compute_reachability()
        assert "config.base.j2" in result.config_files
        assert "config.com" in result.config_files

    def test_fatal_error_on_missing_jjob_in_full_pipeline(
        self, integration_dev_root: Path
    ):
        """FATAL ERROR (PipelineError) when workflow references missing J-Job."""
        yaml_data = {
            "suite": {"name": "gfs_v17"},
            "families": [
                {
                    "name": "family",
                    "tasks": [{"name": "t", "jjob": "JNONEXISTENT_JOB"}],
                },
            ],
        }
        dag = DAGFilter(integration_dev_root, yaml_data, "hera")
        with pytest.raises(PipelineError) as exc_info:
            dag.compute_reachability()
        assert "JNONEXISTENT_JOB" in str(exc_info.value)

    def test_fatal_error_on_missing_ex_script_in_full_pipeline(
        self, integration_dev_root: Path
    ):
        """FATAL ERROR (PipelineError) when J-Job references missing ex-script."""
        # Create a J-Job that references a non-existent ex-script
        jobs_dir = integration_dev_root / "jobs"
        (jobs_dir / "JBAD_SCRIPT").write_text(
            '#!/bin/bash\n'
            '${SCRglobal}/exnonexistent.sh\n'
        )
        yaml_data = {
            "suite": {"name": "gfs_v17"},
            "families": [
                {
                    "name": "family",
                    "tasks": [{"name": "t", "jjob": "JBAD_SCRIPT"}],
                },
            ],
        }
        dag = DAGFilter(integration_dev_root, yaml_data, "hera")
        with pytest.raises(PipelineError) as exc_info:
            dag.compute_reachability()
        assert "exnonexistent.sh" in str(exc_info.value)

    def test_warning_on_missing_ush_script_in_full_pipeline(
        self, integration_dev_root: Path
    ):
        """WARNING (non-fatal) when ex-script references missing ush script."""
        # Modify an ex-script to reference a non-existent ush script
        scripts_dir = integration_dev_root / "scripts"
        (scripts_dir / "exglobal_forecast.sh").write_text(
            '#!/bin/bash\n'
            'source "${USHglobal}/forecast_predet.sh"\n'
            'source "${USHglobal}/optional_missing.sh"\n'
        )
        yaml_data = {
            "suite": {"name": "gfs_v17"},
            "families": [
                {
                    "name": "forecast",
                    "tasks": [{"name": "fcst", "jjob": "JGLOBAL_FORECAST"}],
                },
            ],
        }
        dag = DAGFilter(integration_dev_root, yaml_data, "hera")
        result = dag.compute_reachability()
        # Should NOT raise — missing ush is a warning, not fatal
        assert result.is_valid
        # Warning should mention the missing script
        assert any("optional_missing.sh" in w for w in result.warnings)

    def test_statistics_reflect_full_dev_tree(self, integration_dev_root: Path):
        """Statistics count all available artifacts, not just reachable ones."""
        yaml_data = {
            "suite": {"name": "gfs_v17"},
            "families": [
                {
                    "name": "forecast",
                    "tasks": [{"name": "fcst", "jjob": "JGLOBAL_FORECAST"}],
                },
            ],
        }
        dag = DAGFilter(integration_dev_root, yaml_data, "hera")
        result = dag.compute_reachability()

        # Total available should be larger than reachable
        assert result.total_available_jjobs == 3  # 3 J-Jobs in fixture
        assert len(result.jjobs) == 1  # Only 1 referenced

        # Staged < total demonstrates filtering
        assert len(result.jjobs) < result.total_available_jjobs
