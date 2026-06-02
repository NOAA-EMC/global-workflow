"""Unit tests for dag_generator module.

Tests the ecFlow Suite_Definition emission including:
- Basic suite/family/task hierarchy generation
- Trigger, complete, event, meter, and variable emission
- Inter-cycle dependencies via RepeatDate constructs
- Boolean composition support in triggers
- Definition fidelity (task set matches source DAG)
- File output to correct path

Traces to: Requirements 1.2, 2.1, 2.3, 2.4
"""

import os
import sys
import tempfile

import pytest

sys.path.insert(0, os.path.join(os.path.dirname(__file__), ".."))

from deployment.dag_generator import (
    CycleDef,
    DefFileWriter,
    RepeatDateDef,
    SuiteConfig,
    generate_def,
    generate_def_text,
    parse_def_tasks,
    parse_suite_config,
)
from deployment.workflow_config import DAG, Edge, MeterDef, TaskNode


# ---------------------------------------------------------------------------
# Fixtures
# ---------------------------------------------------------------------------


def _make_simple_dag() -> DAG:
    """Create a simple DAG with a linear chain: prep -> anal -> post."""
    dag = DAG(suite_name="test_suite")
    dag.nodes["app/prep/prep"] = TaskNode(
        name="prep",
        family_path="app/prep",
        jjob="JAPP_PREP",
    )
    dag.nodes["app/analysis/anal"] = TaskNode(
        name="anal",
        family_path="app/analysis",
        jjob="JAPP_ANAL",
        trigger="app/prep/prep == complete",
    )
    dag.nodes["app/post/post"] = TaskNode(
        name="post",
        family_path="app/post",
        jjob="JAPP_POST",
        trigger="app/analysis/anal == complete",
    )
    dag.edges = [
        Edge(source="app/prep/prep", target="app/analysis/anal", kind="trigger"),
        Edge(source="app/analysis/anal", target="app/post/post", kind="trigger"),
    ]
    return dag


def _make_dag_with_meters() -> DAG:
    """Create a DAG with meter dependencies (forecast -> post tasks)."""
    dag = DAG(suite_name="gfs_v17")
    dag.nodes["gfs/forecast/fcst"] = TaskNode(
        name="fcst",
        family_path="gfs/forecast",
        jjob="JGFS_FORECAST",
        events=["forecast_hour"],
        meters=[MeterDef(name="forecast_hour", min_value=0, max_value=120)],
    )
    dag.nodes["gfs/post/post_f000"] = TaskNode(
        name="post_f000",
        family_path="gfs/post",
        jjob="JGFS_POST",
        trigger="gfs/forecast/fcst:forecast_hour ge 0",
        variables={"FHOUR": "0"},
    )
    dag.nodes["gfs/post/post_f006"] = TaskNode(
        name="post_f006",
        family_path="gfs/post",
        jjob="JGFS_POST",
        trigger="gfs/forecast/fcst:forecast_hour ge 6",
        variables={"FHOUR": "6"},
    )
    return dag


def _make_dag_with_complete() -> DAG:
    """Create a DAG with a complete expression."""
    dag = DAG(suite_name="test_complete")
    dag.nodes["app/main/task_a"] = TaskNode(
        name="task_a",
        family_path="app/main",
        jjob="JAPP_A",
    )
    dag.nodes["app/main/task_b"] = TaskNode(
        name="task_b",
        family_path="app/main",
        jjob="JAPP_B",
        trigger="task_a == complete",
        complete="task_a == aborted",
    )
    return dag


def _make_dag_with_boolean_trigger() -> DAG:
    """Create a DAG with boolean composition in triggers."""
    dag = DAG(suite_name="test_bool")
    dag.nodes["app/prep/prep1"] = TaskNode(
        name="prep1",
        family_path="app/prep",
        jjob="JAPP_PREP1",
    )
    dag.nodes["app/prep/prep2"] = TaskNode(
        name="prep2",
        family_path="app/prep",
        jjob="JAPP_PREP2",
    )
    dag.nodes["app/analysis/anal"] = TaskNode(
        name="anal",
        family_path="app/analysis",
        jjob="JAPP_ANAL",
        trigger="app/prep/prep1 == complete and app/prep/prep2 == complete",
    )
    dag.nodes["app/fallback/fallback"] = TaskNode(
        name="fallback",
        family_path="app/fallback",
        jjob="JAPP_FALLBACK",
        trigger="app/prep/prep1 == complete or app/prep/prep2 == complete",
    )
    return dag


# ---------------------------------------------------------------------------
# Tests: DefFileWriter
# ---------------------------------------------------------------------------


class TestDefFileWriter:
    """Tests for the low-level DefFileWriter."""

    def test_empty_suite(self):
        writer = DefFileWriter()
        writer.write_suite_start("empty")
        writer.write_suite_end()
        text = writer.get_text()
        assert "suite empty" in text
        assert "endsuite" in text

    def test_indentation(self):
        writer = DefFileWriter()
        writer.write_suite_start("s")
        writer.write_family_start("f")
        writer.write_task("t")
        writer.write_task_end()
        writer.write_family_end()
        writer.write_suite_end()
        text = writer.get_text()
        lines = text.splitlines()
        assert lines[0] == "suite s"
        assert lines[1] == "  family f"
        assert lines[2] == "    task t"
        assert lines[3] == "  endfamily"
        assert lines[4] == "endsuite"

    def test_trigger_emission(self):
        writer = DefFileWriter()
        writer.write_suite_start("s")
        writer.write_task("t")
        writer.write_trigger("a == complete and b == complete")
        writer.write_task_end()
        writer.write_suite_end()
        text = writer.get_text()
        assert "trigger a == complete and b == complete" in text

    def test_meter_emission(self):
        writer = DefFileWriter()
        writer.write_suite_start("s")
        writer.write_task("t")
        writer.write_meter("forecast_hour", 0, 120)
        writer.write_task_end()
        writer.write_suite_end()
        text = writer.get_text()
        assert "meter forecast_hour 0 120" in text

    def test_meter_with_threshold(self):
        writer = DefFileWriter()
        writer.write_suite_start("s")
        writer.write_task("t")
        writer.write_meter("progress", 0, 100, 50)
        writer.write_task_end()
        writer.write_suite_end()
        text = writer.get_text()
        assert "meter progress 0 100 50" in text

    def test_event_emission(self):
        writer = DefFileWriter()
        writer.write_suite_start("s")
        writer.write_task("t")
        writer.write_event("done")
        writer.write_task_end()
        writer.write_suite_end()
        text = writer.get_text()
        assert "event done" in text

    def test_variable_emission(self):
        writer = DefFileWriter()
        writer.write_suite_start("s")
        writer.write_variable("FHOUR", "6")
        writer.write_suite_end()
        text = writer.get_text()
        assert "edit FHOUR '6'" in text

    def test_repeat_date_emission(self):
        writer = DefFileWriter()
        writer.write_suite_start("s")
        writer.write_repeat_date("YMD", "20250101", "20250131", 1)
        writer.write_suite_end()
        text = writer.get_text()
        assert "repeat date YMD 20250101 20250131 1" in text

    def test_time_emission(self):
        writer = DefFileWriter()
        writer.write_suite_start("s")
        writer.write_time("00:00 06:00 12:00 18:00")
        writer.write_suite_end()
        text = writer.get_text()
        assert "time 00:00 06:00 12:00 18:00" in text

    def test_cron_emission(self):
        writer = DefFileWriter()
        writer.write_suite_start("s")
        writer.write_cron("0 0 1 1 *")
        writer.write_suite_end()
        text = writer.get_text()
        assert "cron 0 0 1 1 *" in text

    def test_date_emission(self):
        writer = DefFileWriter()
        writer.write_suite_start("s")
        writer.write_date("1.*.*")
        writer.write_suite_end()
        text = writer.get_text()
        assert "date 1.*.*" in text

    def test_complete_emission(self):
        writer = DefFileWriter()
        writer.write_suite_start("s")
        writer.write_task("t")
        writer.write_complete("a == aborted")
        writer.write_task_end()
        writer.write_suite_end()
        text = writer.get_text()
        assert "complete a == aborted" in text


# ---------------------------------------------------------------------------
# Tests: generate_def_text (basic hierarchy)
# ---------------------------------------------------------------------------


class TestGenerateDefText:
    """Tests for generate_def_text producing correct hierarchy."""

    def test_simple_linear_dag(self):
        dag = _make_simple_dag()
        text = generate_def_text(dag)

        assert "suite test_suite" in text
        assert "endsuite" in text
        assert "family app" in text
        assert "family prep" in text
        assert "family analysis" in text
        assert "family post" in text
        assert "task prep" in text
        assert "task anal" in text
        assert "task post" in text

    def test_triggers_emitted(self):
        dag = _make_simple_dag()
        text = generate_def_text(dag)

        assert "trigger app/prep/prep == complete" in text
        assert "trigger app/analysis/anal == complete" in text

    def test_meters_emitted(self):
        dag = _make_dag_with_meters()
        text = generate_def_text(dag)

        assert "meter forecast_hour 0 120" in text
        assert "event forecast_hour" in text

    def test_variables_emitted(self):
        dag = _make_dag_with_meters()
        text = generate_def_text(dag)

        assert "edit FHOUR '0'" in text
        assert "edit FHOUR '6'" in text

    def test_complete_expression_emitted(self):
        dag = _make_dag_with_complete()
        text = generate_def_text(dag)

        assert "complete task_a == aborted" in text

    def test_boolean_trigger_and(self):
        dag = _make_dag_with_boolean_trigger()
        text = generate_def_text(dag)

        assert "trigger app/prep/prep1 == complete and app/prep/prep2 == complete" in text

    def test_boolean_trigger_or(self):
        dag = _make_dag_with_boolean_trigger()
        text = generate_def_text(dag)

        assert "trigger app/prep/prep1 == complete or app/prep/prep2 == complete" in text

    def test_nested_families(self):
        """Test that deeply nested family paths produce correct nesting."""
        dag = DAG(suite_name="deep")
        dag.nodes["a/b/c/task1"] = TaskNode(
            name="task1",
            family_path="a/b/c",
            jjob="JTASK1",
        )
        text = generate_def_text(dag)

        lines = text.splitlines()
        # Should have: suite deep / family a / family b / family c / task task1
        assert "suite deep" in text
        assert "family a" in text
        assert "family b" in text
        assert "family c" in text
        assert "task task1" in text
        # Verify nesting order
        a_idx = next(i for i, l in enumerate(lines) if "family a" in l)
        b_idx = next(i for i, l in enumerate(lines) if "family b" in l)
        c_idx = next(i for i, l in enumerate(lines) if "family c" in l)
        t_idx = next(i for i, l in enumerate(lines) if "task task1" in l)
        assert a_idx < b_idx < c_idx < t_idx

    def test_multiple_tasks_same_family(self):
        """Test multiple tasks in the same family."""
        dag = DAG(suite_name="multi")
        dag.nodes["app/step/task_a"] = TaskNode(
            name="task_a", family_path="app/step", jjob="JA"
        )
        dag.nodes["app/step/task_b"] = TaskNode(
            name="task_b", family_path="app/step", jjob="JB"
        )
        dag.nodes["app/step/task_c"] = TaskNode(
            name="task_c", family_path="app/step", jjob="JC"
        )
        text = generate_def_text(dag)

        assert "task task_a" in text
        assert "task task_b" in text
        assert "task task_c" in text
        # Only one "family step" should appear
        assert text.count("family step") == 1


# ---------------------------------------------------------------------------
# Tests: Suite config with cycles and defaults
# ---------------------------------------------------------------------------


class TestSuiteConfig:
    """Tests for suite configuration emission."""

    def test_defaults_emitted(self):
        dag = _make_simple_dag()
        config = SuiteConfig(
            ecf_home="/path/to/ecf",
            ecf_files="/path/to/ecf/scripts",
            ecf_include="/path/to/ecf/include",
            defaults={"ECF_TRIES": "2", "ACCOUNT": "fv3"},
        )
        text = generate_def_text(dag, suite_config=config)

        assert "edit ECF_HOME '/path/to/ecf'" in text
        assert "edit ECF_FILES '/path/to/ecf/scripts'" in text
        assert "edit ECF_INCLUDE '/path/to/ecf/include'" in text
        assert "edit ECF_TRIES '2'" in text
        assert "edit ACCOUNT 'fv3'" in text

    def test_repeat_date_for_cycle(self):
        """Test that cycle repeat date is emitted at the correct family level."""
        dag = DAG(suite_name="cycled")
        dag.nodes["gdas/atmos/prep/prep"] = TaskNode(
            name="prep",
            family_path="gdas/atmos/prep",
            jjob="JGDAS_PREP",
        )
        config = SuiteConfig(
            cycles=[
                CycleDef(
                    name="gdas",
                    repeat=RepeatDateDef(
                        variable="YMD",
                        start="20250101",
                        end="20250131",
                        step=1,
                    ),
                    time="00:00 06:00 12:00 18:00",
                )
            ]
        )
        text = generate_def_text(dag, suite_config=config)

        assert "repeat date YMD 20250101 20250131 1" in text
        assert "time 00:00 06:00 12:00 18:00" in text

    def test_cron_for_cycle(self):
        """Test cron specification on a cycle family."""
        dag = DAG(suite_name="cron_test")
        dag.nodes["hourly/task/run"] = TaskNode(
            name="run",
            family_path="hourly/task",
            jjob="JHOURLY",
        )
        config = SuiteConfig(
            cycles=[
                CycleDef(
                    name="hourly",
                    cron="0 * * * *",
                )
            ]
        )
        text = generate_def_text(dag, suite_config=config)

        assert "cron 0 * * * *" in text


# ---------------------------------------------------------------------------
# Tests: generate_def (file output)
# ---------------------------------------------------------------------------


class TestGenerateDefFile:
    """Tests for generate_def writing to disk."""

    def test_writes_file(self):
        dag = _make_simple_dag()
        with tempfile.TemporaryDirectory() as tmpdir:
            output_path = os.path.join(tmpdir, "ecf", "defs", "test_suite.def")
            result = generate_def(dag, output_path)

            assert os.path.exists(output_path)
            with open(output_path) as f:
                content = f.read()
            assert content == result
            assert "suite test_suite" in content

    def test_creates_parent_directories(self):
        dag = _make_simple_dag()
        with tempfile.TemporaryDirectory() as tmpdir:
            output_path = os.path.join(
                tmpdir, "deep", "nested", "path", "suite.def"
            )
            generate_def(dag, output_path)
            assert os.path.exists(output_path)

    def test_output_path_convention(self):
        """Test that the expected path pattern works."""
        dag = _make_simple_dag()
        with tempfile.TemporaryDirectory() as tmpdir:
            expdir = os.path.join(tmpdir, "EXPDIR")
            output_path = os.path.join(
                expdir, "ecf", "defs", f"{dag.suite_name}.def"
            )
            generate_def(dag, output_path)
            assert os.path.exists(output_path)


# ---------------------------------------------------------------------------
# Tests: parse_def_tasks (Definition Fidelity helper)
# ---------------------------------------------------------------------------


class TestParseDefTasks:
    """Tests for parse_def_tasks extracting (family_path, task_name) pairs."""

    def test_simple_extraction(self):
        dag = _make_simple_dag()
        text = generate_def_text(dag)
        tasks = parse_def_tasks(text)

        expected = {
            ("app/prep", "prep"),
            ("app/analysis", "anal"),
            ("app/post", "post"),
        }
        assert tasks == expected

    def test_nested_family_extraction(self):
        dag = DAG(suite_name="nested")
        dag.nodes["a/b/c/task1"] = TaskNode(
            name="task1", family_path="a/b/c", jjob="J1"
        )
        dag.nodes["a/b/task2"] = TaskNode(
            name="task2", family_path="a/b", jjob="J2"
        )
        text = generate_def_text(dag)
        tasks = parse_def_tasks(text)

        assert ("a/b/c", "task1") in tasks
        assert ("a/b", "task2") in tasks

    def test_definition_fidelity(self):
        """Property 13: set of (family_path, task_name) in def == set in DAG."""
        dag = _make_dag_with_meters()
        text = generate_def_text(dag)
        def_tasks = parse_def_tasks(text)

        # Build expected set from DAG
        dag_tasks = {
            (node.family_path, node.name) for node in dag.nodes.values()
        }

        assert def_tasks == dag_tasks

    def test_definition_fidelity_complex(self):
        """Property 13 with a more complex DAG."""
        dag = _make_dag_with_boolean_trigger()
        text = generate_def_text(dag)
        def_tasks = parse_def_tasks(text)

        dag_tasks = {
            (node.family_path, node.name) for node in dag.nodes.values()
        }

        assert def_tasks == dag_tasks


# ---------------------------------------------------------------------------
# Tests: parse_suite_config
# ---------------------------------------------------------------------------


class TestParseSuiteConfig:
    """Tests for parse_suite_config from raw YAML dict."""

    def test_basic_config(self):
        raw = {
            "suite": {
                "name": "gfs_v17",
                "ecf_home": "/path/ecf",
                "ecf_files": "/path/ecf/scripts",
                "ecf_include": "/path/ecf/include",
            },
            "defaults": {"ECF_TRIES": 2, "ACCOUNT": "fv3"},
            "cycles": [
                {
                    "name": "gdas",
                    "repeat": {
                        "type": "date",
                        "variable": "YMD",
                        "start": "20250101",
                        "end": "20250131",
                        "step": 1,
                    },
                    "time": "00:00 06:00 12:00 18:00",
                }
            ],
            "inter_cycle_dependencies": [
                {
                    "task": "gdas/atmos/prep",
                    "depends_on": "gdas/atmos/archive/arch == complete",
                    "cycle_offset": -1,
                }
            ],
        }

        config = parse_suite_config(raw)

        assert config.ecf_home == "/path/ecf"
        assert config.ecf_files == "/path/ecf/scripts"
        assert config.ecf_include == "/path/ecf/include"
        assert config.defaults == {"ECF_TRIES": 2, "ACCOUNT": "fv3"}
        assert len(config.cycles) == 1
        assert config.cycles[0].name == "gdas"
        assert config.cycles[0].repeat.variable == "YMD"
        assert config.cycles[0].repeat.start == "20250101"
        assert config.cycles[0].repeat.end == "20250131"
        assert config.cycles[0].repeat.step == 1
        assert config.cycles[0].time == "00:00 06:00 12:00 18:00"
        assert len(config.inter_cycle_deps) == 1

    def test_empty_config(self):
        raw = {"suite": {"name": "empty"}}
        config = parse_suite_config(raw)

        assert config.ecf_home == ""
        assert config.defaults == {}
        assert config.cycles == []
        assert config.inter_cycle_deps == []


# ---------------------------------------------------------------------------
# Tests: Integration with workflow_config.parse
# ---------------------------------------------------------------------------


class TestIntegrationWithParser:
    """Tests that dag_generator works with DAGs from workflow_config.parse."""

    def test_forecast_only_config(self):
        """Generate def from the gfs_forecast_only.yaml sample config."""
        from deployment.workflow_config import parse

        sample_dir = os.path.join(
            os.path.dirname(__file__), "..", "..", "parm", "workflow"
        )
        path = os.path.join(sample_dir, "gfs_forecast_only.yaml")
        dag = parse(path)

        text = generate_def_text(dag)

        # Verify suite name
        assert "suite gfs_v17_fcst_only" in text
        assert "endsuite" in text

        # Verify key tasks are present
        assert "task stage_ic" in text
        assert "task fcst" in text
        assert "task post_f000" in text
        assert "task post_f120" in text
        assert "task arch" in text

        # Verify triggers
        assert "trigger gfs/atmos/stage/stage_ic == complete" in text
        assert "trigger gfs/atmos/post/post_f120 == complete" in text

        # Verify meters
        assert "meter forecast_hour 0 120" in text

        # Verify definition fidelity
        def_tasks = parse_def_tasks(text)
        dag_tasks = {
            (node.family_path, node.name) for node in dag.nodes.values()
        }
        assert def_tasks == dag_tasks

    def test_cycled_config(self):
        """Generate def from the gfs_cycled.yaml sample config."""
        from deployment.workflow_config import parse

        sample_dir = os.path.join(
            os.path.dirname(__file__), "..", "..", "parm", "workflow"
        )
        path = os.path.join(sample_dir, "gfs_cycled.yaml")
        dag = parse(path)

        text = generate_def_text(dag)

        # Verify suite name
        assert "suite gfs_v17" in text
        assert "endsuite" in text

        # Should have many tasks
        def_tasks = parse_def_tasks(text)
        assert len(def_tasks) > 30

        # Verify definition fidelity
        dag_tasks = {
            (node.family_path, node.name) for node in dag.nodes.values()
        }
        assert def_tasks == dag_tasks

    def test_generate_def_to_file(self):
        """Test full pipeline: parse config -> generate def -> write file."""
        from deployment.workflow_config import parse

        sample_dir = os.path.join(
            os.path.dirname(__file__), "..", "..", "parm", "workflow"
        )
        path = os.path.join(sample_dir, "gfs_forecast_only.yaml")
        dag = parse(path)

        with tempfile.TemporaryDirectory() as tmpdir:
            expdir = os.path.join(tmpdir, "EXPDIR")
            output_path = os.path.join(
                expdir, "ecf", "defs", f"{dag.suite_name}.def"
            )
            result = generate_def(dag, output_path)

            # File exists at correct path
            assert os.path.exists(output_path)

            # Content matches return value
            with open(output_path) as f:
                assert f.read() == result
