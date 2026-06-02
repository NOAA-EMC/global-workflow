"""Unit tests for workflow_config.pretty_print().

Tests the Pretty-Printer that serializes an in-memory DAG object back
into canonical Workflow_Configuration YAML.

Verifies:
- Deterministic output (byte-for-byte identical across invocations)
- Round-trip: parse(pretty_print(d)) == d
- Correct handling of all TaskNode fields
- Canonical key ordering

Traces to: Requirements 10.3, 10.6
"""

import os
import sys
import tempfile

import pytest

sys.path.insert(0, os.path.join(os.path.dirname(__file__), ".."))

from deployment.workflow_config import (
    DAG,
    Edge,
    MeterDef,
    TaskNode,
    parse,
    pretty_print,
)


# ---------------------------------------------------------------------------
# Helpers
# ---------------------------------------------------------------------------

SAMPLE_DIR = os.path.join(
    os.path.dirname(__file__), "..", "..", "parm", "workflow"
)


def _parse_from_string(yaml_str: str) -> DAG:
    """Write YAML string to temp file and parse it."""
    f = tempfile.NamedTemporaryFile(mode="w", suffix=".yaml", delete=False)
    f.write(yaml_str)
    f.close()
    try:
        return parse(f.name)
    finally:
        os.unlink(f.name)


def _round_trip_dag(dag: DAG) -> DAG:
    """Pretty-print a DAG and parse it back."""
    output = pretty_print(dag)
    f = tempfile.NamedTemporaryFile(mode="w", suffix=".yaml", delete=False)
    f.write(output)
    f.close()
    try:
        return parse(f.name)
    finally:
        os.unlink(f.name)


def _dags_structurally_equal(d1: DAG, d2: DAG) -> bool:
    """Check if two DAGs are structurally equal."""
    if d1.suite_name != d2.suite_name:
        return False
    if set(d1.nodes.keys()) != set(d2.nodes.keys()):
        return False
    for key in d1.nodes:
        n1 = d1.nodes[key]
        n2 = d2.nodes[key]
        if n1.name != n2.name:
            return False
        if n1.family_path != n2.family_path:
            return False
        if n1.jjob != n2.jjob:
            return False
        if n1.trigger != n2.trigger:
            return False
        if n1.complete != n2.complete:
            return False
        if n1.events != n2.events:
            return False
        if n1.meters != n2.meters:
            return False
        if n1.variables != n2.variables:
            return False
        if n1.resources != n2.resources:
            return False
    return True


# ---------------------------------------------------------------------------
# Tests: Determinism (Requirement 10.6)
# ---------------------------------------------------------------------------


class TestDeterminism:
    """Verify pretty_print produces byte-for-byte identical output."""

    def test_deterministic_simple_dag(self):
        dag = DAG(suite_name="test")
        dag.nodes["fam/task1"] = TaskNode(
            name="task1", family_path="fam", jjob="JTASK1"
        )
        output1 = pretty_print(dag)
        output2 = pretty_print(dag)
        assert output1 == output2

    def test_deterministic_complex_dag(self):
        dag = DAG(suite_name="gfs_v17")
        dag.nodes["gdas/atmos/prep"] = TaskNode(
            name="prep",
            family_path="gdas/atmos",
            jjob="JGDAS_PREP",
            events=["done"],
            meters=[MeterDef("progress", 0, 100, 50)],
            variables={"NET": "gfs", "RUN": "gdas"},
            resources={"walltime": "00:30:00", "memory": "4G"},
        )
        dag.nodes["gdas/atmos/anal"] = TaskNode(
            name="anal",
            family_path="gdas/atmos",
            jjob="JGDAS_ANAL",
            trigger="gdas/atmos/prep == complete",
        )
        for _ in range(10):
            output = pretty_print(dag)
        # All 10 calls should produce the same output
        assert pretty_print(dag) == output

    def test_deterministic_with_sample_config(self):
        path = os.path.join(SAMPLE_DIR, "gfs_forecast_only.yaml")
        dag = parse(path)
        output1 = pretty_print(dag)
        output2 = pretty_print(dag)
        assert output1 == output2

    def test_deterministic_with_cycled_config(self):
        path = os.path.join(SAMPLE_DIR, "gfs_cycled.yaml")
        dag = parse(path)
        output1 = pretty_print(dag)
        output2 = pretty_print(dag)
        assert output1 == output2


# ---------------------------------------------------------------------------
# Tests: Round-Trip (Requirements 10.3, 10.5)
# ---------------------------------------------------------------------------


class TestRoundTrip:
    """Verify parse(pretty_print(d)) is structurally equal to d."""

    def test_round_trip_minimal(self):
        dag = DAG(suite_name="minimal")
        dag.nodes["fam/task1"] = TaskNode(
            name="task1", family_path="fam", jjob="JTASK1"
        )
        dag2 = _round_trip_dag(dag)
        assert _dags_structurally_equal(dag, dag2)

    def test_round_trip_with_trigger(self):
        dag = DAG(suite_name="test")
        dag.nodes["app/prep"] = TaskNode(
            name="prep", family_path="app", jjob="JPREP"
        )
        dag.nodes["app/anal"] = TaskNode(
            name="anal",
            family_path="app",
            jjob="JANAL",
            trigger="app/prep == complete",
        )
        dag2 = _round_trip_dag(dag)
        assert _dags_structurally_equal(dag, dag2)

    def test_round_trip_with_complete(self):
        dag = DAG(suite_name="test")
        dag.nodes["app/task1"] = TaskNode(
            name="task1",
            family_path="app",
            jjob="JTASK1",
            complete="app/other == complete",
        )
        dag2 = _round_trip_dag(dag)
        assert dag2.nodes["app/task1"].complete == "app/other == complete"

    def test_round_trip_with_events(self):
        dag = DAG(suite_name="test")
        dag.nodes["app/task1"] = TaskNode(
            name="task1",
            family_path="app",
            jjob="JTASK1",
            events=["event_a", "event_b"],
        )
        dag2 = _round_trip_dag(dag)
        assert dag2.nodes["app/task1"].events == ["event_a", "event_b"]

    def test_round_trip_with_meters(self):
        dag = DAG(suite_name="test")
        dag.nodes["app/task1"] = TaskNode(
            name="task1",
            family_path="app",
            jjob="JTASK1",
            meters=[
                MeterDef("progress", 0, 100),
                MeterDef("fhr", 0, 384, 120),
            ],
        )
        dag2 = _round_trip_dag(dag)
        assert len(dag2.nodes["app/task1"].meters) == 2
        m1 = dag2.nodes["app/task1"].meters[0]
        assert m1.name == "progress"
        assert m1.min_value == 0
        assert m1.max_value == 100
        assert m1.threshold is None
        m2 = dag2.nodes["app/task1"].meters[1]
        assert m2.name == "fhr"
        assert m2.threshold == 120

    def test_round_trip_with_variables(self):
        dag = DAG(suite_name="test")
        dag.nodes["app/task1"] = TaskNode(
            name="task1",
            family_path="app",
            jjob="JTASK1",
            variables={"FHOUR": "6", "NET": "gfs"},
        )
        dag2 = _round_trip_dag(dag)
        # Variables are sorted by key in pretty_print
        assert dag2.nodes["app/task1"].variables == {"FHOUR": "6", "NET": "gfs"}

    def test_round_trip_with_resources(self):
        dag = DAG(suite_name="test")
        dag.nodes["app/task1"] = TaskNode(
            name="task1",
            family_path="app",
            jjob="JTASK1",
            resources={"walltime": "01:00:00", "memory": "8G", "nodes": 2},
        )
        dag2 = _round_trip_dag(dag)
        assert dag2.nodes["app/task1"].resources["walltime"] == "01:00:00"
        assert dag2.nodes["app/task1"].resources["memory"] == "8G"
        assert dag2.nodes["app/task1"].resources["nodes"] == 2

    def test_round_trip_multiple_families(self):
        dag = DAG(suite_name="multi")
        dag.nodes["gdas/prep/prep"] = TaskNode(
            name="prep", family_path="gdas/prep", jjob="JGDAS_PREP"
        )
        dag.nodes["gdas/anal/anal"] = TaskNode(
            name="anal",
            family_path="gdas/anal",
            jjob="JGDAS_ANAL",
            trigger="gdas/prep/prep == complete",
        )
        dag.nodes["gfs/fcst/fcst"] = TaskNode(
            name="fcst",
            family_path="gfs/fcst",
            jjob="JGFS_FCST",
            trigger="gdas/anal/anal == complete",
        )
        dag2 = _round_trip_dag(dag)
        assert _dags_structurally_equal(dag, dag2)

    def test_round_trip_sample_forecast_only(self):
        path = os.path.join(SAMPLE_DIR, "gfs_forecast_only.yaml")
        dag = parse(path)
        dag2 = _round_trip_dag(dag)
        assert _dags_structurally_equal(dag, dag2)

    def test_round_trip_sample_cycled(self):
        path = os.path.join(SAMPLE_DIR, "gfs_cycled.yaml")
        dag = parse(path)
        dag2 = _round_trip_dag(dag)
        assert _dags_structurally_equal(dag, dag2)


# ---------------------------------------------------------------------------
# Tests: Output format and structure
# ---------------------------------------------------------------------------


class TestOutputFormat:
    """Verify the canonical YAML output structure."""

    def test_output_is_valid_yaml(self):
        import yaml

        dag = DAG(suite_name="test")
        dag.nodes["fam/task1"] = TaskNode(
            name="task1", family_path="fam", jjob="JTASK1"
        )
        output = pretty_print(dag)
        parsed = yaml.safe_load(output)
        assert isinstance(parsed, dict)
        assert "suite" in parsed
        assert "families" in parsed

    def test_suite_name_preserved(self):
        import yaml

        dag = DAG(suite_name="gfs_v17_special")
        dag.nodes["fam/task1"] = TaskNode(
            name="task1", family_path="fam", jjob="JTASK1"
        )
        output = pretty_print(dag)
        parsed = yaml.safe_load(output)
        assert parsed["suite"]["name"] == "gfs_v17_special"

    def test_sort_keys_false(self):
        """Verify keys appear in canonical order, not alphabetical."""
        dag = DAG(suite_name="test")
        dag.nodes["fam/task1"] = TaskNode(
            name="task1",
            family_path="fam",
            jjob="JTASK1",
            trigger="other == complete",
        )
        output = pretty_print(dag)
        lines = output.strip().splitlines()
        # 'suite' should come before 'families'
        suite_idx = next(i for i, l in enumerate(lines) if l.startswith("suite"))
        families_idx = next(
            i for i, l in enumerate(lines) if l.startswith("families")
        )
        assert suite_idx < families_idx

    def test_task_key_order(self):
        """Verify task keys appear in canonical order: name, trigger, jjob."""
        dag = DAG(suite_name="test")
        dag.nodes["fam/task1"] = TaskNode(
            name="task1",
            family_path="fam",
            jjob="JTASK1",
            trigger="other == complete",
        )
        output = pretty_print(dag)
        # Find the task section
        lines = output.strip().splitlines()
        task_lines = [l.strip() for l in lines if l.strip().startswith("name:") or l.strip().startswith("trigger:") or l.strip().startswith("jjob:")]
        # name should come first, then trigger, then jjob
        assert task_lines[0].startswith("name:")
        assert task_lines[1].startswith("trigger:")
        assert task_lines[2].startswith("jjob:")

    def test_empty_trigger_quoted(self):
        """Verify empty trigger is represented as quoted empty string."""
        dag = DAG(suite_name="test")
        dag.nodes["fam/task1"] = TaskNode(
            name="task1", family_path="fam", jjob="JTASK1", trigger=None
        )
        output = pretty_print(dag)
        assert '""' in output

    def test_families_grouped_by_path(self):
        """Verify tasks are grouped under their family path."""
        import yaml

        dag = DAG(suite_name="test")
        dag.nodes["fam_a/task1"] = TaskNode(
            name="task1", family_path="fam_a", jjob="JA"
        )
        dag.nodes["fam_b/task2"] = TaskNode(
            name="task2", family_path="fam_b", jjob="JB"
        )
        dag.nodes["fam_a/task3"] = TaskNode(
            name="task3", family_path="fam_a", jjob="JC"
        )
        output = pretty_print(dag)
        parsed = yaml.safe_load(output)
        families = parsed["families"]
        assert len(families) == 2
        assert families[0]["path"] == "fam_a"
        assert len(families[0]["tasks"]) == 2
        assert families[1]["path"] == "fam_b"
        assert len(families[1]["tasks"]) == 1


# ---------------------------------------------------------------------------
# Tests: Edge cases
# ---------------------------------------------------------------------------


class TestEdgeCases:
    """Test edge cases for pretty_print."""

    def test_empty_dag(self):
        """DAG with no nodes should still produce valid YAML."""
        dag = DAG(suite_name="empty")
        output = pretty_print(dag)
        import yaml

        parsed = yaml.safe_load(output)
        assert parsed["suite"]["name"] == "empty"
        assert parsed["families"] == []

    def test_special_characters_in_trigger(self):
        """Triggers with special YAML characters should be properly quoted."""
        dag = DAG(suite_name="test")
        dag.nodes["fam/task1"] = TaskNode(
            name="task1",
            family_path="fam",
            jjob="JTASK1",
            trigger="a/b/c == complete and d/e/f == complete",
        )
        dag2 = _round_trip_dag(dag)
        assert (
            dag2.nodes["fam/task1"].trigger
            == "a/b/c == complete and d/e/f == complete"
        )

    def test_meter_with_threshold(self):
        """Meters with threshold should round-trip correctly."""
        dag = DAG(suite_name="test")
        dag.nodes["fam/task1"] = TaskNode(
            name="task1",
            family_path="fam",
            jjob="JTASK1",
            meters=[MeterDef("fhr", 0, 384, 120)],
        )
        dag2 = _round_trip_dag(dag)
        m = dag2.nodes["fam/task1"].meters[0]
        assert m.threshold == 120

    def test_integer_variable_values(self):
        """Integer values in variables should round-trip as strings."""
        dag = DAG(suite_name="test")
        dag.nodes["fam/task1"] = TaskNode(
            name="task1",
            family_path="fam",
            jjob="JTASK1",
            variables={"FHOUR": "6"},
        )
        dag2 = _round_trip_dag(dag)
        # After round-trip, FHOUR should still be "6" (string)
        assert dag2.nodes["fam/task1"].variables["FHOUR"] == "6"
