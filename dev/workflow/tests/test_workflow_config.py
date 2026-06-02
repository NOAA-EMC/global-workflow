"""Unit tests for workflow_config.parse() and pretty_print().

Tests the Workflow_Configuration YAML parser including:
- Successful parsing of valid configs
- for_each expansion of parameterized tasks
- Edge construction from trigger/complete expressions
- Error handling for malformed inputs

Tests the Pretty-Printer including:
- Deterministic output (byte-for-byte identical across invocations)
- Round-trip fidelity (parse -> pretty_print -> parse yields equal DAG)
- Correct YAML structure and key ordering

Traces to: Requirements 10.1, 10.2, 10.3, 10.6
"""

import os
import tempfile

import pytest

import sys
sys.path.insert(0, os.path.join(os.path.dirname(__file__), ".."))

from deployment.workflow_config import (
    DAG,
    Edge,
    MeterDef,
    ParseError,
    TaskNode,
    parse,
    pretty_print,
)


# ---------------------------------------------------------------------------
# Fixtures
# ---------------------------------------------------------------------------

SAMPLE_DIR = os.path.join(
    os.path.dirname(__file__), "..", "..", "parm", "workflow"
)


def _write_yaml(content: str) -> str:
    """Write content to a temp YAML file and return its path."""
    f = tempfile.NamedTemporaryFile(
        mode="w", suffix=".yaml", delete=False
    )
    f.write(content)
    f.close()
    return f.name


MINIMAL_VALID = """\
suite:
  name: "test_suite"
  ecf_home: "/tmp/ecf"
families:
  - path: "app/step"
    tasks:
      - name: "run"
        trigger: ""
        jjob: "JAPP_RUN"
"""

FOR_EACH_CONFIG = """\
suite:
  name: "test_foreach"
  ecf_home: "/tmp/ecf"
families:
  - path: "gfs/post"
    tasks:
      - name: "post_f{{ '%03d' % fhr }}"
        trigger: "gfs/forecast/fcst:forecast_hour ge {{ fhr }}"
        jjob: "JGFS_POST"
        variables:
          FHOUR: "{{ fhr }}"
        for_each:
          fhr: [0, 6, 12]
  - path: "gfs/forecast"
    tasks:
      - name: "fcst"
        trigger: ""
        jjob: "JGFS_FORECAST"
        meters:
          - name: "forecast_hour"
            min: 0
            max: 12
"""

MULTI_TRIGGER_CONFIG = """\
suite:
  name: "test_multi"
  ecf_home: "/tmp/ecf"
families:
  - path: "app/prep"
    tasks:
      - name: "prep"
        trigger: ""
        jjob: "JAPP_PREP"
  - path: "app/analysis"
    tasks:
      - name: "anal"
        trigger: "app/prep/prep == complete"
        jjob: "JAPP_ANAL"
      - name: "analcalc"
        trigger: "anal == complete"
        jjob: "JAPP_ANALCALC"
  - path: "app/archive"
    tasks:
      - name: "arch"
        trigger: "app/analysis/anal == complete and app/analysis/analcalc == complete"
        jjob: "JAPP_ARCHIVE"
"""


# ---------------------------------------------------------------------------
# Tests: Successful parsing
# ---------------------------------------------------------------------------


class TestParseValid:
    """Tests for successful parsing of valid configurations."""

    def test_parse_minimal(self):
        path = _write_yaml(MINIMAL_VALID)
        try:
            dag = parse(path)
            assert dag.suite_name == "test_suite"
            assert len(dag.nodes) == 1
            assert "app/step/run" in dag.nodes
            node = dag.nodes["app/step/run"]
            assert node.name == "run"
            assert node.family_path == "app/step"
            assert node.jjob == "JAPP_RUN"
            assert node.trigger is None  # empty string becomes None
            assert dag.edges == []
        finally:
            os.unlink(path)

    def test_parse_forecast_only_yaml(self):
        path = os.path.join(SAMPLE_DIR, "gfs_forecast_only.yaml")
        dag = parse(path)
        assert dag.suite_name == "gfs_v17_fcst_only"
        # stage_ic + fcst + 7 post tasks + arch = 10
        assert len(dag.nodes) == 10
        assert "gfs/atmos/stage/stage_ic" in dag.nodes
        assert "gfs/atmos/forecast/fcst" in dag.nodes
        assert "gfs/atmos/post/post_f000" in dag.nodes
        assert "gfs/atmos/post/post_f120" in dag.nodes
        assert "gfs/atmos/archive/arch" in dag.nodes

    def test_parse_cycled_yaml(self):
        path = os.path.join(SAMPLE_DIR, "gfs_cycled.yaml")
        dag = parse(path)
        assert dag.suite_name == "gfs_v17"
        # Should have many nodes (GDAS + GFS families)
        assert len(dag.nodes) > 30
        assert len(dag.edges) > 20

    def test_full_path_property(self):
        path = _write_yaml(MINIMAL_VALID)
        try:
            dag = parse(path)
            node = dag.nodes["app/step/run"]
            assert node.full_path == "app/step/run"
        finally:
            os.unlink(path)


# ---------------------------------------------------------------------------
# Tests: for_each expansion
# ---------------------------------------------------------------------------


class TestForEachExpansion:
    """Tests for for_each parameterized task expansion."""

    def test_for_each_creates_multiple_nodes(self):
        path = _write_yaml(FOR_EACH_CONFIG)
        try:
            dag = parse(path)
            # Should have 3 post tasks + 1 fcst = 4
            assert len(dag.nodes) == 4
            assert "gfs/post/post_f000" in dag.nodes
            assert "gfs/post/post_f006" in dag.nodes
            assert "gfs/post/post_f012" in dag.nodes
        finally:
            os.unlink(path)

    def test_for_each_expands_name_with_format(self):
        path = _write_yaml(FOR_EACH_CONFIG)
        try:
            dag = parse(path)
            # Names should be zero-padded to 3 digits
            assert dag.nodes["gfs/post/post_f000"].name == "post_f000"
            assert dag.nodes["gfs/post/post_f006"].name == "post_f006"
            assert dag.nodes["gfs/post/post_f012"].name == "post_f012"
        finally:
            os.unlink(path)

    def test_for_each_expands_variables(self):
        path = _write_yaml(FOR_EACH_CONFIG)
        try:
            dag = parse(path)
            assert dag.nodes["gfs/post/post_f000"].variables == {"FHOUR": "0"}
            assert dag.nodes["gfs/post/post_f006"].variables == {"FHOUR": "6"}
            assert dag.nodes["gfs/post/post_f012"].variables == {"FHOUR": "12"}
        finally:
            os.unlink(path)

    def test_for_each_expands_trigger(self):
        path = _write_yaml(FOR_EACH_CONFIG)
        try:
            dag = parse(path)
            node = dag.nodes["gfs/post/post_f006"]
            assert node.trigger == "gfs/forecast/fcst:forecast_hour ge 6"
        finally:
            os.unlink(path)


# ---------------------------------------------------------------------------
# Tests: Edge construction
# ---------------------------------------------------------------------------


class TestEdgeConstruction:
    """Tests for building edges from trigger/complete expressions."""

    def test_simple_trigger_edge(self):
        path = _write_yaml(MULTI_TRIGGER_CONFIG)
        try:
            dag = parse(path)
            # anal depends on prep
            anal_edges = [e for e in dag.edges if e.target == "app/analysis/anal"]
            assert len(anal_edges) == 1
            assert anal_edges[0].source == "app/prep/prep"
            assert anal_edges[0].kind == "trigger"
        finally:
            os.unlink(path)

    def test_relative_trigger_resolved(self):
        path = _write_yaml(MULTI_TRIGGER_CONFIG)
        try:
            dag = parse(path)
            # analcalc trigger is "anal == complete" (relative)
            analcalc_edges = [
                e for e in dag.edges if e.target == "app/analysis/analcalc"
            ]
            assert len(analcalc_edges) == 1
            # Should resolve to app/analysis/anal
            assert analcalc_edges[0].source == "app/analysis/anal"
        finally:
            os.unlink(path)

    def test_compound_trigger_creates_multiple_edges(self):
        path = _write_yaml(MULTI_TRIGGER_CONFIG)
        try:
            dag = parse(path)
            # arch depends on both anal and analcalc
            arch_edges = [e for e in dag.edges if e.target == "app/archive/arch"]
            assert len(arch_edges) == 2
            sources = {e.source for e in arch_edges}
            assert "app/analysis/anal" in sources
            assert "app/analysis/analcalc" in sources
        finally:
            os.unlink(path)

    def test_meter_trigger_edge(self):
        path = _write_yaml(FOR_EACH_CONFIG)
        try:
            dag = parse(path)
            # post tasks depend on fcst meter
            post_edges = [
                e for e in dag.edges if e.target == "gfs/post/post_f006"
            ]
            assert len(post_edges) == 1
            assert post_edges[0].source == "gfs/forecast/fcst"
            assert post_edges[0].kind == "meter"
        finally:
            os.unlink(path)

    def test_no_edges_for_empty_trigger(self):
        path = _write_yaml(MINIMAL_VALID)
        try:
            dag = parse(path)
            assert dag.edges == []
        finally:
            os.unlink(path)


# ---------------------------------------------------------------------------
# Tests: Meters and Events
# ---------------------------------------------------------------------------


class TestMetersAndEvents:
    """Tests for meter and event parsing."""

    def test_meters_parsed(self):
        path = _write_yaml(FOR_EACH_CONFIG)
        try:
            dag = parse(path)
            fcst = dag.nodes["gfs/forecast/fcst"]
            assert len(fcst.meters) == 1
            meter = fcst.meters[0]
            assert meter.name == "forecast_hour"
            assert meter.min_value == 0
            assert meter.max_value == 12
            assert meter.threshold is None
        finally:
            os.unlink(path)

    def test_events_parsed(self):
        path = os.path.join(SAMPLE_DIR, "gfs_forecast_only.yaml")
        dag = parse(path)
        fcst = dag.nodes["gfs/atmos/forecast/fcst"]
        assert "forecast_hour" in fcst.events


# ---------------------------------------------------------------------------
# Tests: Error handling (Requirement 10.2)
# ---------------------------------------------------------------------------


class TestParseErrors:
    """Tests for descriptive error reporting on malformed input."""

    def test_file_not_found(self):
        with pytest.raises(ParseError) as exc_info:
            parse("/nonexistent/path.yaml")
        assert "File not found" in str(exc_info.value)
        assert "/nonexistent/path.yaml" in str(exc_info.value)

    def test_invalid_yaml_syntax(self):
        path = _write_yaml("invalid: yaml: [unclosed")
        try:
            with pytest.raises(ParseError) as exc_info:
                parse(path)
            err = exc_info.value
            assert "YAML syntax error" in err.reason
            assert err.line is not None
        finally:
            os.unlink(path)

    def test_empty_file(self):
        path = _write_yaml("")
        try:
            with pytest.raises(ParseError) as exc_info:
                parse(path)
            assert "empty" in str(exc_info.value).lower()
        finally:
            os.unlink(path)

    def test_missing_suite_key(self):
        path = _write_yaml("families:\n  - path: x\n    tasks: []\n")
        try:
            with pytest.raises(ParseError) as exc_info:
                parse(path)
            assert "suite" in str(exc_info.value)
        finally:
            os.unlink(path)

    def test_missing_families_key(self):
        path = _write_yaml("suite:\n  name: test\n")
        try:
            with pytest.raises(ParseError) as exc_info:
                parse(path)
            assert "families" in str(exc_info.value)
        finally:
            os.unlink(path)

    def test_missing_suite_name(self):
        path = _write_yaml("suite:\n  ecf_home: /tmp\nfamilies: []\n")
        try:
            with pytest.raises(ParseError) as exc_info:
                parse(path)
            assert "suite.name" in str(exc_info.value)
        finally:
            os.unlink(path)

    def test_family_missing_path(self):
        path = _write_yaml(
            "suite:\n  name: t\nfamilies:\n  - tasks:\n      - name: x\n        jjob: J\n"
        )
        try:
            with pytest.raises(ParseError) as exc_info:
                parse(path)
            assert "path" in str(exc_info.value)
        finally:
            os.unlink(path)

    def test_family_missing_tasks(self):
        path = _write_yaml("suite:\n  name: t\nfamilies:\n  - path: foo\n")
        try:
            with pytest.raises(ParseError) as exc_info:
                parse(path)
            assert "tasks" in str(exc_info.value).lower()
        finally:
            os.unlink(path)

    def test_task_missing_name(self):
        path = _write_yaml(
            "suite:\n  name: t\nfamilies:\n  - path: foo\n    tasks:\n      - jjob: J\n"
        )
        try:
            with pytest.raises(ParseError) as exc_info:
                parse(path)
            assert "name" in str(exc_info.value)
        finally:
            os.unlink(path)

    def test_task_missing_jjob(self):
        path = _write_yaml(
            "suite:\n  name: t\nfamilies:\n  - path: foo\n    tasks:\n      - name: bar\n"
        )
        try:
            with pytest.raises(ParseError) as exc_info:
                parse(path)
            assert "jjob" in str(exc_info.value)
        finally:
            os.unlink(path)

    def test_duplicate_task_path(self):
        config = """\
suite:
  name: test
families:
  - path: "app/step"
    tasks:
      - name: "run"
        jjob: "J1"
      - name: "run"
        jjob: "J2"
"""
        path = _write_yaml(config)
        try:
            with pytest.raises(ParseError) as exc_info:
                parse(path)
            assert "Duplicate" in str(exc_info.value)
        finally:
            os.unlink(path)

    def test_error_includes_file_path(self):
        path = _write_yaml("not_a_mapping")
        try:
            with pytest.raises(ParseError) as exc_info:
                parse(path)
            assert path in str(exc_info.value)
        finally:
            os.unlink(path)


# ---------------------------------------------------------------------------
# Tests: Pretty-Printer (Requirements 10.3, 10.6)
# ---------------------------------------------------------------------------


class TestPrettyPrint:
    """Tests for pretty_print() serialization."""

    def test_determinism_same_dag(self):
        """pretty_print produces byte-for-byte identical output on same input."""
        path = _write_yaml(MINIMAL_VALID)
        try:
            dag = parse(path)
            output1 = pretty_print(dag)
            output2 = pretty_print(dag)
            assert output1 == output2
        finally:
            os.unlink(path)

    def test_determinism_forecast_only(self):
        """Determinism holds for the full forecast-only config."""
        path = os.path.join(SAMPLE_DIR, "gfs_forecast_only.yaml")
        dag = parse(path)
        output1 = pretty_print(dag)
        output2 = pretty_print(dag)
        assert output1 == output2

    def test_round_trip_minimal(self):
        """parse(pretty_print(parse(f))) == parse(f) for minimal config."""
        path = _write_yaml(MINIMAL_VALID)
        try:
            dag1 = parse(path)
            yaml_str = pretty_print(dag1)
            tmp = _write_yaml(yaml_str)
            try:
                dag2 = parse(tmp)
                assert dag1.suite_name == dag2.suite_name
                assert set(dag1.nodes.keys()) == set(dag2.nodes.keys())
                for key in dag1.nodes:
                    n1 = dag1.nodes[key]
                    n2 = dag2.nodes[key]
                    assert n1.name == n2.name
                    assert n1.family_path == n2.family_path
                    assert n1.jjob == n2.jjob
                    assert n1.trigger == n2.trigger
            finally:
                os.unlink(tmp)
        finally:
            os.unlink(path)

    def test_round_trip_forecast_only(self):
        """Round-trip with the full forecast-only sample config."""
        path = os.path.join(SAMPLE_DIR, "gfs_forecast_only.yaml")
        dag1 = parse(path)
        yaml_str = pretty_print(dag1)
        tmp = _write_yaml(yaml_str)
        try:
            dag2 = parse(tmp)
            assert dag1.suite_name == dag2.suite_name
            assert set(dag1.nodes.keys()) == set(dag2.nodes.keys())
            for key in dag1.nodes:
                n1 = dag1.nodes[key]
                n2 = dag2.nodes[key]
                assert n1.name == n2.name
                assert n1.family_path == n2.family_path
                assert n1.jjob == n2.jjob
                assert n1.trigger == n2.trigger
                assert n1.events == n2.events
                assert n1.meters == n2.meters
                assert n1.variables == n2.variables
            # Edges should be structurally equal
            edges1 = {(e.source, e.target, e.kind) for e in dag1.edges}
            edges2 = {(e.source, e.target, e.kind) for e in dag2.edges}
            assert edges1 == edges2
        finally:
            os.unlink(tmp)

    def test_round_trip_cycled(self):
        """Round-trip with the full cycled config."""
        path = os.path.join(SAMPLE_DIR, "gfs_cycled.yaml")
        dag1 = parse(path)
        yaml_str = pretty_print(dag1)
        tmp = _write_yaml(yaml_str)
        try:
            dag2 = parse(tmp)
            assert dag1.suite_name == dag2.suite_name
            assert set(dag1.nodes.keys()) == set(dag2.nodes.keys())
            edges1 = {(e.source, e.target, e.kind) for e in dag1.edges}
            edges2 = {(e.source, e.target, e.kind) for e in dag2.edges}
            assert edges1 == edges2
        finally:
            os.unlink(tmp)

    def test_output_is_valid_yaml(self):
        """pretty_print output is valid YAML that can be loaded."""
        path = _write_yaml(MINIMAL_VALID)
        try:
            dag = parse(path)
            output = pretty_print(dag)
            import yaml
            loaded = yaml.safe_load(output)
            assert isinstance(loaded, dict)
            assert "suite" in loaded
            assert "families" in loaded
        finally:
            os.unlink(path)

    def test_output_contains_suite_name(self):
        """Output includes the suite name."""
        path = _write_yaml(MINIMAL_VALID)
        try:
            dag = parse(path)
            output = pretty_print(dag)
            assert "test_suite" in output
        finally:
            os.unlink(path)

    def test_output_preserves_family_paths(self):
        """Output preserves family path structure."""
        path = _write_yaml(MULTI_TRIGGER_CONFIG)
        try:
            dag = parse(path)
            output = pretty_print(dag)
            assert "app/prep" in output
            assert "app/analysis" in output
            assert "app/archive" in output
        finally:
            os.unlink(path)

    def test_output_preserves_meters(self):
        """Output preserves meter definitions."""
        path = _write_yaml(FOR_EACH_CONFIG)
        try:
            dag = parse(path)
            output = pretty_print(dag)
            assert "forecast_hour" in output
            # Meter min/max should be present
            import yaml
            loaded = yaml.safe_load(output)
            fcst_family = None
            for fam in loaded["families"]:
                if fam["path"] == "gfs/forecast":
                    fcst_family = fam
                    break
            assert fcst_family is not None
            fcst_task = fcst_family["tasks"][0]
            assert "meters" in fcst_task
            meter = fcst_task["meters"][0]
            assert meter["name"] == "forecast_hour"
            assert meter["min"] == 0
            assert meter["max"] == 12
        finally:
            os.unlink(path)

    def test_output_preserves_events(self):
        """Output preserves event declarations."""
        path = os.path.join(SAMPLE_DIR, "gfs_forecast_only.yaml")
        dag = parse(path)
        output = pretty_print(dag)
        import yaml
        loaded = yaml.safe_load(output)
        # Find the forecast family
        fcst_family = None
        for fam in loaded["families"]:
            if fam["path"] == "gfs/atmos/forecast":
                fcst_family = fam
                break
        assert fcst_family is not None
        fcst_task = fcst_family["tasks"][0]
        assert "events" in fcst_task
        assert "forecast_hour" in fcst_task["events"]

    def test_output_preserves_variables(self):
        """Output preserves task-level variables."""
        path = _write_yaml(FOR_EACH_CONFIG)
        try:
            dag = parse(path)
            output = pretty_print(dag)
            import yaml
            loaded = yaml.safe_load(output)
            post_family = None
            for fam in loaded["families"]:
                if fam["path"] == "gfs/post":
                    post_family = fam
                    break
            assert post_family is not None
            # First post task should have FHOUR variable
            task = post_family["tasks"][0]
            assert "variables" in task
            assert "FHOUR" in task["variables"]
        finally:
            os.unlink(path)

    def test_sort_keys_false(self):
        """Output does not alphabetically sort keys (preserves insertion order)."""
        path = _write_yaml(MINIMAL_VALID)
        try:
            dag = parse(path)
            output = pretty_print(dag)
            # 'suite' should come before 'families' in output
            suite_pos = output.index("suite:")
            families_pos = output.index("families:")
            assert suite_pos < families_pos
            # Within a task, 'name' should come before 'trigger' before 'jjob'
            name_pos = output.index("name: ")
            trigger_pos = output.index("trigger:")
            jjob_pos = output.index("jjob:")
            assert name_pos < trigger_pos < jjob_pos
        finally:
            os.unlink(path)
