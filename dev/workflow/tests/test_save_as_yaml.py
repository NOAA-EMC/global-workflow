"""Tests for save_as_yaml round-trip property.

Verifies that parse_yaml(save_as_yaml(cfg, path)) returns a tree equal to cfg.

Traces to: Requirement 4.8
"""

from __future__ import annotations

import os
import sys
import tempfile
from pathlib import Path

import yaml
import pytest

sys.path.insert(0, os.path.join(os.path.dirname(__file__), ".."))

from deployment.template_renderer import save_as_yaml


class TestSaveAsYaml:
    """Unit tests for save_as_yaml function."""

    def setup_method(self):
        self.tmpdir = tempfile.mkdtemp()

    def test_round_trip_simple_dict(self):
        """A simple dict round-trips through save_as_yaml and yaml.safe_load."""
        cfg = {"name": "gfs_v17", "version": "17.0.0", "enabled": True}
        path = Path(self.tmpdir) / "simple.yaml"

        save_as_yaml(cfg, path)
        loaded = yaml.safe_load(path.read_text(encoding="utf-8"))

        assert loaded == cfg

    def test_round_trip_nested_dict(self):
        """Nested dicts round-trip correctly."""
        cfg = {
            "suite": {"name": "gfs_v17", "ecf_home": "/path/to/ecf"},
            "defaults": {"ECF_TRIES": 2, "ECF_JOB_CMD": "uwtools submit"},
            "cycles": [
                {"name": "gdas", "time": "00:00 06:00 12:00 18:00"}
            ],
        }
        path = Path(self.tmpdir) / "nested.yaml"

        save_as_yaml(cfg, path)
        loaded = yaml.safe_load(path.read_text(encoding="utf-8"))

        assert loaded == cfg

    def test_round_trip_preserves_key_order(self):
        """Key order is preserved (sort_keys=False)."""
        cfg = {"zebra": 1, "alpha": 2, "middle": 3}
        path = Path(self.tmpdir) / "order.yaml"

        serialized = save_as_yaml(cfg, path)

        # Verify keys appear in insertion order in the serialized string
        zebra_pos = serialized.index("zebra")
        alpha_pos = serialized.index("alpha")
        middle_pos = serialized.index("middle")
        assert zebra_pos < alpha_pos < middle_pos

    def test_round_trip_with_lists(self):
        """Lists of mixed types round-trip correctly."""
        cfg = {
            "platforms": ["WCOSS2", "HERA", "HERCULES"],
            "fhr": [0, 6, 12, 24, 48, 72, 120, 180, 240, 384],
        }
        path = Path(self.tmpdir) / "lists.yaml"

        save_as_yaml(cfg, path)
        loaded = yaml.safe_load(path.read_text(encoding="utf-8"))

        assert loaded == cfg

    def test_round_trip_with_special_values(self):
        """None, booleans, and numeric types round-trip correctly."""
        cfg = {
            "nullable": None,
            "flag_true": True,
            "flag_false": False,
            "integer": 42,
            "floating": 3.14,
        }
        path = Path(self.tmpdir) / "special.yaml"

        save_as_yaml(cfg, path)
        loaded = yaml.safe_load(path.read_text(encoding="utf-8"))

        assert loaded == cfg

    def test_returns_serialized_string(self):
        """save_as_yaml returns the serialized YAML string."""
        cfg = {"key": "value"}
        path = Path(self.tmpdir) / "return.yaml"

        result = save_as_yaml(cfg, path)

        assert isinstance(result, str)
        assert "key: value" in result

    def test_creates_parent_directories(self):
        """Parent directories are created if they don't exist."""
        cfg = {"test": "data"}
        path = Path(self.tmpdir) / "sub" / "dir" / "output.yaml"

        save_as_yaml(cfg, path)

        assert path.exists()
        loaded = yaml.safe_load(path.read_text(encoding="utf-8"))
        assert loaded == cfg

    def test_round_trip_workflow_config(self):
        """A realistic workflow configuration round-trips correctly."""
        cfg = {
            "suite": {
                "name": "gfs_v17",
                "ecf_home": "{{ EXPDIR }}/ecf",
                "ecf_files": "{{ EXPDIR }}/ecf/scripts",
                "ecf_include": "{{ EXPDIR }}/ecf/include",
            },
            "defaults": {
                "ECF_TRIES": 2,
                "ECF_JOB_CMD": "uwtools submit %ECF_JOB% %ECF_JOBOUT%",
            },
            "families": [
                {
                    "path": "gdas/atmos/analysis",
                    "tasks": [
                        {
                            "name": "anal",
                            "trigger": "gdas/atmos/prep == complete",
                            "jjob": "JGDAS_ATMOS_ANALYSIS",
                        },
                    ],
                },
            ],
        }
        path = Path(self.tmpdir) / "workflow.yaml"

        save_as_yaml(cfg, path)
        loaded = yaml.safe_load(path.read_text(encoding="utf-8"))

        assert loaded == cfg
