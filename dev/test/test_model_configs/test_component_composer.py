"""Unit tests for component composition logic.

Tests component YAML loading, active component filtering, model section merge,
family merge, cross-component trigger resolution, and dangling reference removal.

Traces to: Requirements 10.3, 10.4, 10.7, 10.8, 10.9
"""

from __future__ import annotations

import os
import sys
from pathlib import Path

import pytest
import yaml

sys.path.insert(0, os.path.join(os.path.dirname(__file__), "..", "..", "workflow"))

from deployment.component_composer import (
    COMPONENT_REGISTRY,
    ComponentCompositionError,
    _extract_trigger_paths,
    _path_belongs_to_component,
    _path_belongs_to_excluded_component,
    _remove_dangling_refs,
    compose_components,
    load_active_components,
    load_component_yaml,
    merge_families,
    merge_model_sections,
    resolve_triggers,
)


# ---------------------------------------------------------------------------
# Fixtures
# ---------------------------------------------------------------------------


@pytest.fixture
def components_dir(tmp_path: Path) -> Path:
    """Create a temporary components directory with sample YAML files."""
    comp_dir = tmp_path / "components"
    comp_dir.mkdir()

    # atmos.yaml
    atmos = {
        "model": {
            "fv3": {
                "npx": 385,
                "npy": 385,
                "npz": 127,
                "layout": [6, 6],
                "total_tasks": 216,
            }
        },
        "families": [
            {
                "path": "gfs/atmos/forecast",
                "tasks": [
                    {
                        "name": "fcst",
                        "trigger": "",
                        "jjob": "JGLOBAL_FORECAST",
                    }
                ],
            },
            {
                "path": "gfs/atmos/post",
                "tasks": [
                    {
                        "name": "post",
                        "trigger": "gfs/atmos/forecast/fcst == complete",
                        "jjob": "JGFS_ATMOS_POST",
                    }
                ],
            },
        ],
    }
    (comp_dir / "atmos.yaml").write_text(yaml.dump(atmos))

    # ocean.yaml
    ocean = {
        "model": {
            "ocean": {
                "resolution": "025",
                "dt_ocean": 900,
                "tasks": 120,
            }
        },
        "families": [
            {
                "path": "gfs/ocean",
                "tasks": [
                    {
                        "name": "prep",
                        "trigger": "gfs/atmos/forecast/fcst == complete",
                        "jjob": "JGLOBAL_OCEAN_PREP",
                    },
                    {
                        "name": "post",
                        "trigger": "gfs/atmos/forecast/fcst == complete",
                        "jjob": "JGFS_OCEAN_POST",
                    },
                ],
            }
        ],
    }
    (comp_dir / "ocean.yaml").write_text(yaml.dump(ocean))

    # ice.yaml
    ice = {
        "model": {
            "ice": {
                "resolution": "025",
                "nprocs": 48,
                "dt_ice": 900,
            }
        },
        "families": [
            {
                "path": "gfs/ice",
                "tasks": [
                    {
                        "name": "post",
                        "trigger": "gfs/atmos/forecast/fcst == complete",
                        "jjob": "JGFS_ICE_POST",
                    }
                ],
            }
        ],
    }
    (comp_dir / "ice.yaml").write_text(yaml.dump(ice))

    # wave.yaml
    wave = {
        "model": {
            "wave": {
                "resolution": "gwes_30m",
                "tasks": 100,
                "dt_wave": 900,
            }
        },
        "families": [
            {
                "path": "gfs/wave",
                "tasks": [
                    {
                        "name": "postsbs",
                        "trigger": "gfs/atmos/forecast/fcst == complete",
                        "jjob": "JGFS_WAVE_POSTSBS",
                    }
                ],
            }
        ],
    }
    (comp_dir / "wave.yaml").write_text(yaml.dump(wave))

    # gocart.yaml
    gocart = {
        "model": {
            "aerosol": {
                "emission_dataset": "qfed",
                "active_collections": ["inst_aod"],
                "grid_label": "PC720x361-DC",
            }
        },
        "families": [
            {
                "path": "gfs/aerosol",
                "tasks": [
                    {
                        "name": "post",
                        "trigger": "gfs/atmos/forecast/fcst == complete",
                        "jjob": "JGFS_AERO_POST",
                    }
                ],
            }
        ],
    }
    (comp_dir / "gocart.yaml").write_text(yaml.dump(gocart))

    return comp_dir


@pytest.fixture
def full_workflow_config() -> dict:
    """A sample top-level workflow configuration."""
    return {
        "suite": {"name": "gfs_v17"},
        "components": ["atmosphere", "ocean", "ice", "wave", "aerosol"],
        "model": {
            "resolution": "C384",
            "physics_suite": "gfdl",
            "coupling_mode": "s2swa",
            "dt_atmos": 225,
            "output_grid": "gaussian_grid",
            "output_fields": "standard",
        },
        "families": [],
    }


# ---------------------------------------------------------------------------
# Tests: load_component_yaml
# ---------------------------------------------------------------------------


class TestLoadComponentYaml:
    """Tests for loading individual component YAML files."""

    def test_loads_atmosphere(self, components_dir: Path):
        data = load_component_yaml("atmosphere", components_dir)
        assert "model" in data
        assert "fv3" in data["model"]
        assert data["model"]["fv3"]["npx"] == 385

    def test_loads_ocean(self, components_dir: Path):
        data = load_component_yaml("ocean", components_dir)
        assert "model" in data
        assert "ocean" in data["model"]
        assert data["model"]["ocean"]["resolution"] == "025"

    def test_loads_ice(self, components_dir: Path):
        data = load_component_yaml("ice", components_dir)
        assert data["model"]["ice"]["nprocs"] == 48

    def test_loads_wave(self, components_dir: Path):
        data = load_component_yaml("wave", components_dir)
        assert data["model"]["wave"]["resolution"] == "gwes_30m"

    def test_loads_aerosol(self, components_dir: Path):
        data = load_component_yaml("aerosol", components_dir)
        assert data["model"]["aerosol"]["emission_dataset"] == "qfed"

    def test_unknown_component_raises(self, components_dir: Path):
        with pytest.raises(ComponentCompositionError, match="Unknown component"):
            load_component_yaml("unknown", components_dir)

    def test_missing_file_raises(self, tmp_path: Path):
        empty_dir = tmp_path / "empty"
        empty_dir.mkdir()
        with pytest.raises(ComponentCompositionError, match="not found"):
            load_component_yaml("atmosphere", empty_dir)

    def test_invalid_yaml_raises(self, tmp_path: Path):
        comp_dir = tmp_path / "bad"
        comp_dir.mkdir()
        (comp_dir / "atmos.yaml").write_text("{{invalid yaml")
        with pytest.raises(ComponentCompositionError, match="Failed to parse"):
            load_component_yaml("atmosphere", comp_dir)

    def test_empty_yaml_raises(self, tmp_path: Path):
        comp_dir = tmp_path / "empty_yaml"
        comp_dir.mkdir()
        (comp_dir / "atmos.yaml").write_text("")
        with pytest.raises(ComponentCompositionError, match="empty"):
            load_component_yaml("atmosphere", comp_dir)


# ---------------------------------------------------------------------------
# Tests: load_active_components
# ---------------------------------------------------------------------------


class TestLoadActiveComponents:
    """Tests for loading multiple active components."""

    def test_loads_all_components(self, components_dir: Path):
        loaded = load_active_components(
            ["atmosphere", "ocean", "ice"], components_dir
        )
        assert len(loaded) == 3
        assert "atmosphere" in loaded
        assert "ocean" in loaded
        assert "ice" in loaded

    def test_loads_subset(self, components_dir: Path):
        loaded = load_active_components(["atmosphere"], components_dir)
        assert len(loaded) == 1
        assert "atmosphere" in loaded

    def test_empty_list_returns_empty(self, components_dir: Path):
        loaded = load_active_components([], components_dir)
        assert loaded == {}

    def test_unknown_component_raises(self, components_dir: Path):
        with pytest.raises(ComponentCompositionError):
            load_active_components(["atmosphere", "unknown"], components_dir)


# ---------------------------------------------------------------------------
# Tests: merge_model_sections
# ---------------------------------------------------------------------------


class TestMergeModelSections:
    """Tests for merging component model sections."""

    def test_merges_ocean_section(self, components_dir: Path):
        base_model = {"resolution": "C384", "dt_atmos": 225}
        active = load_active_components(["ocean"], components_dir)
        merged = merge_model_sections(base_model, active)

        assert "ocean" in merged
        assert merged["ocean"]["resolution"] == "025"
        assert merged["ocean"]["dt_ocean"] == 900

    def test_merges_multiple_components(self, components_dir: Path):
        base_model = {"resolution": "C384"}
        active = load_active_components(
            ["atmosphere", "ocean", "ice"], components_dir
        )
        merged = merge_model_sections(base_model, active)

        assert "fv3" in merged
        assert "ocean" in merged
        assert "ice" in merged

    def test_explicit_values_override_component_defaults(self, components_dir: Path):
        # Base model already has ocean.resolution set
        base_model = {
            "resolution": "C384",
            "ocean": {"resolution": "050"},  # Override component default
        }
        active = load_active_components(["ocean"], components_dir)
        merged = merge_model_sections(base_model, active)

        # Explicit value should win
        assert merged["ocean"]["resolution"] == "050"
        # But other keys from component should still be merged
        assert merged["ocean"]["dt_ocean"] == 900

    def test_derives_active_components_list(self, components_dir: Path):
        base_model = {"resolution": "C384"}
        active = load_active_components(
            ["atmosphere", "ocean"], components_dir
        )
        merged = merge_model_sections(base_model, active)

        assert "active_components" in merged
        assert merged["active_components"] == ["atmosphere", "ocean"]

    def test_empty_active_components(self):
        base_model = {"resolution": "C384"}
        merged = merge_model_sections(base_model, {})
        assert merged["active_components"] == []

    def test_does_not_mutate_base_model(self, components_dir: Path):
        base_model = {"resolution": "C384"}
        original = base_model.copy()
        active = load_active_components(["ocean"], components_dir)
        merge_model_sections(base_model, active)

        # Original should not be modified
        assert base_model == original


# ---------------------------------------------------------------------------
# Tests: merge_families
# ---------------------------------------------------------------------------


class TestMergeFamilies:
    """Tests for merging component families."""

    def test_merges_component_families(self, components_dir: Path):
        base_families: list = []
        active = load_active_components(["ocean"], components_dir)
        merged = merge_families(base_families, active)

        assert len(merged) == 1
        assert merged[0]["path"] == "gfs/ocean"

    def test_merges_multiple_component_families(self, components_dir: Path):
        base_families: list = []
        active = load_active_components(
            ["atmosphere", "ocean", "wave"], components_dir
        )
        merged = merge_families(base_families, active)

        paths = [f["path"] for f in merged]
        assert "gfs/atmos/forecast" in paths
        assert "gfs/atmos/post" in paths
        assert "gfs/ocean" in paths
        assert "gfs/wave" in paths

    def test_deduplicates_by_path(self, components_dir: Path):
        # Base already has gfs/ocean
        base_families = [
            {"path": "gfs/ocean", "tasks": [{"name": "existing", "jjob": "J1"}]}
        ]
        active = load_active_components(["ocean"], components_dir)
        merged = merge_families(base_families, active)

        # Should not duplicate gfs/ocean
        ocean_families = [f for f in merged if f["path"] == "gfs/ocean"]
        assert len(ocean_families) == 1
        # Should keep the base version
        assert ocean_families[0]["tasks"][0]["name"] == "existing"

    def test_preserves_base_families(self, components_dir: Path):
        base_families = [
            {"path": "custom/family", "tasks": [{"name": "task1", "jjob": "J1"}]}
        ]
        active = load_active_components(["ocean"], components_dir)
        merged = merge_families(base_families, active)

        paths = [f["path"] for f in merged]
        assert "custom/family" in paths
        assert "gfs/ocean" in paths

    def test_empty_active_components(self):
        base_families = [{"path": "gfs/atmos", "tasks": []}]
        merged = merge_families(base_families, {})
        assert merged == base_families


# ---------------------------------------------------------------------------
# Tests: trigger path extraction
# ---------------------------------------------------------------------------


class TestExtractTriggerPaths:
    """Tests for extracting task paths from trigger expressions."""

    def test_simple_complete_trigger(self):
        paths = _extract_trigger_paths("gfs/atmos/forecast/fcst == complete")
        assert paths == ["gfs/atmos/forecast/fcst"]

    def test_meter_trigger(self):
        paths = _extract_trigger_paths(
            "gfs/atmos/forecast/fcst:forecast_hour ge 6"
        )
        assert paths == ["gfs/atmos/forecast/fcst"]

    def test_compound_trigger(self):
        paths = _extract_trigger_paths(
            "gfs/atmos/forecast/fcst == complete and gfs/ocean/prep == complete"
        )
        assert "gfs/atmos/forecast/fcst" in paths
        assert "gfs/ocean/prep" in paths

    def test_empty_trigger(self):
        assert _extract_trigger_paths("") == []
        assert _extract_trigger_paths(None) == []


# ---------------------------------------------------------------------------
# Tests: path belongs to component
# ---------------------------------------------------------------------------


class TestPathBelongsToComponent:
    """Tests for checking if a path belongs to a component."""

    def test_atmos_path(self):
        assert _path_belongs_to_component("gfs/atmos/forecast/fcst", "atmosphere")
        assert _path_belongs_to_component("gdas/atmos/analysis/anal", "atmosphere")

    def test_ocean_path(self):
        assert _path_belongs_to_component("gfs/ocean/post", "ocean")
        assert not _path_belongs_to_component("gfs/atmos/post", "ocean")

    def test_wave_path(self):
        assert _path_belongs_to_component("gfs/wave/postsbs", "wave")
        assert not _path_belongs_to_component("gfs/ocean/post", "wave")

    def test_ice_path(self):
        assert _path_belongs_to_component("gfs/ice/post", "ice")

    def test_aerosol_path(self):
        assert _path_belongs_to_component("gfs/aerosol/post", "aerosol")
        assert _path_belongs_to_component("gfs/aero/post", "aerosol")


# ---------------------------------------------------------------------------
# Tests: resolve_triggers (dangling reference removal)
# ---------------------------------------------------------------------------


class TestResolveTriggers:
    """Tests for cross-component trigger resolution."""

    def test_no_excluded_components_no_changes(self):
        families = [
            {
                "path": "gfs/ocean",
                "tasks": [
                    {
                        "name": "post",
                        "trigger": "gfs/atmos/forecast/fcst == complete",
                    }
                ],
            }
        ]
        resolved, warnings = resolve_triggers(families, set())
        assert resolved == families
        assert warnings == []

    def test_removes_dangling_ocean_reference(self):
        families = [
            {
                "path": "gfs/atmos/post",
                "tasks": [
                    {
                        "name": "post",
                        "trigger": "gfs/ocean/prep == complete",
                    }
                ],
            }
        ]
        resolved, warnings = resolve_triggers(families, {"ocean"})

        assert resolved[0]["tasks"][0]["trigger"] == ""
        assert len(warnings) == 1
        assert "gfs/ocean/prep" in warnings[0]

    def test_removes_dangling_wave_reference_keeps_valid(self):
        families = [
            {
                "path": "gfs/post",
                "tasks": [
                    {
                        "name": "final",
                        "trigger": "gfs/atmos/forecast/fcst == complete and gfs/wave/postsbs == complete",
                    }
                ],
            }
        ]
        resolved, warnings = resolve_triggers(families, {"wave"})

        trigger = resolved[0]["tasks"][0]["trigger"]
        assert "gfs/wave" not in trigger
        assert "gfs/atmos/forecast/fcst" in trigger
        assert len(warnings) == 1

    def test_multiple_dangling_references(self):
        families = [
            {
                "path": "gfs/post",
                "tasks": [
                    {
                        "name": "final",
                        "trigger": "gfs/ocean/post == complete and gfs/wave/postsbs == complete",
                    }
                ],
            }
        ]
        resolved, warnings = resolve_triggers(families, {"ocean", "wave"})

        trigger = resolved[0]["tasks"][0]["trigger"]
        assert "gfs/ocean" not in trigger
        assert "gfs/wave" not in trigger
        assert len(warnings) == 2

    def test_does_not_modify_original(self):
        families = [
            {
                "path": "gfs/atmos",
                "tasks": [
                    {
                        "name": "post",
                        "trigger": "gfs/ocean/prep == complete",
                    }
                ],
            }
        ]
        original_trigger = families[0]["tasks"][0]["trigger"]
        resolve_triggers(families, {"ocean"})

        # Original should not be modified
        assert families[0]["tasks"][0]["trigger"] == original_trigger

    def test_empty_trigger_unchanged(self):
        families = [
            {
                "path": "gfs/atmos",
                "tasks": [{"name": "fcst", "trigger": ""}],
            }
        ]
        resolved, warnings = resolve_triggers(families, {"ocean"})
        assert resolved[0]["tasks"][0]["trigger"] == ""
        assert warnings == []


# ---------------------------------------------------------------------------
# Tests: _remove_dangling_refs helper
# ---------------------------------------------------------------------------


class TestRemoveDanglingRefs:
    """Tests for the dangling reference removal helper."""

    def test_removes_single_ref(self):
        result = _remove_dangling_refs(
            "gfs/ocean/prep == complete", ["gfs/ocean/prep"]
        )
        assert result == ""

    def test_removes_ref_with_and(self):
        result = _remove_dangling_refs(
            "gfs/atmos/fcst == complete and gfs/ocean/prep == complete",
            ["gfs/ocean/prep"],
        )
        assert "gfs/atmos/fcst == complete" in result
        assert "gfs/ocean" not in result

    def test_removes_meter_ref(self):
        result = _remove_dangling_refs(
            "gfs/wave/init:step ge 5", ["gfs/wave/init"]
        )
        assert result == ""


# ---------------------------------------------------------------------------
# Tests: compose_components (integration)
# ---------------------------------------------------------------------------


class TestComposeComponents:
    """Integration tests for the full composition pipeline."""

    def test_full_composition(
        self, full_workflow_config: dict, components_dir: Path
    ):
        result = compose_components(full_workflow_config, components_dir)

        # Model sections should be merged
        assert "fv3" in result["model"]
        assert "ocean" in result["model"]
        assert "ice" in result["model"]
        assert "wave" in result["model"]
        assert "aerosol" in result["model"]

        # Active components should be derived
        assert "active_components" in result["model"]
        assert set(result["model"]["active_components"]) == {
            "atmosphere", "ocean", "ice", "wave", "aerosol"
        }

        # Families should be merged
        family_paths = [f["path"] for f in result["families"]]
        assert "gfs/atmos/forecast" in family_paths
        assert "gfs/ocean" in family_paths
        assert "gfs/ice" in family_paths
        assert "gfs/wave" in family_paths
        assert "gfs/aerosol" in family_paths

    def test_subset_composition(
        self, full_workflow_config: dict, components_dir: Path
    ):
        # Only atmosphere and ocean
        full_workflow_config["components"] = ["atmosphere", "ocean"]
        result = compose_components(full_workflow_config, components_dir)

        assert "fv3" in result["model"]
        assert "ocean" in result["model"]
        assert "ice" not in result["model"]
        assert "wave" not in result["model"]

        # Active components should reflect the subset
        assert result["model"]["active_components"] == ["atmosphere", "ocean"]

    def test_excluded_component_triggers_removed(
        self, full_workflow_config: dict, components_dir: Path
    ):
        # Exclude wave - ocean's trigger to atmos should remain valid
        full_workflow_config["components"] = ["atmosphere", "ocean"]
        result = compose_components(full_workflow_config, components_dir)

        # Ocean's trigger references atmos, which is active - should remain
        ocean_families = [
            f for f in result["families"] if f["path"] == "gfs/ocean"
        ]
        assert len(ocean_families) == 1
        for task in ocean_families[0]["tasks"]:
            if task["trigger"]:
                assert "gfs/atmos" in task["trigger"]

    def test_no_components_key_returns_unchanged(self, components_dir: Path):
        config = {"suite": {"name": "test"}, "model": {"resolution": "C96"}}
        result = compose_components(config, components_dir)
        assert result == config

    def test_empty_components_list_returns_unchanged(self, components_dir: Path):
        config = {
            "suite": {"name": "test"},
            "components": [],
            "model": {"resolution": "C96"},
        }
        result = compose_components(config, components_dir)
        # Should still have the original model section
        assert result["model"]["resolution"] == "C96"

    def test_unknown_component_raises(self, components_dir: Path):
        config = {
            "components": ["atmosphere", "unknown_component"],
            "model": {},
        }
        with pytest.raises(ComponentCompositionError, match="Unknown component"):
            compose_components(config, components_dir)

    def test_does_not_mutate_input(
        self, full_workflow_config: dict, components_dir: Path
    ):
        import copy
        original = copy.deepcopy(full_workflow_config)
        compose_components(full_workflow_config, components_dir)
        assert full_workflow_config == original

    def test_atmosphere_only(
        self, full_workflow_config: dict, components_dir: Path
    ):
        full_workflow_config["components"] = ["atmosphere"]
        result = compose_components(full_workflow_config, components_dir)

        assert "fv3" in result["model"]
        assert result["model"]["active_components"] == ["atmosphere"]

        # No ocean/ice/wave/aerosol families
        family_paths = [f["path"] for f in result["families"]]
        assert "gfs/ocean" not in family_paths
        assert "gfs/ice" not in family_paths
        assert "gfs/wave" not in family_paths
        assert "gfs/aerosol" not in family_paths
