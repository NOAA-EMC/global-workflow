"""Unit tests for ModelConfigRenderer enhanced DAG-aware rendering.

Tests the DAG-aware rendering logic that determines active UFS components
from the reachability set and renders only templates for active components.
Also tests:
- Zero-token verification catches unresolved Jinja2 tokens
- Shell variable preservation in rendered output
- Fortran namelist output parseability (Requirement 14.1-14.4)
- FATAL ERROR on undefined Jinja2 variable (Requirement 6.6)

Traces to: Requirements 6.1–6.7, 14.1–14.4
"""

from __future__ import annotations

import os
import sys
from pathlib import Path

import pytest

sys.path.insert(0, os.path.join(os.path.dirname(__file__), ".."))

from deployment.dag_filter import DAGReachabilitySet
from deployment.model_config_renderer import (
    ModelConfigRenderer,
    RenderedFile,
    _UFS_COMPONENT_FLAGS,
    _UFS_COMPONENT_KEYWORDS,
    _is_truthy,
)
from deployment.pipeline import PipelineError
from deployment.template_renderer import TemplateRenderError
from deployment.validators import NamelistValidator


# ---------------------------------------------------------------------------
# Fixtures
# ---------------------------------------------------------------------------


@pytest.fixture
def tmp_dev_root(tmp_path: Path) -> Path:
    """Create a minimal dev/ directory structure with UFS component templates."""
    dev_root = tmp_path / "dev"
    ufs_dir = dev_root / "parm" / "ufs"

    # Create component directories with templates
    fv3_dir = ufs_dir / "fv3"
    ocean_dir = ufs_dir / "ocean"
    ice_dir = ufs_dir / "ice"
    wave_dir = ufs_dir / "wave"
    gocart_dir = ufs_dir / "gocart"

    for d in [fv3_dir, ocean_dir, ice_dir, wave_dir, gocart_dir]:
        d.mkdir(parents=True)

    # FV3 templates (core atmosphere — always active)
    (fv3_dir / "model_configure.j2").write_text(
        "print_esmf:          .true.\n"
        "total_member:        1\n"
        "PE_MEMBER01:         {{ model.fv3.total_tasks }}\n"
        "dt_atmos:            {{ model.dt_atmos }}\n"
        "restart_interval:    {{ model.fv3.restart_interval }}\n"
        "quilting:            {{ model.fv3.quilting | fortran_logical }}\n"
        "write_groups:        {{ model.fv3.write_group }}\n"
        "write_tasks_per_group: {{ model.fv3.wrttask_per_group }}\n"
    )
    (fv3_dir / "input.nml.j2").write_text(
        "&atmos_model_nml\n"
        "  blocksize = {{ model.fv3.blocksize | default(32) }}\n"
        "/\n"
        "\n"
        "&fv_core_nml\n"
        "  npx = {{ model.fv3.npx }}\n"
        "  npy = {{ model.fv3.npy }}\n"
        "  npz = {{ model.fv3.npz }}\n"
        "  dt_atmos = {{ model.dt_atmos }}\n"
        "/\n"
    )

    # Ocean template
    (ocean_dir / "MOM_input.j2").write_text(
        "! MOM6 parameter file\n"
        "DT = {{ model.ocean.dt_ocean | default(900) }}\n"
    )

    # Ice template
    (ice_dir / "ice_in.j2").write_text(
        "&setup_nml\n"
        "  dt = {{ model.ice.dt_ice | default(900) }}\n"
        "/\n"
    )

    # Wave template
    (wave_dir / "ww3_shel.nml.j2").write_text(
        "&domain_nml\n"
        "  dt = {{ model.wave.dt_wave | default(3600) }}\n"
        "/\n"
    )

    # GOCART template
    (gocart_dir / "AERO_HISTORY.rc.j2").write_text(
        "# GOCART history\n"
        "VERSION: 1\n"
        "EXPID:  gocart\n"
        "COLLECTIONS::\n"
        "::\n"
    )

    # Top-level UFS configure template (always included)
    (ufs_dir / "ufs.configure.j2").write_text(
        "# UFS configure for {{ model.coupling_mode }}\n"
        "EARTH_component_list: ATM\n"
        "EARTH_attributes::\n"
        "  Verbosity = 0\n"
        "::\n"
        "runSeq::\n"
        "  @{{ model.dt_atmos }}\n"
        "    ATM\n"
        "  @\n"
        "::\n"
    )

    return dev_root


@pytest.fixture
def valid_model_context() -> dict:
    """A minimal valid model context for testing."""
    return {
        "resolution": "C96",
        "physics_suite": "gfdl",
        "coupling_mode": "atm",
        "dt_atmos": 450,
        "output_grid": "gaussian_grid",
        "output_fields": "standard",
        "fv3": {
            "npx": 97,
            "npy": 97,
            "npz": 127,
            "layout": [2, 2],
            "io_layout": [1, 1],
            "quilting": True,
            "write_group": 1,
            "wrttask_per_group": 24,
            "restart_interval": 12,
            "total_tasks": 24,
        },
    }


@pytest.fixture
def expdir(tmp_path: Path) -> Path:
    """Create a temporary EXPDIR."""
    exp = tmp_path / "expdir"
    exp.mkdir()
    return exp


def _make_reachability_set(
    jjobs: set[str] | None = None,
    ex_scripts: set[str] | None = None,
) -> DAGReachabilitySet:
    """Helper to create a DAGReachabilitySet with given artifacts."""
    return DAGReachabilitySet(
        jjobs=frozenset(jjobs or set()),
        ex_scripts=frozenset(ex_scripts or set()),
        ush_scripts=frozenset(),
        config_files=frozenset(),
        warnings=(),
    )


# ---------------------------------------------------------------------------
# Tests: _is_truthy helper
# ---------------------------------------------------------------------------


class TestIsTruthy:
    """Tests for the _is_truthy helper function."""

    def test_bool_true(self):
        assert _is_truthy(True) is True

    def test_bool_false(self):
        assert _is_truthy(False) is False

    def test_string_yes(self):
        assert _is_truthy("YES") is True
        assert _is_truthy("yes") is True

    def test_string_true(self):
        assert _is_truthy("True") is True
        assert _is_truthy("true") is True
        assert _is_truthy(".true.") is True

    def test_string_no(self):
        assert _is_truthy("NO") is False
        assert _is_truthy("no") is False

    def test_string_false(self):
        assert _is_truthy("False") is False
        assert _is_truthy("false") is False

    def test_int_truthy(self):
        assert _is_truthy(1) is True
        assert _is_truthy(0) is False

    def test_none(self):
        assert _is_truthy(None) is False

    def test_empty_string(self):
        assert _is_truthy("") is False


# ---------------------------------------------------------------------------
# Tests: _determine_active_components
# ---------------------------------------------------------------------------


class TestDetermineActiveComponents:
    """Tests for component detection from reachability set."""

    def test_fv3_always_active(self, tmp_dev_root: Path):
        """FV3 is always active regardless of DAG content."""
        renderer = ModelConfigRenderer(dev_root=tmp_dev_root)
        reachability = _make_reachability_set(
            jjobs={"JGLOBAL_FORECAST"},
            ex_scripts={"exglobal_forecast.sh"},
        )
        active = renderer._determine_active_components(reachability, {})
        assert "fv3" in active

    def test_wave_active_when_wave_jjob_present(self, tmp_dev_root: Path):
        """Wave component active when a wave-related J-Job is in the DAG."""
        renderer = ModelConfigRenderer(dev_root=tmp_dev_root)
        reachability = _make_reachability_set(
            jjobs={"JGLOBAL_FORECAST", "JGFS_WAVE_POST"},
            ex_scripts={"exglobal_forecast.sh", "exgfs_wave_post.sh"},
        )
        active = renderer._determine_active_components(reachability, {})
        assert "wave" in active

    def test_wave_inactive_when_no_wave_artifacts(self, tmp_dev_root: Path):
        """Wave component inactive when no wave-related artifacts in DAG."""
        renderer = ModelConfigRenderer(dev_root=tmp_dev_root)
        reachability = _make_reachability_set(
            jjobs={"JGLOBAL_FORECAST"},
            ex_scripts={"exglobal_forecast.sh"},
        )
        active = renderer._determine_active_components(reachability, {})
        assert "wave" not in active

    def test_ocean_active_when_ocn_jjob_present(self, tmp_dev_root: Path):
        """Ocean component active when an ocean-related J-Job is in the DAG."""
        renderer = ModelConfigRenderer(dev_root=tmp_dev_root)
        reachability = _make_reachability_set(
            jjobs={"JGLOBAL_FORECAST", "JGLOBAL_OCEAN_ANALYSIS"},
            ex_scripts={"exglobal_forecast.sh"},
        )
        active = renderer._determine_active_components(reachability, {})
        assert "ocean" in active

    def test_ocean_active_via_do_ocn_flag(self, tmp_dev_root: Path):
        """Ocean component active when DO_OCN flag is truthy."""
        renderer = ModelConfigRenderer(dev_root=tmp_dev_root)
        reachability = _make_reachability_set(
            jjobs={"JGLOBAL_FORECAST"},
            ex_scripts={"exglobal_forecast.sh"},
        )
        context = {"DO_OCN": "YES"}
        active = renderer._determine_active_components(reachability, context)
        assert "ocean" in active

    def test_ice_active_when_ice_jjob_present(self, tmp_dev_root: Path):
        """Ice component active when an ice-related J-Job is in the DAG."""
        renderer = ModelConfigRenderer(dev_root=tmp_dev_root)
        reachability = _make_reachability_set(
            jjobs={"JGLOBAL_FORECAST", "JGFS_ICE_POST"},
            ex_scripts={"exglobal_forecast.sh", "exgfs_ice_post.sh"},
        )
        active = renderer._determine_active_components(reachability, {})
        assert "ice" in active

    def test_ice_active_via_do_ice_flag(self, tmp_dev_root: Path):
        """Ice component active when DO_ICE flag is truthy."""
        renderer = ModelConfigRenderer(dev_root=tmp_dev_root)
        reachability = _make_reachability_set(
            jjobs={"JGLOBAL_FORECAST"},
            ex_scripts={"exglobal_forecast.sh"},
        )
        context = {"DO_ICE": "YES"}
        active = renderer._determine_active_components(reachability, context)
        assert "ice" in active

    def test_gocart_active_when_aero_jjob_present(self, tmp_dev_root: Path):
        """GOCART component active when an aero-related J-Job is in the DAG."""
        renderer = ModelConfigRenderer(dev_root=tmp_dev_root)
        reachability = _make_reachability_set(
            jjobs={"JGLOBAL_FORECAST", "JGDAS_AERO_ANALYSIS_GENERATE_BMATRIX"},
            ex_scripts={"exglobal_forecast.sh"},
        )
        active = renderer._determine_active_components(reachability, {})
        assert "gocart" in active

    def test_gocart_active_via_do_aero_flag(self, tmp_dev_root: Path):
        """GOCART component active when DO_AERO flag is truthy."""
        renderer = ModelConfigRenderer(dev_root=tmp_dev_root)
        reachability = _make_reachability_set(
            jjobs={"JGLOBAL_FORECAST"},
            ex_scripts={"exglobal_forecast.sh"},
        )
        context = {"DO_AERO": "YES"}
        active = renderer._determine_active_components(reachability, context)
        assert "gocart" in active

    def test_multiple_components_active(self, tmp_dev_root: Path):
        """Multiple components can be active simultaneously."""
        renderer = ModelConfigRenderer(dev_root=tmp_dev_root)
        reachability = _make_reachability_set(
            jjobs={"JGLOBAL_FORECAST", "JGFS_WAVE_POST", "JGFS_ICE_POST"},
            ex_scripts={"exglobal_forecast.sh", "exgfs_wave_post.sh"},
        )
        active = renderer._determine_active_components(reachability, {})
        assert "fv3" in active
        assert "wave" in active
        assert "ice" in active

    def test_case_insensitive_keyword_matching(self, tmp_dev_root: Path):
        """Keyword matching is case-insensitive."""
        renderer = ModelConfigRenderer(dev_root=tmp_dev_root)
        reachability = _make_reachability_set(
            jjobs={"JGFS_WAVE_POST"},  # uppercase
            ex_scripts=set(),
        )
        active = renderer._determine_active_components(reachability, {})
        assert "wave" in active

    def test_do_ocn_false_does_not_activate(self, tmp_dev_root: Path):
        """DO_OCN=NO does not activate ocean component."""
        renderer = ModelConfigRenderer(dev_root=tmp_dev_root)
        reachability = _make_reachability_set(
            jjobs={"JGLOBAL_FORECAST"},
            ex_scripts={"exglobal_forecast.sh"},
        )
        context = {"DO_OCN": "NO"}
        active = renderer._determine_active_components(reachability, context)
        assert "ocean" not in active


# ---------------------------------------------------------------------------
# Tests: render_for_dag
# ---------------------------------------------------------------------------


class TestRenderForDag:
    """Tests for the render_for_dag method."""

    def test_renders_only_fv3_when_no_coupled_tasks(
        self, tmp_dev_root: Path, valid_model_context: dict, expdir: Path
    ):
        """When only atmosphere tasks are reachable, only fv3 templates render."""
        renderer = ModelConfigRenderer(dev_root=tmp_dev_root)
        reachability = _make_reachability_set(
            jjobs={"JGLOBAL_FORECAST"},
            ex_scripts={"exglobal_forecast.sh"},
        )
        results = renderer.render_for_dag(valid_model_context, expdir, reachability)

        # Should have fv3 files and top-level ufs.configure
        rendered_paths = {str(r.path.relative_to(expdir)) for r in results}
        assert "parm/ufs/fv3/model_configure" in rendered_paths
        assert "parm/ufs/fv3/input.nml" in rendered_paths
        assert "parm/ufs/ufs.configure" in rendered_paths

        # Should NOT have ocean, ice, wave, gocart files
        assert not any("ocean" in p for p in rendered_paths)
        assert not any("ice" in p for p in rendered_paths)
        assert not any("wave" in p for p in rendered_paths)
        assert not any("gocart" in p for p in rendered_paths)

    def test_renders_wave_when_wave_task_reachable(
        self, tmp_dev_root: Path, valid_model_context: dict, expdir: Path
    ):
        """When wave tasks are reachable, wave templates are rendered."""
        # Add wave context needed for rendering
        valid_model_context["wave"] = {"dt_wave": 3600}

        renderer = ModelConfigRenderer(dev_root=tmp_dev_root)
        reachability = _make_reachability_set(
            jjobs={"JGLOBAL_FORECAST", "JGFS_WAVE_PREP"},
            ex_scripts={"exglobal_forecast.sh", "exgfs_wave_prep.sh"},
        )
        results = renderer.render_for_dag(valid_model_context, expdir, reachability)

        rendered_paths = {str(r.path.relative_to(expdir)) for r in results}
        assert "parm/ufs/wave/ww3_shel.nml" in rendered_paths
        assert "parm/ufs/fv3/model_configure" in rendered_paths

    def test_renders_gocart_when_aero_task_reachable(
        self, tmp_dev_root: Path, valid_model_context: dict, expdir: Path
    ):
        """When aero tasks are reachable, gocart templates are rendered."""
        renderer = ModelConfigRenderer(dev_root=tmp_dev_root)
        reachability = _make_reachability_set(
            jjobs={"JGLOBAL_FORECAST", "JGDAS_AERO_ANALYSIS_GENERATE_BMATRIX"},
            ex_scripts={"exglobal_forecast.sh"},
        )
        results = renderer.render_for_dag(valid_model_context, expdir, reachability)

        rendered_paths = {str(r.path.relative_to(expdir)) for r in results}
        assert "parm/ufs/gocart/AERO_HISTORY.rc" in rendered_paths

    def test_skips_wave_when_no_wave_tasks(
        self, tmp_dev_root: Path, valid_model_context: dict, expdir: Path
    ):
        """Wave templates are skipped when no wave tasks are reachable."""
        renderer = ModelConfigRenderer(dev_root=tmp_dev_root)
        reachability = _make_reachability_set(
            jjobs={"JGLOBAL_FORECAST"},
            ex_scripts={"exglobal_forecast.sh"},
        )
        results = renderer.render_for_dag(valid_model_context, expdir, reachability)

        rendered_paths = {str(r.path.relative_to(expdir)) for r in results}
        assert not any("wave" in p for p in rendered_paths)

    def test_renders_ocean_via_context_flag(
        self, tmp_dev_root: Path, valid_model_context: dict, expdir: Path
    ):
        """Ocean templates rendered when DO_OCN flag is set in context."""
        valid_model_context["DO_OCN"] = "YES"
        valid_model_context["ocean"] = {"dt_ocean": 900, "resolution": "100"}

        renderer = ModelConfigRenderer(dev_root=tmp_dev_root)
        reachability = _make_reachability_set(
            jjobs={"JGLOBAL_FORECAST"},
            ex_scripts={"exglobal_forecast.sh"},
        )
        results = renderer.render_for_dag(valid_model_context, expdir, reachability)

        rendered_paths = {str(r.path.relative_to(expdir)) for r in results}
        assert "parm/ufs/ocean/MOM_input" in rendered_paths

    def test_all_rendered_files_exist(
        self, tmp_dev_root: Path, valid_model_context: dict, expdir: Path
    ):
        """All returned RenderedFile instances point to existing files."""
        renderer = ModelConfigRenderer(dev_root=tmp_dev_root)
        reachability = _make_reachability_set(
            jjobs={"JGLOBAL_FORECAST"},
            ex_scripts={"exglobal_forecast.sh"},
        )
        results = renderer.render_for_dag(valid_model_context, expdir, reachability)

        for r in results:
            assert r.path.exists(), f"Rendered file does not exist: {r.path}"
            assert r.sha256, "SHA-256 hash should be non-empty"
            assert r.method in ("render", "copy")

    def test_top_level_templates_always_included(
        self, tmp_dev_root: Path, valid_model_context: dict, expdir: Path
    ):
        """Top-level templates (ufs.configure) are always rendered."""
        renderer = ModelConfigRenderer(dev_root=tmp_dev_root)
        reachability = _make_reachability_set(
            jjobs={"JGLOBAL_FORECAST"},
            ex_scripts={"exglobal_forecast.sh"},
        )
        results = renderer.render_for_dag(valid_model_context, expdir, reachability)

        rendered_paths = {str(r.path.relative_to(expdir)) for r in results}
        assert "parm/ufs/ufs.configure" in rendered_paths

    def test_static_files_filtered_by_component(
        self, tmp_dev_root: Path, valid_model_context: dict, expdir: Path
    ):
        """Static files under inactive components are not copied."""
        # Add a static file under wave/
        wave_static = tmp_dev_root / "parm" / "ufs" / "wave" / "ww3_grid.inp"
        wave_static.write_text("# Wave grid input\n")

        renderer = ModelConfigRenderer(dev_root=tmp_dev_root)
        reachability = _make_reachability_set(
            jjobs={"JGLOBAL_FORECAST"},
            ex_scripts={"exglobal_forecast.sh"},
        )
        results = renderer.render_for_dag(valid_model_context, expdir, reachability)

        rendered_paths = {str(r.path.relative_to(expdir)) for r in results}
        assert "parm/ufs/wave/ww3_grid.inp" not in rendered_paths


# ---------------------------------------------------------------------------
# Tests: verify_no_unresolved_tokens (Requirements 6.4, 6.6)
# ---------------------------------------------------------------------------


class TestVerifyNoUnresolvedTokens:
    """Tests for the verify_no_unresolved_tokens method."""

    def test_passes_clean_rendered_files(self, tmp_dev_root: Path, tmp_path: Path):
        """No error raised when rendered files contain no Jinja2 tokens."""
        renderer = ModelConfigRenderer(dev_root=tmp_dev_root)

        # Create a clean rendered file
        output = tmp_path / "clean_output.nml"
        output.write_text(
            "&fv_core_nml\n"
            "  npx = 97\n"
            "  npy = 97\n"
            "  dt_atmos = 450\n"
            "/\n"
        )
        rendered_files = [
            RenderedFile(path=output, sha256="abc123", source="fv3/input.nml.j2", method="render")
        ]

        # Should not raise
        renderer.verify_no_unresolved_tokens(rendered_files)

    def test_detects_unresolved_variable_token(self, tmp_dev_root: Path, tmp_path: Path):
        """Raises PipelineError when {{ is found in rendered output."""
        from deployment.pipeline import PipelineError

        renderer = ModelConfigRenderer(dev_root=tmp_dev_root)

        output = tmp_path / "bad_output.nml"
        output.write_text(
            "&fv_core_nml\n"
            "  npx = {{ model.fv3.npx }}\n"
            "  npy = 97\n"
            "/\n"
        )
        rendered_files = [
            RenderedFile(path=output, sha256="abc123", source="fv3/input.nml.j2", method="render")
        ]

        with pytest.raises(PipelineError) as exc_info:
            renderer.verify_no_unresolved_tokens(rendered_files)

        assert "{{" in str(exc_info.value)
        assert "line 2" in str(exc_info.value)
        assert str(output) in str(exc_info.value)

    def test_detects_unresolved_block_token(self, tmp_dev_root: Path, tmp_path: Path):
        """Raises PipelineError when {% is found in rendered output."""
        from deployment.pipeline import PipelineError

        renderer = ModelConfigRenderer(dev_root=tmp_dev_root)

        output = tmp_path / "bad_block.nml"
        output.write_text(
            "# Config file\n"
            "{% if model.do_wave %}\n"
            "WAVE=YES\n"
            "{% endif %}\n"
        )
        rendered_files = [
            RenderedFile(path=output, sha256="abc123", source="config.j2", method="render")
        ]

        with pytest.raises(PipelineError) as exc_info:
            renderer.verify_no_unresolved_tokens(rendered_files)

        assert "{%" in str(exc_info.value)
        assert "line 2" in str(exc_info.value)

    def test_detects_unresolved_comment_token(self, tmp_dev_root: Path, tmp_path: Path):
        """Raises PipelineError when {# is found in rendered output."""
        from deployment.pipeline import PipelineError

        renderer = ModelConfigRenderer(dev_root=tmp_dev_root)

        output = tmp_path / "bad_comment.nml"
        output.write_text(
            "# Config\n"
            "VALUE=42\n"
            "{# This is a Jinja2 comment #}\n"
        )
        rendered_files = [
            RenderedFile(path=output, sha256="abc123", source="config.j2", method="render")
        ]

        with pytest.raises(PipelineError) as exc_info:
            renderer.verify_no_unresolved_tokens(rendered_files)

        assert "{#" in str(exc_info.value)
        assert "line 3" in str(exc_info.value)

    def test_preserves_shell_vars_not_flagged(self, tmp_dev_root: Path, tmp_path: Path):
        """Shell variables like ${DATA} do NOT trigger unresolved token detection."""
        renderer = ModelConfigRenderer(dev_root=tmp_dev_root)

        output = tmp_path / "shell_vars.sh"
        output.write_text(
            "#!/bin/bash\n"
            "export WORKDIR=${DATA}/forecast\n"
            "cpreq ${ROTDIR}/input.nml ${DATA}/input.nml\n"
        )
        rendered_files = [
            RenderedFile(path=output, sha256="abc123", source="script.sh.j2", method="render")
        ]

        # Should not raise — ${} is shell, not Jinja2
        renderer.verify_no_unresolved_tokens(rendered_files)

    def test_error_message_includes_file_line_token(self, tmp_dev_root: Path, tmp_path: Path):
        """PipelineError message includes file path, line number, and token."""
        from deployment.pipeline import PipelineError

        renderer = ModelConfigRenderer(dev_root=tmp_dev_root)

        output = tmp_path / "error_detail.cfg"
        output.write_text(
            "LINE1=ok\n"
            "LINE2=ok\n"
            "LINE3=ok\n"
            "LINE4={{ undefined_var }}\n"
            "LINE5=ok\n"
        )
        rendered_files = [
            RenderedFile(path=output, sha256="abc123", source="test.j2", method="render")
        ]

        with pytest.raises(PipelineError) as exc_info:
            renderer.verify_no_unresolved_tokens(rendered_files)

        error_msg = str(exc_info.value)
        assert "line 4" in error_msg
        assert "{{" in error_msg
        assert "error_detail.cfg" in error_msg

    def test_skips_nonexistent_files(self, tmp_dev_root: Path, tmp_path: Path):
        """Non-existent rendered files are silently skipped."""
        renderer = ModelConfigRenderer(dev_root=tmp_dev_root)

        rendered_files = [
            RenderedFile(
                path=tmp_path / "does_not_exist.nml",
                sha256="abc123",
                source="missing.j2",
                method="render",
            )
        ]

        # Should not raise
        renderer.verify_no_unresolved_tokens(rendered_files)

    def test_multiple_files_first_error_reported(self, tmp_dev_root: Path, tmp_path: Path):
        """When multiple files have issues, the first one encountered raises."""
        from deployment.pipeline import PipelineError

        renderer = ModelConfigRenderer(dev_root=tmp_dev_root)

        good_file = tmp_path / "good.nml"
        good_file.write_text("npx = 97\n")

        bad_file = tmp_path / "bad.nml"
        bad_file.write_text("npx = {{ model.fv3.npx }}\n")

        rendered_files = [
            RenderedFile(path=good_file, sha256="a", source="a.j2", method="render"),
            RenderedFile(path=bad_file, sha256="b", source="b.j2", method="render"),
        ]

        with pytest.raises(PipelineError) as exc_info:
            renderer.verify_no_unresolved_tokens(rendered_files)

        assert "bad.nml" in str(exc_info.value)


# ---------------------------------------------------------------------------
# Tests: verify_shell_vars_preserved (Requirement 6.5)
# ---------------------------------------------------------------------------


class TestVerifyShellVarsPreserved:
    """Tests for the verify_shell_vars_preserved method."""

    def test_passes_when_runtime_vars_present(self, tmp_dev_root: Path, tmp_path: Path):
        """No error when expected runtime variables are found in rendered output."""
        renderer = ModelConfigRenderer(dev_root=tmp_dev_root)

        output = tmp_path / "forecast_config.sh"
        output.write_text(
            "#!/bin/bash\n"
            "export WORKDIR=${DATA}/forecast\n"
            "cpreq ${ROTDIR}/input.nml ${DATA}/input.nml\n"
        )
        rendered_files = [
            RenderedFile(path=output, sha256="abc123", source="config.j2", method="render")
        ]

        # Should not raise — both DATA and ROTDIR are present
        renderer.verify_shell_vars_preserved(rendered_files, {"DATA", "ROTDIR"})

    def test_passes_with_empty_runtime_vars(self, tmp_dev_root: Path, tmp_path: Path):
        """No error when runtime_vars set is empty."""
        renderer = ModelConfigRenderer(dev_root=tmp_dev_root)

        output = tmp_path / "config.sh"
        output.write_text("VALUE=42\n")
        rendered_files = [
            RenderedFile(path=output, sha256="abc123", source="config.j2", method="render")
        ]

        # Should not raise — nothing to check
        renderer.verify_shell_vars_preserved(rendered_files, set())

    def test_passes_when_var_not_expected_in_file(self, tmp_dev_root: Path, tmp_path: Path):
        """No error when a runtime var is not in a file that never referenced it."""
        renderer = ModelConfigRenderer(dev_root=tmp_dev_root)

        output = tmp_path / "simple.cfg"
        output.write_text(
            "# Simple config\n"
            "npx = 97\n"
            "npy = 97\n"
        )
        rendered_files = [
            RenderedFile(path=output, sha256="abc123", source="simple.j2", method="render")
        ]

        # DATA is not in this file at all — should not raise
        renderer.verify_shell_vars_preserved(rendered_files, {"DATA"})

    def test_detects_dollar_brace_var_form(self, tmp_dev_root: Path, tmp_path: Path):
        """Recognizes ${VAR} form of shell variables."""
        renderer = ModelConfigRenderer(dev_root=tmp_dev_root)

        output = tmp_path / "script.sh"
        output.write_text(
            "#!/bin/bash\n"
            "cp ${DATA}/input.nml .\n"
            "ls ${ROTDIR}/\n"
        )
        rendered_files = [
            RenderedFile(path=output, sha256="abc123", source="script.j2", method="render")
        ]

        # Both vars present — should pass
        renderer.verify_shell_vars_preserved(rendered_files, {"DATA", "ROTDIR"})

    def test_skips_nonexistent_files(self, tmp_dev_root: Path, tmp_path: Path):
        """Non-existent rendered files are silently skipped."""
        renderer = ModelConfigRenderer(dev_root=tmp_dev_root)

        rendered_files = [
            RenderedFile(
                path=tmp_path / "missing.sh",
                sha256="abc123",
                source="missing.j2",
                method="render",
            )
        ]

        # Should not raise
        renderer.verify_shell_vars_preserved(rendered_files, {"DATA"})

    def test_multiple_vars_all_preserved(self, tmp_dev_root: Path, tmp_path: Path):
        """Multiple runtime variables can all be verified in one call."""
        renderer = ModelConfigRenderer(dev_root=tmp_dev_root)

        output = tmp_path / "multi_var.sh"
        output.write_text(
            "#!/bin/bash\n"
            "WORKDIR=${DATA}/work\n"
            "COMOUT=${ROTDIR}/com\n"
            "COMIN=${COMINgfs}/input\n"
        )
        rendered_files = [
            RenderedFile(path=output, sha256="abc123", source="multi.j2", method="render")
        ]

        # All three vars present
        renderer.verify_shell_vars_preserved(
            rendered_files, {"DATA", "ROTDIR", "COMINgfs"}
        )


# ---------------------------------------------------------------------------
# Tests: Fortran namelist output parseability (Requirements 14.1–14.4)
# ---------------------------------------------------------------------------


class TestFortranNamelistParseability:
    """Tests that rendered Fortran namelist output is parseable.

    Validates that the NamelistValidator accepts rendered namelist files
    produced by the ModelConfigRenderer, ensuring proper &group / structure,
    correct boolean representation (.true./.false.), and valid syntax.

    Traces to: Requirements 14.1, 14.2, 14.4
    """

    def test_rendered_input_nml_is_parseable(
        self, tmp_dev_root: Path, valid_model_context: dict, expdir: Path
    ):
        """Rendered input.nml passes NamelistValidator without errors."""
        renderer = ModelConfigRenderer(dev_root=tmp_dev_root)
        reachability = _make_reachability_set(
            jjobs={"JGLOBAL_FORECAST"},
            ex_scripts={"exglobal_forecast.sh"},
        )
        results = renderer.render_for_dag(valid_model_context, expdir, reachability)

        # Find the rendered input.nml
        input_nml_files = [r for r in results if r.path.name == "input.nml"]
        assert len(input_nml_files) == 1, "Expected exactly one input.nml"

        # Validate with NamelistValidator
        validator = NamelistValidator()
        content = input_nml_files[0].path.read_text()
        errors = validator.validate(content, str(input_nml_files[0].path))
        assert errors == [], f"NamelistValidator errors: {errors}"

    def test_rendered_namelist_has_group_structure(
        self, tmp_dev_root: Path, valid_model_context: dict, expdir: Path
    ):
        """Rendered namelist contains proper &group_name / structure."""
        renderer = ModelConfigRenderer(dev_root=tmp_dev_root)
        reachability = _make_reachability_set(
            jjobs={"JGLOBAL_FORECAST"},
            ex_scripts={"exglobal_forecast.sh"},
        )
        results = renderer.render_for_dag(valid_model_context, expdir, reachability)

        input_nml_files = [r for r in results if r.path.name == "input.nml"]
        assert len(input_nml_files) == 1
        content = input_nml_files[0].path.read_text()

        # Verify &group / structure
        assert "&atmos_model_nml" in content
        assert "&fv_core_nml" in content
        # Each group must be closed with /
        lines = content.splitlines()
        group_opens = sum(1 for l in lines if l.strip().startswith("&"))
        group_closes = sum(1 for l in lines if l.strip() == "/")
        assert group_opens == group_closes, (
            f"Mismatched groups: {group_opens} opens vs {group_closes} closes"
        )

    def test_rendered_namelist_contains_resolved_values(
        self, tmp_dev_root: Path, valid_model_context: dict, expdir: Path
    ):
        """Rendered namelist contains resolved numeric values, not Jinja2 tokens."""
        renderer = ModelConfigRenderer(dev_root=tmp_dev_root)
        reachability = _make_reachability_set(
            jjobs={"JGLOBAL_FORECAST"},
            ex_scripts={"exglobal_forecast.sh"},
        )
        results = renderer.render_for_dag(valid_model_context, expdir, reachability)

        input_nml_files = [r for r in results if r.path.name == "input.nml"]
        assert len(input_nml_files) == 1
        content = input_nml_files[0].path.read_text()

        # Values from valid_model_context should be resolved
        assert "97" in content  # npx and npy
        assert "127" in content  # npz
        assert "450" in content  # dt_atmos
        # No Jinja2 tokens should remain
        assert "{{" not in content
        assert "{%" not in content

    def test_ice_in_namelist_parseable_when_active(
        self, tmp_dev_root: Path, valid_model_context: dict, expdir: Path
    ):
        """Rendered ice_in namelist passes NamelistValidator when ice is active."""
        valid_model_context["DO_ICE"] = "YES"
        valid_model_context["ice"] = {"dt_ice": 900}

        renderer = ModelConfigRenderer(dev_root=tmp_dev_root)
        reachability = _make_reachability_set(
            jjobs={"JGLOBAL_FORECAST"},
            ex_scripts={"exglobal_forecast.sh"},
        )
        results = renderer.render_for_dag(valid_model_context, expdir, reachability)

        ice_in_files = [r for r in results if r.path.name == "ice_in"]
        assert len(ice_in_files) == 1, "Expected exactly one ice_in"

        validator = NamelistValidator()
        content = ice_in_files[0].path.read_text()
        errors = validator.validate(content, str(ice_in_files[0].path))
        assert errors == [], f"NamelistValidator errors for ice_in: {errors}"

    def test_wave_namelist_parseable_when_active(
        self, tmp_dev_root: Path, valid_model_context: dict, expdir: Path
    ):
        """Rendered ww3_shel.nml passes NamelistValidator when wave is active."""
        valid_model_context["DO_WAVE"] = "YES"
        valid_model_context["wave"] = {"dt_wave": 3600}

        renderer = ModelConfigRenderer(dev_root=tmp_dev_root)
        reachability = _make_reachability_set(
            jjobs={"JGLOBAL_FORECAST", "JGFS_WAVE_PREP"},
            ex_scripts={"exglobal_forecast.sh", "exgfs_wave_prep.sh"},
        )
        results = renderer.render_for_dag(valid_model_context, expdir, reachability)

        wave_files = [r for r in results if r.path.name == "ww3_shel.nml"]
        assert len(wave_files) == 1, "Expected exactly one ww3_shel.nml"

        validator = NamelistValidator()
        content = wave_files[0].path.read_text()
        errors = validator.validate(content, str(wave_files[0].path))
        assert errors == [], f"NamelistValidator errors for ww3_shel.nml: {errors}"

    def test_namelist_validator_rejects_malformed_content(self, tmp_path: Path):
        """NamelistValidator correctly rejects malformed namelist content."""
        validator = NamelistValidator()

        # Missing group terminator
        bad_content = "&fv_core_nml\n  npx = 97\n  npy = 97\n"
        errors = validator.validate(bad_content, "bad.nml")
        assert len(errors) > 0
        assert "Unclosed" in errors[0]

    def test_namelist_validator_accepts_well_formed_content(self, tmp_path: Path):
        """NamelistValidator accepts properly structured namelist content."""
        validator = NamelistValidator()

        good_content = (
            "&atmos_model_nml\n"
            "  blocksize = 32\n"
            "/\n"
            "\n"
            "&fv_core_nml\n"
            "  npx = 97\n"
            "  npy = 97\n"
            "  npz = 127\n"
            "  dt_atmos = 450\n"
            "/\n"
        )
        errors = validator.validate(good_content, "good.nml")
        assert errors == []


# ---------------------------------------------------------------------------
# Tests: FATAL ERROR on undefined Jinja2 variable (Requirement 6.6)
# ---------------------------------------------------------------------------


class TestFatalErrorOnUndefinedVariable:
    """Tests that undefined Jinja2 variables produce FATAL ERROR.

    When a required Jinja2 variable is undefined in the deploy-time context,
    the Model_Input_Renderer SHALL emit a FATAL ERROR naming the undefined
    variable, the template file, and the line number.

    Traces to: Requirement 6.6
    """

    def test_undefined_variable_raises_template_render_error(
        self, tmp_dev_root: Path, expdir: Path
    ):
        """Undefined Jinja2 variable raises TemplateRenderError with FATAL ERROR."""
        # Use a complete context that passes schema validation
        context = {
            "resolution": "C96",
            "physics_suite": "gfdl",
            "coupling_mode": "atm",
            "dt_atmos": 450,
            "output_grid": "gaussian_grid",
            "output_fields": "standard",
            "fv3": {
                "npx": 97,
                "npy": 97,
                "npz": 127,
                "layout": [2, 2],
                "io_layout": [1, 1],
                "quilting": True,
                "write_group": 1,
                "wrttask_per_group": 24,
                "restart_interval": 12,
                "total_tasks": 24,
            },
        }

        # Add a template that references an undefined variable
        # (one that passes schema validation but fails at render time)
        ufs_dir = tmp_dev_root / "parm" / "ufs" / "fv3"
        (ufs_dir / "custom_config.j2").write_text(
            "# Custom config\n"
            "VALUE = {{ model.fv3.completely_undefined_variable }}\n"
        )

        renderer = ModelConfigRenderer(dev_root=tmp_dev_root)
        reachability = _make_reachability_set(
            jjobs={"JGLOBAL_FORECAST"},
            ex_scripts={"exglobal_forecast.sh"},
        )

        with pytest.raises(TemplateRenderError) as exc_info:
            renderer.render_for_dag(context, expdir, reachability)

        error_msg = str(exc_info.value)
        assert "FATAL ERROR" in error_msg

    def test_undefined_variable_error_names_the_variable(
        self, tmp_dev_root: Path, expdir: Path
    ):
        """Error message includes information about the undefined variable."""
        context = {
            "resolution": "C96",
            "physics_suite": "gfdl",
            "coupling_mode": "atm",
            "dt_atmos": 450,
            "output_grid": "gaussian_grid",
            "output_fields": "standard",
            "fv3": {
                "npx": 97,
                "npy": 97,
                "npz": 127,
                "layout": [2, 2],
                "io_layout": [1, 1],
                "quilting": True,
                "write_group": 1,
                "wrttask_per_group": 24,
                "restart_interval": 12,
                "total_tasks": 24,
            },
        }

        # Template referencing a top-level undefined variable (not nested dict access)
        ufs_dir = tmp_dev_root / "parm" / "ufs" / "fv3"
        (ufs_dir / "broken_template.j2").write_text(
            "# Broken template\n"
            "MISSING = {{ totally_undefined_var }}\n"
        )

        renderer = ModelConfigRenderer(dev_root=tmp_dev_root)
        reachability = _make_reachability_set(
            jjobs={"JGLOBAL_FORECAST"},
            ex_scripts={"exglobal_forecast.sh"},
        )

        with pytest.raises(TemplateRenderError) as exc_info:
            renderer.render_for_dag(context, expdir, reachability)

        error_msg = str(exc_info.value)
        # The error should name the undefined variable
        assert "totally_undefined_var" in error_msg

    def test_undefined_variable_error_names_the_file(
        self, tmp_dev_root: Path, expdir: Path
    ):
        """Error message includes the template file path."""
        context = {
            "resolution": "C96",
            "physics_suite": "gfdl",
            "coupling_mode": "atm",
            "dt_atmos": 450,
            "output_grid": "gaussian_grid",
            "output_fields": "standard",
            "fv3": {
                "npx": 97,
                "npy": 97,
                "npz": 127,
                "layout": [2, 2],
                "io_layout": [1, 1],
                "quilting": True,
                "write_group": 1,
                "wrttask_per_group": 24,
                "restart_interval": 12,
                "total_tasks": 24,
            },
        }

        # Template with undefined variable
        ufs_dir = tmp_dev_root / "parm" / "ufs" / "fv3"
        (ufs_dir / "named_template.j2").write_text(
            "VALUE = {{ model.fv3.missing_var }}\n"
        )

        renderer = ModelConfigRenderer(dev_root=tmp_dev_root)
        reachability = _make_reachability_set(
            jjobs={"JGLOBAL_FORECAST"},
            ex_scripts={"exglobal_forecast.sh"},
        )

        with pytest.raises(TemplateRenderError) as exc_info:
            renderer.render_for_dag(context, expdir, reachability)

        error_msg = str(exc_info.value)
        # The error should reference the template file
        assert "named_template.j2" in error_msg

    def test_all_variables_defined_no_error(
        self, tmp_dev_root: Path, valid_model_context: dict, expdir: Path
    ):
        """No error raised when all Jinja2 variables are defined in context."""
        renderer = ModelConfigRenderer(dev_root=tmp_dev_root)
        reachability = _make_reachability_set(
            jjobs={"JGLOBAL_FORECAST"},
            ex_scripts={"exglobal_forecast.sh"},
        )

        # Should not raise — valid_model_context has all required variables
        results = renderer.render_for_dag(valid_model_context, expdir, reachability)
        assert len(results) > 0


# ---------------------------------------------------------------------------
# Tests: Shell variable ${DATA} preserved in rendered output (Requirement 6.5)
# ---------------------------------------------------------------------------


class TestShellVarPreservedInRenderedOutput:
    """Tests that shell variables like ${DATA} survive Jinja2 rendering.

    The Model_Input_Renderer SHALL preserve shell variable references
    (${VAR}) for variables that are only known at runtime.

    Traces to: Requirement 6.5
    """

    def test_shell_var_in_template_preserved_after_render(
        self, tmp_dev_root: Path, valid_model_context: dict, expdir: Path
    ):
        """Shell variables ${DATA} and ${ROTDIR} in templates survive rendering."""
        # Create a template that contains shell variables
        fv3_dir = tmp_dev_root / "parm" / "ufs" / "fv3"
        (fv3_dir / "runtime_paths.j2").write_text(
            "# Runtime paths for forecast\n"
            "WORKDIR=${DATA}/forecast\n"
            "INPUT_DIR=${ROTDIR}/input\n"
            "RESOLUTION={{ model.resolution }}\n"
        )

        renderer = ModelConfigRenderer(dev_root=tmp_dev_root)
        reachability = _make_reachability_set(
            jjobs={"JGLOBAL_FORECAST"},
            ex_scripts={"exglobal_forecast.sh"},
        )
        results = renderer.render_for_dag(valid_model_context, expdir, reachability)

        # Find the rendered runtime_paths file
        runtime_files = [r for r in results if r.path.name == "runtime_paths"]
        assert len(runtime_files) == 1

        content = runtime_files[0].path.read_text()
        # Shell variables should be preserved verbatim
        assert "${DATA}" in content
        assert "${ROTDIR}" in content
        # Jinja2 variable should be resolved
        assert "C96" in content
        assert "{{" not in content

    def test_mixed_shell_and_jinja2_vars_both_handled(
        self, tmp_dev_root: Path, valid_model_context: dict, expdir: Path
    ):
        """Templates with both ${SHELL} and {{ jinja2 }} vars render correctly."""
        fv3_dir = tmp_dev_root / "parm" / "ufs" / "fv3"
        (fv3_dir / "mixed_vars.j2").write_text(
            "# Mixed variable types\n"
            "NPX={{ model.fv3.npx }}\n"
            "OUTDIR=${COMOUT}/output\n"
            "DT={{ model.dt_atmos }}\n"
            "INDIR=${COMINgfs}/input\n"
        )

        renderer = ModelConfigRenderer(dev_root=tmp_dev_root)
        reachability = _make_reachability_set(
            jjobs={"JGLOBAL_FORECAST"},
            ex_scripts={"exglobal_forecast.sh"},
        )
        results = renderer.render_for_dag(valid_model_context, expdir, reachability)

        mixed_files = [r for r in results if r.path.name == "mixed_vars"]
        assert len(mixed_files) == 1

        content = mixed_files[0].path.read_text()
        # Jinja2 vars resolved
        assert "NPX=97" in content
        assert "DT=450" in content
        # Shell vars preserved
        assert "${COMOUT}" in content
        assert "${COMINgfs}" in content
