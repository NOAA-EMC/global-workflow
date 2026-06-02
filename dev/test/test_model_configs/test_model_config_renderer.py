"""Unit tests for ModelConfigRenderer orchestration module.

Tests the rendering pipeline including context validation, template discovery,
format validation dispatch, output placement, fallback resolution, and
template_overrides support.

Traces to: Requirements 9.1, 9.2, 9.3, 11.1, 11.2, 11.3
"""

from __future__ import annotations

import hashlib
import os
import sys
from pathlib import Path

import pytest

sys.path.insert(0, os.path.join(os.path.dirname(__file__), "..", "..", "workflow"))

from deployment.model_config_renderer import (
    ModelConfigRenderer,
    RenderedFile,
    _compute_sha256,
    _get_validator,
)
from deployment.template_renderer import TemplateRenderError
from deployment.validators import (
    ESMFConfigValidator,
    FieldTableValidator,
    ModelConfigureValidator,
    NamelistValidator,
)


# ---------------------------------------------------------------------------
# Fixtures
# ---------------------------------------------------------------------------


@pytest.fixture
def tmp_dev_root(tmp_path: Path) -> Path:
    """Create a minimal dev/ directory structure with templates."""
    dev_root = tmp_path / "dev"
    ufs_dir = dev_root / "parm" / "ufs"
    fv3_dir = ufs_dir / "fv3"
    gocart_dir = ufs_dir / "gocart"
    collections_dir = gocart_dir / "collections"

    fv3_dir.mkdir(parents=True)
    gocart_dir.mkdir(parents=True)
    collections_dir.mkdir(parents=True)

    # Create a simple model_configure.j2 template
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

    # Create a simple field_table.j2 template
    (fv3_dir / "field_table.j2").write_text(
        '# Field table for {{ model.physics_suite }}\n'
        ' "TRACER", "atmos_mod", "sphum"\n'
        '           "longname",     "specific humidity"\n'
        '           "units",        "kg/kg"\n'
        '       "profile_type", "fixed", "surface_value=1.e30" /\n'
    )

    # Create a simple input.nml.j2 template
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

    # Create a simple diag_table.j2 template (just comments for simplicity)
    (fv3_dir / "diag_table.j2").write_text(
        "# Diag table for {{ model.resolution }}\n"
    )

    # Create a simple ufs.configure.j2 template
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

    # Create a GOCART template
    (gocart_dir / "AERO_HISTORY.rc.j2").write_text(
        "# GOCART history\n"
        "VERSION: 1\n"
        "EXPID:  gocart\n"
        "COLLECTIONS::\n"
        "::\n"
    )

    # Create a collection fragment (should be excluded from discovery)
    (collections_dir / "inst_aod.j2").write_text(
        "# inst_aod collection fields\n"
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


# ---------------------------------------------------------------------------
# Tests: _get_validator dispatch
# ---------------------------------------------------------------------------


class TestValidatorDispatch:
    """Tests for the validator dispatch function."""

    def test_model_configure_validator(self):
        v = _get_validator("model_configure")
        assert isinstance(v, ModelConfigureValidator)

    def test_input_nml_validator(self):
        v = _get_validator("input.nml")
        assert isinstance(v, NamelistValidator)

    def test_field_table_validator(self):
        v = _get_validator("field_table")
        assert isinstance(v, FieldTableValidator)

    def test_ufs_configure_validator(self):
        v = _get_validator("ufs.configure")
        assert isinstance(v, ESMFConfigValidator)

    def test_rc_file_validator(self):
        v = _get_validator("AERO_HISTORY.rc")
        assert isinstance(v, ESMFConfigValidator)

    def test_unknown_file_returns_none(self):
        v = _get_validator("unknown_file.txt")
        assert v is None

    def test_diag_table_validator(self):
        from deployment.validators import DiagTableValidator
        v = _get_validator("diag_table")
        assert isinstance(v, DiagTableValidator)


# ---------------------------------------------------------------------------
# Tests: _compute_sha256
# ---------------------------------------------------------------------------


class TestComputeSha256:
    """Tests for SHA-256 computation."""

    def test_computes_correct_hash(self, tmp_path: Path):
        test_file = tmp_path / "test.txt"
        content = "hello world\n"
        test_file.write_text(content)

        expected = hashlib.sha256(content.encode()).hexdigest()
        assert _compute_sha256(test_file) == expected

    def test_empty_file(self, tmp_path: Path):
        test_file = tmp_path / "empty.txt"
        test_file.write_text("")

        expected = hashlib.sha256(b"").hexdigest()
        assert _compute_sha256(test_file) == expected


# ---------------------------------------------------------------------------
# Tests: Template discovery
# ---------------------------------------------------------------------------


class TestTemplateDiscovery:
    """Tests for template discovery logic."""

    def test_discovers_j2_templates(self, tmp_dev_root: Path):
        renderer = ModelConfigRenderer(dev_root=tmp_dev_root)
        templates = renderer._discover_templates()

        # Should find templates but NOT collection fragments
        template_names = [t.name for t in templates]
        assert "model_configure.j2" in template_names
        assert "field_table.j2" in template_names
        assert "input.nml.j2" in template_names
        assert "ufs.configure.j2" in template_names
        assert "AERO_HISTORY.rc.j2" in template_names
        # Collection fragments should be excluded
        assert "inst_aod.j2" not in template_names

    def test_empty_template_dir(self, tmp_path: Path):
        dev_root = tmp_path / "dev"
        dev_root.mkdir()
        renderer = ModelConfigRenderer(dev_root=dev_root)
        templates = renderer._discover_templates()
        assert templates == []

    def test_templates_are_sorted(self, tmp_dev_root: Path):
        renderer = ModelConfigRenderer(dev_root=tmp_dev_root)
        templates = renderer._discover_templates()
        assert templates == sorted(templates)


# ---------------------------------------------------------------------------
# Tests: render_all
# ---------------------------------------------------------------------------


class TestRenderAll:
    """Tests for the full render_all pipeline."""

    def test_renders_all_templates(
        self, tmp_dev_root: Path, valid_model_context: dict, expdir: Path
    ):
        renderer = ModelConfigRenderer(dev_root=tmp_dev_root)
        results = renderer.render_all(valid_model_context, expdir)

        assert len(results) > 0
        assert all(isinstance(r, RenderedFile) for r in results)

        # Check that output files exist
        for r in results:
            assert r.path.exists()
            assert r.sha256  # Non-empty hash
            assert r.method in ("render", "copy")

    def test_output_placement_fv3(
        self, tmp_dev_root: Path, valid_model_context: dict, expdir: Path
    ):
        renderer = ModelConfigRenderer(dev_root=tmp_dev_root)
        results = renderer.render_all(valid_model_context, expdir)

        # FV3 files should be at expdir/parm/ufs/fv3/
        fv3_dir = expdir / "parm" / "ufs" / "fv3"
        assert (fv3_dir / "model_configure").exists()
        assert (fv3_dir / "field_table").exists()
        assert (fv3_dir / "input.nml").exists()

    def test_output_placement_ufs_configure(
        self, tmp_dev_root: Path, valid_model_context: dict, expdir: Path
    ):
        renderer = ModelConfigRenderer(dev_root=tmp_dev_root)
        renderer.render_all(valid_model_context, expdir)

        # ufs.configure should be at expdir/parm/ufs/
        assert (expdir / "parm" / "ufs" / "ufs.configure").exists()

    def test_output_placement_gocart(
        self, tmp_dev_root: Path, valid_model_context: dict, expdir: Path
    ):
        renderer = ModelConfigRenderer(dev_root=tmp_dev_root)
        renderer.render_all(valid_model_context, expdir)

        # GOCART files should be at expdir/parm/ufs/gocart/
        assert (expdir / "parm" / "ufs" / "gocart" / "AERO_HISTORY.rc").exists()

    def test_rendered_content_uses_context(
        self, tmp_dev_root: Path, valid_model_context: dict, expdir: Path
    ):
        renderer = ModelConfigRenderer(dev_root=tmp_dev_root)
        renderer.render_all(valid_model_context, expdir)

        # Check model_configure has rendered values
        mc_content = (expdir / "parm" / "ufs" / "fv3" / "model_configure").read_text()
        assert "PE_MEMBER01:         24" in mc_content
        assert "dt_atmos:            450" in mc_content
        assert "quilting:            .true." in mc_content

    def test_sha256_is_correct(
        self, tmp_dev_root: Path, valid_model_context: dict, expdir: Path
    ):
        renderer = ModelConfigRenderer(dev_root=tmp_dev_root)
        results = renderer.render_all(valid_model_context, expdir)

        for r in results:
            expected = _compute_sha256(r.path)
            assert r.sha256 == expected

    def test_schema_validation_failure(
        self, tmp_dev_root: Path, expdir: Path
    ):
        # Missing required keys
        bad_context = {"resolution": "C96"}
        renderer = ModelConfigRenderer(dev_root=tmp_dev_root)

        with pytest.raises(TemplateRenderError, match="schema validation failed"):
            renderer.render_all(bad_context, expdir)

    def test_invalid_resolution(
        self, tmp_dev_root: Path, valid_model_context: dict, expdir: Path
    ):
        valid_model_context["resolution"] = "C999"
        renderer = ModelConfigRenderer(dev_root=tmp_dev_root)

        with pytest.raises(TemplateRenderError, match="schema validation failed"):
            renderer.render_all(valid_model_context, expdir)

    def test_resolution_defaults_merged(
        self, tmp_dev_root: Path, expdir: Path
    ):
        """Test that resolution defaults are merged into fv3 section."""
        context = {
            "resolution": "C96",
            "physics_suite": "gfdl",
            "coupling_mode": "atm",
            "dt_atmos": 450,
            "output_grid": "gaussian_grid",
            "output_fields": "standard",
            "defaults": {
                "C96": {
                    "npx": 97,
                    "npy": 97,
                    "layout": [2, 2],
                    "write_group": 1,
                    "wrttask_per_group": 24,
                }
            },
            "fv3": {
                "npz": 127,
                "io_layout": [1, 1],
                "quilting": True,
                "restart_interval": 12,
                "total_tasks": 24,
            },
        }
        renderer = ModelConfigRenderer(dev_root=tmp_dev_root)
        results = renderer.render_all(context, expdir)

        # Should succeed because defaults fill in missing fv3 keys
        assert len(results) > 0
        mc_content = (expdir / "parm" / "ufs" / "fv3" / "model_configure").read_text()
        assert "write_groups:        1" in mc_content


# ---------------------------------------------------------------------------
# Tests: Fallback resolution (static file copy)
# ---------------------------------------------------------------------------


class TestFallbackResolution:
    """Tests for static file fallback when no .j2 template exists."""

    def test_copies_static_file_when_no_template(
        self, tmp_dev_root: Path, valid_model_context: dict, expdir: Path
    ):
        # Add a static file with no corresponding .j2 template
        static_file = tmp_dev_root / "parm" / "ufs" / "fv3" / "data_table"
        static_file.write_text("# Static data table\n")

        renderer = ModelConfigRenderer(dev_root=tmp_dev_root)
        results = renderer.render_all(valid_model_context, expdir)

        # Find the static file result
        static_results = [r for r in results if r.method == "copy"]
        assert len(static_results) == 1
        assert static_results[0].path == expdir / "parm" / "ufs" / "fv3" / "data_table"
        assert static_results[0].path.exists()
        assert static_results[0].path.read_text() == "# Static data table\n"

    def test_prefers_template_over_static(
        self, tmp_dev_root: Path, valid_model_context: dict, expdir: Path
    ):
        # The field_table.j2 template exists, so even if a static field_table
        # existed, the template should be used
        renderer = ModelConfigRenderer(dev_root=tmp_dev_root)
        results = renderer.render_all(valid_model_context, expdir)

        field_table_results = [
            r for r in results
            if r.path.name == "field_table"
        ]
        assert len(field_table_results) == 1
        assert field_table_results[0].method == "render"


# ---------------------------------------------------------------------------
# Tests: template_overrides support
# ---------------------------------------------------------------------------


class TestTemplateOverrides:
    """Tests for template_overrides incremental migration support."""

    def test_renders_template_when_in_overrides(
        self, tmp_dev_root: Path, valid_model_context: dict, expdir: Path
    ):
        valid_model_context["template_overrides"] = ["model_configure", "field_table"]
        renderer = ModelConfigRenderer(dev_root=tmp_dev_root)
        results = renderer.render_all(valid_model_context, expdir)

        # Templates should still be rendered
        rendered_names = [r.path.name for r in results if r.method == "render"]
        assert "model_configure" in rendered_names

    def test_renders_template_even_when_not_in_overrides(
        self, tmp_dev_root: Path, valid_model_context: dict, expdir: Path
    ):
        """Template is preferred over static even when not in overrides (Req 11.3)."""
        valid_model_context["template_overrides"] = ["model_configure"]
        renderer = ModelConfigRenderer(dev_root=tmp_dev_root)
        results = renderer.render_all(valid_model_context, expdir)

        # field_table.j2 exists, so it should still be rendered
        field_table_results = [
            r for r in results if r.path.name == "field_table"
        ]
        assert len(field_table_results) == 1
        assert field_table_results[0].method == "render"

    def test_static_file_skipped_when_in_overrides(
        self, tmp_dev_root: Path, valid_model_context: dict, expdir: Path
    ):
        # Add a static file
        static_file = tmp_dev_root / "parm" / "ufs" / "fv3" / "data_table"
        static_file.write_text("# Static data table\n")

        # Put data_table in overrides (meaning user wants template version)
        valid_model_context["template_overrides"] = ["data_table"]
        renderer = ModelConfigRenderer(dev_root=tmp_dev_root)
        results = renderer.render_all(valid_model_context, expdir)

        # Static file should NOT be copied since it's in overrides
        static_results = [
            r for r in results
            if r.method == "copy" and r.path.name == "data_table"
        ]
        assert len(static_results) == 0


# ---------------------------------------------------------------------------
# Tests: RenderedFile dataclass
# ---------------------------------------------------------------------------


class TestRenderedFile:
    """Tests for the RenderedFile dataclass."""

    def test_rendered_file_attributes(self):
        rf = RenderedFile(
            path=Path("/tmp/test/model_configure"),
            sha256="abc123",
            source="parm/ufs/fv3/model_configure.j2",
            method="render",
        )
        assert rf.path == Path("/tmp/test/model_configure")
        assert rf.sha256 == "abc123"
        assert rf.source == "parm/ufs/fv3/model_configure.j2"
        assert rf.method == "render"
