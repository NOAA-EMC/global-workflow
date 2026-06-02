"""Property-based tests for templated model configuration rendering.

Uses hypothesis to verify universal correctness properties across all valid
Model_Context inputs. Each property test generates random but valid
configurations and asserts invariants that must hold for all inputs.

Traces to: Requirements 1, 2, 3, 6, 7, 8, 10
"""

from __future__ import annotations

import os
import re
import sys
from pathlib import Path

import pytest
from hypothesis import given, settings, assume, HealthCheck
from hypothesis import strategies as st

sys.path.insert(0, os.path.join(os.path.dirname(__file__), "..", "..", "workflow"))

from deployment.atparse_migration import validate_no_atparse_remaining
from deployment.model_config_renderer import ModelConfigRenderer
from deployment.model_context import (
    SUPPORTED_COUPLING_MODES,
    SUPPORTED_EMISSION_DATASETS,
    SUPPORTED_PHYSICS_SUITES,
    SUPPORTED_RESOLUTIONS,
)
from deployment.component_composer import (
    COMPONENT_FAMILY_PREFIXES,
    COMPONENT_REGISTRY,
    compose_components,
)
from deployment.template_renderer import TemplateRenderer, TemplateRenderError

# ---------------------------------------------------------------------------
# Constants
# ---------------------------------------------------------------------------

# Path to the dev/ root containing templates
DEV_ROOT = Path(__file__).resolve().parent.parent.parent

# Atparse pattern regex for direct assertion
_ATPARSE_RE = re.compile(r"@\[[A-Za-z_][A-Za-z0-9_]*\]")

# Available GOCART collection templates (derived from files on disk)
_COLLECTIONS_DIR = DEV_ROOT / "parm" / "ufs" / "gocart" / "collections"
AVAILABLE_COLLECTIONS: list[str] = sorted(
    p.stem for p in _COLLECTIONS_DIR.glob("*.j2")
) if _COLLECTIONS_DIR.exists() else ["inst_aod"]


# ---------------------------------------------------------------------------
# Hypothesis strategies for valid Model_Context generation
# ---------------------------------------------------------------------------

# Resolution-dependent defaults (mirrors model_context.py design)
_RESOLUTION_DEFAULTS = {
    "C48": {"npx": 49, "npy": 49, "layout": [1, 1], "write_group": 1, "wrttask_per_group": 6, "imo": 192, "jmo": 94},
    "C96": {"npx": 97, "npy": 97, "layout": [2, 2], "write_group": 1, "wrttask_per_group": 24, "imo": 384, "jmo": 190},
    "C384": {"npx": 385, "npy": 385, "layout": [6, 6], "write_group": 2, "wrttask_per_group": 40, "imo": 1536, "jmo": 768},
    "C768": {"npx": 769, "npy": 769, "layout": [8, 12], "write_group": 4, "wrttask_per_group": 80, "imo": 3072, "jmo": 1536},
    "C1152": {"npx": 1153, "npy": 1153, "layout": [12, 12], "write_group": 4, "wrttask_per_group": 120, "imo": 4608, "jmo": 2304},
}


def _components_for_coupling_mode(coupling_mode: str) -> list[str]:
    """Return the active components list appropriate for a coupling mode."""
    if coupling_mode == "atm":
        return ["atmosphere"]
    elif coupling_mode == "atmaero":
        return ["atmosphere", "aerosol"]
    elif coupling_mode == "s2s":
        return ["atmosphere", "ocean", "ice"]
    elif coupling_mode == "s2sa":
        return ["atmosphere", "ocean", "ice", "aerosol"]
    elif coupling_mode == "s2sw":
        return ["atmosphere", "ocean", "ice", "wave"]
    elif coupling_mode == "s2swa":
        return ["atmosphere", "ocean", "ice", "wave", "aerosol"]
    elif coupling_mode == "leapfrog_atm_wav":
        return ["atmosphere", "wave"]
    else:
        return ["atmosphere"]


class _StartDate:
    """Simple object with year/month/day/hour attributes for template rendering."""

    def __init__(self, year: int, month: int, day: int, hour: int):
        self.year = year
        self.month = month
        self.day = day
        self.hour = hour


@st.composite
def valid_model_context(draw: st.DrawFn) -> dict:
    """Generate a valid Model_Context dict that passes schema validation.

    Produces contexts with all required keys and consistent component
    configurations based on the selected coupling_mode.
    """
    resolution = draw(st.sampled_from(sorted(SUPPORTED_RESOLUTIONS)))
    physics_suite = draw(st.sampled_from(sorted(SUPPORTED_PHYSICS_SUITES)))
    coupling_mode = draw(st.sampled_from(sorted(SUPPORTED_COUPLING_MODES)))
    dt_atmos = draw(st.sampled_from([150, 225, 300, 450, 600, 720, 900]))
    pbl_scheme = draw(st.sampled_from(["satmedmf", "default"]))
    progsigma = draw(st.booleans())

    active_components = _components_for_coupling_mode(coupling_mode)

    # Get resolution defaults
    res_defaults = _RESOLUTION_DEFAULTS.get(resolution, _RESOLUTION_DEFAULTS["C96"])
    npx = res_defaults["npx"]
    npy = res_defaults["npy"]
    layout = res_defaults["layout"]
    write_group = res_defaults["write_group"]
    wrttask_per_group = res_defaults["wrttask_per_group"]
    imo = res_defaults["imo"]
    jmo = res_defaults["jmo"]

    total_tasks = layout[0] * layout[1] * 6

    # Build fv3 section
    fv3 = {
        "npx": npx,
        "npy": npy,
        "npz": draw(st.sampled_from([64, 91, 127])),
        "layout": layout,
        "io_layout": [1, 1],
        "quilting": True,
        "write_group": write_group,
        "wrttask_per_group": wrttask_per_group,
        "restart_interval": draw(st.sampled_from([0, 6, 12, 24])),
        "total_tasks": total_tasks,
        "omp_threads": 1,
        "type": draw(st.sampled_from(["nh", "hydro"])),
        "imp_physics": {"gfdl": 11, "thompson": 8, "wsm6": 6, "zhaocarr": 99}[physics_suite],
        "ccpp_suite": "FV3_GFS_v17_p8",
        "fhrot": 0,
        "imo": imo,
        "jmo": jmo,
        "output_fh": "0 1 2 3 6 12",
        "iau_offset": 0,
        "output_filetype_atm": "netcdf",
        "output_filetype_sfc": "netcdf",
        "blocksize": 32,
        "atm_model": "fv3",
        "do_nest": False,
    }

    # Generate a start_date object (needed by model_configure.j2 and diag_table.j2)
    start_year = draw(st.integers(min_value=2020, max_value=2030))
    start_month = draw(st.integers(min_value=1, max_value=12))
    start_day = draw(st.integers(min_value=1, max_value=28))
    start_hour = draw(st.sampled_from([0, 6, 12, 18]))
    start_date = _StartDate(start_year, start_month, start_day, start_hour)

    context: dict = {
        "resolution": resolution,
        "physics_suite": physics_suite,
        "coupling_mode": coupling_mode,
        "dt_atmos": dt_atmos,
        "output_grid": "gaussian_grid",
        "output_fields": draw(st.sampled_from(["standard", "da", "aod", "aero"])),
        "pbl_scheme": pbl_scheme,
        "progsigma": progsigma,
        "active_components": active_components,
        "coupling_interval_slow": dt_atmos * 4,
        "coupling_interval_fast": dt_atmos,
        "start_date": start_date,
        "fv3": fv3,
    }

    # Add ocean section if ocean is active
    if "ocean" in active_components:
        context["ocean"] = {
            "resolution": "025",
            "dt_ocean": 900,
            "tasks": draw(st.sampled_from([60, 120, 240])),
            "omp_threads": 1,
            "output_dir": "./MOM6_OUTPUT",
            "restart_dir": "./MOM6_RESTART",
            "output_frequency_hours": 6,
        }

    # Add ice section if ice is active
    if "ice" in active_components:
        context["ice"] = {
            "resolution": "025",
            "nprocs": draw(st.sampled_from([24, 48, 96])),
            "omp_threads": 1,
            "dt_ice": 900,
        }

    # Add wave section if wave is active
    if "wave" in active_components:
        context["wave"] = {
            "resolution": "gwes_30m",
            "tasks": draw(st.sampled_from([60, 100, 120])),
            "omp_threads": 1,
            "mesh": "mesh.ww3.gwes_30m",
            "dt_wave": 900,
        }

    # Add aerosol section if aerosol is active
    if "aerosol" in active_components:
        emission_dataset = draw(st.sampled_from(sorted(SUPPORTED_EMISSION_DATASETS)))
        # Pick 1-3 collections from available ones
        num_collections = draw(
            st.integers(min_value=1, max_value=min(3, len(AVAILABLE_COLLECTIONS)))
        )
        collections = draw(
            st.lists(
                st.sampled_from(AVAILABLE_COLLECTIONS),
                min_size=num_collections,
                max_size=num_collections,
                unique=True,
            )
        )
        context["aerosol"] = {
            "emission_dataset": emission_dataset,
            "active_collections": collections,
            "grid_label": "PC720x361-DC",
            "grid_im": 720,
            "grid_jm": 361,
            "frequencies": {coll: "010000" for coll in collections},
        }

    return context


# ---------------------------------------------------------------------------
# Property 4: No Legacy atparse Tokens
# ---------------------------------------------------------------------------


def _render_templates_individually(
    model_context: dict, expdir: Path
) -> list[Path]:
    """Render each template individually, skipping those that fail validation.

    This renders templates one at a time so that format validation failures
    in one template don't prevent checking other templates for atparse tokens.
    Returns paths to all successfully rendered files.
    """
    from deployment.model_context import merge_resolution_defaults, ModelContextSchema

    # Merge defaults and validate schema first
    model_context = merge_resolution_defaults(model_context)
    schema = ModelContextSchema()
    errors = schema.validate(model_context)
    if errors:
        return []

    # Build the rendering context
    context = {"model": model_context}
    template_dir = DEV_ROOT / "parm" / "ufs"

    if not template_dir.exists():
        return []

    # Searchpath for includes
    searchpath = [
        str(template_dir),
        str(DEV_ROOT / "parm"),
        str(DEV_ROOT / "parm" / "ufs"),
    ]
    searchpath = [p for p in searchpath if Path(p).is_dir()]

    renderer = TemplateRenderer(context=context, searchpath=searchpath, strict=True)

    # Discover templates (excluding collection fragments)
    templates = []
    for j2_file in sorted(template_dir.rglob("*.j2")):
        rel = j2_file.relative_to(template_dir)
        if "collections" in rel.parts:
            continue
        templates.append(j2_file)

    rendered_paths: list[Path] = []
    output_dir = expdir / "parm" / "ufs"

    for template_path in templates:
        rel_path = template_path.relative_to(template_dir)
        stem = str(rel_path)
        if stem.endswith(".j2"):
            stem = stem[:-3]
        output_path = output_dir / stem
        output_path.parent.mkdir(parents=True, exist_ok=True)

        try:
            renderer.render_file(template_path, output_path)
            rendered_paths.append(output_path)
        except TemplateRenderError:
            # Template rendering or format validation failed — skip this file.
            # This property test only checks for atparse tokens in files that
            # render successfully. Format validity is tested by Property 3.
            continue

    return rendered_paths


class TestNoLegacyAtparseTokens:
    """Property 4: No rendered file contains @[...] atparse substitution patterns.

    **Validates: Requirements 8.1, 8.2, 8.3**

    After rendering all templates with valid Model_Context values, no output
    file should contain any legacy @[VAR_NAME] atparse tokens. All such
    patterns must have been converted to Jinja2 {{ var }} syntax.
    """

    @settings(
        max_examples=100,
        suppress_health_check=[HealthCheck.too_slow],
        deadline=None,
    )
    @given(model_context=valid_model_context())
    def test_no_atparse_tokens_in_rendered_output(
        self, model_context: dict, tmp_path_factory
    ):
        """Assert no rendered file contains @[...] atparse substitution patterns.

        **Validates: Requirements 8.1, 8.2, 8.3**

        Generates valid Model_Context dicts, renders all templates using
        the TemplateRenderer, reads each rendered file's content, and asserts
        no @[...] patterns exist using validate_no_atparse_remaining.
        """
        # Create a unique temp directory for this test iteration
        expdir = tmp_path_factory.mktemp("expdir")

        # Render templates individually (skip format validation failures)
        rendered_paths = _render_templates_individually(model_context, expdir)

        # We must have rendered at least one file to make this test meaningful
        assume(len(rendered_paths) > 0)

        # Assert no rendered file contains atparse tokens
        for output_path in rendered_paths:
            content = output_path.read_text(encoding="utf-8")

            # Use validate_no_atparse_remaining from atparse_migration module
            remaining_tokens = validate_no_atparse_remaining(content)
            assert remaining_tokens == [], (
                f"Legacy atparse tokens found in {output_path.name}: "
                f"{['@[' + t + ']' for t in remaining_tokens]}"
            )

            # Double-check with direct regex search
            matches = _ATPARSE_RE.findall(content)
            assert matches == [], (
                f"Legacy @[...] patterns found in {output_path.name}: {matches}"
            )


# ---------------------------------------------------------------------------
# Property 3: Format Validity
# Validates: Requirements 7.1, 7.2, 7.3, 7.4, 7.5
# ---------------------------------------------------------------------------


@st.composite
def _full_model_context_for_format_validity(draw: st.DrawFn) -> dict:
    """Generate a Model_Context with ALL component sections populated.

    Unlike valid_model_context which only adds component sections when
    they are in active_components, this strategy always includes all
    component sections because ModelConfigRenderer.render_all() discovers
    and renders ALL .j2 templates (including GOCART) regardless of which
    components are active.
    """
    resolution = draw(st.sampled_from(sorted(SUPPORTED_RESOLUTIONS)))
    physics_suite = draw(st.sampled_from(sorted(SUPPORTED_PHYSICS_SUITES)))
    coupling_mode = draw(st.sampled_from(sorted(SUPPORTED_COUPLING_MODES)))
    dt_atmos = draw(st.sampled_from([150, 225, 300, 450, 600, 720, 900]))
    pbl_scheme = draw(st.sampled_from(["satmedmf", "default"]))
    progsigma = draw(st.booleans())

    active_components = _components_for_coupling_mode(coupling_mode)

    # Get resolution defaults
    res_defaults = _RESOLUTION_DEFAULTS.get(resolution, _RESOLUTION_DEFAULTS["C96"])
    npx = res_defaults["npx"]
    npy = res_defaults["npy"]
    layout = res_defaults["layout"]
    write_group = res_defaults["write_group"]
    wrttask_per_group = res_defaults["wrttask_per_group"]
    imo = res_defaults["imo"]
    jmo = res_defaults["jmo"]

    total_tasks = layout[0] * layout[1] * 6

    # Build fv3 section
    fv3 = {
        "npx": npx,
        "npy": npy,
        "npz": draw(st.sampled_from([64, 91, 127])),
        "layout": layout,
        "io_layout": [1, 1],
        "quilting": True,
        "write_group": write_group,
        "wrttask_per_group": wrttask_per_group,
        "restart_interval": draw(st.sampled_from([0, 6, 12, 24])),
        "total_tasks": total_tasks,
        "omp_threads": 1,
        "type": draw(st.sampled_from(["nh", "hydro"])),
        "imp_physics": {"gfdl": 11, "thompson": 8, "wsm6": 6, "zhaocarr": 99}[physics_suite],
        "ccpp_suite": "FV3_GFS_v17_p8",
        "fhrot": 0,
        "imo": imo,
        "jmo": jmo,
        "output_fh": "0 1 2 3 6 12",
        "iau_offset": 0,
        "output_filetype_atm": "netcdf",
        "output_filetype_sfc": "netcdf",
        "blocksize": 32,
        "atm_model": "fv3",
        "do_nest": False,
    }

    # Generate a start_date object
    start_date = _StartDate(
        draw(st.integers(min_value=2020, max_value=2030)),
        draw(st.integers(min_value=1, max_value=12)),
        draw(st.integers(min_value=1, max_value=28)),
        draw(st.sampled_from([0, 6, 12, 18])),
    )

    # Pick collections for aerosol
    num_collections = draw(
        st.integers(min_value=1, max_value=min(3, len(AVAILABLE_COLLECTIONS)))
    )
    collections = draw(
        st.lists(
            st.sampled_from(AVAILABLE_COLLECTIONS),
            min_size=num_collections,
            max_size=num_collections,
            unique=True,
        )
    )

    context: dict = {
        "resolution": resolution,
        "physics_suite": physics_suite,
        "coupling_mode": coupling_mode,
        "dt_atmos": dt_atmos,
        "output_grid": "gaussian_grid",
        "output_fields": draw(st.sampled_from(["standard", "da", "aod", "aero"])),
        "pbl_scheme": pbl_scheme,
        "progsigma": progsigma,
        "active_components": active_components,
        "coupling_interval_slow": dt_atmos * 4,
        "coupling_interval_fast": dt_atmos,
        "start_date": start_date,
        "fv3": fv3,
        # Always include all component sections since render_all() renders
        # ALL templates regardless of active_components
        "ocean": {
            "resolution": "025",
            "dt_ocean": 900,
            "dt_therm": 3600,
            "tasks": draw(st.sampled_from([60, 120, 240])),
            "omp_threads": 1,
            "output_dir": "./MOM6_OUTPUT",
            "restart_dir": "./MOM6_RESTART",
            "output_frequency_hours": 6,
            "use_waves": draw(st.booleans()),
            "oda_incupd": draw(st.booleans()),
            "oda_incupd_nhours": 6,
            "do_sppt": draw(st.booleans()),
            "river_runoff": draw(st.booleans()),
            "diag_coord_def_z_file": "oceanda_zgrid_75L.nc",
            "frunoff": "INPUT/runoff.daitren.clim.nc",
            "nx_glb": 1440,
            "ny_glb": 1080,
            "nk": 75,
        },
        "ice": {
            "resolution": "025",
            "nprocs": draw(st.sampled_from([24, 48, 96])),
            "omp_threads": 1,
            "decomposition": "slenderX2",
            "dt_ice": 900,
            "grid": "grid_cice_NEMS_mx025.nc",
            "mask": "kmtu_cice_NEMS_mx025.nc",
            "nx_glb": 1440,
            "ny_glb": 1080,
            "warm_start": draw(st.booleans()),
            "histfreq_n": 1,
            "hist_avg": draw(st.booleans()),
            "dumpfreq": "d",
            "dumpfreq_n": 1,
            "ktherm": 2,
            "tr_pond_lvl": draw(st.booleans()),
        },
        "wave": {
            "resolution": "gwes_30m",
            "tasks": draw(st.sampled_from([60, 100, 120])),
            "omp_threads": 1,
            "mesh": "mesh.ww3.gwes_30m",
            "dt_wave": 900,
            "ice_input": draw(st.sampled_from(["CPL", "YES"])),
            "current_input": draw(st.sampled_from(["CPL", "YES"])),
            "output_params": "HS FP DP PHS PTP PDIR CHA",
            "dt_field_output": 10800,
            "dt_point_output": 3600,
            "grid_output_dir": "./",
            "point_output_dir": "./",
            "restart_output_dir": "./RESTART/",
        },
        "post": {
            "system": draw(st.sampled_from(["gfs", "gcafs", "gefs", "sfs"])),
        },
        "aerosol": {
            "emission_dataset": draw(st.sampled_from(sorted(SUPPORTED_EMISSION_DATASETS))),
            "active_collections": collections,
            "grid_label": "PC720x361-DC",
            "grid_im": 720,
            "grid_jm": 361,
            "frequencies": {coll: "010000" for coll in collections},
        },
    }

    return context


class TestFormatValidity:
    """Property 3: Format Validity.

    **Validates: Requirements 7.1, 7.2, 7.3, 7.4, 7.5**

    For any valid Model_Context (resolution x physics_suite x coupling_mode
    x component set), every rendered UFS_Model_Config file passes its
    format-specific validator without errors.
    """

    @settings(
        max_examples=100,
        suppress_health_check=[HealthCheck.too_slow],
        deadline=None,
    )
    @given(model_context=_full_model_context_for_format_validity())
    def test_all_rendered_files_pass_format_validation(
        self, model_context: dict, tmp_path_factory
    ):
        """Every rendered config file passes its format-specific validator.

        **Validates: Requirements 7.1, 7.2, 7.3, 7.4, 7.5**

        Generates valid Model_Context values across the full combinatorial
        space (resolution x physics_suite x coupling_mode x component set)
        and asserts that ModelConfigRenderer.render_all() completes without
        raising TemplateRenderError. Since render_all() runs format-specific
        validators on every rendered file (model_configure, input.nml,
        diag_table, ufs.configure, field_table, GOCART .rc files), a
        successful call guarantees all outputs pass format validation.
        """
        # Create a unique temp directory for this example
        expdir = tmp_path_factory.mktemp("expdir")

        renderer = ModelConfigRenderer(dev_root=DEV_ROOT)

        # render_all performs schema validation, template rendering,
        # AND format validation. If any rendered file fails its
        # format-specific validator, TemplateRenderError is raised.
        try:
            results = renderer.render_all(model_context, expdir)
        except TemplateRenderError as e:
            pytest.fail(
                f"Format validation failed for context "
                f"(resolution={model_context['resolution']}, "
                f"physics_suite={model_context['physics_suite']}, "
                f"coupling_mode={model_context['coupling_mode']}, "
                f"components={model_context['active_components']}): {e}"
            )

        # At least one file must have been rendered
        assert len(results) > 0, "No files were rendered"

        # Verify each rendered file exists on disk and has a valid hash
        for rendered_file in results:
            assert rendered_file.path.exists(), (
                f"Rendered file {rendered_file.path} does not exist"
            )
            assert rendered_file.sha256, (
                f"Rendered file {rendered_file.path} has no SHA-256 hash"
            )



# ---------------------------------------------------------------------------
# Property 5: Component Composition Validity
# Validates: Requirements 10.3, 10.4, 10.7, 10.9
# ---------------------------------------------------------------------------

# Supported component names for the composition strategy
_SUPPORTED_COMPONENTS = ["atmosphere", "ocean", "ice", "wave", "aerosol"]

# Path to the real component YAML files
_COMPONENTS_DIR = DEV_ROOT / "parm" / "components"

# Strategy: non-empty subsets of supported components
_component_subsets = st.sets(
    st.sampled_from(_SUPPORTED_COMPONENTS),
    min_size=1,
)


def _make_workflow_config_for_composition(components: set[str]) -> dict:
    """Build a minimal workflow configuration for the given component subset."""
    return {
        "suite": {"name": "gfs_v17"},
        "components": sorted(components),
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


def _extract_all_trigger_paths(families: list[dict]) -> list[str]:
    """Extract all trigger path references from a families list."""
    trigger_path_re = re.compile(
        r"([\w/]+?)(?:\s*==\s*(?:complete|active|aborted|queued|submitted|unknown)"
        r"|:[\w]+\s+(?:ge|gt|le|lt|eq|ne)\s+\d+)"
    )
    paths = []
    for family in families:
        for task in family.get("tasks", []):
            trigger = task.get("trigger", "")
            if trigger:
                paths.extend(
                    m.group(1) for m in trigger_path_re.finditer(trigger)
                )
    return paths


@pytest.mark.skipif(
    not (_COMPONENTS_DIR).exists(),
    reason=f"Component YAMLs not found at {_COMPONENTS_DIR}",
)
class TestComponentCompositionValidity:
    """Property 5: Component Composition Validity.

    **Validates: Requirements 10.3, 10.4, 10.7, 10.9**

    Verifies that for any non-empty subset of supported components:
    1. The merged Model_Context contains exactly the union of included
       components' model sections.
    2. active_components matches the input subset.
    3. No trigger references to excluded component family paths remain.
    4. Families only contain paths belonging to active components.
    """

    @given(components=_component_subsets)
    @settings(max_examples=100)
    def test_model_section_contains_exactly_included_components(
        self, components: set[str]
    ):
        """Assert merged Model_Context contains exactly the union of
        included components' model sections.

        **Validates: Requirements 10.3**
        """
        workflow_config = _make_workflow_config_for_composition(components)
        result = compose_components(workflow_config, _COMPONENTS_DIR)

        model = result["model"]

        # Each active component's model_key should be present
        for comp_name in components:
            model_key = COMPONENT_REGISTRY[comp_name]["model_key"]
            assert model_key in model, (
                f"Expected model.{model_key} for active component "
                f"'{comp_name}' but it was missing"
            )

        # No excluded component's model_key should be present
        excluded = set(_SUPPORTED_COMPONENTS) - components
        for comp_name in excluded:
            model_key = COMPONENT_REGISTRY[comp_name]["model_key"]
            assert model_key not in model, (
                f"model.{model_key} should not be present when "
                f"component '{comp_name}' is excluded"
            )

    @given(components=_component_subsets)
    @settings(max_examples=100)
    def test_active_components_matches_input_subset(
        self, components: set[str]
    ):
        """Assert active_components matches the input subset.

        **Validates: Requirements 10.4**
        """
        workflow_config = _make_workflow_config_for_composition(components)
        result = compose_components(workflow_config, _COMPONENTS_DIR)

        active = result["model"]["active_components"]
        assert set(active) == components, (
            f"active_components {set(active)} does not match "
            f"input components {components}"
        )

    @given(components=_component_subsets)
    @settings(max_examples=100)
    def test_no_dangling_trigger_references_to_excluded_components(
        self, components: set[str]
    ):
        """Assert no trigger references to excluded component family paths
        remain in the resulting DAG.

        **Validates: Requirements 10.7, 10.9**
        """
        workflow_config = _make_workflow_config_for_composition(components)
        result = compose_components(workflow_config, _COMPONENTS_DIR)

        excluded = set(_SUPPORTED_COMPONENTS) - components
        if not excluded:
            return  # Nothing to check when all components are active

        # Get all trigger paths in the resolved families
        trigger_paths = _extract_all_trigger_paths(result["families"])

        # None of the trigger paths should belong to excluded components
        for path in trigger_paths:
            for excl_comp in excluded:
                prefixes = COMPONENT_FAMILY_PREFIXES.get(excl_comp, [])
                for prefix in prefixes:
                    assert not path.startswith(prefix), (
                        f"Dangling trigger reference '{path}' belongs to "
                        f"excluded component '{excl_comp}' "
                        f"(prefix: '{prefix}')"
                    )

    @given(components=_component_subsets)
    @settings(max_examples=100)
    def test_families_only_contain_paths_belonging_to_active_components(
        self, components: set[str]
    ):
        """Assert families only contain paths belonging to active components.

        **Validates: Requirements 10.4, 10.9**
        """
        workflow_config = _make_workflow_config_for_composition(components)
        result = compose_components(workflow_config, _COMPONENTS_DIR)

        excluded = set(_SUPPORTED_COMPONENTS) - components
        if not excluded:
            return  # Nothing to check when all components are active

        # Get all family paths in the result
        family_paths = [f["path"] for f in result["families"] if "path" in f]

        # None of the family paths should belong to excluded components
        for fpath in family_paths:
            for excl_comp in excluded:
                prefixes = COMPONENT_FAMILY_PREFIXES.get(excl_comp, [])
                for prefix in prefixes:
                    assert not fpath.startswith(prefix), (
                        f"Family path '{fpath}' belongs to excluded "
                        f"component '{excl_comp}' (prefix: '{prefix}')"
                    )
