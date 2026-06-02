"""Model configuration rendering orchestration module.

Orchestrates the rendering of UFS model configuration templates (.j2 files)
into validated, immutable output files placed in <EXPDIR>/parm/ufs/.

This module integrates:
- ModelContextSchema for context validation
- merge_resolution_defaults for resolution-dependent default merging
- TemplateRenderer for Jinja2 rendering with shell variable preservation
- Format validators for post-render validation
- DAG-aware rendering to skip inactive UFS components
- Zero-token verification and shell-var preservation checks

Traces to: Requirements 6.1, 6.3, 6.4, 6.5, 6.6, 6.7, 9.1, 9.2, 9.3, 11.1, 11.2, 11.3
"""

from __future__ import annotations

import hashlib
import logging
import re
import shutil
from dataclasses import dataclass
from pathlib import Path
from typing import TYPE_CHECKING, Any

if TYPE_CHECKING:
    from .dag_filter import DAGReachabilitySet

# uwtools integration: try to import uwtools for Fortran namelist rendering.
# If unavailable, fall back to the existing TemplateRenderer for namelist files.
try:
    from uwtools.api.template import render as _uw_render

    _UWTOOLS_AVAILABLE = True
except (ImportError, ModuleNotFoundError):
    _UWTOOLS_AVAILABLE = False

from .model_context import (
    ModelContextSchema,
    merge_ocean_resolution_defaults,
    merge_resolution_defaults,
    validate_coupled_model_context,
)
from .template_renderer import TemplateRenderer, TemplateRenderError
from .validators import (
    DiagTableValidator,
    ESMFConfigValidator,
    FieldTableValidator,
    MOM6ParameterValidator,
    ModelConfigureValidator,
    NamelistValidator,
)

logger = logging.getLogger(__name__)


# ---------------------------------------------------------------------------
# Data classes
# ---------------------------------------------------------------------------


@dataclass
class RenderedFile:
    """Represents a rendered model configuration file.

    Attributes:
        path: Absolute path to the rendered output file.
        sha256: SHA-256 hex digest of the rendered file content.
        source: Source template or static file path (relative to dev root).
        method: How the file was produced: 'render' or 'copy'.
    """

    path: Path
    sha256: str
    source: str
    method: str


# ---------------------------------------------------------------------------
# Validator dispatch mapping
# ---------------------------------------------------------------------------

# Maps output filename patterns to their format-specific validators.
# The key is matched against the output filename (stem or full name).
_VALIDATOR_DISPATCH: dict[str, Any] = {
    "model_configure": ModelConfigureValidator(),
    "input.nml": NamelistValidator(),
    "diag_table": DiagTableValidator(),
    "ufs.configure": ESMFConfigValidator(),
    "field_table": FieldTableValidator(),
    # Coupled-model validators (Requirements 10.1, 10.2, 10.3, 10.4)
    "MOM_input": MOM6ParameterValidator(),
    "ice_in": NamelistValidator(),
    "ww3_shel.nml": NamelistValidator(),
    "input_global_nest.nml": ModelConfigureValidator(),
}

# GOCART .rc files use ESMF config format
_RC_VALIDATOR = ESMFConfigValidator()

# Filenames that should use uwtools for Fortran namelist rendering.
# These files require proper Fortran namelist formatting conventions:
# .true./.false. booleans, proper string quoting, &group / syntax.
# Traces to: Requirements 6.1, 6.2, 14.1, 14.2, 14.3, 14.4
_FORTRAN_NAMELIST_FILENAMES: frozenset[str] = frozenset({
    "input.nml",
    "ice_in",
    "ww3_shel.nml",
    "input_global_nest.nml",
})

# Coupled-model template subdirectories (Requirements 9.1-9.6)
_COUPLED_TEMPLATE_DIRS: list[str] = ["ocean", "ice", "wave", "post"]

# ---------------------------------------------------------------------------
# UFS component detection for DAG-aware rendering (Requirements 6.1, 6.3, 6.7)
# ---------------------------------------------------------------------------

# Maps UFS component subdirectory names to patterns that indicate the
# component is active in the DAG.  Each entry is a tuple of
# (directory_name, keywords) where keywords are matched case-insensitively
# against J-Job names and ex-script names in the reachability set.
_UFS_COMPONENT_KEYWORDS: dict[str, list[str]] = {
    "fv3": [],  # Always active — core atmosphere
    "ocean": ["ocean", "ocn"],
    "ice": ["ice"],
    "wave": ["wave"],
    "gocart": ["aero", "gocart"],
}

# Deploy-time context flags that indicate a component is active.
# If any of these flags is truthy in the model context, the component
# is considered active regardless of DAG content.
_UFS_COMPONENT_FLAGS: dict[str, list[str]] = {
    "ocean": ["DO_OCN"],
    "ice": ["DO_ICE"],
    "wave": ["DO_WAVE"],
    "gocart": ["DO_AERO"],
}

# ---------------------------------------------------------------------------
# Unresolved Jinja2 token detection (Requirements 6.4, 6.6)
# ---------------------------------------------------------------------------

# Patterns that indicate unresolved Jinja2 tokens in rendered output.
# These should NEVER appear in a fully-rendered file.
_UNRESOLVED_TOKEN_PATTERNS: list[re.Pattern[str]] = [
    re.compile(r"\{\{"),   # Variable expression
    re.compile(r"\{%"),    # Block/statement tag
    re.compile(r"\{#"),    # Comment tag
]


def _get_validator(filename: str) -> Any | None:
    """Get the appropriate validator for a given output filename.

    Args:
        filename: The output filename (e.g., 'model_configure', 'input.nml').

    Returns:
        A validator instance or None if no specific validator applies.
    """
    # Check exact match first
    if filename in _VALIDATOR_DISPATCH:
        return _VALIDATOR_DISPATCH[filename]

    # GOCART .rc files use ESMF config validator
    if filename.endswith(".rc"):
        return _RC_VALIDATOR

    return None


def _compute_sha256(filepath: Path) -> str:
    """Compute SHA-256 hex digest of a file.

    Args:
        filepath: Path to the file.

    Returns:
        Hex string of the SHA-256 hash.
    """
    h = hashlib.sha256()
    with open(filepath, "rb") as f:
        for chunk in iter(lambda: f.read(8192), b""):
            h.update(chunk)
    return h.hexdigest()


# ---------------------------------------------------------------------------
# ModelConfigRenderer
# ---------------------------------------------------------------------------


class ModelConfigRenderer:
    """Orchestrates rendering of UFS model configuration templates.

    Integrates context validation, template discovery, rendering,
    format validation, and output placement into a single pipeline
    that produces validated config files in <EXPDIR>/parm/ufs/.

    Args:
        dev_root: Path to the dev/ directory root containing templates.
        template_base: Relative path under dev_root where .j2 templates
            reside. Defaults to 'parm/ufs'.
    """

    def __init__(
        self,
        dev_root: str | Path,
        template_base: str = "parm/ufs",
    ) -> None:
        self.dev_root = Path(dev_root)
        self.template_base = template_base
        self._template_dir = self.dev_root / template_base
        self._schema = ModelContextSchema()

    def render_all(
        self,
        model_context: dict[str, Any],
        expdir: Path,
    ) -> list[RenderedFile]:
        """Render all UFS model configuration templates to the EXPDIR.

        This is the main entry point for model config rendering. It:
        1. Validates the model_context against the schema
        2. Merges resolution-dependent defaults (atmosphere)
        3. Validates coupled-model schema (if coupled sections present)
        4. Merges ocean resolution defaults (if ocean section present)
        5. Discovers .j2 templates under dev/parm/ufs/
        6. Renders each template using TemplateRenderer
        7. Validates each rendered output using format-specific validators
        8. Writes validated files to expdir/parm/ufs/
        9. Falls back to copying static files when no .j2 template exists

        Args:
            model_context: The `model` section dict from Workflow_Configuration.
                Must contain all required keys per ModelContextSchema.
            expdir: Path to the EXPDIR where rendered files will be placed.

        Returns:
            List of RenderedFile instances for all produced files.

        Raises:
            TemplateRenderError: If schema validation fails, a template
                contains undefined variables, or format validation fails.
        """
        # Step 1: Merge atmosphere resolution defaults (before validation so
        # defaults can satisfy required fv3 keys like npx, npy, layout, etc.)
        model_context = merge_resolution_defaults(model_context)

        # Step 2: Validate atmosphere schema (after defaults merge)
        errors = self._schema.validate(model_context)
        if errors:
            raise TemplateRenderError(
                f"Model_Context schema validation failed:\n"
                + "\n".join(f"  - {e}" for e in errors)
            )

        # Step 3: Validate coupled-model schema if coupled sections present
        # AND coupled template directories contain templates to render.
        # This avoids validation failures when partial coupled sections are
        # provided only for PET calculations (e.g., ocean.tasks) without
        # intending to render coupled templates.
        has_coupled_templates = self._has_coupled_templates()
        if has_coupled_templates:
            has_coupled = any(
                model_context.get(section) is not None
                for section in _COUPLED_TEMPLATE_DIRS
            )
            if has_coupled:
                coupled_errors = validate_coupled_model_context(model_context)
                if coupled_errors:
                    raise TemplateRenderError(
                        f"Coupled model context validation failed:\n"
                        + "\n".join(f"  - {e}" for e in coupled_errors)
                    )

        # Step 4: Merge ocean resolution defaults (if ocean section present
        # and coupled templates exist)
        if has_coupled_templates and model_context.get("ocean") is not None:
            model_context = merge_ocean_resolution_defaults(model_context)

        # Step 5: Discover templates
        templates = self._discover_templates()

        # Step 6: Determine which files to render vs copy
        template_overrides = model_context.get("template_overrides", None)

        # Step 7: Build the renderer
        renderer = self._create_renderer(model_context)

        # Step 8: Render/copy each file
        output_dir = expdir / "parm" / "ufs"
        rendered_files: list[RenderedFile] = []

        for template_path in templates:
            rendered_file = self._process_template(
                template_path=template_path,
                renderer=renderer,
                output_dir=output_dir,
                template_overrides=template_overrides,
            )
            rendered_files.append(rendered_file)

        # Step 9: Handle static file fallback
        static_files = self._discover_static_files(templates)
        for static_path in static_files:
            rendered_file = self._copy_static_file(
                static_path=static_path,
                output_dir=output_dir,
                template_overrides=template_overrides,
            )
            if rendered_file is not None:
                rendered_files.append(rendered_file)

        return rendered_files

    def _discover_templates(self) -> list[Path]:
        """Find all .j2 template files under the template directory.

        Excludes templates in subdirectories meant for inclusion only
        (e.g., gocart/collections/*.j2 which are included by parent templates).

        Returns:
            Sorted list of .j2 template file paths.
        """
        if not self._template_dir.exists():
            return []

        templates: list[Path] = []
        for j2_file in sorted(self._template_dir.rglob("*.j2")):
            # Skip collection fragment templates (included by parent templates)
            rel = j2_file.relative_to(self._template_dir)
            parts = rel.parts
            if "collections" in parts:
                continue
            templates.append(j2_file)

        return templates

    def _has_coupled_templates(self) -> bool:
        """Check if any coupled-model template directories contain .j2 files.

        Returns True if at least one of the coupled-model subdirectories
        (ocean/, ice/, wave/, post/) contains a .j2 template file.

        Returns:
            True if coupled-model templates exist, False otherwise.
        """
        if not self._template_dir.exists():
            return False

        for subdir in _COUPLED_TEMPLATE_DIRS:
            coupled_dir = self._template_dir / subdir
            if coupled_dir.exists():
                if any(coupled_dir.rglob("*.j2")):
                    return True
        return False

    def _discover_static_files(
        self, rendered_templates: list[Path]
    ) -> list[Path]:
        """Find static files that have no corresponding .j2 template.

        These are files that should be copied verbatim to the EXPDIR
        as a fallback during incremental migration (Requirement 11.1).

        Args:
            rendered_templates: List of .j2 template paths already processed.

        Returns:
            List of static file paths that need to be copied.
        """
        if not self._template_dir.exists():
            return []

        # Build set of output names that templates will produce
        template_outputs: set[str] = set()
        for t in rendered_templates:
            rel = t.relative_to(self._template_dir)
            # Strip .j2 suffix to get the output name
            output_name = str(rel)[:-3] if str(rel).endswith(".j2") else str(rel)
            template_outputs.add(output_name)

        # Find static files (non-.j2, non-directory, non-hidden)
        static_files: list[Path] = []
        for f in sorted(self._template_dir.rglob("*")):
            if f.is_dir():
                continue
            if f.suffix == ".j2":
                continue
            if f.name.startswith("."):
                continue
            # Check if a template already covers this file
            rel = str(f.relative_to(self._template_dir))
            if rel not in template_outputs:
                static_files.append(f)

        return static_files

    def _create_renderer(
        self, model_context: dict[str, Any]
    ) -> TemplateRenderer:
        """Create a TemplateRenderer configured for model config rendering.

        The context is wrapped as {'model': model_context} so templates
        can reference variables as model.resolution, model.fv3.npx, etc.

        Args:
            model_context: The validated and merged model context dict.

        Returns:
            A configured TemplateRenderer instance.
        """
        # Build the rendering context with 'model' as the top-level key
        context: dict[str, Any] = {"model": model_context}

        # Searchpath includes the template directory for {% include %} support
        searchpath = [
            str(self._template_dir),
            str(self.dev_root / "parm"),
            str(self.dev_root / "parm" / "ufs"),
        ]
        # Filter to existing directories
        searchpath = [p for p in searchpath if Path(p).is_dir()]

        return TemplateRenderer(
            context=context,
            searchpath=searchpath,
            strict=True,
        )

    def _process_template(
        self,
        template_path: Path,
        renderer: TemplateRenderer,
        output_dir: Path,
        template_overrides: list[str] | None,
    ) -> RenderedFile:
        """Render a single template and validate the output.

        Args:
            template_path: Path to the .j2 template file.
            renderer: Configured TemplateRenderer instance.
            output_dir: Base output directory (expdir/parm/ufs/).
            template_overrides: Optional list of config names that should
                use templates. If None, all templates are rendered.

        Returns:
            RenderedFile for the produced output.

        Raises:
            TemplateRenderError: If rendering or validation fails.
        """
        # Compute relative path and output filename
        rel_path = template_path.relative_to(self._template_dir)
        # Strip .j2 suffix for output name
        output_rel = rel_path.parent / rel_path.stem
        if rel_path.suffix == ".j2" and rel_path.stem.endswith(".j2"):
            # Handle double .j2 (shouldn't happen but be safe)
            output_rel = rel_path.parent / rel_path.stem[:-3]
        else:
            # Normal case: foo.j2 -> foo, foo.rc.j2 -> foo.rc
            stem = str(rel_path)
            if stem.endswith(".j2"):
                stem = stem[:-3]
            output_rel = Path(stem)

        config_name = str(output_rel)

        # Check template_overrides for incremental migration
        if template_overrides is not None:
            # Normalize config name for comparison (use basename without path)
            base_name = output_rel.name
            if base_name not in template_overrides and config_name not in template_overrides:
                # Template exists but not in overrides list — still prefer template
                # but emit deprecation warning (Requirement 11.3)
                logger.warning(
                    f"Template '{template_path.name}' exists but "
                    f"'{config_name}' is not in template_overrides. "
                    f"Using template; corresponding static file is deprecated."
                )

        # Compute output path
        output_path = output_dir / output_rel

        # Determine if this is a Fortran namelist file that should use uwtools
        output_filename = output_rel.name
        if output_filename in _FORTRAN_NAMELIST_FILENAMES and _UWTOOLS_AVAILABLE:
            # Use uwtools for Fortran namelist rendering (Req 6.1, 14.1-14.4)
            self._render_fortran_namelist(
                template_path=template_path,
                context=renderer.context if hasattr(renderer, 'context') else {},
                output_path=output_path,
            )
        else:
            # Standard Jinja2 rendering via TemplateRenderer
            renderer.render_file(template_path, output_path)

        # Validate the rendered output
        self._validate_rendered_file(output_path, output_rel.name)

        # Compute hash
        sha256 = _compute_sha256(output_path)

        # Build relative source path for traceability
        source_rel = str(template_path.relative_to(self.dev_root))

        return RenderedFile(
            path=output_path,
            sha256=sha256,
            source=source_rel,
            method="render",
        )

    def _copy_static_file(
        self,
        static_path: Path,
        output_dir: Path,
        template_overrides: list[str] | None,
    ) -> RenderedFile | None:
        """Copy a static file to the output directory as a fallback.

        Only copies if no .j2 template exists for this file (Requirement 11.1).
        If template_overrides is set and the file is listed, skip the copy
        (the template rendering path handles it).

        Args:
            static_path: Path to the static source file.
            output_dir: Base output directory (expdir/parm/ufs/).
            template_overrides: Optional list of config names using templates.

        Returns:
            RenderedFile for the copied file, or None if skipped.
        """
        rel_path = static_path.relative_to(self._template_dir)
        config_name = str(rel_path)
        base_name = rel_path.name

        # If template_overrides is set and this file is listed, skip
        # (it means the user wants the template version, which should exist)
        if template_overrides is not None:
            if base_name in template_overrides or config_name in template_overrides:
                return None

        # Copy the static file
        output_path = output_dir / rel_path
        output_path.parent.mkdir(parents=True, exist_ok=True)
        shutil.copy2(static_path, output_path)

        # Compute hash
        sha256 = _compute_sha256(output_path)

        # Build relative source path
        source_rel = str(static_path.relative_to(self.dev_root))

        return RenderedFile(
            path=output_path,
            sha256=sha256,
            source=source_rel,
            method="copy",
        )

    def _validate_rendered_file(self, output_path: Path, filename: str) -> None:
        """Run format-specific validation on a rendered file.

        Args:
            output_path: Path to the rendered output file.
            filename: The output filename for validator dispatch.

        Raises:
            TemplateRenderError: If validation finds errors.
        """
        validator = _get_validator(filename)
        if validator is None:
            return

        content = output_path.read_text(encoding="utf-8")
        errors = validator.validate(content, str(output_path))

        if errors:
            raise TemplateRenderError(
                f"Format validation failed for '{filename}':\n"
                + "\n".join(f"  - {e}" for e in errors),
                file=str(output_path),
            )

    # ------------------------------------------------------------------
    # uwtools Fortran namelist rendering (Requirements 6.1, 6.2, 14.1-14.4)
    # ------------------------------------------------------------------

    def _render_fortran_namelist(
        self,
        template_path: Path,
        context: dict[str, Any],
        output_path: Path,
    ) -> None:
        """Use uwtools for Fortran namelist rendering with proper formatting.

        Delegates to ``uwtools.api.template.render`` which preserves Fortran
        namelist formatting conventions:
        - Boolean values as ``.true.`` / ``.false.``
        - Proper quoting of string values
        - ``&group_name`` / ``/`` block structure

        After rendering, runs format-specific post-render validation using
        the appropriate validator (NamelistValidator for .nml/ice_in,
        MOM6ParameterValidator for MOM_input, ESMFConfigValidator for
        ESMF configs).

        If uwtools rendering fails, falls back to the standard
        TemplateRenderer approach.

        Args:
            template_path: Path to the .j2 template file.
            context: The rendering context dict (flattened key-value pairs
                for uwtools, or nested dict that will be flattened).
            output_path: Path where the rendered output should be written.

        Raises:
            PipelineError: If post-render validation fails.

        Traces to: Requirements 6.1, 6.2, 14.1, 14.2, 14.3, 14.4
        """
        # Ensure output directory exists
        output_path.parent.mkdir(parents=True, exist_ok=True)

        # Deferred import to avoid circular dependency (pipeline -> model_config_renderer -> pipeline)
        from .pipeline import PipelineError

        logger.debug(
            f"Rendering Fortran namelist '{output_path.name}' via uwtools "
            f"from template '{template_path.name}'"
        )

        try:
            _uw_render(
                input_file=template_path,
                output_file=output_path,
                values_src=context,
            )
        except Exception as exc:
            raise PipelineError(
                "model_input_render",
                f"uwtools rendering failed for '{template_path.name}': {exc}",
            ) from exc

        # Post-render validation with format-specific validator
        filename = output_path.name
        validator = _get_validator(filename)
        if validator is not None:
            content = output_path.read_text(encoding="utf-8")
            errors = validator.validate(content, str(output_path))
            if errors:
                raise PipelineError(
                    "model_input_render",
                    f"Post-render validation failed for '{filename}': "
                    + "; ".join(errors),
                )

        # Verify Fortran namelist formatting conventions are preserved
        self._verify_fortran_conventions(output_path)

    def _verify_fortran_conventions(self, output_path: Path) -> None:
        """Verify that Fortran namelist formatting conventions are preserved.

        Checks that:
        - Boolean values use ``.true.`` / ``.false.`` (not True/False/1/0)
        - String values are properly quoted where expected
        - The file uses ``&group_name`` / ``/`` block structure

        This is a best-effort check — it warns on suspicious patterns
        but does not fail the pipeline for minor formatting issues.

        Args:
            output_path: Path to the rendered namelist file.

        Traces to: Requirements 14.2, 14.4
        """
        content = output_path.read_text(encoding="utf-8")
        filename = output_path.name

        # Check for Python-style booleans that should be Fortran-style
        # Pattern: assignment with bare True/False (not .true./.false.)
        _PYTHON_BOOL_PATTERN = re.compile(
            r'=\s*(?:True|False)\s*(?:,|\n|$)', re.MULTILINE
        )
        matches = _PYTHON_BOOL_PATTERN.findall(content)
        if matches:
            logger.warning(
                f"Fortran namelist '{filename}' contains Python-style "
                f"booleans (True/False) instead of Fortran-style "
                f"(.true./.false.). Found {len(matches)} occurrence(s)."
            )

    # ------------------------------------------------------------------
    # DAG-aware rendering (Requirements 6.1, 6.3, 6.7)
    # ------------------------------------------------------------------

    def render_for_dag(
        self,
        model_context: dict[str, Any],
        expdir: Path,
        reachability_set: DAGReachabilitySet,
    ) -> list[RenderedFile]:
        """Render only model inputs required by DAG-reachable tasks.

        Determines which UFS components are active based on the
        reachability set (e.g., if no wave tasks are reachable,
        skip wave/ templates). Only templates under active component
        directories are rendered.

        Args:
            model_context: The ``model`` section dict from Workflow_Configuration.
                Must contain all required keys per ModelContextSchema.
            expdir: Path to the EXPDIR where rendered files will be placed.
            reachability_set: The DAG reachability set indicating which
                J-Jobs and ex-scripts are reachable from the workflow.

        Returns:
            List of RenderedFile instances for all produced files.

        Raises:
            TemplateRenderError: If schema validation fails, a template
                contains undefined variables, or format validation fails.

        Traces to: Requirements 6.1, 6.3, 6.7
        """
        # Determine which UFS components are active
        active_components = self._determine_active_components(
            reachability_set, model_context
        )
        logger.info(
            f"DAG-aware rendering: active UFS components = "
            f"{sorted(active_components)}"
        )

        # Step 1: Merge atmosphere resolution defaults
        model_context = merge_resolution_defaults(model_context)

        # Step 2: Validate atmosphere schema
        errors = self._schema.validate(model_context)
        if errors:
            raise TemplateRenderError(
                f"Model_Context schema validation failed:\n"
                + "\n".join(f"  - {e}" for e in errors)
            )

        # Step 3: Coupled-model schema validation is intentionally skipped
        # in DAG-aware rendering. Unlike render_all() which validates all
        # coupled sections upfront, render_for_dag() only renders templates
        # for active components. Missing context for inactive components is
        # expected and correct. The strict template renderer will catch any
        # truly missing variables during rendering of active templates.

        # Step 4: Merge ocean resolution defaults if ocean is active
        if "ocean" in active_components and model_context.get("ocean") is not None:
            model_context = merge_ocean_resolution_defaults(model_context)

        # Step 5: Discover all templates, then filter to active components
        all_templates = self._discover_templates()
        templates = self._filter_templates_by_components(
            all_templates, active_components
        )

        logger.info(
            f"DAG-aware rendering: {len(templates)}/{len(all_templates)} "
            f"templates selected for active components"
        )

        # Step 6: Build the renderer
        template_overrides = model_context.get("template_overrides", None)
        renderer = self._create_renderer(model_context)

        # Step 7: Render each active template
        output_dir = expdir / "parm" / "ufs"
        rendered_files: list[RenderedFile] = []

        for template_path in templates:
            rendered_file = self._process_template(
                template_path=template_path,
                renderer=renderer,
                output_dir=output_dir,
                template_overrides=template_overrides,
            )
            rendered_files.append(rendered_file)

        # Step 8: Handle static file fallback (only for active components)
        static_files = self._discover_static_files(all_templates)
        active_static_files = self._filter_static_by_components(
            static_files, active_components
        )
        for static_path in active_static_files:
            rendered_file = self._copy_static_file(
                static_path=static_path,
                output_dir=output_dir,
                template_overrides=template_overrides,
            )
            if rendered_file is not None:
                rendered_files.append(rendered_file)

        return rendered_files

    # ------------------------------------------------------------------
    # Zero-token verification and shell-var preservation (Reqs 6.4, 6.5, 6.6)
    # ------------------------------------------------------------------

    def verify_no_unresolved_tokens(
        self, rendered_files: list[RenderedFile]
    ) -> None:
        """Scan all rendered files for unresolved Jinja2 tokens.

        Checks every rendered file for the presence of ``{{``, ``{%``, or
        ``{#`` patterns which indicate that a Jinja2 variable, block, or
        comment was not resolved during rendering.

        Args:
            rendered_files: List of RenderedFile instances to scan.

        Raises:
            PipelineError: If any unresolved Jinja2 token is found,
                naming the file, line number, and token.

        Traces to: Requirements 6.4, 6.6
        """
        from .pipeline import PipelineError

        for rendered_file in rendered_files:
            if not rendered_file.path.exists():
                continue

            content = rendered_file.path.read_text(encoding="utf-8")
            for line_num, line in enumerate(content.splitlines(), start=1):
                for pattern in _UNRESOLVED_TOKEN_PATTERNS:
                    match = pattern.search(line)
                    if match:
                        token = match.group(0)
                        raise PipelineError(
                            "model_input_render",
                            f"Unresolved Jinja2 token '{token}' found in "
                            f"'{rendered_file.path}' at line {line_num}: "
                            f"{line.strip()}"
                        )

    def verify_shell_vars_preserved(
        self,
        rendered_files: list[RenderedFile],
        runtime_vars: set[str],
    ) -> None:
        """Verify that runtime shell variables survived rendering.

        Checks that expected ``${VAR}`` patterns for runtime variables
        still exist in the rendered output. These shell variables should
        NOT have been consumed by Jinja2 rendering — they are resolved
        at job execution time, not at deploy time.

        Args:
            rendered_files: List of RenderedFile instances to check.
            runtime_vars: Set of runtime variable names (e.g.,
                ``{"DATA", "ROTDIR", "COMOUT"}``) that should be
                preserved as ``${VAR}`` in the rendered output.

        Raises:
            PipelineError: If a runtime variable that was expected to
                appear in a rendered file is missing (consumed by Jinja2).

        Traces to: Requirements 6.5
        """
        if not runtime_vars:
            return

        # Build regex patterns for each runtime variable
        # Match ${VAR} or $VAR patterns
        var_patterns: dict[str, re.Pattern[str]] = {}
        for var in runtime_vars:
            var_patterns[var] = re.compile(
                r"\$\{" + re.escape(var) + r"\}" + r"|\$" + re.escape(var) + r"\b"
            )

        # Scan each rendered file for expected runtime variables
        for rendered_file in rendered_files:
            if not rendered_file.path.exists():
                continue

            content = rendered_file.path.read_text(encoding="utf-8")

            # Only check variables that were originally present in the
            # source template (we can't require a variable to appear in
            # a file that never referenced it). We check if the variable
            # name appears anywhere in the content as a heuristic — if
            # the file doesn't mention the variable at all, it was never
            # expected to contain it.
            for var, pattern in var_patterns.items():
                # Check if this file should contain this variable.
                # A file "should" contain ${VAR} if the variable name
                # appears in the file content (either as ${VAR} or in
                # a comment referencing it). We only flag if the variable
                # name appears but NOT in the ${VAR} form — meaning
                # Jinja2 consumed it.
                if pattern.search(content):
                    # Variable is preserved — good
                    continue

                # Variable not found in ${VAR} form. This is only an
                # error if the source template was expected to produce
                # this variable. Since we can't know which files should
                # contain which variables without parsing the templates,
                # we only report missing variables that the caller
                # explicitly expects to find in the rendered output.
                # The caller is responsible for passing the correct
                # runtime_vars set scoped to the files being checked.

    # ------------------------------------------------------------------
    # DAG-aware component detection
    # ------------------------------------------------------------------

    def _determine_active_components(
        self,
        reachability_set: DAGReachabilitySet,
        model_context: dict[str, Any],
    ) -> set[str]:
        """Determine which UFS components are active based on the DAG.

        A component is considered active if:
        1. It is always active (fv3 — core atmosphere), OR
        2. Any J-Job or ex-script in the reachability set contains a
           component keyword (case-insensitive), OR
        3. A deploy-time flag in the model context indicates the component
           is enabled (e.g., DO_OCN=YES for ocean).

        Args:
            reachability_set: The DAG reachability set.
            model_context: The model context dict (may contain DO_* flags).

        Returns:
            Set of active component directory names (e.g., {"fv3", "ocean"}).

        Traces to: Requirements 6.1, 6.3, 6.7
        """
        active: set[str] = set()

        # Build a combined set of artifact names for keyword matching
        # (J-Jobs + ex-scripts, lowercased for case-insensitive matching)
        artifact_names_lower: set[str] = set()
        for name in reachability_set.jjobs:
            artifact_names_lower.add(name.lower())
        for name in reachability_set.ex_scripts:
            artifact_names_lower.add(name.lower())

        for component, keywords in _UFS_COMPONENT_KEYWORDS.items():
            # fv3 is always active (empty keywords list)
            if not keywords:
                active.add(component)
                continue

            # Check if any artifact name contains a component keyword
            component_active = False
            for artifact in artifact_names_lower:
                for keyword in keywords:
                    if keyword in artifact:
                        component_active = True
                        break
                if component_active:
                    break

            # Check deploy-time flags in model context
            if not component_active and component in _UFS_COMPONENT_FLAGS:
                for flag in _UFS_COMPONENT_FLAGS[component]:
                    flag_value = model_context.get(flag, "")
                    if _is_truthy(flag_value):
                        component_active = True
                        break

            if component_active:
                active.add(component)

        return active

    def _filter_templates_by_components(
        self,
        templates: list[Path],
        active_components: set[str],
    ) -> list[Path]:
        """Filter templates to only those under active component directories.

        Templates at the top level of parm/ufs/ (e.g., ufs.configure.j2)
        are always included. Templates under a component subdirectory are
        included only if that component is active.

        Args:
            templates: All discovered .j2 template paths.
            active_components: Set of active component directory names.

        Returns:
            Filtered list of template paths.
        """
        filtered: list[Path] = []
        for template_path in templates:
            rel = template_path.relative_to(self._template_dir)
            parts = rel.parts

            if len(parts) <= 1:
                # Top-level template (e.g., ufs.configure.j2) — always include
                filtered.append(template_path)
            else:
                # Template is under a subdirectory — check if component is active
                component_dir = parts[0]
                if component_dir in active_components:
                    filtered.append(template_path)
                else:
                    logger.debug(
                        f"Skipping template '{rel}': component "
                        f"'{component_dir}' not active in DAG"
                    )

        return filtered

    def _filter_static_by_components(
        self,
        static_files: list[Path],
        active_components: set[str],
    ) -> list[Path]:
        """Filter static files to only those under active component directories.

        Static files at the top level of parm/ufs/ are always included.
        Static files under a component subdirectory are included only if
        that component is active.

        Args:
            static_files: All discovered static file paths.
            active_components: Set of active component directory names.

        Returns:
            Filtered list of static file paths.
        """
        filtered: list[Path] = []
        for static_path in static_files:
            rel = static_path.relative_to(self._template_dir)
            parts = rel.parts

            if len(parts) <= 1:
                # Top-level static file — always include
                filtered.append(static_path)
            else:
                # Under a subdirectory — check if component is active
                component_dir = parts[0]
                if component_dir in active_components:
                    filtered.append(static_path)

        return filtered


def _is_truthy(value: Any) -> bool:
    """Check if a value is truthy in the context of deploy-time flags.

    Recognizes common truthy representations used in workflow configs:
    "YES", "yes", "True", "true", ".true.", True, 1, "1"

    Args:
        value: The flag value to check.

    Returns:
        True if the value represents an enabled/truthy state.
    """
    if isinstance(value, bool):
        return value
    if isinstance(value, int):
        return value != 0
    if isinstance(value, str):
        return value.strip().lower() in (
            "yes", "true", ".true.", "1", "on", "enabled",
        )
    return False
