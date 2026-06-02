"""Model_Context schema validation for UFS model configuration rendering.

Implements schema validation and resolution-dependent default merging for
the `model` section of the Workflow_Configuration YAML. This module is used
during Stage 3 (Render Templates) of the deployment pipeline to ensure all
required template variables are present and valid before rendering UFS model
configuration templates.

Traces to: Requirements 4.1, 4.2, 4.3, 4.5, 4.6, 4.7, 7.1, 7.2, 7.3, 7.4, 7.5, 12.1, 12.2, 12.3, 12.4
"""

from __future__ import annotations

from dataclasses import dataclass, field
from typing import Any


# ---------------------------------------------------------------------------
# Exceptions
# ---------------------------------------------------------------------------

class FatalDeploymentError(Exception):
    """Raised when a fatal deployment-time error occurs.

    Used for unrecoverable configuration errors such as unsupported
    resolution values or missing required keys that prevent template
    rendering from proceeding.
    """


# ---------------------------------------------------------------------------
# Supported value enums
# ---------------------------------------------------------------------------

SUPPORTED_RESOLUTIONS: set[str] = {
    "C48", "C96", "C384", "C768", "C1152",
}

SUPPORTED_PHYSICS_SUITES: set[str] = {
    "gfdl", "thompson", "wsm6", "zhaocarr",
}

SUPPORTED_COUPLING_MODES: set[str] = {
    "atm", "atmaero", "s2s", "s2sa", "s2sw", "s2swa", "leapfrog_atm_wav",
}

SUPPORTED_EMISSION_DATASETS: set[str] = {
    "qfed", "gbbepx", "none",
}


# ---------------------------------------------------------------------------
# Schema definition
# ---------------------------------------------------------------------------

# Required top-level keys in the model section (Requirement 4.1)
REQUIRED_TOP_LEVEL_KEYS: list[str] = [
    "resolution",
    "physics_suite",
    "coupling_mode",
    "dt_atmos",
    "output_grid",
    "output_fields",
]

# Required keys in model.fv3 subsection (Requirement 4.2)
REQUIRED_FV3_KEYS: list[str] = [
    "npx",
    "npy",
    "npz",
    "layout",
    "io_layout",
    "quilting",
    "write_group",
    "wrttask_per_group",
    "restart_interval",
]

# Required keys in model.aerosol subsection (Requirement 4.3)
REQUIRED_AEROSOL_KEYS: list[str] = [
    "emission_dataset",
    "active_collections",
    "grid_label",
]


# ---------------------------------------------------------------------------
# Validation result
# ---------------------------------------------------------------------------

@dataclass
class ModelContextSchema:
    """Schema validator for the Model_Context section of Workflow_Configuration.

    Validates that all required keys are present and have valid values,
    emitting FATAL ERROR messages for any violations.

    Attributes:
        required_top_level_keys: Keys required at the top level of the model section.
        required_fv3_keys: Keys required in the model.fv3 subsection.
        required_aerosol_keys: Keys required in the model.aerosol subsection.
    """

    required_top_level_keys: list[str] = field(
        default_factory=lambda: list(REQUIRED_TOP_LEVEL_KEYS)
    )
    required_fv3_keys: list[str] = field(
        default_factory=lambda: list(REQUIRED_FV3_KEYS)
    )
    required_aerosol_keys: list[str] = field(
        default_factory=lambda: list(REQUIRED_AEROSOL_KEYS)
    )

    def validate(self, model_context: dict[str, Any]) -> list[str]:
        """Validate the model_context dict against the schema.

        Returns a list of FATAL ERROR messages for missing or invalid keys.
        An empty list indicates the context is valid.

        Args:
            model_context: The `model` section dict from Workflow_Configuration.

        Returns:
            List of FATAL ERROR message strings. Empty if valid.
        """
        errors: list[str] = []

        # Validate required top-level keys (Requirement 4.1, 4.5)
        for key in self.required_top_level_keys:
            if key not in model_context:
                errors.append(
                    f"FATAL ERROR: Missing required key 'model.{key}' "
                    f"in Model_Context"
                )

        # Validate top-level value constraints
        if "resolution" in model_context:
            resolution = model_context["resolution"]
            if resolution not in SUPPORTED_RESOLUTIONS:
                errors.append(
                    f"FATAL ERROR: Unsupported resolution '{resolution}'. "
                    f"Supported values: {sorted(SUPPORTED_RESOLUTIONS)}"
                )

        if "physics_suite" in model_context:
            physics_suite = model_context["physics_suite"]
            if physics_suite not in SUPPORTED_PHYSICS_SUITES:
                errors.append(
                    f"FATAL ERROR: Unsupported physics_suite '{physics_suite}'. "
                    f"Supported values: {sorted(SUPPORTED_PHYSICS_SUITES)}"
                )

        if "coupling_mode" in model_context:
            coupling_mode = model_context["coupling_mode"]
            if coupling_mode not in SUPPORTED_COUPLING_MODES:
                errors.append(
                    f"FATAL ERROR: Unsupported coupling_mode '{coupling_mode}'. "
                    f"Supported values: {sorted(SUPPORTED_COUPLING_MODES)}"
                )

        if "dt_atmos" in model_context:
            dt_atmos = model_context["dt_atmos"]
            if not isinstance(dt_atmos, int) or dt_atmos <= 0:
                errors.append(
                    f"FATAL ERROR: 'model.dt_atmos' must be a positive integer, "
                    f"got {dt_atmos!r}"
                )

        # Validate model.fv3 subsection (Requirement 4.2)
        fv3 = model_context.get("fv3")
        if fv3 is not None:
            errors.extend(self._validate_fv3(fv3))

        # Validate model.aerosol subsection (Requirement 4.3)
        aerosol = model_context.get("aerosol")
        if aerosol is not None:
            errors.extend(self._validate_aerosol(aerosol))

        return errors

    def _validate_fv3(self, fv3: dict[str, Any]) -> list[str]:
        """Validate the model.fv3 subsection.

        Args:
            fv3: The model.fv3 dict.

        Returns:
            List of FATAL ERROR messages for invalid fv3 keys.
        """
        errors: list[str] = []

        for key in self.required_fv3_keys:
            if key not in fv3:
                errors.append(
                    f"FATAL ERROR: Missing required key 'model.fv3.{key}' "
                    f"in Model_Context"
                )

        # Type validations for present keys
        if "npx" in fv3:
            if not isinstance(fv3["npx"], int) or fv3["npx"] <= 0:
                errors.append(
                    f"FATAL ERROR: 'model.fv3.npx' must be a positive integer, "
                    f"got {fv3['npx']!r}"
                )

        if "npy" in fv3:
            if not isinstance(fv3["npy"], int) or fv3["npy"] <= 0:
                errors.append(
                    f"FATAL ERROR: 'model.fv3.npy' must be a positive integer, "
                    f"got {fv3['npy']!r}"
                )

        if "npz" in fv3:
            if not isinstance(fv3["npz"], int) or fv3["npz"] <= 0:
                errors.append(
                    f"FATAL ERROR: 'model.fv3.npz' must be a positive integer, "
                    f"got {fv3['npz']!r}"
                )

        if "layout" in fv3:
            layout = fv3["layout"]
            if (
                not isinstance(layout, (list, tuple))
                or len(layout) != 2
                or not all(isinstance(x, int) and x > 0 for x in layout)
            ):
                errors.append(
                    f"FATAL ERROR: 'model.fv3.layout' must be a two-element list "
                    f"of positive integers, got {layout!r}"
                )

        if "io_layout" in fv3:
            io_layout = fv3["io_layout"]
            if (
                not isinstance(io_layout, (list, tuple))
                or len(io_layout) != 2
                or not all(isinstance(x, int) and x >= 0 for x in io_layout)
            ):
                errors.append(
                    f"FATAL ERROR: 'model.fv3.io_layout' must be a two-element list "
                    f"of non-negative integers, got {io_layout!r}"
                )

        if "quilting" in fv3:
            if not isinstance(fv3["quilting"], bool):
                errors.append(
                    f"FATAL ERROR: 'model.fv3.quilting' must be a boolean, "
                    f"got {fv3['quilting']!r}"
                )

        if "write_group" in fv3:
            if not isinstance(fv3["write_group"], int) or fv3["write_group"] <= 0:
                errors.append(
                    f"FATAL ERROR: 'model.fv3.write_group' must be a positive integer, "
                    f"got {fv3['write_group']!r}"
                )

        if "wrttask_per_group" in fv3:
            if not isinstance(fv3["wrttask_per_group"], int) or fv3["wrttask_per_group"] <= 0:
                errors.append(
                    f"FATAL ERROR: 'model.fv3.wrttask_per_group' must be a positive "
                    f"integer, got {fv3['wrttask_per_group']!r}"
                )

        if "restart_interval" in fv3:
            if not isinstance(fv3["restart_interval"], int) or fv3["restart_interval"] < 0:
                errors.append(
                    f"FATAL ERROR: 'model.fv3.restart_interval' must be a non-negative "
                    f"integer, got {fv3['restart_interval']!r}"
                )

        return errors

    def _validate_aerosol(self, aerosol: dict[str, Any]) -> list[str]:
        """Validate the model.aerosol subsection.

        Args:
            aerosol: The model.aerosol dict.

        Returns:
            List of FATAL ERROR messages for invalid aerosol keys.
        """
        errors: list[str] = []

        for key in self.required_aerosol_keys:
            if key not in aerosol:
                errors.append(
                    f"FATAL ERROR: Missing required key 'model.aerosol.{key}' "
                    f"in Model_Context"
                )

        if "emission_dataset" in aerosol:
            emission_dataset = aerosol["emission_dataset"]
            if emission_dataset not in SUPPORTED_EMISSION_DATASETS:
                errors.append(
                    f"FATAL ERROR: Unsupported emission_dataset '{emission_dataset}'. "
                    f"Supported values: {sorted(SUPPORTED_EMISSION_DATASETS)}"
                )

        if "active_collections" in aerosol:
            collections = aerosol["active_collections"]
            if not isinstance(collections, list) or len(collections) == 0:
                errors.append(
                    f"FATAL ERROR: 'model.aerosol.active_collections' must be a "
                    f"non-empty list, got {collections!r}"
                )

        if "grid_label" in aerosol:
            grid_label = aerosol["grid_label"]
            if not isinstance(grid_label, str) or not grid_label:
                errors.append(
                    f"FATAL ERROR: 'model.aerosol.grid_label' must be a non-empty "
                    f"string, got {grid_label!r}"
                )

        return errors


# ---------------------------------------------------------------------------
# Resolution defaults merge
# ---------------------------------------------------------------------------

def merge_resolution_defaults(model_context: dict[str, Any]) -> dict[str, Any]:
    """Merge resolution-dependent defaults into model.fv3.

    Looks up the resolution in `model.defaults` and applies default values
    to `model.fv3` for any keys not already explicitly set. Explicit
    `model.fv3` values always override resolution-dependent defaults
    (Requirement 4.7).

    Args:
        model_context: The `model` section dict from Workflow_Configuration.
            Must contain `resolution` key. May contain `defaults` and `fv3`
            subsections.

    Returns:
        The model_context dict with `fv3` subsection populated with
        resolution defaults for any missing keys.
    """
    resolution = model_context.get("resolution")
    if resolution is None:
        return model_context

    defaults = model_context.get("defaults", {}).get(resolution, {})
    fv3 = model_context.get("fv3", {})

    # Apply defaults only for keys not already explicitly provided
    for key, value in defaults.items():
        if key not in fv3:
            fv3[key] = value

    model_context["fv3"] = fv3
    return model_context


# ---------------------------------------------------------------------------
# Coupled-model schema definitions (Requirements 7.1, 7.2, 7.3, 7.4, 7.5)
# ---------------------------------------------------------------------------

SUPPORTED_OCEAN_RESOLUTIONS: set[str] = {"025", "050", "100", "500"}

SUPPORTED_WAVE_ICE_INPUT: set[str] = {"YES", "CPL"}

SUPPORTED_WAVE_CURRENT_INPUT: set[str] = {"YES", "CPL"}

SUPPORTED_POST_SYSTEMS: set[str] = {"gfs", "gcafs", "gefs", "sfs"}

# Required keys for each coupled-model section
COUPLED_REQUIRED_KEYS: dict[str, list[str]] = {
    "ocean": [
        "resolution", "dt_ocean", "dt_therm", "use_waves",
        "oda_incupd", "do_sppt", "river_runoff",
        "diag_coord_def_z_file", "frunoff", "tasks",
    ],
    "ice": [
        "nprocs", "decomposition", "dt_ice", "grid", "mask",
        "nx_glb", "ny_glb", "warm_start", "histfreq_n",
        "hist_avg", "dumpfreq", "dumpfreq_n", "ktherm", "tr_pond_lvl",
    ],
    "wave": [
        "ice_input", "current_input", "output_params",
        "dt_field_output", "dt_point_output",
        "grid_output_dir", "point_output_dir", "restart_output_dir",
    ],
    "post": ["system"],
}


def validate_coupled_model_context(model_context: dict[str, Any]) -> list[str]:
    """Validate required keys and enum constraints for coupled-model sections.

    Checks that the `ocean`, `ice`, `wave`, and `post` sections of the
    model context contain all required keys and that enum-constrained
    values are within their allowed sets.

    Args:
        model_context: The `model` section dict from Workflow_Configuration.
            Expected to contain `ocean`, `ice`, `wave`, and `post` subsections.

    Returns:
        List of FATAL ERROR message strings. Empty if valid.

    Traces to: Requirements 7.1, 7.2, 7.3, 7.4, 7.5
    """
    errors: list[str] = []

    # Validate required keys for each coupled-model section
    for section, keys in COUPLED_REQUIRED_KEYS.items():
        section_data = model_context.get(section)
        if section_data is None:
            errors.append(
                f"FATAL ERROR: Missing required section 'model.{section}'"
            )
            continue
        for key in keys:
            if key not in section_data:
                errors.append(
                    f"FATAL ERROR: Missing required key 'model.{section}.{key}'"
                )

    # Validate enum constraints
    ocean = model_context.get("ocean")
    if ocean is not None and "resolution" in ocean:
        resolution = ocean["resolution"]
        if resolution not in SUPPORTED_OCEAN_RESOLUTIONS:
            errors.append(
                f"FATAL ERROR: Invalid ocean.resolution '{resolution}'. "
                f"Must be one of: {', '.join(sorted(SUPPORTED_OCEAN_RESOLUTIONS))}"
            )

    wave = model_context.get("wave")
    if wave is not None:
        if "ice_input" in wave:
            ice_input = wave["ice_input"]
            if ice_input not in SUPPORTED_WAVE_ICE_INPUT:
                errors.append(
                    f"FATAL ERROR: Invalid wave.ice_input '{ice_input}'. "
                    f"Must be one of: {', '.join(sorted(SUPPORTED_WAVE_ICE_INPUT))}"
                )
        if "current_input" in wave:
            current_input = wave["current_input"]
            if current_input not in SUPPORTED_WAVE_CURRENT_INPUT:
                errors.append(
                    f"FATAL ERROR: Invalid wave.current_input '{current_input}'. "
                    f"Must be one of: {', '.join(sorted(SUPPORTED_WAVE_CURRENT_INPUT))}"
                )

    post = model_context.get("post")
    if post is not None and "system" in post:
        system = post["system"]
        if system not in SUPPORTED_POST_SYSTEMS:
            errors.append(
                f"FATAL ERROR: Invalid post.system '{system}'. "
                f"Must be one of: {', '.join(sorted(SUPPORTED_POST_SYSTEMS))}"
            )

    return errors


# ---------------------------------------------------------------------------
# Ocean resolution defaults (Requirements 12.1, 12.4)
# ---------------------------------------------------------------------------

OCEAN_RESOLUTION_DEFAULTS: dict[str, dict[str, Any]] = {
    "025": {
        "nx_glb": 1440,
        "ny_glb": 1080,
        "dt_ocean": 900,
        "dt_therm": 3600,
        "KHTH": 10.0,
        "KHTR": 10.0,
        "SMAG_BI_CONST": 0.06,
    },
    "050": {
        "nx_glb": 720,
        "ny_glb": 576,
        "dt_ocean": 1800,
        "dt_therm": 3600,
        "KHTH": 50.0,
        "KHTR": 50.0,
        "SMAG_BI_CONST": 0.06,
    },
    "100": {
        "nx_glb": 360,
        "ny_glb": 320,
        "dt_ocean": 3600,
        "dt_therm": 7200,
        "KHTH": 600.0,
        "KHTR": 600.0,
        "SMAG_BI_CONST": 0.15,
    },
    "500": {
        "nx_glb": 72,
        "ny_glb": 35,
        "dt_ocean": 7200,
        "dt_therm": 14400,
        "KHTH": 1000.0,
        "KHTR": 1000.0,
    },
}


# ---------------------------------------------------------------------------
# Ocean resolution defaults merge (Requirements 12.1, 12.2, 12.3, 12.4)
# ---------------------------------------------------------------------------

def merge_ocean_resolution_defaults(model_context: dict[str, Any]) -> dict[str, Any]:
    """Merge ocean resolution-dependent defaults into model.ocean.

    Looks up the ocean resolution in ``model.ocean.defaults[resolution]``
    (if present) or falls back to the built-in ``OCEAN_RESOLUTION_DEFAULTS``
    mapping, and applies default values to ``model.ocean`` for any keys not
    already explicitly set. Explicit ``model.ocean`` values always override
    resolution-dependent defaults (Requirement 12.2).

    Args:
        model_context: The ``model`` section dict from Workflow_Configuration.
            Must contain an ``ocean`` subsection with a ``resolution`` key.

    Returns:
        The model_context dict with ``ocean`` subsection populated with
        resolution defaults for any missing keys.

    Raises:
        FatalDeploymentError: If ``ocean.resolution`` is not one of the
            supported values (025, 050, 100, 500).

    Traces to: Requirements 12.1, 12.2, 12.3, 12.4
    """
    ocean = model_context.get("ocean", {})
    resolution = ocean.get("resolution")

    if resolution not in SUPPORTED_OCEAN_RESOLUTIONS:
        raise FatalDeploymentError(
            f"Unsupported ocean resolution '{resolution}'. "
            f"Supported: 025, 050, 100, 500"
        )

    # Use user-provided defaults if available, otherwise use built-in defaults
    defaults = ocean.get("defaults", {}).get(resolution, {})
    if not defaults:
        defaults = OCEAN_RESOLUTION_DEFAULTS.get(resolution, {})

    # Apply defaults only for keys not already explicitly provided in ocean
    for key, value in defaults.items():
        if key not in ocean:
            ocean[key] = value

    model_context["ocean"] = ocean
    return model_context
