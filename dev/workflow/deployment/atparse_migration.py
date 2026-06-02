"""Atparse-to-Jinja2 migration utility.

Converts legacy `@[VAR_NAME]` atparse template syntax to Jinja2 `{{ expr }}`
syntax using a configurable variable mapping table. Shell variables (`${VAR}`)
are preserved verbatim through the conversion.

Traces to: Requirements 8.1, 8.2, 8.3, 8.4, 8.5
"""

from __future__ import annotations

import re
import warnings
from typing import Dict, List, NamedTuple, Optional


# ---------------------------------------------------------------------------
# Data types
# ---------------------------------------------------------------------------


class MigrationResult(NamedTuple):
    """Result of an atparse-to-Jinja2 conversion."""

    content: str
    converted: List[str]  # List of @[VAR] names that were converted
    unknown: List[str]  # List of @[VAR] names not found in mapping


# ---------------------------------------------------------------------------
# Atparse pattern
# ---------------------------------------------------------------------------

# Matches @[VAR_NAME] — the atparse substitution syntax
_ATPARSE_PATTERN = re.compile(r"@\[([A-Za-z_][A-Za-z0-9_]*)\]")

# Matches ${VAR} or $VAR shell variable references (to be preserved)
_SHELL_VAR_PATTERN = re.compile(r"\$\{[A-Za-z_][A-Za-z0-9_]*\}|\$[A-Za-z_][A-Za-z0-9_]*")


# ---------------------------------------------------------------------------
# Variable mapping tables
# ---------------------------------------------------------------------------

# model_configure variables: atparse uppercase → Jinja2 Model_Context expression
MODEL_CONFIGURE_MAPPING: Dict[str, str] = {
    # Task and timing
    "TOTAL_TASKS": "model.fv3.total_tasks",
    "PE_MEMBER01": "model.fv3.total_tasks",
    "DT_ATMOS": "model.dt_atmos",
    "RESTART_INTERVAL": "model.fv3.restart_interval",
    "FHROT": "model.fv3.fhrot",
    # Start date components
    "SYEAR": "model.start_date.year",
    "SMONTH": "model.start_date.month",
    "SDAY": "model.start_date.day",
    "SHOUR": "model.start_date.hour",
    "CHOUR": "model.start_date.hour",
    # Quilting and output
    "QUILTING": "model.fv3.quilting | fortran_logical",
    "QUILTING_RESTART": "model.fv3.quilting_restart | default(model.fv3.quilting) | fortran_logical",
    "WRITE_GROUP": "model.fv3.write_group",
    "WRTTASK_PER_GROUP": "model.fv3.wrttask_per_group",
    "NUM_FILES": "model.fv3.num_output_files | default(2)",
    "FILENAME_BASE": "model.fv3.filename_base | default(\"'atm' 'sfc'\")",
    "OUTPUT_GRID": "model.output_grid",
    "OUTPUT_FILE": "model.fv3.output_file",
    "OUTPUT_FILETYPE_ATM": "model.fv3.output_filetype_atm",
    "OUTPUT_FILETYPE_SFC": "model.fv3.output_filetype_sfc",
    "IMO": "model.fv3.imo",
    "JMO": "model.fv3.jmo",
    "OUTPUT_FH": "model.fv3.output_fh",
    "IAU_OFFSET": "model.fv3.iau_offset | default(0)",
    # Compression
    "IDEFLATE": "model.fv3.ideflate | default(0)",
    "QUANTIZE_NSD": "model.fv3.quantize_nsd | default(0)",
    "ZSTANDARD_LEVEL": "model.fv3.zstandard_level | default(0)",
    "ICHUNK2D": "model.fv3.ichunk2d",
    "JCHUNK2D": "model.fv3.jchunk2d",
    "ICHUNK3D": "model.fv3.ichunk3d",
    "JCHUNK3D": "model.fv3.jchunk3d",
    "KCHUNK3D": "model.fv3.kchunk3d | default(1)",
    # Misc
    "WRITE_DOPOST": "model.fv3.write_dopost | default('.false.')",
    "WRITE_NSFLIP": "model.fv3.write_nsflip | default('.false.')",
    "OUTPUT_HISTORY": "model.fv3.output_history | default('.true.')",
    "HISTORY_FILE_ON_NATIVE_GRID": "model.fv3.history_file_on_native_grid | default('.false.')",
}

# ufs.configure variables: atparse uppercase → Jinja2 Model_Context expression
UFS_CONFIGURE_MAPPING: Dict[str, str] = {
    # Component models
    "atm_model": "model.fv3.atm_model | default('fv3')",
    "ocn_model": "model.ocean.model_name | default('mom6')",
    "ice_model": "model.ice.model_name | default('cice6')",
    "wav_model": "model.wave.model_name | default('ww3')",
    "chm_model": "model.aerosol.model_name | default('gocart')",
    "med_model": "model.mediator.model_name | default('cmeps')",
    # PET list bounds
    "atm_petlist_bounds": "atm_pet_start ~ ' ' ~ atm_pet_end",
    "ocn_petlist_bounds": "ocn_pet_start ~ ' ' ~ ocn_pet_end",
    "ice_petlist_bounds": "ice_pet_start ~ ' ' ~ ice_pet_end",
    "wav_petlist_bounds": "wav_pet_start ~ ' ' ~ wav_pet_end",
    "chm_petlist_bounds": "chm_pet_start ~ ' ' ~ chm_pet_end",
    "med_petlist_bounds": "med_pet_start ~ ' ' ~ med_pet_end",
    # Thread counts
    "atm_omp_num_threads": "model.fv3.omp_threads | default(1)",
    "ocn_omp_num_threads": "model.ocean.omp_threads | default(1)",
    "ice_omp_num_threads": "model.ice.omp_threads | default(1)",
    "wav_omp_num_threads": "model.wave.omp_threads | default(1)",
    "chm_omp_num_threads": "model.fv3.omp_threads | default(1)",
    "med_omp_num_threads": "model.fv3.omp_threads | default(1)",
    # Component task counts
    "ATMPETS": "model.fv3.total_tasks",
    "OCNPETS": "model.ocean.tasks",
    "ICEPETS": "model.ice.nprocs",
    "WAVPETS": "model.wave.tasks",
    "CHMPETS": "model.fv3.total_tasks",
    "MEDPETS": "model.fv3.total_tasks",
    # Coupling intervals
    "coupling_interval_slow_sec": "model.coupling_interval_slow",
    "coupling_interval_fast_sec": "model.coupling_interval_fast",
    "coupling_interval_sec": "model.coupling_interval_fast",
    "CPL_SLOW": "model.coupling_interval_slow",
    "CPL_FAST": "model.coupling_interval_fast",
    # CMEPS configuration
    "RUNTYPE": "model.get('cmeps_run_type', 'startup')",
    "CMEPS_RESTART_DIR": "model.get('cmeps_restart_dir', 'CMEPS_RESTART/')",
    "CPLMODE": "model.get('cplmode', 'nems_frac')",
    "CMEPS_PIO_FORMAT": "model.get('cmeps_pio_format', 'pnetcdf')",
    "CMEPS_PIO_STRIDE": "model.get('cmeps_pio_stride', 4)",
    "CMEPS_PIO_IOTASKS": "model.get('cmeps_pio_iotasks', -99)",
    "CMEPS_PIO_REARR": "model.get('pio_rearranger', 'box')",
    "CMEPS_PIO_ROOT": "model.get('cmeps_pio_root', -99)",
    "RESTART_N": "model.get('restart_n', 999999)",
    "MED_history_n": "model.get('med_history_n', 1000000)",
    "ATMTILESIZE": "model.fv3.npx - 1",
    "ocean_albedo_limit": "model.get('ocean_albedo_limit', 0.06)",
    "MAPUV3D": "model.get('mapuv3d', 'true')",
    "WRITE_ENDOFRUN_RESTART": "model.get('write_endofrun_restart', '.false.')",
    # Ocean attributes
    "MOM6_OUTPUT_DIR": "model.ocean.output_dir | default('./MOM6_OUTPUT')",
    "MOM6_RESTART_DIR": "model.ocean.restart_dir | default('./MOM6_RESTART')",
    "MOM6_HISTFREQ_N": "model.ocean.output_frequency_hours | default(6)",
    # Wave attributes
    "WW3_user_histname": "model.wave.get('user_histname', 'false')",
    "WW3_historync": "model.wave.get('historync', 'false')",
    "WW3_restartnc": "model.wave.get('restartnc', 'true')",
    "WW3_PIO_FORMAT": "model.wave.get('pio_format', 'pnetcdf')",
    "WW3_PIO_IOTASKS": "model.wave.get('pio_iotasks', -99)",
    "WW3_PIO_STRIDE": "model.wave.get('pio_stride', 4)",
    "WW3_PIO_REARR": "model.wave.get('pio_rearranger', 'box')",
    "WW3_PIO_ROOT": "model.wave.get('pio_root', -99)",
    # ESMF configuration
    "esmf_logkind": "model.get('esmf_logkind', 'ESMF_LOGKIND_MULTI')",
    "DumpFields": "model.get('dump_fields', 'false')",
    "cap_dbug_flag": "model.get('cap_dbug_flag', 0)",
    "ESMF_THREADING": "model.get('esmf_threading', 'false')",
    "use_coldstart": "model.get('use_coldstart', '.false.')",
    "use_mommesh": "model.get('use_mommesh', 'true')",
    "eps_imesh": "model.get('eps_imesh', '1.0e-1')",
}

# diag_table variables: atparse uppercase → Jinja2 Model_Context expression
DIAG_TABLE_MAPPING: Dict[str, str] = {
    "MOM6_OUTPUT_DIR": "model.ocean.output_dir | default('./MOM6_OUTPUT')",
    "FHOUT_OCN": "model.ocean.output_frequency_hours | default(6)",
    "SYEAR": "model.start_date.year",
    "SMONTH": "model.start_date.month",
    "SDAY": "model.start_date.day",
    "CHOUR": "model.start_date.hour",
}

# AERO_HISTORY.rc variables: atparse uppercase → Jinja2 Model_Context expression
AERO_HISTORY_MAPPING: Dict[str, str] = {
    # Collection frequency variables
    "inst_du_ss_freq": "model.aerosol.get('inst_du_ss_freq', '010000')",
    "tavg_du_ss_freq": "model.aerosol.get('tavg_du_ss_freq', '010000')",
    "inst_ca_freq": "model.aerosol.get('inst_ca_freq', '010000')",
    "inst_ni_freq": "model.aerosol.get('inst_ni_freq', '010000')",
    "inst_su_freq": "model.aerosol.get('inst_su_freq', '010000')",
    "inst_du_bin_freq": "model.aerosol.get('inst_du_bin_freq', '010000')",
    "tavg_du_bin_freq": "model.aerosol.get('tavg_du_bin_freq', '010000')",
    "inst_ss_bin_freq": "model.aerosol.get('inst_ss_bin_freq', '010000')",
    "inst_ca_bin_freq": "model.aerosol.get('inst_ca_bin_freq', '010000')",
    "inst_ni_bin_freq": "model.aerosol.get('inst_ni_bin_freq', '010000')",
    "inst_su_bin_freq": "model.aerosol.get('inst_su_bin_freq', '010000')",
    "inst_2d_freq": "model.aerosol.get('inst_2d_freq', '030000')",
    "inst_3d_freq": "model.aerosol.get('inst_3d_freq', '010000')",
    "inst_aod_freq": "model.aerosol.get('inst_aod_freq', '010000')",
    "tavg_2d_rad_freq": "model.aerosol.get('tavg_2d_rad_freq', '120000')",
    "tavg_3d_rad_freq": "model.aerosol.get('tavg_3d_rad_freq', '120000')",
    # Grid labels
    "GRID_LABEL": "model.aerosol.grid_label",
    "GRID_IM": "model.aerosol.grid_im",
    "GRID_JM": "model.aerosol.grid_jm",
    "LM": "model.fv3.npz",
}

# Combined default mapping covering all config file types
DEFAULT_VAR_MAPPING: Dict[str, str] = {
    **MODEL_CONFIGURE_MAPPING,
    **UFS_CONFIGURE_MAPPING,
    **DIAG_TABLE_MAPPING,
    **AERO_HISTORY_MAPPING,
}


# ---------------------------------------------------------------------------
# Conversion function
# ---------------------------------------------------------------------------


def atparse_to_jinja2(
    content: str,
    var_mapping: Optional[Dict[str, str]] = None,
) -> MigrationResult:
    """Convert atparse `@[VAR]` patterns to Jinja2 `{{ expr }}` syntax.

    Parameters
    ----------
    content : str
        The template content containing `@[VAR_NAME]` atparse patterns.
    var_mapping : dict[str, str] | None
        Mapping of atparse variable names to Jinja2 expressions.
        If None, uses the DEFAULT_VAR_MAPPING.

    Returns
    -------
    MigrationResult
        A named tuple with:
        - content: The converted content with Jinja2 syntax
        - converted: List of variable names that were successfully converted
        - unknown: List of variable names not found in the mapping

    Notes
    -----
    - Shell variables (`${VAR}`) are preserved verbatim through conversion.
    - Unknown `@[VAR]` patterns (not in the mapping) are left as-is and
      a warning is emitted for each.
    """
    if var_mapping is None:
        var_mapping = DEFAULT_VAR_MAPPING

    converted: list[str] = []
    unknown: list[str] = []

    def _replace_atparse(match: re.Match) -> str:
        var_name = match.group(1)
        if var_name in var_mapping:
            converted.append(var_name)
            return "{{ " + var_mapping[var_name] + " }}"
        else:
            unknown.append(var_name)
            warnings.warn(
                f"Unknown atparse variable '@[{var_name}]' — "
                f"no mapping defined, leaving unchanged.",
                stacklevel=2,
            )
            return match.group(0)  # Leave unchanged

    result = _ATPARSE_PATTERN.sub(_replace_atparse, content)

    return MigrationResult(content=result, converted=converted, unknown=unknown)


def get_mapping_for_file(filename: str) -> Dict[str, str]:
    """Return the appropriate variable mapping for a given config file type.

    Parameters
    ----------
    filename : str
        The filename (or path) of the config file being migrated.

    Returns
    -------
    Dict[str, str]
        The variable mapping appropriate for the file type.
    """
    lower = filename.lower()
    if "model_configure" in lower:
        return MODEL_CONFIGURE_MAPPING
    elif "ufs.configure" in lower or "ufs_configure" in lower:
        return UFS_CONFIGURE_MAPPING
    elif "diag_table" in lower:
        return DIAG_TABLE_MAPPING
    elif "aero_history" in lower:
        return AERO_HISTORY_MAPPING
    else:
        return DEFAULT_VAR_MAPPING


def list_atparse_variables(content: str) -> List[str]:
    """Extract all unique @[VAR] variable names from content.

    Parameters
    ----------
    content : str
        The template content to scan.

    Returns
    -------
    list[str]
        Sorted list of unique atparse variable names found.
    """
    return sorted(set(_ATPARSE_PATTERN.findall(content)))


def validate_no_atparse_remaining(content: str) -> List[str]:
    """Check that no @[VAR] patterns remain in converted content.

    Parameters
    ----------
    content : str
        The converted content to validate.

    Returns
    -------
    list[str]
        List of any remaining @[VAR] patterns found (empty if clean).
    """
    return _ATPARSE_PATTERN.findall(content)
