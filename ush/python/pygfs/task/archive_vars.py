#!/usr/bin/env python3
"""
Archive Variables Task

Overview
--------
This module provides variables needed by YAML templates for archiving verification
(vrfy) data for GFS, GEFS, and GCAFS systems. File set generation logic (loops,
conditionals, path construction) is handled by the YAML templates themselves.

Architecture
------------
Python provides VARIABLES → YAML templates build FILE SETS

Python Code Responsibilities:
  - Compute cycle-specific variables (cycle_HH, cycle_YMDH, cycle_YMD, head)
  - Calculate COM directory paths with grid loops (0p25, 0p50, 1p00)
  - Extract configuration keys (RUN, DO_* flags, FHMAX*, etc.)
  - Provide complete arch_dict to YAML templates

YAML Template Responsibilities (parm/archive/*_arcdir.yaml.j2):
  - Build file sets with source → destination mappings
  - Handle loops (forecast hours, grids, basins)
  - Apply conditionals (DO_* flags, MODE, RUN type)
  - Create mkdir lists for directory creation

Key Methods
-----------
get_all_yaml_vars():
  Main entry point - collects all variables for YAML templates

add_config_vars():
  Extracts configuration keys and COM* template variables

_get_cycle_vars():
  Computes cycle-specific variables (cycle_HH, cycle_YMDH, cycle_YMD, head)

_calculate_com_paths():
  Generates all COM paths (ROTDIR-based) with grid loops and conditional logic

_get_template_dict():
  Creates base template substitution dictionary

Logging
-------
All public operational methods are decorated with @logit(logger).
"""
import os
from logging import getLogger
from typing import Any, Dict
from wxflow import Task, logit, to_YMD, to_YMDH, Template, TemplateConstants

logger = getLogger(__name__.split('.')[-1])


class ArchiveVrfy(Task):
    """
    Task class for archive verification operations.

    This class provides variables for YAML templates that handle archiving
    for three systems:
    - GFS: Global Forecast System
    - GEFS: Global Ensemble Forecast System
    - GCAFS: Global Climate Analysis Forecast System

    The YAML templates (parm/archive/*_arcdir.yaml.j2) contain all file set
    generation logic. This class only provides the variables they need.
    """

    @logit(logger, name="ArchiveVrfy")
    def __init__(self, config: Dict[str, Any]) -> None:
        """Constructor for the ArchiveVrfy task

        Parameters
        ----------
        config : Dict[str, Any]
            Incoming configuration for the task from the environment
        """
        super().__init__(config)

    @logit(logger)
    def get_all_yaml_vars(self) -> Dict[str, Any]:
        """Collect all variables needed for YAML templates.

        This method provides only the VARIABLES needed by the YAML templates
        (cycle vars, COM paths, config keys). The YAML templates handle all
        file set generation logic (loops, conditionals, path construction).

        Returns
        -------
        Dict[str, Any]
            Dictionary containing variables for Jinja2 templates:
            - cycle_HH, cycle_YMDH, cycle_YMD, head: Cycle-specific variables
            - COMIN_*, COMOUT_*, COM_*: All COM directory paths
            - Config keys: RUN, PSLOT, ROTDIR, DO_* flags, FHMAX*, etc.

        Notes
        -----
        File set generation (mkdir lists, copy operations) is handled entirely
        by the YAML templates. This method only provides the variables they need.
        """
        # Build arch_dict with variables for Jinja2 templates
        arch_dict = {}

        # Add config variables (config keys, COM* variables)
        arch_dict.update(self.add_config_vars())

        # Add cycle-specific variables
        arch_dict.update(self._get_cycle_vars())

        # Add COM paths
        base_dict = self._get_template_dict()
        template_specs = self._get_com_template_specs()
        arch_dict.update(self._construct_com_paths(base_dict, template_specs))

        logger.info(f"Collected {len(arch_dict)} variables for YAML templates")
        logger.debug(f"arch_dict keys: {list(arch_dict.keys())}")

        return arch_dict

    @logit(logger)
    def add_config_vars(self) -> Dict[str, Any]:
        """Collect and format general variables for archive operations.

        This method:
        1. Updates resolution variables to be 3-digit formatted strings (if present)
        2. Extracts all required configuration keys for archiving
        3. Collects all COM* directory and template variables
        4. Returns complete dictionary ready for arch_dict

        Variables updated (if present in task_config):
        - OCNRES: Ocean resolution (formatted to 3 digits)
        - ICERES: Ice resolution (formatted to 3 digits)

        Configuration keys extracted (if present):
        - current_cycle, RUN, PSLOT, ROTDIR, PARMgfs, ARCDIR, MODE
        - DO_JEDIATMENS, DO_FIT2OBS, DO_JEDIATMVAR, DO_JEDISNOWDA
        - DO_AERO_ANL, DO_PREP_OBS_AERO, DO_GSISOILDA, DO_LAND_IAU
        - NET, FHOUT_GFS, FHMAX_HF_GFS, FHMAX_FITS, FHMAX, FHOUT, FHMAX_GFS
        - FHMIN_GFS (if present in task_config)

        COM variable prefixes collected:
        - COM_, COMIN_, COMOUT_

        Returns
        -------
        Dict[str, Any]
            Dictionary containing all general archive variables

        Notes
        -----
        Missing keys will be silently skipped (not added to general_dict).
        """
        general_dict = {}

        # Update resolution keys to be 3 digits if they are part of task_config
        for key in ['OCNRES', 'ICERES']:
            if key in self.task_config:
                self.task_config[key] = f"{self.task_config[key]:03d}"

        # Configuration keys to extract (if present)
        config_keys = ['current_cycle', 'RUN', 'PSLOT', 'ROTDIR', 'PARMgfs',
                       'ARCDIR', 'MODE', 'DO_JEDIATMENS', 'DO_FIT2OBS', 'DO_JEDIATMVAR',
                       'DO_JEDISNOWDA', 'DO_AERO_ANL', 'DO_PREP_OBS_AERO', 'NET',
                       'FHOUT_GFS', 'FHMAX_HF_GFS', 'FHMAX_FITS', 'FHMAX', 'FHOUT',
                       'FHMAX_GFS', 'DO_GSISOILDA', 'DO_LAND_IAU']

        # Add FHMIN_GFS only if NET does not contain 'enkf'
        if 'enkf' not in self.task_config.get('NET', ''):
            config_keys.append('FHMIN_GFS')

        # Extract keys if they exist in task_config
        for key in config_keys:
            if key in self.task_config:
                general_dict[key] = self.task_config[key]
            else:
                logger.warning(f"Config key '{key}' not found in task_config; skipping.")

        # Import COM* directory and template variables
        for key in self.task_config.keys():
            if key.startswith(("COM_", "COMIN_", "COMOUT_")):
                general_dict[key] = self.task_config.get(key)

        logger.info(f"Collected {len(general_dict)} general archive variables")
        logger.debug(f"General variables: {list(general_dict.keys())}")

        return general_dict

    @logit(logger)
    def _get_cycle_vars(self) -> Dict[str, Any]:
        """Calculate cycle-specific variables using wxflow timetools.

        Returns
        -------
        Dict[str, Any]
            Dictionary containing:
            - cycle_HH: Cycle hour (e.g., '00', '06')
            - cycle_YMDH: Full cycle timestamp (YYYYMMDDHH)
            - cycle_YMD: Cycle date (YYYYMMDD)
            - head: System head designation (e.g., 'gfs.t00z.')
            - VFYARC: Verification archive directory (ROTDIR/vrfyarch)
        """
        current_cycle = self.task_config.current_cycle
        cycle_HH = current_cycle.strftime("%H")
        cycle_YMDH = to_YMDH(current_cycle)
        cycle_YMD = to_YMD(current_cycle)

        # Build head string (e.g., 'gfs.t00z.')
        head = f"{self.task_config.RUN}.t{cycle_HH}z."

        # Archive directory (used by all systems)
        VFYARC = os.path.join(self.task_config.ROTDIR, "vrfyarch")

        return {
            'cycle_HH': cycle_HH,
            'cycle_YMDH': cycle_YMDH,
            'cycle_YMD': cycle_YMD,
            'head': head,
            'VFYARC': VFYARC
        }

    @logit(logger)
    def _get_template_dict(self) -> Dict[str, str]:
        """Create template substitution dictionary for COM path generation.

        This method builds the base dictionary used for template variable substitution.
        For GEFS, it includes MEMDIR: 'ensstat' to support ensemble statistics paths.
        All values default to empty string if not found.

        Returns
        -------
        Dict[str, str]
            Template substitution dictionary with keys:
            - ROTDIR: Rotating directory path
            - RUN: Run type (gfs, gdas, gefs, etc.)
            - YMD/PDY: Cycle date (YYYYMMDD)
            - HH/cyc: Cycle hour (HH)
            - GRID: Grid resolution (added per-call for grid-specific paths)
            - MEMDIR: 'ensstat' (GEFS only, for ensemble statistics)

        Examples
        --------
        GFS/GDAS:
            {'ROTDIR': '/path', 'RUN': 'gfs', 'YMD': '20240101', 'HH': '00', ...}

        GEFS:
            {'ROTDIR': '/path', 'RUN': 'gefs', 'YMD': '20240101', 'HH': '00',
             'MEMDIR': 'ensstat', ...}
        """
        cycle_vars = self._get_cycle_vars()

        # Base template substitution dictionary with empty string defaults
        base_dict = {
            'ROTDIR': self.task_config.get('ROTDIR', ''),
            'RUN': self.task_config.get('RUN', ''),
            'YMD': cycle_vars.get('cycle_YMD', ''),
            'HH': cycle_vars.get('cycle_HH', ''),
            'PDY': cycle_vars.get('cycle_YMD', ''),
            'cyc': cycle_vars.get('cycle_HH', '')
        }

        # GEFS-specific: Add MEMDIR for ensemble statistics
        # Corresponds to YAML: '${MEMDIR}': 'ensstat'
        if 'gefs' in self.task_config.get('RUN', '').lower():
            base_dict['MEMDIR'] = 'ensstat'

        return base_dict

    def _get_com_template_specs(self) -> list:
        """Collect COM template specifications.

        This method defines which COM variables need to be generated from which
        templates, along with any additional template variables required.

        Returns
        -------
        list of tuples
            Each tuple contains (com_key, template_key, extra_vars):
            - com_key: Output variable name (e.g., 'COMIN_ATMOS_ANALYSIS')
            - template_key: Template key in task_config (e.g., 'COM_ATMOS_ANALYSIS_TMPL')
            - extra_vars: Dict of additional template variables (e.g., {'GRID': '0p25'})
                         Empty dict {} if no additional variables needed
        """
        # EnKF-specific: Only these 3 ENSSTAT paths with MEMDIR='ensstat'
        if 'enkf' in self.task_config.RUN:
            template_specs = [
                ('COMIN_ATMOS_ANALYSIS_ENSSTAT', 'COM_ATMOS_ANALYSIS_TMPL', {'MEMDIR': 'ensstat'}),
                ('COMIN_ATMOS_HISTORY_ENSSTAT', 'COM_ATMOS_HISTORY_TMPL', {'MEMDIR': 'ensstat'}),
                ('COMIN_SNOW_ANALYSIS_ENSSTAT', 'COM_SNOW_ANALYSIS_TMPL', {'MEMDIR': 'ensstat'})
            ]
        else:
            # All other systems (GFS, GEFS, GCAFS) get common + grid-specific paths
            template_specs = [
                ('COMIN_ATMOS_ANALYSIS', 'COM_ATMOS_ANALYSIS_TMPL', {}),
                ('COMIN_ATMOS_GENESIS', 'COM_ATMOS_GENESIS_TMPL', {}),
                ('COMIN_ATMOS_HISTORY', 'COM_ATMOS_HISTORY_TMPL', {}),
                ('COMIN_ATMOS_TRACK', 'COM_ATMOS_TRACK_TMPL', {}),
                ('COMIN_CHEM_ANALYSIS', 'COM_CHEM_ANALYSIS_TMPL', {}),
                ('COMIN_SNOW_ANALYSIS', 'COM_SNOW_ANALYSIS_TMPL', {}),
                ('COMIN_OBS', 'COM_OBS_TMPL', {}),
                ('COMOUT_ATMOS_TRACK', 'COM_ATMOS_TRACK_TMPL', {}),
            ]
            # Grid-specific paths
            for grid in ["0p25", "0p50", "1p00"]:
                com_key = f"COMIN_ATMOS_GRIB_{grid}"
                template_specs.append((com_key, 'COM_ATMOS_GRIB_GRID_TMPL', {'GRID': grid}))

            # GEFS-specific: Ensemble statistics path
            if 'gefs' in self.task_config.RUN:
                template_specs.append(('COMIN_ATMOS_ENSSTAT_1p00', 'COM_ATMOS_GRIB_GRID_TMPL', {'GRID': '1p00'}))

        return template_specs

    def _construct_com_paths(self, base_dict: Dict[str, str], template_specs: list) -> Dict[str, str]:
        """Construct COM paths from template specifications.

        This method takes template specifications and constructs the actual paths
        by substituting template variables using base_dict updated with extra_vars.

        Parameters
        ----------
        base_dict : Dict[str, str]
            Base template substitution dictionary from _get_template_dict()
        template_specs : list of tuples
            List from _get_com_template_specs() containing specifications

        Returns
        -------
        Dict[str, str]
            Dictionary mapping COM variable names to resolved paths
        """
        com_paths = {}

        for com_key, template_key, extra_vars in template_specs:
            # Use base_dict directly, updated with any extra variables
            tmpl_dict = {**base_dict, **extra_vars}

            template = self.task_config.get(template_key, '')
            com_paths[com_key] = Template.substitute_string(
                template, TemplateConstants.DOLLAR_CURLY_BRACE,
                lambda key: tmpl_dict.get(key, '')) if template else ''

        return com_paths


# ============================================================================
# FILE SET GENERATION NOW HANDLED BY YAML TEMPLATES
# ============================================================================
# The following methods have been removed and their logic moved to YAML templates:
#   - _build_gfs_list()
#   - gfs_arcdir()
#   - _build_gefs_list()
#   - gefs_arcdir()
#   - _build_gcafs_list()
#   - gcafs_arcdir()
#
# The YAML templates (parm/archive/*_arcdir.yaml.j2) now contain all file set
# generation logic (loops, conditionals, file path construction).
#
# The Python code only provides VARIABLES (cycle vars, COM paths, config vars)
# that the YAML templates need via get_all_yaml_vars().
# ============================================================================
