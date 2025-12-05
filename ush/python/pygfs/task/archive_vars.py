#!/usr/bin/env python3
"""
Archive Variables Utility Module

Overview
--------
This module provides utility functions to collect variables needed by YAML templates
for archiving verification (vrfy) data for GFS, GEFS, and GCAFS systems. File set
generation logic (loops, conditionals, path construction) is handled by the YAML
templates themselves.

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

Key Functions
-------------
get_all_yaml_vars(config_dict):
  Main entry point - collects all variables for YAML templates

add_config_vars(config_dict):
  Extracts configuration keys and COM* variables (created in job scripts)

_get_cycle_vars(config_dict):
  Computes cycle-specific variables (cycle_HH, cycle_YMDH, cycle_YMD, head)

Design Note
-----------
This is NOT a Task class - it's a utility module with functions that operate on
config_dict dictionaries. This avoids duplicate Task instantiation in archive workflows.

Logging
-------
All public operational functions are decorated with @logit(logger).
"""
import os
from logging import getLogger
from typing import Any, Dict
from wxflow import AttrDict, logit, to_YMD, to_YMDH

logger = getLogger(__name__.split('.')[-1])


class ArchiveVrfy:
    """
    Utility class for collecting archive verification variables.

    This class provides variables for YAML templates that handle archiving
    for three systems:
    - GFS: Global Forecast System
    - GEFS: Global Ensemble Forecast System
    - GCAFS: Global Climate Analysis Forecast System

    The YAML templates (parm/archive/*_arcdir.yaml.j2) contain all file set
    generation logic. This class only provides the variables they need.
    """

    @staticmethod
    @logit(logger)
    def get_all_yaml_vars(config_dict: AttrDict) -> Dict[str, Any]:
        """Collect all variables needed for YAML templates.

        This method provides only the VARIABLES needed by the YAML templates
        (cycle vars, COM paths, config keys). The YAML templates handle all
        file set generation logic (loops, conditionals, path construction).

        Parameters
        ----------
        config_dict : AttrDict
            Configuration dictionary from Archive.task_config

        Returns
        -------
        Dict[str, Any]
            Dictionary containing variables for Jinja2 templates:
            - cycle_HH, cycle_YMDH, cycle_YMD, head: Cycle-specific variables
            - COMIN_*, COMOUT_*, COM_*: All COM directory paths (from job scripts)
            - Config keys: RUN, PSLOT, ROTDIR, DO_* flags, FHMAX*, etc.

        Notes
        -----
        File set generation (mkdir lists, copy operations) is handled entirely
        by the YAML templates. This method only provides the variables they need.
        COM paths are created in the job scripts (JGLOBAL_ARCHIVE_VRFY and
        JGLOBAL_ENKF_ARCHIVE_VRFY) and passed through config_dict.
        """
        # Build arch_dict with variables for Jinja2 templates
        arch_dict = {}

        # Add config variables (config keys, COM* variables from job scripts)
        arch_dict.update(ArchiveVrfy.add_config_vars(config_dict))

        # Add cycle-specific variables
        arch_dict.update(ArchiveVrfy._get_cycle_vars(config_dict))

        logger.info(f"Collected {len(arch_dict)} variables for YAML templates")
        logger.debug(f"arch_dict keys: {list(arch_dict.keys())}")

        return arch_dict

    @staticmethod
    @logit(logger)
    def add_config_vars(config_dict: AttrDict) -> Dict[str, Any]:
        """Collect and format general variables for archive operations.

        This method:
        1. Updates resolution variables to be 3-digit formatted strings (if present)
        2. Extracts all required configuration keys for archiving
        3. Collects all COM* directory and template variables
        4. Returns complete dictionary ready for arch_dict

        Parameters
        ----------
        config_dict : AttrDict
            Configuration dictionary from Archive.task_config

        Variables updated (if present in config_dict):
        - OCNRES: Ocean resolution (formatted to 3 digits)
        - ICERES: Ice resolution (formatted to 3 digits)

        Configuration keys extracted (if present):
        - current_cycle, RUN, PSLOT, ROTDIR, PARMgfs, ARCDIR, MODE
        - DO_JEDIATMENS, DO_FIT2OBS, DO_JEDIATMVAR, DO_JEDISNOWDA
        - DO_AERO_ANL, DO_PREP_OBS_AERO, DO_GSISOILDA, DO_LAND_IAU
        - NET, FHOUT_GFS, FHMAX_HF_GFS, FHMAX_FITS, FHMAX, FHOUT, FHMAX_GFS
        - FHMIN_GFS (if present in config_dict)

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

        # Update resolution keys to be 3 digits if they are part of config_dict
        for key in ['OCNRES', 'ICERES']:
            if key in config_dict:
                config_dict[key] = f"{config_dict[key]:03d}"

        # Configuration keys to extract (if present)
        config_keys = ['current_cycle', 'RUN', 'PSLOT', 'ROTDIR', 'PARMgfs',
                       'ARCDIR', 'MODE', 'DO_JEDIATMENS', 'DO_FIT2OBS', 'DO_JEDIATMVAR',
                       'DO_JEDISNOWDA', 'DO_AERO_ANL', 'DO_PREP_OBS_AERO', 'NET',
                       'FHOUT_GFS', 'FHMAX_HF_GFS', 'FHMAX_FITS', 'FHMAX', 'FHOUT',
                       'FHMAX_GFS', 'DO_GSISOILDA', 'DO_LAND_IAU']

        # Add FHMIN_GFS only if RUN does not contain 'enkf'
        if 'enkf' not in config_dict.get('RUN', ''):
            config_keys.append('FHMIN_GFS')

        # Extract keys if they exist in config_dict
        for key in config_keys:
            if key in config_dict:
                general_dict[key] = config_dict[key]
            else:
                logger.warning(f"Config key '{key}' not found in config_dict; skipping.")

        # Import COM* directory and template variables created by job scripts
        # Job scripts use declare_from_tmpl -rx which exports variables to environment
        # Python reads os.environ, so these COM variables are in config_dict
        for key in config_dict.keys():
            if key.startswith(("COM_", "COMIN_", "COMOUT_")):
                general_dict[key] = config_dict.get(key)

        logger.info(f"Collected {len(general_dict)} general archive variables")
        logger.debug(f"General variables: {list(general_dict.keys())}")

        return general_dict

    @staticmethod
    @logit(logger)
    def _get_cycle_vars(config_dict: AttrDict) -> Dict[str, Any]:
        """Calculate cycle-specific variables using wxflow timetools.

        Parameters
        ----------
        config_dict : AttrDict
            Configuration dictionary from Archive.task_config

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
        current_cycle = config_dict.current_cycle
        cycle_HH = current_cycle.strftime("%H")
        cycle_YMDH = to_YMDH(current_cycle)
        cycle_YMD = to_YMD(current_cycle)

        # Build head string (e.g., 'gfs.t00z.')
        head = f"{config_dict.RUN}.t{cycle_HH}z."

        # Archive directory (used by all systems)
        VFYARC = os.path.join(config_dict.ROTDIR, "vrfyarch")

        return {
            'cycle_HH': cycle_HH,
            'cycle_YMDH': cycle_YMDH,
            'cycle_YMD': cycle_YMD,
            'head': head,
            'VFYARC': VFYARC
        }
