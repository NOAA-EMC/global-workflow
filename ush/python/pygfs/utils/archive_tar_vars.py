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
from typing import Any, Dict, Tuple
from wxflow import AttrDict, logit, to_YMD, to_YMDH

logger = getLogger(__name__.split('.')[-1])


class ArchiveTarVars:
    """
    Utility class for collecting archive tar variables.

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
        arch_dict.update(ArchiveTarVars.add_config_vars(config_dict))

        # Add cycle-specific variables
        arch_dict.update(ArchiveTarVars._get_cycle_vars(config_dict))

        logger.info(f"Collected {len(arch_dict)} variables for YAML templates")
        logger.debug(f"arch_dict keys: {list(arch_dict.keys())}")

        return arch_dict

    @staticmethod
    @logit(logger)
    def add_config_vars(config_dict: AttrDict) -> Dict[str, Any]:
        """Collect configuration variables for archive tar operations.

        This method extracts all required configuration keys for EnKF (ensemble)
        archiving operations, including ensemble-specific parameters.

        Parameters
        ----------
        config_dict : AttrDict
            Configuration dictionary from Archive.task_config

        Configuration keys extracted (if present):
        - Basic: ATARDIR, current_cycle, IAUFHRS, RUN, PDY, PSLOT
        - Archive control: DO_ARCHCOM, ARCHCOM_TO, ROTDIR, PARMgfs, ARCDIR, SDATE, MODE
        - Ensemble: ENSGRP, NMEM_EARCGRP, NMEM_ENS, NMEM_ENS_GFS
        - EnKF operations: DO_CALC_INCREMENT_ENKF_GFS, DO_JEDIATMENS, lobsdiag_forenkf
        - Forecast: FHMIN_ENKF, FHMAX_ENKF_GFS, FHOUT_ENKF_GFS, FHMAX_ENKF, FHOUT_ENKF
        - EnKF settings: ENKF_SPREAD, DOIAU_ENKF, IAU_OFFSET, IAUFHRS_ENKF
        - Restart: restart_interval_enkfgdas, restart_interval_enkfgfs
        - Hybrid/DA: DOHYBVAR, DOIAU, DO_CA, DO_CALC_INCREMENT, assim_freq
        - Archive timing: ARCH_CYC, ARCH_WARMICFREQ, ARCH_FCSTICFREQ
        - Ocean/Ice: DOHYBVAR_OCN, DOLETKF_OCN
        - Other: DO_JEDISNOWDA, NET, DO_GSISOILDA, DO_LAND_IAU

        COM variable prefixes collected:
        - COM_, COMIN_, COMOUT_

        Returns
        -------
        Dict[str, Any]
            Dictionary containing all EnKF archive variables

        Notes
        -----
        Missing keys will be silently skipped (not added to enkf_dict).
        This method should be used for EnKF-specific archiving (enkfgdas, enkfgfs).
        """
        enkf_dict = {}

        # Configuration keys for EnKF archiving
        config_keys = [
            # Basic configuration
            'ATARDIR', 'current_cycle', 'IAUFHRS', 'RUN', 'PDY', 'PSLOT',
            # Archive control
            'DO_ARCHCOM', 'ARCHCOM_TO', 'ROTDIR', 'PARMgfs', 'ARCDIR', 'SDATE', 'MODE',
            # Ensemble configuration
            'ENSGRP', 'NMEM_EARCGRP', 'NMEM_ENS', 'NMEM_ENS_GFS',
            # EnKF-specific operations
            'DO_CALC_INCREMENT_ENKF_GFS', 'DO_JEDIATMENS', 'lobsdiag_forenkf',
            # Forecast configuration
            'FHMIN_ENKF', 'FHMAX_ENKF_GFS', 'FHOUT_ENKF_GFS', 'FHMAX_ENKF', 'FHOUT_ENKF',
            # EnKF settings
            'ENKF_SPREAD', 'DOIAU_ENKF', 'IAU_OFFSET', 'IAUFHRS_ENKF',
            # Restart intervals
            'restart_interval_enkfgdas', 'restart_interval_enkfgfs',
            # Hybrid and data assimilation
            'DOHYBVAR', 'DOIAU', 'DO_CA', 'DO_CALC_INCREMENT', 'assim_freq',
            # Archive timing
            'ARCH_CYC', 'ARCH_WARMICFREQ', 'ARCH_FCSTICFREQ',
            # Ocean and ice DA
            'DOHYBVAR_OCN', 'DOLETKF_OCN',
            # Other
            'DO_JEDISNOWDA', 'NET', 'DO_GSISOILDA', 'DO_LAND_IAU'
        ]

        # Extract keys if they exist in config_dict
        for key in config_keys:
            if key in config_dict:
                enkf_dict[key] = config_dict[key]
            else:
                logger.warning(f"Config key '{key}' not found in config_dict; skipping.")

        # Import COM* directory and template variables created by job scripts
        for key in config_dict.keys():
            if key.startswith(("COM_", "COMIN_", "COMOUT_")):
                enkf_dict[key] = config_dict.get(key)

        logger.info(f"Collected {len(enkf_dict)} archive tar variables")
        logger.debug(f"Archive variables: {list(enkf_dict.keys())}")

        return enkf_dict

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

        return {
            'cycle_HH': cycle_HH,
            'cycle_YMDH': cycle_YMDH,
            'cycle_YMD': cycle_YMD,
        }

    @staticmethod
    def _replace_template_vars(template: str, var_dict: Dict[str, Any]) -> str:
        """Replace template variables in string with actual values

        Parameters
        ----------
        template : str
            Template string with variables to replace
        var_dict : Dict[str, Any]
            Dictionary of variable names and values

        Returns
        -------
        str
            String with variables replaced
        """
        replace_com = template
        for var, value in var_dict.items():
            replace_com = replace_com.replace(var, value)
        return replace_com

    @staticmethod
    @logit(logger)
    def _create_cycle_dicts(config_dict: AttrDict) -> Dict[str, Dict[str, str]]:
        """Create cycle directories for template substitution

        Parameters
        ----------
        rotdir : str
            ROTDIR path
        run : str
            RUN type

        Returns
        -------
        Dict[str, Dict[str, str]]
            Dictionary containing current_cycle_dict and previous_cycle_dict
        """
        return {
            'temp_dict': {
                '${ROTDIR}': config_dict['ROTDIR'],
                '${RUN}': config_dict['RUN'],
                '${YMD}': to_YMD(config_dict['current_cycle']),
                '${HH}': config_dict['current_cycle'].strftime("%H"),
            }
        }

    @staticmethod
    @logit(logger)
    def get_member_com_paths(config_dict: AttrDict, member: int) -> Dict[str, str]:
        """Get member-specific COM paths for a single ensemble member.

        This method generates COM paths for ONE ensemble member at a time.
        The driver script should loop over members and call this method for each.

        Parameters
        ----------
        config_dict : AttrDict
            Configuration dictionary from Archive.task_config
        member : int
            Member number (e.g., 1, 2, 3, ..., NMEM_ENS)

        Returns
        -------
        Dict[str, str]
            Dictionary of member-specific COM paths for this single member

        Notes
        -----
        This follows the stage_ic.py pattern where the loop is external to the
        utility module. The driver script loops over members and calls this
        method for each member individually.
        """
        # Create cycle dictionary and add member string
        cycle_dict = ArchiveTarVars._create_cycle_dicts(config_dict)['temp_dict']
        cycle_dict['${MEMDIR}'] = f"mem{member:03d}"

        # Define template mappings for member COM paths
        template_mappings = [
            ('COMIN_ATMOS_ANALYSIS_MEM', 'COM_ATMOS_ANALYSIS_TMPL'),
            ('COMIN_ATMOS_HISTORY_MEM', 'COM_ATMOS_HISTORY_TMPL'),
            ('COMIN_ATMOS_RESTART_MEM', 'COM_ATMOS_RESTART_TMPL'),
            ('COMIN_OCEAN_ANALYSIS_MEM', 'COM_OCEAN_ANALYSIS_TMPL'),
            ('COMIN_OCEAN_LETKF_MEM', 'COM_OCEAN_LETKF_TMPL'),
            ('COMIN_OCEAN_HISTORY_MEM', 'COM_OCEAN_HISTORY_TMPL'),
            ('COMIN_OCEAN_RESTART_MEM', 'COM_OCEAN_RESTART_TMPL'),
            ('COMIN_ICE_ANALYSIS_MEM', 'COM_ICE_ANALYSIS_TMPL'),
            ('COMIN_ICE_LETKF_MEM', 'COM_ICE_LETKF_TMPL'),
            ('COMIN_ICE_HISTORY_MEM', 'COM_ICE_HISTORY_TMPL'),
            ('COMIN_ICE_RESTART_MEM', 'COM_ICE_RESTART_TMPL'),
            ('COMIN_MED_RESTART_MEM', 'COM_MED_RESTART_TMPL'),
        ]

        # Generate COM paths using template substitution
        member_com_paths = {
            com_key: ArchiveTarVars._replace_template_vars(config_dict.get(template_key, ""), cycle_dict)
            for com_key, template_key in template_mappings
            if config_dict.get(template_key)
        }

        # Add ensstat path (COMIN_CONF)
        cycle_dict['${MEMDIR}'] = 'ensstat'
        if config_dict.get('COM_CONF_TMPL'):
            member_com_paths['COMIN_CONF'] = ArchiveTarVars._replace_template_vars(
                config_dict['COM_CONF_TMPL'], cycle_dict
            )

        logger.debug(f"Generated COM paths for member {member}")
        return member_com_paths
