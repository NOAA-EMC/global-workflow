#!/usr/bin/env python3
"""
Archive Variables Utility Module

Overview
--------
This module provides utility functions to collect variables needed by YAML templates
for archiving verification (vrfy) data for GFS and GEFS systems. File set
generation logic (loops, conditionals, path construction) is handled by the YAML
templates themselves.

Architecture
------------
Python provides VARIABLES -> YAML templates build FILE SETS

Python Code Responsibilities:
  - Compute cycle-specific variables (cycle_HH, cycle_YMDH, cycle_YMD, head)
  - Calculate COM directory paths with grid loops (0p25, 0p50, 1p00)
  - Extract configuration keys (RUN, DO_* flags, FHMAX*, etc.)
  - Provide complete arch_dict to YAML templates

YAML Template Responsibilities (parm/archive/*_arcdir.yaml.j2):
  - Build file sets with source -> destination mappings
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
    def get_all_yaml_vars(config_dict: AttrDict) -> AttrDict:
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
        arch_dict = AttrDict()

        # Add config variables (config keys, COM* variables from job scripts)
        arch_dict.update(ArchiveTarVars.add_config_vars(config_dict))

        # Add cycle-specific variables
        arch_dict.update(ArchiveTarVars._get_cycle_vars(config_dict))

        # Add member COM paths for ensemble groups (ENSGRP != 0)
        # Returns empty dict if ENSGRP == 0 (ensemble mean archiving)
        arch_dict.update(ArchiveTarVars.get_member_com_paths(config_dict))

        logger.info(f"Collected {len(arch_dict)} variables for YAML templates")
        logger.debug(f"arch_dict keys: {list(arch_dict.keys())}")

        return arch_dict

    @staticmethod
    @logit(logger)
    def add_config_vars(config_dict: AttrDict) -> AttrDict:
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
        enkf_dict = AttrDict()

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
            - cycle_HH: (str) Cycle hour (e.g., '00', '06')
            - cycle_YMDH: (str) Full cycle timestamp (YYYYMMDDHH)
            - cycle_YMD: (str) Cycle date (YYYYMMDD)
            - anl_YMD: (str) Analysis date (depends on DOIAU_ENKF)
            - anl_HH: (str) Analysis hour (depends on DOIAU_ENKF)
            - rst_YMD: (str) Restart date (depends on DOIAU_ENKF)
            - rst_HH: (str) Restart hour (depends on DOIAU_ENKF)
            - assim_freq: (str) Assimilation frequency in hours
            - archive_increments: (bool) Whether to archive ensemble increments (group a)
            - archive_at_cyc: (bool) Whether current cycle hour matches ARCH_CYC
            - archive_ics: (bool) Whether to archive ensemble ICs (group b)
            - archive_ics_at_cyc: (bool) Whether IC offset hour matches archive cycle
        """
        from wxflow import add_to_datetime, to_timedelta

        # Initialize general cycle variables dictionary using AttrDict for dot notation access
        general_cycle_vars = AttrDict()

        current_cycle = config_dict.current_cycle
        doiau_enkf = config_dict.get('DOIAU_ENKF', False)
        assim_freq = config_dict.get('assim_freq', 6)
        sdate = config_dict.get('SDATE')
        arch_warmicfreq = config_dict.get('ARCH_WARMICFREQ', 1)
        arch_cyc = config_dict.get('ARCH_CYC', 0)

        # Cycle time variables
        general_cycle_vars['cycle_HH'] = current_cycle.strftime("%H")
        general_cycle_vars['cycle_YMDH'] = to_YMDH(current_cycle)
        general_cycle_vars['cycle_YMD'] = to_YMD(current_cycle)

        # Analysis time (depends on DOIAU_ENKF: -3H if IAU on, else current cycle)
        anl_delta = to_timedelta("-3H") if doiau_enkf else to_timedelta("0H")
        anl_time = add_to_datetime(current_cycle, anl_delta)
        general_cycle_vars['anl_YMD'] = to_YMD(anl_time)
        general_cycle_vars['anl_HH'] = anl_time.strftime("%H")

        # Restart time (depends on DOIAU_ENKF: +3H if IAU on, else +6H)
        rst_delta = to_timedelta("+3H") if doiau_enkf else to_timedelta("+6H")
        rst_time = add_to_datetime(current_cycle, rst_delta)
        general_cycle_vars['rst_YMD'] = to_YMD(rst_time)
        general_cycle_vars['rst_HH'] = rst_time.strftime("%H")

        # Assimilation frequency
        general_cycle_vars['assim_freq'] = str(assim_freq)

        # Archive timing booleans - increments (group a)
        if sdate:
            current_cycle_days = (current_cycle - sdate).days
            general_cycle_vars['archive_increments'] = (current_cycle_days % arch_warmicfreq == 0)
        else:
            general_cycle_vars['archive_increments'] = False
        general_cycle_vars['archive_at_cyc'] = (arch_cyc == int(general_cycle_vars['cycle_HH']))

        # Archive timing booleans - ICs (group b)
        ics_offset_cycle = add_to_datetime(current_cycle, to_timedelta(f"+{assim_freq}H"))
        if sdate:
            ics_offset_days = (ics_offset_cycle - sdate).days
            general_cycle_vars['archive_ics'] = (ics_offset_days % arch_warmicfreq == 0)
        else:
            general_cycle_vars['archive_ics'] = False
        general_cycle_vars['archive_ics_at_cyc'] = ((arch_cyc - assim_freq) % 24 == int(general_cycle_vars['cycle_HH']))

        # Warm start flags (currently placeholders)
        general_cycle_vars['save_warm_start_forecast'] = False
        general_cycle_vars['save_warm_start_cycled'] = False

        return general_cycle_vars

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
    def get_member_com_paths(config_dict: AttrDict) -> Dict[str, Any]:
        """Get member-specific COM paths (relative to ROTDIR) for ensemble group members.

        This method generates relative COM paths for ensemble members in a group (ENSGRP != 0).
        It calculates the member range for the specified group and returns lists of
        relative COM paths for all members in that group, plus the ensstat path.

        Parameters
        ----------
        config_dict : AttrDict
            Configuration dictionary from Archive.task_config
            Required keys: ENSGRP, NMEM_EARCGRP, NMEM_ENS, ROTDIR, COM_*_TMPL variables

        Returns
        -------
        Dict[str, Any]
            Dictionary with:
            - COMIN_*_MEM_list: Lists of relative COM paths for all members in group
            - COMIN_CONF: Ensstat COM path (single string, relative to ROTDIR)
            All paths are relative to ROTDIR for portability.

        Raises
        ------
        ValueError
            If NMEM_EARCGRP or NMEM_ENS are missing when ENSGRP != 0

        Notes
        -----
        This method is only called when ENSGRP != 0 (archiving individual member data).
        When ENSGRP == 0 (archiving ensemble means/spreads), member COM paths are not
        needed and an empty dict is returned.

        All returned paths are relative to ROTDIR (e.g., 'enkfgdas.20231215/00/atmos/mem001')
        rather than absolute paths (e.g., '/scratch/.../ROTDIR/enkfgdas.20231215/00/atmos/mem001').
        """
        ensgrp = config_dict.get('ENSGRP', 0)

        # Only create member COM paths when ENSGRP != 0 (archiving individual members)
        if ensgrp == 0:
            return {}
        else:
            # Create lists of member paths for the group
            nmem_earcgrp = config_dict.get('NMEM_EARCGRP')
            nmem_ens = config_dict.get('NMEM_ENS')

            if nmem_earcgrp is None or nmem_ens is None:
                raise ValueError("NMEM_EARCGRP and NMEM_ENS required when ENSGRP != 0")

            # Determine which members belong to this group
            first_group_mem = (ensgrp - 1) * nmem_earcgrp + 1
            last_group_mem = min(ensgrp * nmem_earcgrp, nmem_ens)

            logger.info(f"Processing ensemble group {ensgrp}: members {first_group_mem} to {last_group_mem}")

            # Define template mappings (list key -> template key) and initialize empty lists
            template_mappings = [
                ('COMIN_ATMOS_ANALYSIS_MEM_list', 'COM_ATMOS_ANALYSIS_TMPL'),
                ('COMIN_ATMOS_HISTORY_MEM_list', 'COM_ATMOS_HISTORY_TMPL'),
                ('COMIN_ATMOS_RESTART_MEM_list', 'COM_ATMOS_RESTART_TMPL'),
                ('COMIN_OCEAN_ANALYSIS_MEM_list', 'COM_OCEAN_ANALYSIS_TMPL'),
                ('COMIN_OCEAN_LETKF_MEM_list', 'COM_OCEAN_LETKF_TMPL'),
                ('COMIN_OCEAN_HISTORY_MEM_list', 'COM_OCEAN_HISTORY_TMPL'),
                ('COMIN_OCEAN_RESTART_MEM_list', 'COM_OCEAN_RESTART_TMPL'),
                ('COMIN_ICE_ANALYSIS_MEM_list', 'COM_ICE_ANALYSIS_TMPL'),
                ('COMIN_ICE_LETKF_MEM_list', 'COM_ICE_LETKF_TMPL'),
                ('COMIN_ICE_HISTORY_MEM_list', 'COM_ICE_HISTORY_TMPL'),
                ('COMIN_ICE_RESTART_MEM_list', 'COM_ICE_RESTART_TMPL'),
                ('COMIN_MED_RESTART_MEM_list', 'COM_MED_RESTART_TMPL'),
            ]

            # Initialize member lists from template mappings
            member_lists = {list_key: [] for list_key, _ in template_mappings}

            # Get ROTDIR for converting absolute paths to relative paths
            rotdir = config_dict.ROTDIR + os.sep

            for mem in range(first_group_mem, last_group_mem + 1):
                # Create member-specific cycle dictionary
                cycle_dict = ArchiveTarVars._create_cycle_dicts(config_dict)['temp_dict']
                cycle_dict['${MEMDIR}'] = f"mem{mem:03d}"

                # Generate COM paths for this member and append to lists
                for list_key, template_key in template_mappings:
                    if config_dict.get(template_key):
                        com_path = ArchiveTarVars._replace_template_vars(
                            config_dict[template_key], cycle_dict
                        )
                        # Convert to relative path (remove ROTDIR prefix)
                        rel_path = com_path.replace(rotdir, "") if rotdir in com_path else com_path
                        member_lists[list_key].append(rel_path)

            # Add ensstat path (COMIN_CONF)
            # Note: COMIN_CONF is a single path string, not a list like the other entries
            cycle_dict = ArchiveTarVars._create_cycle_dicts(config_dict)['temp_dict']
            cycle_dict['${MEMDIR}'] = 'ensstat'
            if config_dict.get('COM_CONF_TMPL'):
                ensstat_path = ArchiveTarVars._replace_template_vars(
                    config_dict['COM_CONF_TMPL'], cycle_dict
                )
                # Convert to relative path (remove ROTDIR prefix)
                member_lists['COMIN_CONF'] = ensstat_path.replace(rotdir, "") if rotdir in ensstat_path else ensstat_path  # type: ignore[assignment]

            logger.debug(f"Generated relative COM path lists for group {ensgrp} ({last_group_mem - first_group_mem + 1} members)")
            return member_lists
