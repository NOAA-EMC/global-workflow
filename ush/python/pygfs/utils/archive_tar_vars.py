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
from wxflow import AttrDict, logit, to_YMD, to_YMDH, add_to_datetime, to_timedelta

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

        # Add YAML-specific cycle variables (analysis/restart times, archive flags)
        arch_dict.update(ArchiveTarVars._get_yaml_specific_cyc_vars(config_dict))

        # Add COM paths based on ensemble group (all relative to ROTDIR)
        ensgrp = config_dict.get('ENSGRP', 0)
        if ensgrp == 0:
            # ENSGRP=0: Generate relative paths for ensemble mean/spread (enkf.yaml.j2)
            arch_dict.update(ArchiveTarVars.get_enkf_ensstat_com_paths(config_dict))
        else:
            # ENSGRP!=0: Generate relative paths for member data
            arch_dict.update(ArchiveTarVars.get_enkf_member_com_paths(config_dict))

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
    def _get_yaml_specific_cyc_vars(config_dict: AttrDict) -> Dict[str, Any]:
        """Compute YAML-specific cycle variables used by master_enkf.yaml.

        This method computes EnKF-specific cycle variables including analysis/restart
        times, assimilation frequency, and archive timing booleans etc.

        Parameters
        ----------
        config_dict : AttrDict
            Configuration dictionary from Archive.task_config
            Required keys: current_cycle, assim_freq, SDATE, ARCH_CYC, ARCH_WARMICFREQ
            Optional keys: DOIAU_ENKF (default: False), ENSGRP (default: 0),
                          NMEM_EARCGRP, NMEM_ENS

        Returns
        -------
        Dict[str, Any]
            Dictionary containing cycle variables
        """
        # helpers already imported at module level
        current_cycle = config_dict.current_cycle
        doiau_enkf = config_dict.get('DOIAU_ENKF', False)
        assim_freq = config_dict.get('assim_freq', 6)
        sdate = config_dict.get('SDATE')
        arch_warmicfreq = config_dict.get('ARCH_WARMICFREQ', 1)
        arch_cyc = config_dict.get('ARCH_CYC', 0)

        vars_out: Dict[str, Any] = {}
        # Basic cycle variables
        vars_out['cycle_HH'] = current_cycle.strftime("%H")
        vars_out['cycle_YMDH'] = to_YMDH(current_cycle)
        vars_out['cycle_YMD'] = to_YMD(current_cycle)
        # Analysis time
        anl_delta = to_timedelta("-3H") if doiau_enkf else to_timedelta("0H")
        anl_time = add_to_datetime(current_cycle, anl_delta)
        vars_out['anl_YMD'] = to_YMD(anl_time)
        vars_out['anl_HH'] = anl_time.strftime("%H")

        # Restart time
        rst_delta = to_timedelta("+3H") if doiau_enkf else to_timedelta("+6H")
        rst_time = add_to_datetime(current_cycle, rst_delta)
        vars_out['rst_YMD'] = to_YMD(rst_time)
        vars_out['rst_HH'] = rst_time.strftime("%H")

        # Assimilation frequency
        vars_out['assim_freq'] = str(assim_freq)

        # Archive timing booleans - increments (group a)
        if sdate:
            current_cycle_days = (current_cycle - sdate).days
            vars_out['archive_increments'] = (current_cycle_days % arch_warmicfreq == 0)
        else:
            vars_out['archive_increments'] = False
        vars_out['archive_at_cyc'] = (arch_cyc == int(current_cycle.strftime("%H")))

        # Archive timing booleans - ICs (group b)
        ics_offset_cycle = add_to_datetime(current_cycle, to_timedelta(f"+{assim_freq}H"))
        if sdate:
            ics_offset_days = (ics_offset_cycle - sdate).days
            vars_out['archive_ics'] = (ics_offset_days % arch_warmicfreq == 0)
        else:
            vars_out['archive_ics'] = False
        vars_out['archive_ics_at_cyc'] = ((arch_cyc - assim_freq) % 24 == int(current_cycle.strftime("%H")))

        # Warm start flags (placeholders)
        vars_out['save_warm_start_forecast'] = False
        vars_out['save_warm_start_cycled'] = False

        # Ensemble member range calculation for archiving groups
        vars_out['first_group_mem'] = None
        vars_out['last_group_mem'] = None

        ensgrp = config_dict.get('ENSGRP', 0)
        if ensgrp != 0:
            nmem_earcgrp = config_dict.get('NMEM_EARCGRP')
            nmem_ens = config_dict.get('NMEM_ENS')
            if nmem_earcgrp and nmem_ens:
                vars_out['first_group_mem'] = (ensgrp - 1) * nmem_earcgrp + 1
                vars_out['last_group_mem'] = min(ensgrp * nmem_earcgrp, nmem_ens)

        # Forecast hour calculations for archive templates
        # Used by enkf.yaml.j2 for log archiving loops (epos post-processing groups)
        fhmin_enkf = config_dict.get('FHMIN_ENKF', 0)
        fhmax_enkf = config_dict.get('FHMAX_ENKF', 0)
        fhout_enkf = config_dict.get('FHOUT_ENKF', 3)

        # Generate forecast hour list: range(fhmin, fhmax + fhout, fhout)
        # Example: fhmin=0, fhmax=9, fhout=3 -> [0, 3, 6, 9]
        enkf_fhrs = list(range(fhmin_enkf, fhmax_enkf + fhout_enkf, fhout_enkf))
        vars_out['enkf_epos_fhrs'] = enkf_fhrs
        vars_out['enkf_epos_ngrps'] = len(enkf_fhrs)

        return vars_out

    @staticmethod
    @logit(logger)
    def _replace_template_vars(template: str, var_dict: Dict[str, Any], rotdir: str) -> str:
        """Replace template variables and return a path relative to ROTDIR.

        Parameters
        ----------
        template : str
            Template string with variables to replace (e.g., "${ROTDIR}/${RUN}.${YMD}/${HH}")
        var_dict : Dict[str, Any]
            Dictionary of variable names and values
        rotdir : str
            Absolute ROTDIR used to strip from generated paths to create
            relative paths.

        Returns
        -------
        str
            Path relative to ROTDIR for portability in tar archives
        """
        # First replace all template variables
        replace_com = template
        for var, value in var_dict.items():
            replace_com = replace_com.replace(var, value)

        # Then strip ROTDIR prefix to make path relative
        rotdir_prefix = rotdir if rotdir.endswith(os.sep) else rotdir + os.sep
        return replace_com.replace(rotdir_prefix, "") if rotdir_prefix in replace_com else replace_com

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
                '${ROTDIR}': config_dict['ROTDIR'],
                '${RUN}': config_dict['RUN'],
                '${YMD}': to_YMD(config_dict['current_cycle']),
                '${HH}': config_dict['current_cycle'].strftime("%H"),
        }

    @staticmethod
    @logit(logger)
    def get_enkf_member_com_paths(config_dict: AttrDict) -> Dict[str, Any]:
        """Generate relative COM paths for ensemble group members (ENSGRP != 0).

        This method creates relative COM paths (relative to ROTDIR) for ensemble members
        in a group. It calculates the member range for the specified group and returns
        lists of relative COM paths for all members, plus the ensstat path.

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
        This method should ONLY be called when ENSGRP != 0 (archiving individual members).
        All returned paths are relative to ROTDIR (e.g., 'enkfgdas.20231215/00/atmos/mem001').
        These relative paths are portable and suitable for tar archive contents.
        """
        ensgrp = config_dict.get('ENSGRP', 0)

        # Only create member COM paths when ENSGRP != 0 (archiving individual members)
        if ensgrp == 0:
            return {}

        # Get member range by calling _get_yaml_specific_cyc_vars
        yaml_vars = ArchiveTarVars._get_yaml_specific_cyc_vars(config_dict)
        first_group_mem = yaml_vars.get('first_group_mem')
        last_group_mem = yaml_vars.get('last_group_mem')

        if first_group_mem is None or last_group_mem is None:
            raise ValueError("first_group_mem and last_group_mem should be defined in _get_yaml_specific_cyc_vars()")

        logger.info(f"Processing relative paths for ensemble group {ensgrp}: members {first_group_mem} to {last_group_mem}")

        # Define template mappings (list key -> template key)
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

        # Initialize relative member lists
        member_lists = {list_key: [] for list_key, _ in template_mappings}

        for mem in range(first_group_mem, last_group_mem + 1):
            # Create member-specific cycle dictionary
            cycle_dict = ArchiveTarVars._create_cycle_dicts(config_dict)
            cycle_dict['${MEMDIR}'] = f"mem{mem:03d}"

            # Generate relative COM paths for this member
            for list_key, template_key in template_mappings:
                if config_dict.get(template_key):
                    rel_path = ArchiveTarVars._replace_template_vars(
                        config_dict[template_key], cycle_dict, config_dict.ROTDIR
                    )
                    member_lists[list_key].append(rel_path)

        # Add ensstat path (COMIN_CONF)
        # Note: COMIN_CONF is a single path string, not a list like the other entries
        cycle_dict = ArchiveTarVars._create_cycle_dicts(config_dict)
        cycle_dict['${MEMDIR}'] = 'ensstat'
        if config_dict.get('COM_CONF_TMPL'):
            rel_ensstat = ArchiveTarVars._replace_template_vars(
                config_dict['COM_CONF_TMPL'], cycle_dict, config_dict.ROTDIR
            )
            member_lists['COMIN_CONF'] = rel_ensstat  # type: ignore[assignment]

        logger.debug(f"Generated {len(member_lists)} relative COM path lists for group {ensgrp} ({last_group_mem - first_group_mem + 1} members)")
        return member_lists

    @staticmethod
    @logit(logger)
    def get_enkf_ensstat_com_paths(config_dict: AttrDict) -> Dict[str, str]:
        """Generate relative COMIN paths for EnKF ensemble mean/spread (ENSGRP=0).

        This method creates relative COM paths from absolute paths already defined
        in config_dict by the job scripts. If a COMIN_* or COMOUT_* variable exists,
        it will be converted to a relative path (relative to ROTDIR).

        Parameters
        ----------
        config_dict : AttrDict
            Configuration dictionary from Archive.task_config
            Required keys: ROTDIR
            Optional keys: COMIN_*, COMOUT_* variables (created by job scripts)

        Returns
        -------
        Dict[str, str]
            Dictionary with relative COMIN paths for ensemble statistics:
            - Keys match enkf.yaml.j2 template variable names
            - Values are paths relative to ROTDIR for portability

        Notes
        -----
        This method should ONLY be called when ENSGRP == 0 (ensemble mean archiving).
        For individual member archiving (ENSGRP != 0), use get_enkf_member_com_paths().

        All paths are relative to ROTDIR for portability in tar archives.

        Examples
        --------
        >>> # Job script creates: COMIN_ATMOS_HISTORY_ENSSTAT=/path/to/ROTDIR/enkfgdas.20211221/00/atmos/ensstat
        >>> ensstat_paths = ArchiveTarVars.get_enkf_ensstat_com_paths(config)
        >>> ensstat_paths['COMIN_ATMOS_HISTORY_ENSSTAT']
        'enkfgdas.20211221/00/atmos/ensstat'
        """
        ensstat_paths = {}
        rotdir = config_dict.get('ROTDIR', '')

        # Normalize ROTDIR with trailing slash for clean prefix removal
        rotdir_prefix = rotdir if rotdir.endswith(os.sep) else rotdir + os.sep

        # List of COMIN/COMOUT variables to convert to relative paths
        # These are created by the job scripts via declare_from_tmpl
        com_vars = [
            'COMIN_ATMOS_HISTORY',
            'COMIN_ATMOS_HISTORY_ENSSTAT',
            'COMIN_ATMOS_ANALYSIS_ENSSTAT',
            'COMIN_SNOW_ANALYSIS_ENSSTAT',
            'COMIN_OCEAN_ANALYSIS_ENSSTAT',
            'COMIN_ICE_ANALYSIS_ENSSTAT',
            'COMIN_CONF',
        ]

        # Convert absolute paths to relative paths
        for var_name in com_vars:
            if var_name in config_dict:
                abs_path = config_dict[var_name]
                # Strip ROTDIR prefix to create relative path
                rel_path = abs_path.replace(rotdir_prefix, '') if rotdir_prefix in abs_path else abs_path
                ensstat_paths[var_name] = rel_path
                logger.debug(f"Converted {var_name}: {abs_path} -> {rel_path}")

        logger.info(f"Generated {len(ensstat_paths)} relative ensemble statistics COM paths")
        return ensstat_paths
