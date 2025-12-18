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
        """
        Collect all variables needed for YAML templates.

        This method provides only the VARIABLES needed by the YAML templates
        (cycle vars, COM paths, config keys). The YAML templates handle all
        file set generation logic (loops, conditionals, path construction).

        Parameters
        ----------
        config_dict : AttrDict
            Configuration dictionary from Archive.task_config

        Returns
        -------
        AttrDict
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

        if config_dict.get('RUN') in ['enkfgfs', 'enkfgdas']:
            # EnKF systems: Handle ensemble member-specific paths
            ensgrp = config_dict.get('ENSGRP', 0)
            if ensgrp == 0:
                # ENSGRP=0: Ensemble mean/spread (enkf.yaml.j2)
                arch_dict.update(ArchiveTarVars.get_enkf_com_paths(config_dict))
            else:
                # ENSGRP>0: Individual member groups
                arch_dict.update(ArchiveTarVars._create_mem_com_sets(
                    config_dict,
                    arch_dict['first_group_mem'],
                    arch_dict['last_group_mem']
                ))
        elif config_dict.get('RUN') == 'gfs':
            # GFS system: Route through tarball-specific method
            tarball_type = config_dict.get('TARBALL_TYPE', '')
            if tarball_type:
                arch_dict.update(ArchiveTarVars.get_gfs_com_paths(config_dict, tarball_type))
        elif config_dict.get('RUN') == 'gdas':
            # GDAS system: Route through tarball-specific method
            tarball_type = config_dict.get('TARBALL_TYPE', '')
            if tarball_type:
                arch_dict.update(ArchiveTarVars.get_gdas_com_paths(config_dict, tarball_type))
        elif config_dict.get('RUN') == 'gcafs':
            # GCAFS system: All COM paths
            arch_dict.update(ArchiveTarVars.get_gcafs_com_paths(config_dict))
        elif config_dict.get('RUN') == 'gcdas':
            # GCDAS system: All COM paths
            arch_dict.update(ArchiveTarVars.get_gcdas_com_paths(config_dict))
        else:
            logger.warning(f"Unknown RUN type '{config_dict.get('RUN')}', no COM paths added")
        logger.info(f"Collected {len(arch_dict)} variables for YAML templates")
        logger.debug(f"arch_dict keys: {list(arch_dict.keys())}")

        return arch_dict

    @staticmethod
    @logit(logger)
    def add_config_vars(config_dict: AttrDict) -> AttrDict:

        """
        Collect configuration variables for archive tar operations.

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
        AttrDict
            Dictionary containing all archive variables

        Notes
        -----
        Missing keys will be silently skipped (not added to config_dict).
        This method is used for all archive operations (GFS, GDAS, EnKF, etc.).
        """
        config_vars = AttrDict()

        # Common configuration keys (present in both exglobal_enkf_earc_tars.py and exglobal_archive_tars.py)
        config_keys = [
            # Basic configuration
            'ATARDIR', 'current_cycle', 'RUN', 'PDY', 'PSLOT',
            # Archive control
            'DO_ARCHCOM', 'ARCHCOM_TO', 'ROTDIR', 'PARMgfs', 'ARCDIR', 'SDATE', 'MODE',
            # Data assimilation
            'DOHYBVAR', 'DOIAU', 'DO_CA', 'assim_freq', 'IAUFHRS',
            'DO_JEDISNOWDA', 'DO_GSISOILDA', 'DO_LAND_IAU',
            # Ocean/Ice DA
            'DOHYBVAR_OCN', 'DOLETKF_OCN', 'NMEM_ENS',
            # Archive timing and control
            'ARCH_CYC', 'ARCH_WARMICFREQ', 'ARCH_FCSTICFREQ',
            # Other
            'NET',
        ]

        # Add system-specific keys based on RUN type
        if 'enkf' in config_dict.get('RUN', ''):
            # EnKF-specific keys (only in exglobal_enkf_earc_tars.py)
            config_keys.extend([
                # Ensemble configuration
                'ENSGRP', 'NMEM_EARCGRP', 'NMEM_ENS_GFS',
                # EnKF-specific operations
                'DO_CALC_INCREMENT_ENKF_GFS', 'DO_JEDIATMENS', 'lobsdiag_forenkf', 'DO_CALC_INCREMENT',
                # EnKF forecast configuration
                'FHMIN_ENKF', 'FHMAX_ENKF_GFS', 'FHOUT_ENKF_GFS', 'FHMAX_ENKF', 'FHOUT_ENKF',
                # EnKF settings
                'ENKF_SPREAD', 'DOIAU_ENKF', 'IAU_OFFSET', 'IAUFHRS_ENKF',
                # EnKF restart intervals
                'restart_interval_enkfgdas', 'restart_interval_enkfgfs',
            ])
        else:
            # Archive-specific keys (only in exglobal_archive_tars.py)
            config_keys.extend([
                # Forecast configuration
                'FHMIN', 'FHMAX', 'FHOUT',
                'FHMIN_GFS', 'FHMAX_GFS', 'FHOUT_GFS', 'FHOUT_HF_GFS', 'FHMAX_HF_GFS',
                'FHOUT_OCN', 'FHOUT_ICE', 'FHOUT_OCN_GFS', 'FHOUT_ICE_GFS',
                'FHOUT_WAV', 'FHOUT_WAV_GFS', 'FHOUT_HF_WAV', 'FHMAX_WAV', 'FHMAX_HF_WAV', 'FHMAX_WAV_GFS',
                # Monitoring and verification
                'DO_VERFRAD', 'DO_VMINMON', 'DO_VERFOZN', 'DO_FIT2OBS', 'FHMAX_FITS',
                # Model components
                'DO_OCN', 'DO_ICE', 'DO_WAVE', 'DO_PREP_OBS_AERO', 'WRITE_DOPOST',
                # Data assimilation
                'DO_JEDIATMVAR', 'DO_JEDIOCNVAR', 'DO_AERO_ANL', 'DO_AERO_FCST', 'ATMINC_GRID',
                # Restart intervals
                'restart_interval_gdas', 'restart_interval_gfs',
                # Archive control
                'ARCH_GAUSSIAN', 'ARCH_GAUSSIAN_FHMAX', 'ARCH_GAUSSIAN_FHINC',
                'ARCH_EXPDIR', 'ARCH_EXPDIR_FREQ', 'ARCH_HASHES', 'ARCH_DIFFS',
                # Grid and resolution
                'OCNRES', 'ICERES', 'waveGRD', 'WAVE_OUT_GRIDS',
                # Other
                'DO_BUFRSND', 'NUM_SND_COLLECTIVES', 'DOBNDPNT_WAVE',
                'OFFSET_START_HOUR', 'EXPDIR', 'EDATE', 'HOMEgfs',
                'DO_GEMPAK', 'DATASETS_YAML', 'TARBALL_TYPE',
            ])

        # Extract keys if they exist in config_dict
        for key in config_keys:
            if key in config_dict:
                config_vars[key] = config_dict[key]
            else:
                logger.debug(f"Config key '{key}' not found in config_dict; skipping.")

        # Import COM* directory and template variables created by job scripts
        for key in config_dict.keys():
            if key.startswith(("COM_", "COMIN_", "COMOUT_")):
                config_vars[key] = config_dict.get(key)

        logger.info(f"Collected {len(config_vars)} archive tar variables")
        logger.debug(f"Archive variables: {list(config_vars.keys())}")

        return config_vars

    @staticmethod
    @logit(logger)
    def _get_yaml_specific_cyc_vars(config_dict: AttrDict) -> AttrDict:
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
        AttrDict
            Dictionary containing cycle variables
        """
        # helpers already imported at module level
        current_cycle = config_dict.current_cycle
        doiau_enkf = config_dict.get('DOIAU_ENKF', False)
        assim_freq = config_dict.get('assim_freq', 6)
        sdate = config_dict.get('SDATE')
        arch_warmicfreq = config_dict.get('ARCH_WARMICFREQ', 1)
        arch_cyc = config_dict.get('ARCH_CYC', 0)

        vars_out = AttrDict()
        # Basic cycle variables
        vars_out['cycle_HH'] = current_cycle.strftime("%H")
        vars_out['cycle_YMDH'] = to_YMDH(current_cycle)
        vars_out['cycle_YMD'] = to_YMD(current_cycle)

        # Analysis time (for surface analysis restart files)
        anl_delta = to_timedelta("-3H") if doiau_enkf else to_timedelta("0H")
        anl_time = add_to_datetime(current_cycle, anl_delta)
        vars_out['anl_YMD'] = to_YMD(anl_time)
        vars_out['anl_HH'] = anl_time.strftime("%H")
        # Pre-computed analysis prefix for use in YAML templates
        vars_out['anl_prefix'] = vars_out['anl_YMD'] + "." + vars_out['anl_HH'] + "0000"

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
        # Only set these variables if RUN contains 'enkf'
        vars_out['nmem_ens'] = config_dict.get('NMEM_ENS', None)
        if 'enkf' in config_dict.get('RUN', ''):
            vars_out['fhmin'] = config_dict.get('FHMIN_ENKF', 0)
            vars_out['fhmax'] = config_dict.get('FHMAX_ENKF', 0)
            vars_out['fhout'] = config_dict.get('FHOUT_ENKF', 3)
            if config_dict.get('RUN', '') == 'enkfgfs':
                vars_out['do_calc_increment'] = config_dict.get('DO_CALC_INCREMENT_ENKF_GFS', False)
                vars_out['nmem_ens'] = config_dict.get('NMEM_ENS_GFS', None)
                vars_out['restart_interval'] = config_dict.get('restart_interval_enkfgfs', None)
                vars_out['is_gdas'] = False
                vars_out['is_gfs'] = True
            elif config_dict.get('RUN', '') == 'enkfgdas':
                vars_out['do_calc_increment'] = config_dict.get('DO_CALC_INCREMENT', False)
                vars_out['restart_interval'] = config_dict.get('restart_interval_enkfgdas', None)
                vars_out['is_gdas'] = True
                vars_out['is_gfs'] = False
            else:
                logger.warning(
                    f"RUN='{config_dict.get('RUN', '')}' does not match a supported EnKF type ('enkfgfs' or 'enkfgdas'). "
                )
            if config_dict.get('ENSGRP') == 0:
                vars_out['enkf_epos_ngrps'] = len(range(vars_out['fhmin'], vars_out['fhmax'] + vars_out['fhout'], vars_out['fhout']))
            else:
                nmem_earcgrp = config_dict.get('NMEM_EARCGRP')
                nmem_ens = vars_out['nmem_ens']
            if nmem_earcgrp and nmem_ens:
                vars_out['first_group_mem'] = (ensgrp - 1) * nmem_earcgrp + 1
                vars_out['last_group_mem'] = min(ensgrp * nmem_earcgrp, nmem_ens)
            # Pre-compute all restart time prefixes for YAML templates
            if vars_out['is_gdas'] and vars_out.get('restart_interval') and vars_out.get('fhmax'):
                vars_out['restart_prefixes'] = []
                for r_time in range(vars_out.get('restart_interval'), vars_out.get('fhmax') + 1, vars_out.get('restart_interval')):
                    r_dt = add_to_datetime(current_cycle, to_timedelta(f"{r_time}H"))
                    vars_out['restart_prefixes'].append(
                        f"{to_YMD(r_dt)}.{r_dt.strftime('%H')}0000"
                    )
            else:
                vars_out['restart_prefixes'] = []
        return vars_out

    @staticmethod
    @logit(logger)
    def get_gfs_com_paths(config_dict: AttrDict, tarball_type: str) -> AttrDict:
        """Generate relative COM paths for GFS tarball archiving.

        System-specific entry point that routes to get_tarball_com_paths with
        the specified tarball type. Used by master_gfs.yaml.j2 template.

        Parameters
        ----------
        config_dict : AttrDict
            Configuration dictionary with ROTDIR and COMIN_* variables
        tarball_type : str
            Type of GFS tarball (from TARBALL_TYPE variable in template)

        Returns
        -------
        AttrDict
            Dictionary with relative COM paths for the specified GFS tarball
        """
        return ArchiveTarVars.get_tarball_com_paths(config_dict, tarball_type, 'gfs')

    @staticmethod
    @logit(logger)
    def get_gdas_com_paths(config_dict: AttrDict, tarball_type: str) -> AttrDict:
        """Generate relative COM paths for GDAS tarball archiving.

        System-specific entry point that routes to get_tarball_com_paths with
        the specified tarball type. Used by master_gdas.yaml.j2 template.

        Parameters
        ----------
        config_dict : AttrDict
            Configuration dictionary with ROTDIR and COMIN_* variables
        tarball_type : str
            Type of GDAS tarball (from TARBALL_TYPE variable in template)

        Returns
        -------
        AttrDict
            Dictionary with relative COM paths for the specified GDAS tarball
        """
        return ArchiveTarVars.get_tarball_com_paths(config_dict, tarball_type, 'gdas')

    @staticmethod
    @logit(logger)
    def get_gcafs_com_paths(config_dict: AttrDict) -> AttrDict:
        """Generate relative COM paths for GCAFS archiving.

        Parameters
        ----------
        config_dict : AttrDict
            Configuration dictionary with ROTDIR and COMIN_* variables

        Returns
        -------
        AttrDict
            Dictionary with relative COM paths for GCAFS archiving
        """
        com_vars = [
            'COMIN_ATMOS_HISTORY',
            'COMIN_ATMOS_RESTART',
            'COMIN_ATMOS_ANALYSIS',
            'COMIN_ATMOS_INPUT',
            'COMIN_ATMOS_GOES',
            'COMIN_ATMOS_GEMPAK',
            'COMIN_ATMOS_GENESIS',
            'COMIN_ATMOS_BUFR',
            'COMIN_ATMOS_WAFS',
            'COMIN_ATMOS_TRACK',
            'COMIN_ATMOS_WMO',
            'COMIN_CHEM_HISTORY',
            'COMIN_WAVE_HISTORY',
            'COMIN_WAVE_PREP',
            'COMIN_WAVE_GRID',
            'COMIN_WAVE_STATION',
            'COMIN_WAVE_GEMPAK',
            'COMIN_OCEAN_HISTORY',
            'COMIN_OCEAN_RESTART',
            'COMIN_OCEAN_INPUT',
            'COMIN_OCEAN_ANALYSIS',
            'COMIN_ICE_HISTORY',
            'COMIN_ICE_RESTART',
            'COMIN_ICE_INPUT',
            'COMIN_ICE_ANALYSIS',
            'COMIN_MED_RESTART',
            'COMIN_CONF',
        ]
        return ArchiveTarVars._create_relative_com_paths(config_dict, com_vars)

    @staticmethod
    @logit(logger)
    def get_gcdas_com_paths(config_dict: AttrDict) -> AttrDict:
        """Generate relative COM paths for GCDAS archiving.

        Parameters
        ----------
        config_dict : AttrDict
            Configuration dictionary with ROTDIR and COMIN_* variables

        Returns
        -------
        AttrDict
            Dictionary with relative COM paths for GCDAS archiving
        """
        com_vars = [
            'COMIN_OBS',
            'COMIN_ATMOS_RESTART',
            'COMIN_ATMOS_ANALYSIS',
            'COMIN_SNOW_ANALYSIS',
            'COMIN_OCEAN_RESTART',
            'COMIN_OCEAN_ANALYSIS',
            'COMIN_ICE_RESTART',
            'COMIN_ICE_ANALYSIS',
            'COMIN_MED_RESTART',
            'COMIN_CONF',
        ]
        return ArchiveTarVars._create_relative_com_paths(config_dict, com_vars)

    @staticmethod
    @logit(logger)
    def get_enkf_com_paths(config_dict: AttrDict) -> AttrDict:
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
        AttrDict
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
        >>> com_paths = ArchiveTarVars.get_enkf_com_paths(config)
        >>> com_paths['COMIN_ATMOS_HISTORY_ENSSTAT']
        'enkfgdas.20211221/00/atmos/ensstat'
        """
        com_paths = {}
        rotdir = config_dict.get('ROTDIR', '')
        com_vars = [
            'COMIN_ATMOS_HISTORY',
            'COMIN_ATMOS_HISTORY_ENSSTAT',
            'COMIN_ATMOS_ANALYSIS_ENSSTAT',
            'COMIN_SNOW_ANALYSIS_ENSSTAT',
            'COMIN_OCEAN_ANALYSIS_ENSSTAT',
            'COMIN_ICE_ANALYSIS_ENSSTAT',
            'COMIN_CONF',
        ]
        for var_name in com_vars:
            if var_name in config_dict:
                abs_path = config_dict[var_name]
                rel_path = ArchiveTarVars._make_path_relative(abs_path, rotdir)
                com_paths[var_name] = rel_path
                logger.debug(f"Converted {var_name}: {abs_path} -> {rel_path}")
        logger.info(f"Generated {len(com_paths)} relative ensemble statistics COM paths")
        return com_paths

    @staticmethod
    @logit(logger)
    def get_enkf_member_com_paths(config_dict: AttrDict, member: int) -> AttrDict:
        """Generate relative COM paths for a single ensemble member.

        This method creates relative COM paths (relative to ROTDIR) for a specific
        ensemble member. It is designed to be called once per member during
        template rendering iteration.

        Parameters
        ----------
        config_dict : AttrDict
            Configuration dictionary from Archive.task_config
            Required keys: ROTDIR, COM_*_TMPL variables
        member : int
            Member number (e.g., 1, 2, 3, ..., NMEM_ENS)

        Returns
        -------
        AttrDict
            Dictionary with relative COM paths for this specific member:
            - COMIN_ATMOS_ANALYSIS_MEM: Relative path to member analysis directory
            - COMIN_ATMOS_HISTORY_MEM: Relative path to member history directory
            - COMIN_ATMOS_RESTART_MEM: Relative path to member restart directory
            - COMIN_OCEAN_ANALYSIS_MEM: Relative path to member ocean analysis
            - COMIN_OCEAN_LETKF_MEM: Relative path to member ocean LETKF
            - COMIN_OCEAN_HISTORY_MEM: Relative path to member ocean history
            - COMIN_OCEAN_RESTART_MEM: Relative path to member ocean restart
            - COMIN_ICE_ANALYSIS_MEM: Relative path to member ice analysis
            - COMIN_ICE_LETKF_MEM: Relative path to member ice LETKF
            - COMIN_ICE_HISTORY_MEM: Relative path to member ice history
            - COMIN_ICE_RESTART_MEM: Relative path to member ice restart
            - COMIN_MED_RESTART_MEM: Relative path to member mediator restart
            - member_num: Member number (padded to 3 digits, e.g., "001")
            All paths are relative to ROTDIR for portability.

        Notes
        -----
        This method is called during per-member template rendering in configure_tars.
        The singular variable names (COMIN_*_MEM) are used in simplified YAML templates
        that no longer contain member loops.

        Examples
        --------
        >>> # Generate variables for member 5
        >>> member_vars = ArchiveTarVars.get_enkf_single_member_vars(config, 5)
        >>> member_vars['COMIN_ATMOS_RESTART_MEM']
        'enkfgdas.20211221/00/atmos/mem005'
        >>> member_vars['member_num']
        '005'
        """
        # Create member-specific cycle dictionary
        cycle_dict = ArchiveTarVars._create_cycle_dicts(config_dict)
        cycle_dict['${MEMDIR}'] = f"mem{member:03d}"

        # Define template mappings (singular key -> template key)
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

        # Generate relative COM paths for this member
        member_vars = {}
        for var_key, template_key in template_mappings:
            if config_dict.get(template_key):
                rel_path = ArchiveTarVars._create_relative_mem_com_paths(
                    config_dict[template_key], cycle_dict, config_dict.ROTDIR
                )
                member_vars[var_key] = rel_path

        logger.debug(f"Generated {len(member_vars)} relative COM paths for member {member}")
        return member_vars

    @staticmethod
    @logit(logger)
    def _create_mem_com_sets(config_dict: AttrDict, first_group_mem: int, last_group_mem: int) -> AttrDict:
        """Generate COM path sets for a group of ensemble members.

        Parameters
        ----------
        config_dict : AttrDict
            Configuration dictionary with COM templates
        first_group_mem : int
            First member number in this archive group
        last_group_mem : int
            Last member number in this archive group

        Returns
        -------
        AttrDict
            Dictionary mapping com_set_NNN keys to member-specific COM paths
        """
        mem_var_set = {}
        for member in range(first_group_mem, last_group_mem + 1):
            mem_var_set[f"com_set_{member:02d}"] = ArchiveTarVars.get_enkf_member_com_paths(config_dict, member)
        return mem_var_set

    @staticmethod
    @logit(logger)
    def get_tarball_com_paths(config_dict: AttrDict, tarball_type: str, run: str) -> AttrDict:
        """Generate relative COM paths for specific GFS/GDAS tarball types.

        Maps tarball types to their required COM variables and creates relative
        paths (relative to ROTDIR) for archiving. This method centralizes the
        tarball-to-COM-variable mapping logic.

        Parameters
        ----------
        config_dict : AttrDict
            Configuration dictionary with ROTDIR and COMIN_* variables
        tarball_type : str
            Type of tarball (e.g., 'gfsa', 'gfs_pgrb2b', 'gdas', etc.)
        run : str
            RUN type ('gfs' or 'gdas')

        Returns
        -------
        AttrDict
            Dictionary with relative COM paths for the specified tarball type

        Raises
        ------
        KeyError
            If tarball_type is not recognized for the specified RUN

        Notes
        -----
        Supported tarball types (matching YAML filenames):
        - GFS: gfsa, gfsb, gfs_flux, gfs_flux_1p00, gfs_pgrb2b, gfs_restarta,
               gfs_netcdfa, gfs_netcdfb, gfs_downstream, gfsocean_analysis, gfswave,
               ocean_6hravg, ocean_grib2, ocean_native, ice_6hravg, ice_grib2,
               ice_native, chem
        - GDAS: gdas, gdas_restarta, gdas_restartb, gdasocean_analysis,
                gdasocean_restart, gdasocean, gdasice, gdasice_restart,
                gdaswave, gdaswave_restart
        """
        # Define tarball-type to COM variables mapping
        # Each key matches a YAML filename (without .yaml.j2 extension)
        if run == 'gfs':
            tarball_mappings = {
                'gfsa': ['COMIN_ATMOS_ANALYSIS', 'COMIN_ATMOS_GENESIS', 'COMIN_ATMOS_GRIB_0p25',
                         'COMIN_ATMOS_GRIB_1p00', 'COMIN_ATMOS_HISTORY', 'COMIN_ATMOS_MINMON',
                         'COMIN_ATMOS_TRACK', 'COMIN_CHEM_ANALYSIS', 'COMIN_CONF', 'COMIN_OBS',
                         'COMIN_SNOW_ANALYSIS'],
                'gfsb': ['COMIN_ATMOS_GRIB_0p25', 'COMIN_ATMOS_GRIB_1p00'],
                'gfs_flux': ['COMIN_ATMOS_MASTER'],
                'gfs_flux_1p00': ['COMIN_ATMOS_GRIB_1p00'],
                'gfs_pgrb2b': ['COMIN_ATMOS_GRIB_0p25', 'COMIN_ATMOS_GRIB_1p00'],
                'gfs_restarta': ['COMIN_ATMOS_INPUT', 'COMIN_ATMOS_RESTART'],
                'gfs_netcdfa': ['COMIN_ATMOS_ANALYSIS'],
                'gfs_netcdfb': ['COMIN_ATMOS_HISTORY'],
                'gfs_downstream': ['COMIN_ATMOS_BUFR', 'COMIN_ATMOS_GEMPAK'],
                'gfsocean_analysis': ['COMIN_CONF', 'COMIN_ICE_ANALYSIS', 'COMIN_ICE_BMATRIX',
                                      'COMIN_OCEAN_ANALYSIS', 'COMIN_OCEAN_BMATRIX'],
                'gfswave': ['COMIN_WAVE_STATION'],
                'ocean_6hravg': ['COMIN_OCEAN_HISTORY'],
                'ocean_grib2': ['COMIN_OCEAN_GRIB'],
                'ocean_native': ['COMIN_OCEAN_NETCDF'],
                'ice_6hravg': ['COMIN_ICE_HISTORY'],
                'ice_grib2': ['COMIN_ICE_GRIB'],
                'ice_native': ['COMIN_ICE_NETCDF'],
                'chem': ['COMIN_CHEM_HISTORY'],
            }
        elif run == 'gdas':  # gdas
            tarball_mappings = {
                'gdas': ['COMIN_ATMOS_ANALYSIS', 'COMIN_ATMOS_ANLMON', 'COMIN_ATMOS_GRIB_0p25',
                         'COMIN_ATMOS_GRIB_1p00', 'COMIN_ATMOS_HISTORY', 'COMIN_ATMOS_MASTER',
                         'COMIN_ATMOS_MINMON', 'COMIN_ATMOS_OZNMON', 'COMIN_ATMOS_RADMON',
                         'COMIN_CHEM_ANALYSIS', 'COMIN_CONF', 'COMIN_OBS', 'COMIN_SNOW_ANALYSIS',
                         'COMIN_SNOW_ANLMON'],
                'gdas_restarta': ['COMIN_ATMOS_ANALYSIS', 'COMIN_ATMOS_RESTART', 'COMIN_OBS'],
                'gdas_restartb': ['COMIN_ATMOS_RESTART'],
                'gdasocean_analysis': ['COMIN_CONF', 'COMIN_ICE_ANALYSIS', 'COMIN_ICE_BMATRIX',
                                       'COMIN_OCEAN_ANALYSIS', 'COMIN_OCEAN_BMATRIX'],
                'gdasocean_restart': ['COMIN_MED_RESTART', 'COMIN_OCEAN_RESTART'],
                'gdasocean': ['COMIN_CONF', 'COMIN_OCEAN_HISTORY'],
                'gdasice': ['COMIN_CONF', 'COMIN_ICE_HISTORY'],
                'gdasice_restart': ['COMIN_ICE_RESTART'],
                'gdaswave': ['COMIN_WAVE_RESTART', 'COMIN_WAVE_STATION'],
                'gdaswave_restart': ['COMIN_WAVE_RESTART'],
            }
        else:
            raise ValueError(f"Unsupported RUN type '{run}' for tarball COM path generation. Must be 'gfs' or 'gdas'.")

        if tarball_type not in tarball_mappings:
            raise KeyError(f"Unknown {run.upper()} tarball type: {tarball_type}")

        com_vars = tarball_mappings[tarball_type]
        return ArchiveTarVars._create_relative_com_paths(config_dict, com_vars)

    @staticmethod
    @logit(logger)
    def _create_cycle_dicts(config_dict: AttrDict) -> AttrDict:
        """Create cycle directories for template substitution

        Parameters
        ----------
        rotdir : str
            ROTDIR path
        run : str
            RUN type

        Returns
        -------
        AttrDict
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
    def _make_path_relative(abs_path: str, rotdir: str) -> str:
        """Strip ROTDIR prefix to create relative path.

        Parameters
        ----------
        abs_path : str
            Absolute path that may contain ROTDIR prefix
        rotdir : str
            ROTDIR to strip from path

        Returns
        -------
        str
            Path relative to ROTDIR

        Examples
        --------
        >>> ArchiveTarVars._make_path_relative('/data/rotdir/gfs.20251218/00', '/data/rotdir')
        'gfs.20251218/00'
        """
        rotdir_prefix = rotdir if rotdir.endswith(os.sep) else rotdir + os.sep
        return abs_path.replace(rotdir_prefix, '') if rotdir_prefix in abs_path else abs_path

    @staticmethod
    @logit(logger)
    def _create_relative_mem_com_paths(template: str, var_dict: AttrDict, rotdir: str) -> str:
        """Replace template variables in a member template and return a path relative to ROTDIR.

        Parameters
        ----------
        template : str
            Template string with variables to replace (e.g., "${ROTDIR}/${RUN}.${YMD}/${HH}")
        var_dict : AttrDict
            Dictionary of variable names and values
        rotdir : str
            Absolute ROTDIR used to strip from generated paths

        Returns
        -------
        str
            Path relative to ROTDIR for portability in tar archives

        Examples
        --------
        >>> template = "${ROTDIR}/${RUN}.${YMD}/${HH}"
        >>> var_dict = AttrDict({
        ...     "${ROTDIR}": "/path/to/rotdir",
        ...     "${RUN}": "gfs",
        ...     "${YMD}": "20251218",
        ...     "${HH}": "00",
        ... })
        >>> rotdir = "/path/to/rotdir"
        >>> ArchiveTarVars._create_relative_mem_com_paths(template, var_dict, rotdir)
        'gfs.20251218/00'
        """
        # First replace all template variables
        replace_com = template
        for var, value in var_dict.items():
            replace_com = replace_com.replace(var, value)

        # Then strip ROTDIR prefix to make path relative
        return ArchiveTarVars._make_path_relative(replace_com, rotdir)

    @staticmethod
    @logit(logger)
    def _create_relative_com_paths(config_dict: AttrDict, com_var_list: list) -> AttrDict:
        """
        Convert absolute COM paths to relative paths for non-EnKF archiving.

        Parameters
        ----------
        config_dict : AttrDict
            Must contain ROTDIR and absolute COMIN_*/COMOUT_* variables.
        com_var_list : list
            List of COM variable names to convert (e.g., ['COMIN_ATMOS_HISTORY'])

        Returns
        -------
        AttrDict
            Relative COM paths for requested variables.

        Notes
        -----
        - Only processes variables that exist in config_dict
        - Strips ROTDIR prefix to create relative paths
        - Used by get_gcafs_com_paths, get_gcdas_com_paths

        Examples
        --------
        >>> config_dict = AttrDict({
        ...     'ROTDIR': '/data/rotdir',
        ...     'COMIN_ATMOS_HISTORY': '/data/rotdir/gfs.20251218/00/atmos/history',
        ... })
        >>> ArchiveTarVars._create_relative_com_paths(config_dict, ['COMIN_ATMOS_HISTORY'])
        {'COMIN_ATMOS_HISTORY': 'gfs.20251218/00/atmos/history'}
        """
        com_paths = AttrDict()
        rotdir = config_dict['ROTDIR']

        for var_name in com_var_list:
            if var_name in config_dict:
                abs_path = config_dict[var_name]
                rel_path = ArchiveTarVars._make_path_relative(abs_path, rotdir)
                com_paths[var_name] = rel_path
                logger.debug(f"Converted {var_name}: {abs_path} -> {rel_path}")

        logger.info(f"Created {len(com_paths)} relative COM paths")
        return com_paths
