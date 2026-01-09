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
  - Calculate COM directory paths for ENKF system with grid loops (0p25, 0p50, 1p00)
  - Extract configuration keys (RUN, DO_* flags, FHMAX*, etc.)
  - Provide complete arch_dict to YAML templates

YAML Template Responsibilities (parm/archive/master_*.yaml.j2):
  - Build file sets with source -> destination mappings
  - Handle loops (forecast hours, grids, basins)
  - Apply conditionals (DO_* flags, MODE, RUN type)

Key Functions
-------------
get_all_yaml_vars(config_dict):
  Main entry point - collects all variables for YAML templates

add_config_vars(config_dict):
  Extracts configuration keys and COM* variables (created in job scripts)

Design Note
-----------
This is NOT a Task class - it's a utility module with functions that operate on
config_dict dictionaries.

Logging
-------
All public operational functions are decorated with @logit(logger).
"""
import os
from logging import getLogger
from wxflow import AttrDict, logit, to_YMD, to_YMDH, add_to_datetime, to_timedelta, to_fv3time

logger = getLogger(__name__.split('.')[-1])


class ArchiveTarVars:
    """
    Utility class for collecting archive tar variables.

    This class provides variables for YAML templates that handle archiving
    for three systems:
    - GFS: Global Forecast System
    - GEFS: Global Ensemble Forecast System
    - GCAFS: Global Chemistry and Aerosol Forecast System
    - GDAS: Global Data Assimilation Systems (GDAS and GCDAS)
    - EnKF: Ensemble Kalman Filter systems (EnKFGFS, EnKFGDAS)

    The YAML templates (parm/archive/master_*.yaml.j2) contain all file set
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
        arch_dict.update(ArchiveTarVars._get_all_cyc_vars(config_dict))

        # Add tarball-specific variables if TARBALL_TYPE is defined
        tarball_type = config_dict.get('TARBALL_TYPE', '')
        if tarball_type:
            arch_dict.update(ArchiveTarVars.get_tarball_specific_vars(config_dict, tarball_type))

        if config_dict.get('RUN') in ['enkfgfs', 'enkfgdas']:
            # EnKF systems: Handle ensemble member-specific paths
            ensgrp = config_dict.get('ENSGRP', 0)
            if ensgrp == 0:
                # ENSGRP=0: Ensemble mean/spread (enkf.yaml.j2)
                arch_dict.update(ArchiveTarVars.get_enkf_com_paths(config_dict))
            else:
                # ENSGRP=!0: Individual member groups
                arch_dict.update(ArchiveTarVars._create_enkf_mem_com_sets(
                    config_dict,
                    arch_dict['first_group_mem'],
                    arch_dict['last_group_mem']
                ))
        elif config_dict.get('RUN') in ['gfs', 'gdas']:
            # GFS/GDAS systems: COMIN variables already set in job scripts
            # For wave tarballs, collect all COMIN_WAVE_GRID_* paths from config_dict
            if tarball_type in ['gfswave', 'gdaswave']:
                arch_dict['WAVE_GRID_RES_COM_list'] = [v for k, v in config_dict.items() if k.startswith('COMIN_WAVE_GRID_')]
        elif config_dict.get('RUN') == 'gcafs':
            # GCAFS system: COMIN variables already set in job scripts
            logger.info("GCAFS system: COMIN variables already set in job scripts")
        elif config_dict.get('RUN') == 'gcdas':
            logger.info("GCDAS system: COMIN variables already set in job scripts")
        else:
            logger.info(f"Unknown RUN type '{config_dict.get('RUN')}', no additional COM paths added")

        logger.info(f"Collected {len(arch_dict)} variables for YAML templates")
        logger.debug(f"arch_dict keys: {list(arch_dict.keys())}")

        return arch_dict

    @staticmethod
    @logit(logger)
    def add_config_vars(config_dict: AttrDict) -> AttrDict:

        """
        Collect configuration variables for archive tar operations.

        This method extracts all required configuration keys for
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
            'ATARDIR', 'current_cycle', 'RUN', 'PDY', 'PSLOT', 'NET', 'MODE',
            'PARMgfs', 'ROTDIR', 'SDATE',
            # Archive control
            'DO_ARCHCOM', 'ARCHCOM_TO', 'ARCDIR',
            # Data assimilation
            'DOHYBVAR', 'DOIAU', 'DO_CA', 'assim_freq', 'IAUFHRS',
            'DO_JEDISNOWDA', 'DO_GSISOILDA', 'DO_LAND_IAU',
            # Ocean/Ice DA
            'DOHYBVAR_OCN', 'DOLETKF_OCN', 'NMEM_ENS',
            # Archive timing and control
            'ARCH_CYC', 'ARCH_WARMICFREQ', 'ARCH_FCSTICFREQ',
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
    def _get_all_cyc_vars(config_dict: AttrDict) -> AttrDict:
        """Compute common cycle variables for all archive YAML templates.

        This method computes basic cycle variables used across all archive systems
        (GFS, GDAS, EnKF, GCAFS). System-specific variables are computed in their
        respective methods.

        Parameters
        ----------
        config_dict : AttrDict
            Configuration dictionary from Archive.task_config
            Required keys: current_cycle, assim_freq

        Returns
        -------
        AttrDict
            Dictionary containing common cycle variables:
            - cycle_HH, cycle_YMDH, cycle_YMD: Current cycle time components
            - assim_freq: Assimilation frequency as string
            Plus system-specific variables if RUN is 'enkf*'
        """
        current_cycle = config_dict.current_cycle
        assim_freq = config_dict.get('assim_freq', 6)

        vars_out = AttrDict()
        # Basic cycle variables (common to all systems)
        vars_out['cycle_HH'] = current_cycle.strftime("%H")
        vars_out['cycle_YMDH'] = to_YMDH(current_cycle)
        vars_out['cycle_YMD'] = to_YMD(current_cycle)
        vars_out['cycle_fv3time'] = to_fv3time(current_cycle)

        # Assimilation frequency
        vars_out['assim_freq'] = str(assim_freq)

        # Padded IAU forecast hours for templates
        if 'IAUFHRS' in config_dict:
            vars_out['iaufhrs_str'] = [f"{h:03d}" for h in config_dict['IAUFHRS']]

        # Add EnKF-specific variables if RUN contains 'enkf'
        if 'enkf' in config_dict.get('RUN', ''):
            vars_out.update(ArchiveTarVars._get_enkf_specific_cyc_vars(config_dict, current_cycle))

        # Add GCDAS-specific variables if RUN is 'gcdas'
        if config_dict.get('RUN', '') == 'gcdas':
            vars_out.update(ArchiveTarVars._get_gcdas_specific_cyc_vars(config_dict, current_cycle))

        return vars_out

    @staticmethod
    @logit(logger)
    def _get_enkf_specific_cyc_vars(config_dict: AttrDict, current_cycle) -> AttrDict:
        """Compute EnKF-specific cycle variables.

        This method computes variables specific to EnKF ensemble systems including
        forecast configuration, member ranges, and restart prefixes.

        Parameters
        ----------
        config_dict : AttrDict
            Configuration dictionary from Archive.task_config
            Required keys: RUN
            Optional keys: FHMIN_ENKF, FHMAX_ENKF, FHOUT_ENKF, DO_CALC_INCREMENT,
                          DO_CALC_INCREMENT_ENKF_GFS, NMEM_ENS, NMEM_ENS_GFS,
                          restart_interval_enkfgdas, restart_interval_enkfgfs,
                          ENSGRP, NMEM_EARCGRP
        current_cycle : datetime
            Current cycle time

        Returns
        -------
        AttrDict
            Dictionary containing EnKF-specific variables:
            - fhmin, fhmax, fhout: Forecast time configuration
            - do_calc_increment: Whether to calculate increments
            - nmem_ens: Number of ensemble members
            - restart_interval: Restart interval for this system
            - is_gdas, is_gfs: System type flags
            - enkf_epos_ngrps: Number of ensemble groups (ENSGRP=0 only)
            - first_group_mem, last_group_mem: Member range for this group
            - restart_prefixes: List of restart time prefixes
        """
        enkf_vars = AttrDict()

        # EnKF-specific analysis and restart times (using DOIAU_ENKF)
        doiau_enkf = config_dict.get('DOIAU_ENKF', False)
        dohybvar_ocn = config_dict.get('DOHYBVAR_OCN', False)

        # Analysis time (for surface analysis restart files)
        anl_delta = to_timedelta("-3H") if doiau_enkf else to_timedelta("0H")
        anl_time = add_to_datetime(current_cycle, anl_delta)
        enkf_vars['anl_YMD'] = to_YMD(anl_time)
        enkf_vars['anl_HH'] = anl_time.strftime("%H")

        # Restart hour calculations (when DOHYBVAR_OCN is true)
        # Two different logic blocks for different templates:
        # 1. enkf_restarta: conditional on DOIAU_ENKF (+3H if true, +6H if false)
        # 2. enkf_restartb: always +3H (not conditional)
        if dohybvar_ocn:
            # For enkf_restarta_grp.yaml.j2 (conditional logic)
            rst_delta_a = to_timedelta("+3H") if doiau_enkf else to_timedelta("+6H")
            rst_time_a = add_to_datetime(current_cycle, rst_delta_a)
            enkf_vars['rst_HH_restarta'] = rst_time_a.strftime("%H")

            # For enkf_restartb_grp.yaml.j2 (always +3H)
            rst_delta_b = to_timedelta("+3H")
            rst_time_b = add_to_datetime(current_cycle, rst_delta_b)
            enkf_vars['rst_HH_restartb'] = rst_time_b.strftime("%H")

        # Forecast output frequency
        enkf_vars['fhout'] = config_dict.get('FHOUT_ENKF', 3)
        enkf_vars['fhmin'] = config_dict.get('FHMIN_ENKF', 0)
        enkf_vars['fhmax'] = config_dict.get('FHMAX_ENKF', 0)
        # System-specific configuration
        if config_dict.get('RUN', '') == 'enkfgfs':
            enkf_vars['do_calc_increment'] = config_dict.get('DO_CALC_INCREMENT_ENKF_GFS', False)
            enkf_vars['nmem_ens'] = config_dict.get('NMEM_ENS_GFS', None)
            enkf_vars['restart_interval'] = config_dict.get('restart_interval_enkfgfs', None)
            enkf_vars['is_gdas'] = False
            enkf_vars['is_gfs'] = True
        elif config_dict.get('RUN', '') == 'enkfgdas':
            enkf_vars['do_calc_increment'] = config_dict.get('DO_CALC_INCREMENT', False)
            enkf_vars['nmem_ens'] = config_dict.get('NMEM_ENS')
            enkf_vars['restart_interval'] = config_dict.get('restart_interval_enkfgdas', None)
            enkf_vars['is_gdas'] = True
            enkf_vars['is_gfs'] = False
        else:
            logger.warning(
                f"RUN='{config_dict.get('RUN', '')}' does not match a supported EnKF type ('enkfgfs' or 'enkfgdas'). "
            )

        # ENSGRP-specific calculations
        ensgrp = config_dict.get('ENSGRP', 0)
        if ensgrp == 0:
            enkf_vars['enkf_epos_ngrps'] = len(range(enkf_vars['fhmin'], enkf_vars['fhmax'] + enkf_vars['fhout'], enkf_vars['fhout']))
        else:
            nmem_earcgrp = config_dict.get('NMEM_EARCGRP')
            if nmem_earcgrp and enkf_vars['nmem_ens']:
                enkf_vars['first_group_mem'] = (ensgrp - 1) * nmem_earcgrp + 1
                enkf_vars['last_group_mem'] = min(ensgrp * nmem_earcgrp, enkf_vars['nmem_ens'])

        # Pre-compute all restart time prefixes for YAML templates using helper method
        if enkf_vars.get('is_gdas') and enkf_vars.get('restart_interval') and enkf_vars.get('fhmax'):
            enkf_vars['restart_prefixes'] = ArchiveTarVars._calculate_restart_prefixes(
                current_cycle,
                enkf_vars['restart_interval'],
                enkf_vars['fhmax']
            )
        else:
            enkf_vars['restart_prefixes'] = []

        # Archive timing logic for EnKF systems (from master_enkf.yaml.j2)
        # Both archive groups require: is_gdas AND SDATE AND specific day/cycle conditions
        sdate = config_dict.get('SDATE')
        arch_warmicfreq = config_dict.get('ARCH_WARMICFREQ', 1)
        arch_cyc = config_dict.get('ARCH_CYC', 0)
        assim_freq = config_dict.get('assim_freq', 6)

        # Archive timing booleans - increments (group a)
        # Logic: (current_cycle - SDATE).days % ARCH_WARMICFREQ == 0 AND is_gdas AND ARCH_CYC == cycle_HH
        enkf_vars['archive_increments'] = False
        current_cycle_days = (current_cycle - sdate).days
        enkf_vars['archive_increments'] = (
            (current_cycle_days % arch_warmicfreq == 0) and
            enkf_vars.get('is_gdas', False) and
            (arch_cyc == int(current_cycle.strftime("%H")))
        )

        # Archive timing booleans - ICs (group b)
        # Logic: (ics_offset_cycle - SDATE).days % ARCH_WARMICFREQ == 0 AND is_gdas AND (ARCH_CYC - assim_freq) % 24 == cycle_HH
        enkf_vars['archive_ics'] = False
        ics_offset_cycle = add_to_datetime(current_cycle, to_timedelta(f"+{assim_freq}H"))
        ics_offset_days = (ics_offset_cycle - sdate).days
        enkf_vars['archive_ics'] = (
            (ics_offset_days % arch_warmicfreq == 0) and
            enkf_vars.get('is_gdas', False) and
            ((arch_cyc - assim_freq) % 24 == int(current_cycle.strftime("%H")))
        )

        # Warm start flags (placeholders for future use)
        enkf_vars['save_warm_start_forecast'] = False
        enkf_vars['save_warm_start_cycled'] = False

        return enkf_vars

    @staticmethod
    @logit(logger)
    def _get_gcdas_specific_cyc_vars(config_dict: AttrDict, current_cycle) -> AttrDict:
        """Compute GCDAS-specific cycle variables.

        This method computes variables specific to GCDAS (Global Chemistry Data
        Assimilation System) including restart prefixes for archiving.

        Parameters
        ----------
        config_dict : AttrDict
            Configuration dictionary from Archive.task_config
            Optional keys: restart_interval_gdas, FHMAX
        current_cycle : datetime
            Current cycle time

        Returns
        -------
        AttrDict
            Dictionary containing GCDAS-specific variables:
            - restart_prefixes: List of restart time prefixes
        """
        gcdas_vars = AttrDict()

        # GCDAS restart prefixes calculation
        restart_interval = config_dict.get('restart_interval_gdas', 6)
        fhmax = config_dict.get('FHMAX', 9)
        gcdas_vars['restart_prefixes'] = ArchiveTarVars._calculate_restart_prefixes(
            current_cycle, restart_interval, fhmax
        )

        logger.info(f"Calculated {len(gcdas_vars['restart_prefixes'])} restart prefixes for GCDAS "
                    f"(interval={restart_interval}H, FHMAX={fhmax}H)")

        return gcdas_vars

    @staticmethod
    @logit(logger)
    def get_enkf_com_paths(config_dict: AttrDict) -> AttrDict:
        """Extract absolute COMIN paths for EnKF ensemble mean/spread (ENSGRP=0).

        This method extracts absolute COM paths from config_dict. The paths will be
        converted to relative paths AFTER YAML rendering in archive.py.

        Parameters
        ----------
        config_dict : AttrDict
            Configuration dictionary from Archive.task_config
            Required keys: ROTDIR
            Optional keys: COMIN_*, COMOUT_* variables (created by job scripts)

        Returns
        -------
        AttrDict
            Dictionary with absolute COMIN paths for ensemble statistics:
            - Keys match enkf.yaml.j2 template variable names
            - Values are absolute paths (conversion to relative happens after YAML rendering)

        Notes
        -----
        This method should ONLY be called when ENSGRP == 0 (ensemble mean archiving).
        For individual member archiving (ENSGRP != 0), use get_enkf_member_com_paths().

        Paths remain absolute at this stage. Relative path conversion happens in
        archive.py after YAML rendering.

        Examples
        --------
        >>> # Job script creates: COMIN_ATMOS_HISTORY_ENSSTAT=/path/to/ROTDIR/enkfgdas.20211221/00/atmos/ensstat
        >>> com_paths = ArchiveTarVars.get_enkf_com_paths(config)
        >>> com_paths['COMIN_ATMOS_HISTORY_ENSSTAT']
        '/path/to/ROTDIR/enkfgdas.20211221/00/atmos/ensstat'
        """
        com_paths = AttrDict()
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
                com_paths[var_name] = abs_path
                logger.debug(f"Extracted {var_name}: {abs_path}")
        logger.info(f"Extracted {len(com_paths)} absolute ensemble statistics COM paths (will convert to relative after YAML rendering)")
        return com_paths

    @staticmethod
    @logit(logger)
    def get_enkf_member_com_paths(config_dict: AttrDict, member: int) -> AttrDict:
        """Generate absolute COM paths for a single ensemble member.

        This method creates absolute COM paths for a specific ensemble member.
        The paths will be converted to relative paths AFTER YAML rendering in archive.py.

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
            Dictionary with absolute COM paths for this specific member:
            - COMIN_ATMOS_ANALYSIS_MEM: Absolute path to member analysis directory
            - COMIN_ATMOS_HISTORY_MEM: Absolute path to member history directory
            - COMIN_ATMOS_RESTART_MEM: Absolute path to member restart directory
            - COMIN_OCEAN_ANALYSIS_MEM: Absolute path to member ocean analysis
            - COMIN_OCEAN_LETKF_MEM: Absolute path to member ocean LETKF
            - COMIN_OCEAN_HISTORY_MEM: Absolute path to member ocean history
            - COMIN_OCEAN_RESTART_MEM: Absolute path to member ocean restart
            - COMIN_ICE_ANALYSIS_MEM: Absolute path to member ice analysis
            - COMIN_ICE_LETKF_MEM: Absolute path to member ice LETKF
            - COMIN_ICE_HISTORY_MEM: Absolute path to member ice history
            - COMIN_ICE_RESTART_MEM: Absolute path to member ice restart
            - COMIN_MED_RESTART_MEM: Absolute path to member mediator restart
            - member_num: Member number (padded to 3 digits, e.g., "001")
            Paths are absolute (conversion to relative happens after YAML rendering).

        Notes
        -----
        This method is called during per-member template rendering in configure_tars.
        The singular variable names (COMIN_*_MEM) are used in simplified YAML templates
        that no longer contain member loops.

        Paths remain absolute at this stage. Relative path conversion happens in
        archive.py after YAML rendering.

        Examples
        --------
        >>> # Generate variables for member 5
        >>> member_vars = ArchiveTarVars.get_enkf_single_member_vars(config, 5)
        >>> member_vars['COMIN_ATMOS_RESTART_MEM']
        '/path/to/ROTDIR/enkfgdas.20211221/00/atmos/mem005'
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

        # Generate absolute COM paths for this member
        member_vars = {}
        for var_key, template_key in template_mappings:
            if config_dict.get(template_key):
                # Replace template variables to create absolute path
                abs_path = config_dict[template_key]
                for var, value in cycle_dict.items():
                    abs_path = abs_path.replace(var, value)
                member_vars[var_key] = abs_path

        logger.debug(f"Generated {len(member_vars)} absolute COM paths for member {member} (will convert to relative after YAML rendering)")
        return member_vars

    @staticmethod
    @logit(logger)
    def _create_enkf_mem_com_sets(config_dict: AttrDict, first_group_mem: int, last_group_mem: int) -> AttrDict:
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
    def get_tarball_specific_vars(config_dict: AttrDict, tarball_type: str) -> AttrDict:
        """
        Calculate tarball-specific template variables.

        This method computes variables that are specific to certain tarballs,
        such as offset times for wave restart files and analysis times that depend
        on IAU settings.

        Parameters
        ----------
        config_dict : AttrDict
            Archive configuration dictionary with current_cycle and optional DOIAU, DOHYBVAR
        tarball_type : str
            Type of tarball being created (e.g., 'gdaswave_restart', 'gdas_restarta')

        Returns
        -------
        AttrDict
            Dictionary of tarball-specific variables, which may include:
            - offset_dt: datetime offset for wave restart files based on IAU
            - anl_time: analysis time adjusted for IAU/hybrid var
            - prefix: formatted datetime prefix for restart files (YYYYMMDD.HHMMSS)
            - offset_YMD, offset_HH: formatted date/hour components
            - r_prefix_list: list of restart prefixes for restart intervals

        Notes
        -----
        Time offset logic by tarball type:

        gdaswave_restart, gdaswave:
          - If DOIAU: +3H (IAU window beginning) or +6H (standard)

        gdas_restarta, gfs_restarta, gdasocean_analysis, gfsocean_analysis:
          - If DOHYBVAR and DOIAU: -3H (IAU window beginning)
          - Otherwise: 0H (current cycle)

        gdas_restartb:
          - If DOIAU: -3H offset_dt with formatted prefix
          - Always includes center time prefix
          - Calculates r_prefix_list for restart intervals

        Examples
        --------
        >>> config_dict = AttrDict({
        ...     'current_cycle': datetime(2025, 12, 18, 0),
        ...     'DOIAU': True
        ... })
        >>> ArchiveTarVars.get_tarball_specific_vars(config_dict, 'gdaswave_restart')
        {'offset_dt': datetime(2025, 12, 18, 3)}
        """
        tarball_vars = AttrDict()
        current_cycle = config_dict['current_cycle']

        if tarball_type in ['gdaswave_restart']:
            # Wave restart offset time calculation
            # If IAU is enabled, use +3H offset (beginning of IAU window)
            # Otherwise, use +6H offset (standard forecast time)
            doiau = config_dict.get('DOIAU', False)
            offset_hours = 3 if doiau else 6

            offset_dt = add_to_datetime(current_cycle, to_timedelta(f"+{offset_hours}H"))
            # Return formatted string for direct use in YAML
            tarball_vars['offset_dt_fv3'] = to_fv3time(offset_dt)

            logger.info(f"Calculated offset_dt_fv3 for {tarball_type}: {to_fv3time(offset_dt)} "
                        f"(DOIAU={doiau}, offset={offset_hours}H)")

        elif tarball_type in ['gdaswave']:
            # Wave data always uses +6H offset
            offset_dt = add_to_datetime(current_cycle, to_timedelta("+6H"))
            # Return formatted string for direct use in YAML
            tarball_vars['offset_dt_fv3'] = to_fv3time(offset_dt)

            logger.info(f"Calculated offset_dt_fv3 for {tarball_type}: {to_fv3time(offset_dt)} (+6H)")

        elif tarball_type in ['gdas_restarta', 'gfs_restarta']:
            # Atmosphere restart analysis time calculation
            # If hybrid var with IAU: use -3H (beginning of IAU window)
            # Otherwise: use 0H (current cycle time)
            dohybvar = config_dict.get('DOHYBVAR', False)
            doiau = config_dict.get('DOIAU', False)

            if dohybvar and doiau:
                anl_offset = "-3H"
            else:
                anl_offset = "0H"

            anl_time = add_to_datetime(current_cycle, to_timedelta(anl_offset))
            # Return formatted strings for direct use in YAML
            tarball_vars['anl_time_YMD'] = to_YMD(anl_time)
            tarball_vars['anl_time_HH'] = anl_time.strftime("%H")

            logger.info(f"Calculated anl_time for {tarball_type}: {to_YMD(anl_time)}.{anl_time.strftime('%H')}0000 "
                        f"(DOHYBVAR={dohybvar}, DOIAU={doiau}, offset={anl_offset})")

        elif tarball_type in ['gdasocean_analysis', 'gfsocean_analysis']:
            # Ocean/ice analysis time calculation
            # If IAU: use -3H (beginning of IAU window)
            # Otherwise: use 0H (current cycle time)
            doiau = config_dict.get('DOIAU', False)

            if doiau:
                anl_offset = "-3H"
            else:
                anl_offset = "0H"

            anl_time = add_to_datetime(current_cycle, to_timedelta(anl_offset))
            # Return formatted strings for direct use in YAML
            tarball_vars['anl_time_YMD'] = to_YMD(anl_time)
            tarball_vars['anl_time_HH'] = anl_time.strftime("%H")

            logger.info(f"Calculated anl_time for {tarball_type}: {to_YMD(anl_time)}.{anl_time.strftime('%H')}0000 "
                        f"(DOIAU={doiau}, offset={anl_offset})")

        elif tarball_type == 'gdas_restartb':
            # Restart B has multiple time calculations
            doiau = config_dict.get('DOIAU', False)

            # If IAU is on, calculate offset time (-3H) and its prefix
            if doiau:
                offset_dt = add_to_datetime(current_cycle, to_timedelta("-3H"))
                offset_YMD = to_YMD(offset_dt)
                offset_HH = offset_dt.strftime("%H")
                offset_prefix = f"{offset_YMD}.{offset_HH}0000"

                tarball_vars['offset_dt'] = offset_dt
                tarball_vars['offset_YMD'] = offset_YMD
                tarball_vars['offset_HH'] = offset_HH
                tarball_vars['offset_prefix'] = offset_prefix

                logger.debug(f"Calculated offset_prefix for gdas_restartb: {offset_prefix} (DOIAU=True)")

            # Always calculate center time prefix
            cycle_YMD = to_YMD(current_cycle)
            cycle_HH = current_cycle.strftime("%H")
            tarball_vars['center_prefix'] = f"{cycle_YMD}.{cycle_HH}0000"

            # Calculate restart interval prefixes using helper method
            restart_interval = config_dict.get('restart_interval_gdas', 6)
            fhmax = config_dict.get('FHMAX', 9)
            tarball_vars['r_prefix_list'] = ArchiveTarVars._calculate_restart_prefixes(
                current_cycle, restart_interval, fhmax
            )

            logger.info(f"Calculated {len(tarball_vars['r_prefix_list'])} restart prefixes for gdas_restartb "
                        f"(interval={restart_interval}H, FHMAX={fhmax}H)")

        else:
            logger.warning(f"Tarball type '{tarball_type}' does not have specific variable calculations")

        return tarball_vars

    @staticmethod
    @logit(logger)
    def _calculate_restart_prefixes(current_cycle: 'datetime', restart_interval: int, fhmax: int) -> list:
        """Calculate restart time prefixes for a given interval and forecast max.

        This is a reusable helper for calculating restart file prefixes that appear
        at regular intervals (e.g., every 6 hours) throughout a forecast period.

        Parameters
        ----------
        current_cycle : datetime
            Current cycle time
        restart_interval : int
            Interval in hours between restart files
        fhmax : int
            Maximum forecast hour

        Returns
        -------
        list
            List of restart prefixes in format "YYYYMMDD.HHMMSS"

        Examples
        --------
        >>> from datetime import datetime
        >>> cycle = datetime(2025, 12, 18, 0)
        >>> ArchiveTarVars._calculate_restart_prefixes(cycle, 6, 12)
        ['20251218.060000', '20251218.120000']
        """
        restart_prefixes = []
        for r_time in range(restart_interval, fhmax + 1, restart_interval):
            r_dt = add_to_datetime(current_cycle, to_timedelta(f"+{r_time}H"))
            r_prefix = f"{to_YMD(r_dt)}.{r_dt.strftime('%H')}0000"
            restart_prefixes.append(r_prefix)

        logger.debug(f"Calculated {len(restart_prefixes)} restart prefixes "
                     f"(interval={restart_interval}H, FHMAX={fhmax}H)")
        return restart_prefixes
