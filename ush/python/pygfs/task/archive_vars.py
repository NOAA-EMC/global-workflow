#!/usr/bin/env python3
"""
Archive Variables Task

Overview
--------
This module constructs cycle-specific COM directory path variables and file sets
required for archiving verification (vrfy) data for GFS, GEFS, and GCAFS systems.

Architecture
------------
- One method (_calculate_com_paths) for all COM variables with logic for grids, loops
- Separate method for each arcdir YAML that builds complete file sets:
  * gfs_arcdir() - corresponds to gfs_arcdir.yaml.j2
  * gefs_arcdir() - corresponds to gefs_arcdir.yaml.j2
  * gcafs_arcdir() - corresponds to gcafs_arcdir.yaml.j2

Key Methods
-----------
_get_cycle_vars():
  Computes cycle-specific variables (cycle_HH, cycle_YMDH, cycle_YMD, head)

_calculate_com_paths():
  Generates all COM paths (ROTDIR-based) used across all arcdir YAMLs,
  including grid loops (0p25, 0p50, 1p00) and conditional path logic

gfs_arcdir():
  Complete file set generation for GFS archiving (gfs_arcdir.yaml.j2)

gefs_arcdir():
  Complete file set generation for GEFS archiving (gefs_arcdir.yaml.j2)

gcafs_arcdir():
  Complete file set generation for GCAFS archiving (gcafs_arcdir.yaml.j2)

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

    This class handles archiving for three systems:
    - GFS: Global Forecast System
    - GEFS: Global Ensemble Forecast System
    - GCAFS: Global Climate Analysis Forecast System

    Each system has a corresponding method that builds the complete file set
    for archiving, corresponding to the respective arcdir YAML file.
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
    def add_general_vars(self) -> None:
        """Format general variables for archive operations.

        Updates resolution variables to be 3-digit formatted strings.
        This ensures consistent naming conventions for ocean and ice resolution
        specifications (e.g., 25 -> '025', 100 -> '100').

        Variables updated (if present in task_config):
        - OCNRES: Ocean resolution
        - ICERES: Ice resolution
        """
        # Update these keys to be 3 digits if they are part of task_config
        for key in ['OCNRES', 'ICERES']:
            try:
                self.task_config[key] = f"{self.task_config[key]:03d}"
            except KeyError:
                logger.info(f"key ({key}) not found in task_config")

    @logit(logger)
    def get_all_yaml_vars(self) -> Dict[str, Any]:
        """Collect all archive variables and build complete arch_dict for YAML templates.

        This method:
        1. Formats general variables (OCNRES, ICERES)
        2. Determines system type (GFS, GEFS, GCAFS) from NET
        3. Dispatches to appropriate system-specific method
        4. Builds complete arch_dict with all task_config and archive variables

        Returns
        -------
        Dict[str, Any]
            Complete arch_dict ready for configure_vrfy() and Jinja2 templates,
            containing all task_config variables plus:
            - cycle_HH, cycle_YMDH, cycle_YMD, head: Cycle-specific variables
            - COMIN_*: All COM directory paths
            - file_set: List of [source, destination] file pairs for archiving
            - mkdir_list: List of directories to create

        Notes
        -----
        The NET variable determines which archiving method is called:
        - NET='gefs' → gefs_arcdir()
        - NET='gcafs' → gcafs_arcdir()
        - Otherwise → gfs_arcdir() (handles gfs, gdas, enkfgdas, enkfgfs)
        """
        # Format general variables (e.g., OCNRES, ICERES to 3-digit strings)
        self.add_general_vars()

        NET = self.task_config.get('NET', 'gfs')
        RUN = self.task_config.RUN

        # Dispatch to appropriate system-specific method based on NET
        if NET == 'gefs':
            logger.info(f"Collecting GEFS archive variables for cycle {self.task_config.current_cycle}")
            arcdir_result = self.gefs_arcdir()
        elif NET == 'gcafs':
            logger.info(f"Collecting GCAFS archive variables for cycle {self.task_config.current_cycle}")
            arcdir_result = self.gcafs_arcdir()
        else:  # gfs, gdas, enkfgdas, enkfgfs (default)
            logger.info(f"Collecting GFS/GDAS archive variables for RUN={RUN}, cycle {self.task_config.current_cycle}")
            arcdir_result = self.gfs_arcdir()

        # Build complete arch_dict with all variables for configure_vrfy and Jinja2 templates
        arch_dict = dict(self.task_config)

        # Add cycle-specific variables (cycle_HH, cycle_YMDH, cycle_YMD, head)
        arch_dict.update(arcdir_result['cycle_vars'])

        # Add COM paths (COMIN_ATMOS_ANALYSIS, COMIN_ATMOS_GRIB_*, etc.)
        arch_dict.update(arcdir_result['com_paths'])

        # Add file_set and mkdir_list for Jinja2 templates
        arch_dict['file_set'] = arcdir_result['file_set']
        arch_dict['mkdir_list'] = arcdir_result['mkdir_list']

        logger.info(f"Built arch_dict with {len(arch_dict['file_set'])} files to archive in {len(arch_dict['mkdir_list'])} directories")
        logger.debug(f"arch_dict keys: {list(arch_dict.keys())}")

        return arch_dict

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
        """
        current_cycle = self.task_config.current_cycle

        cycle_HH = current_cycle.strftime("%H")
        cycle_YMDH = to_YMDH(current_cycle)
        cycle_YMD = to_YMD(current_cycle)

        # Build head string (e.g., 'gfs.t00z.')
        RUN = self.task_config.RUN
        head = f"{RUN}.t{cycle_HH}z."

        return {
            'cycle_HH': cycle_HH,
            'cycle_YMDH': cycle_YMDH,
            'cycle_YMD': cycle_YMD,
            'head': head
        }

    @logit(logger)
    def _get_template_dict(self) -> Dict[str, str]:
        """Create template substitution dictionary for COM path generation.

        This method builds the base dictionary used for template variable substitution.
        For GEFS, it includes MEMDIR: 'ensstat' to support ensemble statistics paths.

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

        # Base template substitution dictionary
        base_dict = {
            'ROTDIR': self.task_config.ROTDIR,
            'RUN': self.task_config.RUN,
            'YMD': cycle_vars['cycle_YMD'],
            'HH': cycle_vars['cycle_HH'],
            'PDY': cycle_vars['cycle_YMD'],
            'cyc': cycle_vars['cycle_HH']
        }

        # GEFS-specific: Add MEMDIR for ensemble statistics
        # Corresponds to YAML: '${MEMDIR}': 'ensstat'
        if 'gefs' in self.task_config.RUN.lower():
            base_dict['MEMDIR'] = 'ensstat'

        return base_dict

    @logit(logger)
    def _calculate_com_paths(self, base_dict: Dict[str, str]) -> Dict[str, str]:
        """Calculate all COM paths used across arcdir YAMLs.

        This method generates all ROTDIR-based COM paths with logic for:
        - Multiple grids (0p25, 0p50, 1p00) for GRIB files
        - GEFS ensemble statistics (COMIN_ATMOS_ENSSTAT_1p00)
        - Conditional paths based on RUN, MODE, DO_* flags
        - All paths used by gfs_arcdir, gefs_arcdir, gcafs_arcdir YAMLs

        Parameters
        ----------
        base_dict : Dict[str, str]
            Base template substitution dictionary from _get_template_dict()

        Returns
        -------
        Dict[str, str]
            Dictionary mapping COM variable names to resolved paths.
            Examples:
            - COMIN_ATMOS_ANALYSIS: /path/to/analysis
            - COMIN_ATMOS_GRIB_0p25: /path/to/grib/0p25
            - COMIN_ATMOS_GRIB_0p50: /path/to/grib/0p50
            - COMIN_ATMOS_GRIB_1p00: /path/to/grib/1p00
            - COMIN_ATMOS_ENSSTAT_1p00: /path/to/ensstat (GEFS only)
        """
        com_paths = {}

        # Common paths (always needed)
        common_templates = [
            ('COMIN_ATMOS_ANALYSIS', 'COM_ATMOS_ANALYSIS_TMPL'),
            ('COMIN_ATMOS_GENESIS', 'COM_ATMOS_GENESIS_TMPL'),
            ('COMIN_ATMOS_HISTORY', 'COM_ATMOS_HISTORY_TMPL'),
            ('COMIN_ATMOS_TRACK', 'COM_ATMOS_TRACK_TMPL'),
            ('COMIN_CHEM_ANALYSIS', 'COM_CHEM_ANALYSIS_TMPL'),
            ('COMIN_SNOW_ANALYSIS', 'COM_SNOW_ANALYSIS_TMPL'),
            ('COMIN_OBS', 'COM_OBS_TMPL'),
            ('COMOUT_ATMOS_TRACK', 'COM_ATMOS_TRACK_TMPL'),
        ]

        for com_key, template_key in common_templates:
            template = self.task_config.get(template_key, '')
            if template:
                com_paths[com_key] = Template.substitute_string(
                    template, TemplateConstants.DOLLAR_CURLY_BRACE, base_dict.get)
            else:
                logger.warning(f"Template {template_key} not found for {com_key}")
                com_paths[com_key] = ''

        # Grid-specific paths (loop over grids: 0p25, 0p50, 1p00)
        for grid in ["0p25", "0p50", "1p00"]:
            grid_dict = base_dict.copy()
            grid_dict['GRID'] = grid

            template = self.task_config.get('COM_ATMOS_GRIB_GRID_TMPL', '')
            if template:
                com_key = f"COMIN_ATMOS_GRIB_{grid}"
                com_paths[com_key] = Template.substitute_string(
                    template, TemplateConstants.DOLLAR_CURLY_BRACE, grid_dict.get)
            else:
                logger.warning(f"COM_ATMOS_GRIB_GRID_TMPL not found for grid {grid}")

        # GEFS-specific: Ensemble statistics path
        # Corresponds to YAML: COMIN_ATMOS_ENSSTAT_1p00 with MEMDIR='ensstat'
        if 'gefs' in self.task_config.RUN.lower():
            ensstat_dict = base_dict.copy()
            ensstat_dict['GRID'] = '1p00'
            # MEMDIR is already in base_dict for GEFS (added by _get_template_dict)

            template = self.task_config.get('COM_ATMOS_GRIB_GRID_TMPL', '')
            if template:
                com_paths['COMIN_ATMOS_ENSSTAT_1p00'] = Template.substitute_string(
                    template, TemplateConstants.DOLLAR_CURLY_BRACE, ensstat_dict.get)
            else:
                logger.warning("COM_ATMOS_GRIB_GRID_TMPL not found for COMIN_ATMOS_ENSSTAT_1p00")

        return com_paths

    def _build_gfs_list(self, cycle_vars: Dict[str, Any], com_paths: Dict[str, str],
                        arcdir: str) -> Dict[str, list]:
        """Build mkdir list and file set for GFS archiving.

        This method contains nested helper functions to build the directory list
        and file set for GFS archiving.

        Parameters
        ----------
        cycle_vars : Dict[str, Any]
            Cycle-specific variables
        com_paths : Dict[str, str]
            COM directory paths
        arcdir : str
            Archive directory path

        Returns
        -------
        Dict[str, list]
            Dictionary containing 'mkdir_list' and 'file_set'
        """

        def build_mkdir_list() -> list:
            """Build list of directories to create for GFS archiving."""
            mkdir_list = [arcdir]

            # Add fit2obs directory if enabled
            RUN = self.task_config.RUN
            if RUN == "gfs" and self.task_config.get("DO_FIT2OBS", False):
                vfyarc = os.path.join(self.task_config.ROTDIR, "vrfyarch")
                cycle_YMD = cycle_vars['cycle_YMD']
                cycle_HH = cycle_vars['cycle_HH']
                fit2obs_dir = os.path.join(vfyarc, f"{RUN}.{cycle_YMD}", cycle_HH)
                mkdir_list.append(fit2obs_dir)

            return mkdir_list

        def build_file_set() -> list:
            """Build list of files to archive for GFS."""
            file_set = []

            head = cycle_vars['head']
            cycle_YMDH = cycle_vars['cycle_YMDH']
            cycle_YMD = cycle_vars['cycle_YMD']
            cycle_HH = cycle_vars['cycle_HH']

            RUN = self.task_config.RUN
            MODE = self.task_config.get('MODE', 'cycled')
            CDUMP = self.task_config.get('CDUMP', RUN)

            # Deterministic files (not enkf)
            if "enkf" not in RUN:
                # Common deterministic files
                det_files = [
                    # Log files
                    [f"{com_paths['COMIN_ATMOS_HISTORY']}/{head}logf000.txt", f"{arcdir}/{head}logf000.txt"],
                    [f"{com_paths['COMIN_ATMOS_HISTORY']}/{head}logf001.txt", f"{arcdir}/{head}logf001.txt"],

                    # Restart files
                    [f"{com_paths['COMIN_ATMOS_HISTORY']}/{cycle_YMDH}.coupler.res",
                     f"{arcdir}/{cycle_YMDH}.coupler.res"],
                    [f"{com_paths['COMIN_ATMOS_HISTORY']}/{cycle_YMDH}.fv_core.res.nc",
                     f"{arcdir}/{cycle_YMDH}.fv_core.res.nc"],
                ]
                file_set.extend(det_files)

                # Analysis files (cycled mode)
                if MODE == "cycled":
                    det_anl_files = [
                        # Analysis files
                        [f"{com_paths['COMIN_ATMOS_ANALYSIS']}/{head}atmanl.nc",
                         f"{arcdir}/{head}atmanl.nc"],
                        [f"{com_paths['COMIN_ATMOS_ANALYSIS']}/{head}sfcanl.nc",
                         f"{arcdir}/{head}sfcanl.nc"],

                        # Radiance diagnostic files
                        [f"{com_paths['COMIN_ATMOS_ANALYSIS']}/{head}abias",
                         f"{arcdir}/{head}abias"],
                        [f"{com_paths['COMIN_ATMOS_ANALYSIS']}/{head}abias_pc",
                         f"{arcdir}/{head}abias_pc"],
                        [f"{com_paths['COMIN_ATMOS_ANALYSIS']}/{head}abias_air",
                         f"{arcdir}/{head}abias_air"],
                    ]
                    file_set.extend(det_anl_files)

                # GFS-specific files
                if RUN == "gfs":
                    # GRIB2 files for multiple grids
                    for grid in ["0p25", "0p50", "1p00"]:
                        com_key = f"COMIN_ATMOS_GRIB_{grid}"
                        if com_key in com_paths:
                            FHMAX_GFS = self.task_config.get('FHMAX_GFS', 384)
                            FHOUT_GFS = self.task_config.get('FHOUT_GFS', 3)

                            for fhr in range(0, FHMAX_GFS + 1, FHOUT_GFS):
                                fhr_str = str(fhr).zfill(3)
                                file_set.append([
                                    f"{com_paths[com_key]}/{head}pgrb2.{grid}.f{fhr_str}",
                                    f"{arcdir}/{head}pgrb2.{grid}.f{fhr_str}"
                                ])

                    # Genesis tracker files
                    if self.task_config.get('DO_GENESIS', False):
                        file_set.extend([
                            [f"{com_paths['COMIN_ATMOS_GENESIS']}/genesis.{cycle_YMDH}.dat",
                             f"{arcdir}/genesis.{cycle_YMDH}.dat"],
                        ])

                    # TC tracker files
                    if self.task_config.get('DO_TRACKER', False):
                        file_set.extend([
                            [f"{com_paths['COMIN_ATMOS_TRACK']}/atcfunix.{cycle_YMDH}",
                             f"{arcdir}/atcfunix.{cycle_YMDH}"],
                            [f"{com_paths['COMIN_ATMOS_TRACK']}/storms.{cycle_YMDH}",
                             f"{arcdir}/storms.{cycle_YMDH}"],
                        ])

                    # Fit2Obs files
                    if self.task_config.get("DO_FIT2OBS", False):
                        vfyarc = os.path.join(self.task_config.ROTDIR, "vrfyarch")
                        fit2obs_dir = os.path.join(vfyarc, f"{RUN}.{cycle_YMD}", cycle_HH)

                        file_set.extend([
                            [f"{com_paths['COMIN_OBS']}/prepbufr.{cycle_YMDH}",
                             f"{fit2obs_dir}/prepbufr.{cycle_YMDH}"],
                            [f"{com_paths['COMIN_OBS']}/prepbufr_acft.{cycle_YMDH}",
                             f"{fit2obs_dir}/prepbufr_acft.{cycle_YMDH}"],
                        ])

                # GDAS-specific files
                elif RUN == "gdas":
                    gdas_files = [
                        # Analysis increment files
                        [f"{com_paths['COMIN_ATMOS_ANALYSIS']}/{head}atminc.nc",
                         f"{arcdir}/{head}atminc.nc"],

                        # Observation files
                        [f"{com_paths['COMIN_OBS']}/{CDUMP}.t{cycle_HH}z.prepbufr",
                         f"{arcdir}/{CDUMP}.t{cycle_HH}z.prepbufr"],
                        [f"{com_paths['COMIN_OBS']}/{CDUMP}.t{cycle_HH}z.prepbufr.acft_profiles",
                         f"{arcdir}/{CDUMP}.t{cycle_HH}z.prepbufr.acft_profiles"],
                    ]
                    file_set.extend(gdas_files)

            else:  # Ensemble files (enkfgdas, enkfgfs)
                # EnKF ensemble mean and spread files
                enkf_files = [
                    [f"{com_paths['COMIN_ATMOS_ANALYSIS']}/{head}ensmean.nc",
                     f"{arcdir}/{head}ensmean.nc"],
                    [f"{com_paths['COMIN_ATMOS_ANALYSIS']}/{head}enssprd.nc",
                     f"{arcdir}/{head}enssprd.nc"],
                ]

                # Loop over ensemble members
                NMEM_ENS = self.task_config.get('NMEM_ENS', 80)
                for mem in range(1, NMEM_ENS + 1):
                    mem_str = str(mem).zfill(3)
                    enkf_files.append([
                        f"{com_paths['COMIN_ATMOS_ANALYSIS']}/mem{mem_str}/{head}atmanl.mem{mem_str}.nc",
                        f"{arcdir}/mem{mem_str}/{head}atmanl.mem{mem_str}.nc"
                    ])

                file_set.extend(enkf_files)

            return file_set

        # Call nested helper functions
        return {
            'mkdir_list': build_mkdir_list(),
            'file_set': build_file_set()
        }

    @logit(logger)
    def gfs_arcdir(self) -> Dict[str, Any]:
        """Build complete file set for GFS archiving (gfs_arcdir.yaml.j2).

        This method corresponds to gfs_arcdir.yaml.j2 and builds the complete
        file set with all logic, loops, and conditionals for GFS archiving.

        Returns
        -------
        Dict[str, Any]
            Dictionary containing:
            - cycle_vars: Cycle-specific variables
            - com_paths: All COM paths
            - file_set: List of [source, destination] file pairs
            - mkdir_list: List of directories to create
        """
        cycle_vars = self._get_cycle_vars()
        base_dict = self._get_template_dict()
        com_paths = self._calculate_com_paths(base_dict)

        arcdir = self.task_config.ARCDIR

        # Build mkdir list and file set using helper method with nested functions
        lists = self._build_gfs_list(cycle_vars, com_paths, arcdir)

        return {
            'cycle_vars': cycle_vars,
            'com_paths': com_paths,
            'file_set': lists['file_set'],
            'mkdir_list': lists['mkdir_list']
        }

    def _build_gefs_list(self, cycle_vars: Dict[str, Any], com_paths: Dict[str, str]) -> Dict[str, list]:
        """Build mkdir list and file set for GEFS archiving.

        This method contains nested helper functions to build the directory list
        and file set for GEFS archiving.

        Parameters
        ----------
        cycle_vars : Dict[str, Any]
            Cycle-specific variables
        com_paths : Dict[str, str]
            COM directory paths

        Returns
        -------
        Dict[str, list]
            Dictionary containing 'mkdir_list' and 'file_set'
        """
        gefs_arch = os.path.join(self.task_config.ROTDIR, "gefsarch")

        def build_mkdir_list() -> list:
            """Build list of directories to create for GEFS archiving."""
            return [gefs_arch]

        def build_file_set() -> list:
            """Build list of files to archive for GEFS."""
            file_set = []
            head = cycle_vars['head']

            # GEFS ensemble statistics files
            ensstat_path = com_paths.get('COMIN_ATMOS_ENSSTAT_1p00', '')

            if ensstat_path and os.path.exists(ensstat_path):
                FHMIN_GFS = self.task_config.get('FHMIN_GFS', 0)
                FHMAX_GFS = self.task_config.get('FHMAX_GFS', 384)
                FHOUT_GFS = self.task_config.get('FHOUT_GFS', 3)

                for fhr in range(FHMIN_GFS, FHMAX_GFS + FHOUT_GFS, FHOUT_GFS):
                    fhr_str = str(fhr).zfill(3)
                    source_file = f"{ensstat_path}/{head}mean.pres_.1p00.f{fhr_str}.grib2"
                    file_set.append([source_file, gefs_arch])
            else:
                if not ensstat_path:
                    logger.warning("COMIN_ATMOS_ENSSTAT_1p00 not found in com_paths")
                else:
                    logger.warning(f"COMIN_ATMOS_ENSSTAT_1p00 path does not exist: {ensstat_path}")

            return file_set

        # Call nested helper functions
        return {
            'mkdir_list': build_mkdir_list(),
            'file_set': build_file_set()
        }

    @logit(logger)
    def gefs_arcdir(self) -> Dict[str, Any]:
        """Build complete file set for GEFS archiving (gefs_arcdir.yaml.j2).

        This method corresponds to gefs_arcdir.yaml.j2 and builds the complete
        file set for GEFS ensemble forecast archiving.

        Returns
        -------
        Dict[str, Any]
            Dictionary containing:
            - cycle_vars: Cycle-specific variables
            - com_paths: All COM paths (includes COMIN_ATMOS_ENSSTAT_1p00)
            - file_set: List of [source, destination] file pairs
            - mkdir_list: List of directories to create
        """
        cycle_vars = self._get_cycle_vars()
        base_dict = self._get_template_dict()
        com_paths = self._calculate_com_paths(base_dict)

        # Build mkdir list and file set using helper method with nested functions
        lists = self._build_gefs_list(cycle_vars, com_paths)

        return {
            'cycle_vars': cycle_vars,
            'com_paths': com_paths,
            'file_set': lists['file_set'],
            'mkdir_list': lists['mkdir_list']
        }

    @logit(logger)
    def gcafs_arcdir(self) -> Dict[str, Any]:
        """Build complete file set for GCAFS archiving (gcafs_arcdir.yaml.j2).

        This method corresponds to gcafs_arcdir.yaml.j2. Currently delegates
        to GFS archiving logic as GCAFS uses similar file structure.

        Returns
        -------
        Dict[str, Any]
            Dictionary containing:
            - cycle_vars: Cycle-specific variables
            - com_paths: All COM paths
            - file_set: List of [source, destination] file pairs
            - mkdir_list: List of directories to create
        """
        # GCAFS uses same archiving structure as GFS
        # If GCAFS-specific logic is needed, implement here
        logger.info("GCAFS archiving using GFS archiving logic")
        return self.gfs_arcdir()
