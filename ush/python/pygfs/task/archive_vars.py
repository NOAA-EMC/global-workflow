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

        # Helper function to get template variables with empty string default
        def get_with_default(key):
            """Return value from base_dict, or empty string if key not found."""
            return base_dict.get(key, '')

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
                    template, TemplateConstants.DOLLAR_CURLY_BRACE, get_with_default)
            else:
                logger.warning(f"Template {template_key} not found for {com_key}")
                com_paths[com_key] = ''

        # Grid-specific paths (loop over grids: 0p25, 0p50, 1p00)
        for grid in ["0p25", "0p50", "1p00"]:
            grid_dict = base_dict.copy()
            grid_dict['GRID'] = grid

            # Helper function for grid_dict with empty string default
            def get_grid_with_default(key):
                """Return value from grid_dict, or empty string if key not found."""
                return grid_dict.get(key, '')

            template = self.task_config.get('COM_ATMOS_GRIB_GRID_TMPL', '')
            if template:
                com_key = f"COMIN_ATMOS_GRIB_{grid}"
                com_paths[com_key] = Template.substitute_string(
                    template, TemplateConstants.DOLLAR_CURLY_BRACE, get_grid_with_default)
            else:
                logger.warning(f"COM_ATMOS_GRIB_GRID_TMPL not found for grid {grid}")

        # GEFS-specific: Ensemble statistics path
        # Corresponds to YAML: COMIN_ATMOS_ENSSTAT_1p00 with MEMDIR='ensstat'
        if 'gefs' in self.task_config.RUN.lower():
            ensstat_dict = base_dict.copy()
            ensstat_dict['GRID'] = '1p00'
            # MEMDIR is already in base_dict for GEFS (added by _get_template_dict)

            # Helper function for ensstat_dict with empty string default
            def get_ensstat_with_default(key):
                """Return value from ensstat_dict, or empty string if key not found."""
                return ensstat_dict.get(key, '')

            template = self.task_config.get('COM_ATMOS_GRIB_GRID_TMPL', '')
            if template:
                com_paths['COMIN_ATMOS_ENSSTAT_1p00'] = Template.substitute_string(
                    template, TemplateConstants.DOLLAR_CURLY_BRACE, get_ensstat_with_default)
            else:
                logger.warning("COM_ATMOS_GRIB_GRID_TMPL not found for COMIN_ATMOS_ENSSTAT_1p00")

        # EnKF-specific: Analysis ensemble statistics path
        # Uses COM_ATMOS_ANALYSIS_TMPL with MEMDIR='ensstat' for enkfgdas/enkfgfs
        if 'enkf' in self.task_config.RUN.lower():
            ensstat_anl_dict = base_dict.copy()
            ensstat_anl_dict['MEMDIR'] = 'ensstat'

            # Helper function for ensstat_anl_dict with empty string default
            def get_ensstat_anl_with_default(key):
                """Return value from ensstat_anl_dict, or empty string if key not found."""
                return ensstat_anl_dict.get(key, '')

            template = self.task_config.get('COM_ATMOS_ANALYSIS_TMPL', '')
            if template:
                com_paths['COMIN_ATMOS_ANALYSIS_ENSSTAT'] = Template.substitute_string(
                    template, TemplateConstants.DOLLAR_CURLY_BRACE, get_ensstat_anl_with_default)
            else:
                logger.warning("COM_ATMOS_ANALYSIS_TMPL not found for COMIN_ATMOS_ANALYSIS_ENSSTAT")

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
                # Common deterministic files - Cyclone tracking
                det_files = []
                if com_paths.get('COMIN_ATMOS_TRACK'):
                    # TC tracker files (only if they exist)
                    atcfunix_file = f"{com_paths['COMIN_ATMOS_TRACK']}/atcfunix.{RUN}.{cycle_YMDH}"
                    if os.path.exists(atcfunix_file):
                        det_files.extend([
                            [atcfunix_file, f"{arcdir}/atcfunix.{RUN}.{cycle_YMDH}"],
                            [f"{com_paths['COMIN_ATMOS_TRACK']}/atcfunixp.{RUN}.{cycle_YMDH}",
                             f"{arcdir}/atcfunixp.{RUN}.{cycle_YMDH}"],
                        ])

                    # Basin tracking data
                    for basin in ["epac", "natl"]:
                        basin_dir = os.path.join(com_paths['COMIN_ATMOS_TRACK'], basin)
                        if os.path.exists(basin_dir):
                            det_files.append([basin_dir, f"{arcdir}/{basin}"])

                file_set.extend(det_files)

                # Analysis files (cycled mode)
                if MODE == "cycled":
                    det_anl_files = []

                    # Analysis grib file
                    if com_paths.get('COMIN_ATMOS_GRIB_1p00'):
                        det_anl_files.append([
                            f"{com_paths['COMIN_ATMOS_GRIB_1p00']}/{head}pres_a.1p00.analysis.grib2",
                            f"{arcdir}/pgbanl.{RUN}.{cycle_YMDH}.grib2"
                        ])

                    if com_paths.get('COMIN_ATMOS_ANALYSIS'):
                        # GSI or JEDI atmospheric statistics
                        if self.task_config.get('DO_JEDIATMVAR', False):
                            det_anl_files.append([
                                f"{com_paths['COMIN_ATMOS_ANALYSIS']}/{head}stat.atm.tar",
                                f"{arcdir}/atmstat.{RUN}.{cycle_YMDH}"
                            ])
                        else:
                            det_anl_files.append([
                                f"{com_paths['COMIN_ATMOS_ANALYSIS']}/{head}gsistat.txt",
                                f"{arcdir}/gsistat.{RUN}.{cycle_YMDH}"
                            ])

                    # Snow DA statistics
                    if self.task_config.get('DO_JEDISNOWDA', False) and com_paths.get('COMIN_SNOW_ANALYSIS'):
                        det_anl_files.append([
                            f"{com_paths['COMIN_SNOW_ANALYSIS']}/{head}snow_analysis.ioda_hofx.tar",
                            f"{arcdir}/snowstat.{RUN}.{cycle_YMDH}.tar"
                        ])

                    # Aerosol DA statistics
                    if self.task_config.get('DO_AERO_ANL', False) and com_paths.get('COMIN_CHEM_ANALYSIS'):
                        det_anl_files.append([
                            f"{com_paths['COMIN_CHEM_ANALYSIS']}/{head}aerostat.tgz",
                            f"{arcdir}/aerostat.{RUN}.{cycle_YMDH}.tgz"
                        ])

                    # Aerosol observation files
                    if self.task_config.get('DO_PREP_OBS_AERO', False) and com_paths.get('COMIN_OBS'):
                        det_anl_files.extend([
                            [f"{com_paths['COMIN_OBS']}/{head}aeroobs",
                             f"{arcdir}/aeroobs.{RUN}.{cycle_YMDH}"],
                            [f"{com_paths['COMIN_OBS']}/{head}aeroawobs",
                             f"{arcdir}/aeroawobs.{RUN}.{cycle_YMDH}"],
                        ])

                    file_set.extend(det_anl_files)

                # GFS-specific files
                if RUN == "gfs":
                    gfs_files = []

                    # GRIB2 forecast files (only 1p00 grid for archive)
                    if com_paths.get('COMIN_ATMOS_GRIB_1p00'):
                        FHMAX_GFS = self.task_config.get('FHMAX_GFS', 384)
                        FHOUT_GFS = self.task_config.get('FHOUT_GFS', 3)

                        for fhr in range(0, FHMAX_GFS + 1, FHOUT_GFS):
                            fhr_str = str(fhr).zfill(3)
                            fhr_archive = str(fhr).zfill(2)  # Archive uses 2-digit format
                            gfs_files.append([
                                f"{com_paths['COMIN_ATMOS_GRIB_1p00']}/{head}pres_a.1p00.f{fhr_str}.grib2",
                                f"{arcdir}/pgbf{fhr_archive}.{RUN}.{cycle_YMDH}.grib2"
                            ])

                    # Cyclone genesis data (only if files exist)
                    if com_paths.get('COMIN_ATMOS_GENESIS'):
                        genesis_file = f"{com_paths['COMIN_ATMOS_GENESIS']}/storms.gfso.atcf_gen.{cycle_YMDH}"
                        if os.path.exists(genesis_file):
                            gfs_files.extend([
                                [genesis_file, f"{arcdir}/storms.gfso.atcf_gen.{cycle_YMDH}"],
                                [f"{com_paths['COMIN_ATMOS_GENESIS']}/storms.gfso.atcf_gen.altg.{cycle_YMDH}",
                                 f"{arcdir}/storms.gfso.atcf_gen.altg.{cycle_YMDH}"],
                            ])

                        trak_file = f"{com_paths['COMIN_ATMOS_GENESIS']}/trak.gfso.atcfunix.{cycle_YMDH}"
                        if os.path.exists(trak_file):
                            gfs_files.extend([
                                [trak_file, f"{arcdir}/trak.gfso.atcfunix.{cycle_YMDH}"],
                                [f"{com_paths['COMIN_ATMOS_GENESIS']}/trak.gfso.atcfunix.altg.{cycle_YMDH}",
                                 f"{arcdir}/trak.gfso.atcfunix.altg.{cycle_YMDH}"],
                            ])

                    # Fit2Obs files (atm and sfc forecast history files)
                    if self.task_config.get("DO_FIT2OBS", False):
                        if com_paths.get('COMIN_ATMOS_HISTORY'):
                            vfyarc = os.path.join(self.task_config.ROTDIR, "vrfyarch")
                            fit2obs_dir = os.path.join(vfyarc, f"{RUN}.{cycle_YMD}", cycle_HH)

                            FHMAX_FITS = self.task_config.get('FHMAX_FITS', 180)
                            for fhr in range(0, FHMAX_FITS + 1, 6):
                                fhr_str = str(fhr).zfill(3)
                                sfcfile = f"{head}sfc.f{fhr_str}.nc"
                                sigfile = f"{head}atm.f{fhr_str}.nc"
                                gfs_files.extend([
                                    [f"{com_paths['COMIN_ATMOS_HISTORY']}/{sfcfile}",
                                     f"{fit2obs_dir}/{sfcfile}"],
                                    [f"{com_paths['COMIN_ATMOS_HISTORY']}/{sigfile}",
                                     f"{fit2obs_dir}/{sigfile}"],
                                ])
                        else:
                            logger.warning("DO_FIT2OBS enabled but COMIN_ATMOS_HISTORY path not available")

                    file_set.extend(gfs_files)

                # GDAS-specific files
                elif RUN == "gdas":
                    gdas_files = []

                    # GRIB2 forecast files
                    if com_paths.get('COMIN_ATMOS_GRIB_1p00'):
                        FHMAX = self.task_config.get('FHMAX', 9)
                        FHOUT = self.task_config.get('FHOUT', 3)

                        for fhr in range(0, FHMAX + 1, FHOUT):
                            fhr_str = str(fhr).zfill(3)
                            fhr_archive = str(fhr).zfill(2)  # Archive uses 2-digit format
                            gdas_files.append([
                                f"{com_paths['COMIN_ATMOS_GRIB_1p00']}/{head}pres_a.1p00.f{fhr_str}.grib2",
                                f"{arcdir}/pgbf{fhr_archive}.{RUN}.{cycle_YMDH}.grib2"
                            ])

                    # Radiance bias correction files
                    if com_paths.get('COMIN_ATMOS_ANALYSIS'):
                        gdas_files.extend([
                            [f"{com_paths['COMIN_ATMOS_ANALYSIS']}/{head}abias.txt",
                             f"{arcdir}/abias.{RUN}.{cycle_YMDH}"],
                            [f"{com_paths['COMIN_ATMOS_ANALYSIS']}/{head}abias_pc.txt",
                             f"{arcdir}/abias_pc.{RUN}.{cycle_YMDH}"],
                            [f"{com_paths['COMIN_ATMOS_ANALYSIS']}/{head}abias_air.txt",
                             f"{arcdir}/abias_air.{RUN}.{cycle_YMDH}"],
                            [f"{com_paths['COMIN_ATMOS_ANALYSIS']}/{head}abias_int.txt",
                             f"{arcdir}/abias_int.{RUN}.{cycle_YMDH}"],
                            [f"{com_paths['COMIN_ATMOS_ANALYSIS']}/{head}analysis.dtf.a006.nc",
                             f"{arcdir}/dtfanl.{RUN}.{cycle_YMDH}.nc"],
                        ])

                    file_set.extend(gdas_files)

            else:  # Ensemble files (enkfgdas, enkfgfs) - only statistics archived
                enkf_files = []

                # EnKF ensemble statistics (from ensstat directory)
                if com_paths.get('COMIN_ATMOS_ANALYSIS_ENSSTAT'):
                    if self.task_config.get('DO_JEDIATMENS', False):
                        # JEDI ensemble statistics
                        enkf_files.append([
                            f"{com_paths['COMIN_ATMOS_ANALYSIS_ENSSTAT']}/{head}stat.atm.tar",
                            f"{arcdir}/atmensstat.{RUN}.{cycle_YMDH}"
                        ])
                    else:
                        # GSI EnKF statistics
                        enkf_files.extend([
                            [f"{com_paths['COMIN_ATMOS_ANALYSIS_ENSSTAT']}/{head}enkfstat.txt",
                             f"{arcdir}/enkfstat.{RUN}.{cycle_YMDH}"],
                            [f"{com_paths['COMIN_ATMOS_ANALYSIS_ENSSTAT']}/{head}gsistat.ensmean.txt",
                             f"{arcdir}/gsistat.{RUN}.{cycle_YMDH}.ensmean"],
                        ])
                    file_set.extend(enkf_files)
                else:
                    logger.warning("COMIN_ATMOS_ANALYSIS_ENSSTAT path not available for EnKF, skipping ensemble statistics")

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

    def _build_gcafs_list(self, cycle_vars: Dict[str, Any], com_paths: Dict[str, str],
                          arcdir: str) -> Dict[str, list]:
        """Build mkdir list and file set for GCAFS archiving.

        This method contains nested helper functions to build the directory list
        and file set for GCAFS archiving. GCAFS is simpler than GFS - mainly
        forecast files and optional aerosol files.

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
            """Build list of directories to create for GCAFS archiving."""
            mkdir_list = [arcdir]

            # Add fit2obs directory if enabled
            RUN = self.task_config.RUN
            if self.task_config.get("DO_FIT2OBS", False):
                vfyarc = os.path.join(self.task_config.ROTDIR, "vrfyarch")
                cycle_YMD = cycle_vars['cycle_YMD']
                cycle_HH = cycle_vars['cycle_HH']
                fit2obs_dir = os.path.join(vfyarc, f"{RUN}.{cycle_YMD}", cycle_HH)
                mkdir_list.append(fit2obs_dir)

            return mkdir_list

        def build_file_set() -> list:
            """Build list of files to archive for GCAFS."""
            file_set = []

            head = cycle_vars['head']
            cycle_YMDH = cycle_vars['cycle_YMDH']
            cycle_YMD = cycle_vars['cycle_YMD']
            cycle_HH = cycle_vars['cycle_HH']

            RUN = self.task_config.RUN
            MODE = self.task_config.get('MODE', 'cycled')

            # Deterministic files (not enkf)
            if "enkf" not in RUN:
                # Analysis files (cycled mode) - only aerosol for GCAFS
                if MODE == "cycled":
                    det_anl_files = []

                    # Aerosol DA statistics
                    if self.task_config.get('DO_AERO_ANL', False) and com_paths.get('COMIN_CHEM_ANALYSIS'):
                        det_anl_files.append([
                            f"{com_paths['COMIN_CHEM_ANALYSIS']}/{head}aerostat.tgz",
                            f"{arcdir}/aerostat.{RUN}.{cycle_YMDH}.tgz"
                        ])

                    # Aerosol observation files
                    if self.task_config.get('DO_PREP_OBS_AERO', False) and com_paths.get('COMIN_OBS'):
                        det_anl_files.extend([
                            [f"{com_paths['COMIN_OBS']}/{head}aeroobs",
                             f"{arcdir}/aeroobs.{RUN}.{cycle_YMDH}"],
                            [f"{com_paths['COMIN_OBS']}/{head}aeroawobs",
                             f"{arcdir}/aeroawobs.{RUN}.{cycle_YMDH}"],
                        ])

                    file_set.extend(det_anl_files)

                # GCAFS-specific forecast files
                if RUN == "gcafs":
                    gcafs_files = []

                    # GRIB2 forecast files (only 1p00 grid for archive)
                    if com_paths.get('COMIN_ATMOS_GRIB_1p00'):
                        FHMAX_GFS = self.task_config.get('FHMAX_GFS', 384)
                        FHOUT_GFS = self.task_config.get('FHOUT_GFS', 3)

                        for fhr in range(0, FHMAX_GFS + 1, FHOUT_GFS):
                            fhr_str = str(fhr).zfill(3)
                            fhr_archive = str(fhr).zfill(2)  # Archive uses 2-digit format
                            gcafs_files.append([
                                f"{com_paths['COMIN_ATMOS_GRIB_1p00']}/{head}pres_a.1p00.f{fhr_str}.grib2",
                                f"{arcdir}/pgbf{fhr_archive}.{RUN}.{cycle_YMDH}.grib2"
                            ])

                    # Fit2Obs files (atm and sfc forecast history files)
                    if self.task_config.get("DO_FIT2OBS", False):
                        if com_paths.get('COMIN_ATMOS_HISTORY'):
                            vfyarc = os.path.join(self.task_config.ROTDIR, "vrfyarch")
                            fit2obs_dir = os.path.join(vfyarc, f"{RUN}.{cycle_YMD}", cycle_HH)

                            FHMAX_FITS = self.task_config.get('FHMAX_FITS', 180)
                            for fhr in range(0, FHMAX_FITS + 1, 6):
                                fhr_str = str(fhr).zfill(3)
                                sfcfile = f"{head}sfc.f{fhr_str}.nc"
                                sigfile = f"{head}atm.f{fhr_str}.nc"
                                gcafs_files.extend([
                                    [f"{com_paths['COMIN_ATMOS_HISTORY']}/{sfcfile}",
                                     f"{fit2obs_dir}/{sfcfile}"],
                                    [f"{com_paths['COMIN_ATMOS_HISTORY']}/{sigfile}",
                                     f"{fit2obs_dir}/{sigfile}"],
                                ])
                        else:
                            logger.warning("DO_FIT2OBS enabled but COMIN_ATMOS_HISTORY path not available")

                    file_set.extend(gcafs_files)

                # GCDAS-specific forecast files
                elif RUN == "gcdas":
                    gcdas_files = []

                    # GRIB2 forecast files
                    if com_paths.get('COMIN_ATMOS_GRIB_1p00'):
                        FHMAX = self.task_config.get('FHMAX', 9)
                        FHOUT = self.task_config.get('FHOUT', 3)

                        for fhr in range(0, FHMAX + 1, FHOUT):
                            fhr_str = str(fhr).zfill(3)
                            fhr_archive = str(fhr).zfill(2)  # Archive uses 2-digit format
                            gcdas_files.append([
                                f"{com_paths['COMIN_ATMOS_GRIB_1p00']}/{head}pres_a.1p00.f{fhr_str}.grib2",
                                f"{arcdir}/pgbf{fhr_archive}.{RUN}.{cycle_YMDH}.grib2"
                            ])

                    file_set.extend(gcdas_files)

            else:  # Ensemble files (enkfgcafs, enkfgcdas) - only statistics archived
                enkf_files = []

                # EnKF ensemble statistics (from ensstat directory)
                if com_paths.get('COMIN_ATMOS_ANALYSIS_ENSSTAT'):
                    if self.task_config.get('DO_JEDIATMENS', False):
                        # JEDI ensemble statistics - NOTE: GCAFS uses different filename
                        enkf_files.append([
                            f"{com_paths['COMIN_ATMOS_ANALYSIS_ENSSTAT']}/{head}atmensstat",
                            f"{arcdir}/atmensstat.{RUN}.{cycle_YMDH}"
                        ])
                    else:
                        # GSI EnKF statistics
                        enkf_files.extend([
                            [f"{com_paths['COMIN_ATMOS_ANALYSIS_ENSSTAT']}/{head}enkfstat.txt",
                             f"{arcdir}/enkfstat.{RUN}.{cycle_YMDH}"],
                            [f"{com_paths['COMIN_ATMOS_ANALYSIS_ENSSTAT']}/{head}gsistat.ensmean.txt",
                             f"{arcdir}/gsistat.{RUN}.{cycle_YMDH}.ensmean"],
                        ])
                    file_set.extend(enkf_files)
                else:
                    logger.warning("COMIN_ATMOS_ANALYSIS_ENSSTAT path not available for EnKF, skipping ensemble statistics")

            return file_set

        # Call nested helper functions
        return {
            'mkdir_list': build_mkdir_list(),
            'file_set': build_file_set()
        }

    @logit(logger)
    def gcafs_arcdir(self) -> Dict[str, Any]:
        """Build complete file set for GCAFS archiving (gcafs_arcdir.yaml.j2).

        This method corresponds to gcafs_arcdir.yaml.j2. GCAFS is simpler than GFS,
        archiving mainly forecast files and optional aerosol files.

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
        lists = self._build_gcafs_list(cycle_vars, com_paths, arcdir)

        return {
            'cycle_vars': cycle_vars,
            'com_paths': com_paths,
            'file_set': lists['file_set'],
            'mkdir_list': lists['mkdir_list']
        }
