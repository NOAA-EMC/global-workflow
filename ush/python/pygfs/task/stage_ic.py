#!/usr/bin/env python3
"""
Stage Initial Conditions (IC) Task

Overview
--------
This module constructs cycle and member-specific COM directory path variables
required for initial conditions for the Stage IC task.

High-Level Responsibilities
---------------------------
1. Create cycle variables for:
   - GCAFS (cycled or forecast-only)
   - GFS (deterministic and enkfgdas variant)
   - GEFS (ensemble: real-time and offline modes)

   1.1 Provide a method (calculate_general_cycle_variables) that computes
       variables common to all supported applications (time windows, cycle
       date strings, template substitution dictionaries, etc.).

   1.2 Provide a method (calculate_member) that derives
       application / RUN specific attributes (member indexing rules, GEFSTYPE
       handling etc.).

2. Provide separate methods for populating member-level COM path variables:
   - calculate_member_com_paths_gfs
   - calculate_member_com_paths_gefs_offline
   - calculate_member_com_paths_gefs_rt
   - calculate_member_com_paths_gcafs

Key Methods
-----------
calculate_member():
  Establishes RUN-dependent settings (member ranges, GEFSTYPE logic, replay /
  IAU offsets, and rRUN mapping for coupled cases).

calculate_general_cycle_variables():
  Builds on case-specific variables and computes:
    - Half-window assimilation times
    - Current / previous cycle formatted strings (YMD, HH)
    - Model start date logic (IAU vs replay vs standard start)
    - Template substitution dictionaries for current and previous cycles

calculate_member_com_paths_*():
  For a given member index, formats and injects member-specific COM directory
  paths derived from template variables defined in the master configuration.

execute_stage(stage_dict):
  Renders a YAML (Jinja2) template describing required files, then synchronizes
  (copies/links) those files into the ROTDIR.

Extensibility Notes
-------------------
To add a new application:
  1. Extend calculate_member() for RUN-specific member logic.
  2. Implement a new calculate_member_com_paths_<app>() variant if COM path
     semantics differ from existing cases.
  3. Add dispatch logic inside calculate_stage_vars().

Performance Considerations
--------------------------
The staging loop processes each member sequentially. If significant scaling
issues arise for very large ensembles, a future enhancement could introduce
parallelization or batched FileHandler operations.

Logging
-------
All public operational methods are decorated with @logit(logger, ...),
providing entry/exit logging.

"""
import os
from logging import getLogger
from typing import Any, Dict
from datetime import timedelta
from wxflow import FileHandler, Task, logit, parse_j2yaml, AttrDict

logger = getLogger(__name__.split('.')[-1])


class Stage(Task):
    """Task to stage initial conditions
    """

    @logit(logger, name="Stage")
    def __init__(self, config: Dict[str, Any]) -> None:
        """Constructor for the Stage task
        The constructor is responsible for collecting necessary settings based on
        the runtime options and RUN.

        Parameters
        ----------
        config : Dict[str, Any]
            Incoming configuration for the task from the environment

        Returns
        -------
        None
        """
        super().__init__(config)

        # assign rRUN to RUN
        self.task_config.rRUN = self.task_config.RUN
        # case specific rRUN
        if self.task_config.RUN in ['gfs', 'gcdas', 'enkfgdas']:
            self.task_config.rRUN = "gdas"
        else:
            # RUN not gfs, gcdas, or enkfgdas; leave rRUN unchanged and continue
            logger.debug("No rRUN remap applied: RUN='%s' (rRUN stays '%s')", self.task_config.RUN, self.task_config.rRUN)

        if "OCNRES" in self.task_config:
            self.task_config.OCNRES = f"{int(self.task_config.OCNRES):03d}"

        # START_ICE_FROM_ANA logic (only if DO_ICE is True)
        if getattr(self.task_config, "DO_ICE", False):
            self.task_config.START_ICE_FROM_ANA = False
            if getattr(self.task_config, "DO_JEDIOCNVAR", False) and self.task_config.RUN == "gdas":
                self.task_config.START_ICE_FROM_ANA = True
            if getattr(self.task_config, "DO_STARTMEM_FROM_JEDIICE", False) and self.task_config.RUN == "enkfgdas":
                self.task_config.START_ICE_FROM_ANA = True

                # Calculate half window variables
        self.task_config.half_window = self.task_config.assim_freq // 2

        # Calculate current cycle variables
        if self.task_config.current_cycle:
            if self.task_config.DOIAU and self.task_config.MODE == "cycled":
                self.task_config.model_start_date_current_cycle = self.task_config.current_cycle + timedelta(hours=-self.task_config.half_window)
            else:
                if 'REPLAY_ICS' in self.task_config and self.task_config['REPLAY_ICS']:
                    self.task_config.model_start_date_current_cycle = self.task_config.current_cycle + timedelta(hours=self.task_config.half_window)
                else:
                    self.task_config.model_start_date_current_cycle = self.task_config.current_cycle

            # Calculate YMD and HH formats
            self.task_config.m_prefix = self.task_config.model_start_date_current_cycle.strftime("%Y%m%d.%H0000")

        # Calculate previous cycle variables
        if self.task_config.previous_cycle:
            self.task_config.m_index = self.task_config.current_cycle.hour // self.task_config.assim_freq
            self.task_config.p_prefix = self.task_config.previous_cycle.strftime("%Y%m%d.%H0000")

        # Define cycle directories to update com paths
        self.task_config.current_cycle_dict = {
            "${ROTDIR}": self.task_config.ROTDIR,
            "${RUN}": self.task_config.RUN,
            "${YMD}": self.task_config.current_cycle.strftime("%Y%m%d"),
            "${HH}": self.task_config.current_cycle.strftime("%H"),
        }
        self.task_config.previous_cycle_dict = {
            "${ROTDIR}": self.task_config.ROTDIR,
            "${RUN}": self.task_config.RUN,
            "${YMD}": self.task_config.previous_cycle.strftime("%Y%m%d"),
            "${HH}": self.task_config.previous_cycle.strftime("%H"),
        }

    @logit(logger)
    def execute_stage(self, stage_dict: Dict[str, Any]) -> None:
        """Perform local staging of initial condition files.

        Parameters
        ----------
        stage_dict : Dict[str, Any]
            Configuration dictionary

        Returns
        -------
        None
        """

        if not os.path.isdir(stage_dict.ROTDIR):
            raise FileNotFoundError(f"FATAL ERROR: The ROTDIR ({stage_dict.ROTDIR}) does not exist!")

        # Add the os.path.exists function to the dict for yaml parsing
        stage_dict['path_exists'] = os.path.exists

        # Parse staging yaml to get list of files to stage
        stage_set = parse_j2yaml(self.task_config.STAGE_IC_YAML_TMPL, stage_dict, allow_missing=False)

        # stage files to ROTDIR
        for key in stage_set.keys():
            FileHandler(stage_set[key]).sync()

    @logit(logger)
    def calculate_member(self) -> None:
        """
        Calculate member for master YAML templates
        - gfs
        - gefs
        - gcafs
        - enkfgdas
        - gdas

        Returns
        -------
        None
          Updates the task_config with member-specific variables for staging.
        """
        # Assign last_mem and first_mem to run members
        self.task_config.last_mem = self.task_config.NMEM_ENS
        if self.task_config.RUN in ['enkfgdas']:
            self.task_config.first_mem = 1
        elif self.task_config.RUN in ['gefs']:  # GEFS Ensemble RUN (both regular and RT)
            self.task_config.first_mem = 0
            if self.task_config.GEFSTYPE == "gefs-offline":
                pass
            elif self.task_config.GEFSTYPE == "gefs-real-time":
                # select the relevant member for each GEFS member from GFS outputs
                self.task_config.cyc_ranges = [list(range(1, 31)), list(range(21, 51)),
                                               list(range(41, 71)), list(range(61, 81)) + list(range(1, 11))]
            else:
                # Error handling for unknown GEFSTYPE
                valid_types = ['gefs-offline', 'gefs-real-time']
                raise ValueError(f"Invalid GEFSTYPE '{self.task_config.GEFSTYPE}' for RUN '{self.task_config.RUN}'. "
                                 f"Valid options are: {valid_types}")
        else:  # Deterministic RUN (GFS and GCAFS)
            self.task_config.first_mem = -1
            self.task_config.last_mem = -1

    @logit(logger)
    def calculate_member_com_paths_gfs(self, memdir) -> None:
        """
        Calculate member COM paths for GFS

        Parameters
        ----------
        memdir : int
          The member directory number

        Returns
        -------
        Dict[str, Any]
          Updates the task_config with member-specific COM paths for GFS.
        """
        self.calculate_member()
        memdir = f"mem{memdir:03d}" if memdir >= 0 else ''
        current_cycle_mem_dict = {**self.task_config.current_cycle_dict, "${MEMDIR}": memdir}
        previous_cycle_mem_dict = {**self.task_config.previous_cycle_dict, "${MEMDIR}": memdir, "${RUN}": self.task_config.rRUN}

        self.task_config['COMIN_ATMOS_INPUT_MEM'] = self._replace_template_vars(
            getattr(self.task_config, 'COM_ATMOS_INPUT_TMPL', ''), current_cycle_mem_dict)
        self.task_config['COMOUT_ATMOS_INPUT_MEM'] = self._replace_template_vars(
            getattr(self.task_config, 'COM_ATMOS_INPUT_TMPL', ''), current_cycle_mem_dict)
        self.task_config['COMOUT_ATMOS_RESTART_PREV_MEM'] = self._replace_template_vars(
            getattr(self.task_config, 'COM_ATMOS_RESTART_TMPL', ''), previous_cycle_mem_dict)
        self.task_config['COMOUT_ATMOS_RESTART_MEM'] = self._replace_template_vars(
            getattr(self.task_config, 'COM_ATMOS_RESTART_TMPL', ''), current_cycle_mem_dict)
        self.task_config['COMOUT_ATMOS_ANALYSIS_MEM'] = self._replace_template_vars(
            getattr(self.task_config, 'COM_ATMOS_ANALYSIS_TMPL', ''), current_cycle_mem_dict)
        self.task_config['COMOUT_ICE_ANALYSIS_MEM'] = self._replace_template_vars(
            getattr(self.task_config, 'COM_ICE_ANALYSIS_TMPL', ''), current_cycle_mem_dict)
        self.task_config['COMOUT_ICE_RESTART_PREV_MEM'] = self._replace_template_vars(
            getattr(self.task_config, 'COM_ICE_RESTART_TMPL', ''), previous_cycle_mem_dict)
        self.task_config['COMOUT_OCEAN_RESTART_PREV_MEM'] = self._replace_template_vars(
            getattr(self.task_config, 'COM_OCEAN_RESTART_TMPL', ''), previous_cycle_mem_dict)
        self.task_config['COMOUT_OCEAN_ANALYSIS_MEM'] = self._replace_template_vars(
            getattr(self.task_config, 'COM_OCEAN_ANALYSIS_TMPL', ''), current_cycle_mem_dict)
        self.task_config['COMOUT_MED_RESTART_PREV_MEM'] = self._replace_template_vars(
            getattr(self.task_config, 'COM_MED_RESTART_TMPL', ''), previous_cycle_mem_dict)
        self.task_config['COMOUT_CHEM_ANALYSIS_MEM'] = self._replace_template_vars(
            getattr(self.task_config, 'COM_CHEM_ANALYSIS_TMPL', ''), current_cycle_mem_dict)
        self.task_config['COMOUT_WAVE_RESTART_PREV_MEM'] = self._replace_template_vars(
            getattr(self.task_config, 'COM_WAVE_RESTART_TMPL', ''), previous_cycle_mem_dict)

    @logit(logger)
    def calculate_member_com_paths_gefs_offline(self, memdir) -> None:
        """
        Calculate member COM paths for GEFS offline

        Parameters
        ----------
        memdir : int
          The member directory number

        Returns
        -------
        Dict[str, Any]
          Updates the task_config with member-specific COM paths for GEFS offline.
        """
        self.calculate_member()
        memdir = f"mem{memdir:03d}" if memdir >= 0 else ''
        current_cycle = {**self.task_config.current_cycle_dict, "${MEMDIR}": memdir}
        previous_cycle = {**self.task_config.previous_cycle_dict, "${MEMDIR}": memdir}

        self.task_config['COMIN_ATMOS_INPUT_MEM'] = self._replace_template_vars(getattr(self.task_config, 'COM_ATMOS_INPUT_TMPL', ''), current_cycle)
        self.task_config['COMOUT_ATMOS_INPUT_MEM'] = self._replace_template_vars(getattr(self.task_config, 'COM_ATMOS_INPUT_TMPL', ''), current_cycle)
        self.task_config['COMOUT_ATMOS_RESTART_PREV_MEM'] = self._replace_template_vars(getattr(self.task_config, 'COM_ATMOS_RESTART_TMPL', ''), previous_cycle)
        self.task_config['COMOUT_ATMOS_RESTART_MEM'] = self._replace_template_vars(getattr(self.task_config, 'COM_ATMOS_RESTART_TMPL', ''), current_cycle)
        self.task_config['COMOUT_ATMOS_ANALYSIS_MEM'] = self._replace_template_vars(getattr(self.task_config, 'COM_ATMOS_ANALYSIS_TMPL', ''), current_cycle)
        self.task_config['COMOUT_ATMOS_HISTORY_MEM'] = self._replace_template_vars(getattr(self.task_config, 'COM_ATMOS_HISTORY_TMPL', ''), previous_cycle)
        self.task_config['COMOUT_ICE_ANALYSIS_MEM'] = self._replace_template_vars(getattr(self.task_config, 'COM_ICE_ANALYSIS_TMPL', ''), current_cycle)
        self.task_config['COMOUT_ICE_RESTART_PREV_MEM'] = self._replace_template_vars(getattr(self.task_config, 'COM_ICE_RESTART_TMPL', ''), previous_cycle)
        self.task_config['COMOUT_OCEAN_RESTART_PREV_MEM'] = self._replace_template_vars(getattr(self.task_config, 'COM_OCEAN_RESTART_TMPL', ''), previous_cycle)
        self.task_config['COMOUT_OCEAN_ANALYSIS_MEM'] = self._replace_template_vars(getattr(self.task_config, 'COM_OCEAN_ANALYSIS_TMPL', ''), current_cycle)
        self.task_config['COMOUT_MED_RESTART_PREV_MEM'] = self._replace_template_vars(getattr(self.task_config, 'COM_MED_RESTART_TMPL', ''), previous_cycle)
        self.task_config['COMOUT_WAVE_RESTART_PREV_MEM'] = self._replace_template_vars(getattr(self.task_config, 'COM_WAVE_RESTART_TMPL', ''), previous_cycle)

    @logit(logger)
    def calculate_member_com_paths_gefs_rt(self, memdir) -> None:
        """
        Calculate member COM paths for GEFS real-time

        Parameters
        ----------
        memdir : int
          The member directory number

        Returns
        -------
        Dict[str, Any]
          Updates the task_config with member-specific COM paths for GEFS real-time.
        """
        self.calculate_member()
        if memdir != 0:
            self.task_config.gfs_member = self.task_config.cyc_ranges[self.task_config.m_index][(memdir - 1)]
        memdir = f"mem{memdir:03d}" if memdir >= 0 else ''
        current_cycle = {**self.task_config.current_cycle_dict, "${MEMDIR}": memdir}
        previous_cycle = {**self.task_config.previous_cycle_dict, "${MEMDIR}": memdir}

        self.task_config['COMIN_ATMOS_INPUT_MEM'] = self._replace_template_vars(getattr(self.task_config, 'COM_ATMOS_INPUT_TMPL', ''), current_cycle)
        self.task_config['COMOUT_ATMOS_INPUT_MEM'] = self._replace_template_vars(getattr(self.task_config, 'COM_ATMOS_INPUT_TMPL', ''), current_cycle)
        self.task_config['COMOUT_ATMOS_RESTART_PREV_MEM'] = self._replace_template_vars(getattr(self.task_config, 'COM_ATMOS_RESTART_TMPL', ''), previous_cycle)
        self.task_config['COMOUT_ATMOS_ANALYSIS_MEM'] = self._replace_template_vars(getattr(self.task_config, 'COM_ATMOS_ANALYSIS_TMPL', ''), current_cycle)
        self.task_config['COMOUT_ATMOS_HISTORY_MEM'] = self._replace_template_vars(getattr(self.task_config, 'COM_ATMOS_HISTORY_TMPL', ''), previous_cycle)
        self.task_config['COMOUT_ICE_ANALYSIS_MEM'] = self._replace_template_vars(getattr(self.task_config, 'COM_ICE_ANALYSIS_TMPL', ''), current_cycle)
        self.task_config['COMOUT_ICE_RESTART_PREV_MEM'] = self._replace_template_vars(getattr(self.task_config, 'COM_ICE_RESTART_TMPL', ''), previous_cycle)
        self.task_config['COMOUT_OCEAN_RESTART_PREV_MEM'] = self._replace_template_vars(getattr(self.task_config, 'COM_OCEAN_RESTART_TMPL', ''), previous_cycle)
        self.task_config['COMOUT_OCEAN_ANALYSIS_MEM'] = self._replace_template_vars(getattr(self.task_config, 'COM_OCEAN_ANALYSIS_TMPL', ''), current_cycle)
        self.task_config['COMOUT_MED_RESTART_PREV_MEM'] = self._replace_template_vars(getattr(self.task_config, 'COM_MED_RESTART_TMPL', ''), previous_cycle)
        self.task_config['COMOUT_WAVE_RESTART_PREV_MEM'] = self._replace_template_vars(getattr(self.task_config, 'COM_WAVE_RESTART_TMPL', ''), previous_cycle)

    @logit(logger)
    def calculate_member_com_paths_gcafs(self, memdir) -> None:
        """
        Calculate member COM paths for GCAFS

        Parameters
        ----------
        memdir : int
          The member directory number

        Returns
        -------
        Dict[str, Any]
          Updates the task_config with member-specific COM paths for GCAFS.
        """
        self.calculate_member()
        memdir = f"mem{memdir:03d}" if memdir >= 0 else ''

        # Three contexts:
        # - current (RUN) for outputs
        current_cycle_in = {**self.task_config.current_cycle_dict, "${MEMDIR}": memdir, "${RUN}": self.task_config.rRUN}
        # - current (rRUN) for inputs
        current_cycle = {**current_cycle_in, "${RUN}": self.task_config.rRUN}
        # - previous (rRUN) for prev-cycle restarts
        previous_cycle = {**self.task_config.previous_cycle_dict, "${MEMDIR}": memdir, "${RUN}": self.task_config.rRUN}

        self.task_config['COMIN_ATMOS_INPUT_MEM'] = self._replace_template_vars(getattr(self.task_config, 'COM_ATMOS_INPUT_TMPL', ''), current_cycle_in)
        self.task_config['COMOUT_ATMOS_INPUT_MEM'] = self._replace_template_vars(getattr(self.task_config, 'COM_ATMOS_INPUT_TMPL', ''), current_cycle)
        self.task_config['COMOUT_ATMOS_RESTART_PREV_MEM'] = self._replace_template_vars(getattr(self.task_config, 'COM_ATMOS_RESTART_TMPL', ''), previous_cycle)
        self.task_config['COMOUT_ATMOS_RESTART_MEM'] = self._replace_template_vars(getattr(self.task_config, 'COM_ATMOS_RESTART_TMPL', ''), current_cycle)
        self.task_config['COMOUT_ATMOS_ANALYSIS_MEM'] = self._replace_template_vars(getattr(self.task_config, 'COM_ATMOS_ANALYSIS_TMPL', ''), current_cycle)
        self.task_config['COMOUT_ICE_ANALYSIS_MEM'] = self._replace_template_vars(getattr(self.task_config, 'COM_ICE_ANALYSIS_TMPL', ''), current_cycle)
        self.task_config['COMOUT_ICE_RESTART_PREV_MEM'] = self._replace_template_vars(getattr(self.task_config, 'COM_ICE_RESTART_TMPL', ''), previous_cycle)
        self.task_config['COMOUT_OCEAN_RESTART_PREV_MEM'] = self._replace_template_vars(getattr(self.task_config, 'COM_OCEAN_RESTART_TMPL', ''), previous_cycle)
        self.task_config['COMOUT_OCEAN_ANALYSIS_MEM'] = self._replace_template_vars(getattr(self.task_config, 'COM_OCEAN_ANALYSIS_TMPL', ''), current_cycle)
        self.task_config['COMOUT_MED_RESTART_PREV_MEM'] = self._replace_template_vars(getattr(self.task_config, 'COM_MED_RESTART_TMPL', ''), previous_cycle)
        self.task_config['COMOUT_WAVE_RESTART_PREV_MEM'] = self._replace_template_vars(getattr(self.task_config, 'COM_WAVE_RESTART_TMPL', ''), previous_cycle)

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
        replaced_com = template
        for var, value in var_dict.items():
            replaced_com = replaced_com.replace(var, value)
        return replaced_com

    @logit(logger)
    def execute_stage_member(self) -> None:
        """
        Prepare all required staging variables used to
        locate and sync files defined in the master YAML templates.
        """
        self.calculate_member()
        run = getattr(self.task_config, 'RUN', None)
        for memdir in range(self.task_config.first_mem, self.task_config.last_mem + 1):
            self.task_config.memdir = memdir
            if run == 'gefs':
                gefstype = getattr(self.task_config, 'GEFSTYPE', None)
                if gefstype == 'gefs-real-time':
                    self.calculate_member_com_paths_gefs_rt(memdir)
                    self.execute_stage(self.task_config)
                elif gefstype == 'gefs-offline':
                    self.calculate_member_com_paths_gefs_offline(memdir)
                    self.execute_stage(self.task_config)
                else:
                    raise ValueError(f"Invalid GEFSTYPE '{gefstype}' for RUN 'gefs'.")
            elif run in ('gcafs', 'enkfgdas', 'gcdas', 'gdas'):
                self.calculate_member_com_paths_gcafs(memdir)
                self.execute_stage(self.task_config)
            elif run == 'gfs':
                self.calculate_member_com_paths_gfs(memdir)
                self.execute_stage(self.task_config)
            else:
                raise ValueError(f"Unknown RUN type: {run}")
