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

   1.2 Provide a method (calculate_case_specific_variables) that derives
       application / RUN specific attributes (member indexing rules, GEFSTYPE
       handling etc.).

2. Provide separate methods for populating member-level COM path variables:
   - calculate_member_com_paths_gfs
   - calculate_member_com_paths_gefs_offline
   - calculate_member_com_paths_gefs_rt
   - calculate_member_com_paths_gcafs

Key Methods
-----------
calculate_case_specific_variables():
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
  1. Extend calculate_case_specific_variables() for RUN-specific member logic.
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
    def calculate_case_specific_variables(self) -> Dict[str, Any]:
        """
        Calculate case-specific variables needed for master YAML templates
        - gfs
        - gefs
        - gcafs
        - enkfgdas
        - gdas

        Returns
        -------
        Dict[str, Any]
          Dictionary containing case-specific variables with metadata
        """
        # Initialize a dictionary using configuration variables
        case_vars = self.task_config
        # assign rRUN to RUN
        case_vars.rRUN = case_vars.RUN
        # case specific rRUN
        if case_vars.RUN in ['gfs', 'gcdas', 'enkfgdas']:
            case_vars.rRUN = "gdas"
        else:
            # RUN not gfs, gcdas, or enkfgdas; leave rRUN unchanged and continue
            logger.debug("No rRUN remap applied: RUN='%s' (rRUN stays '%s')", case_vars.RUN, case_vars.rRUN)

        # START_ICE_FROM_ANA logic (only if DO_ICE is True)
        case_vars.START_ICE_FROM_ANA = False
        if getattr(case_vars, "DO_ICE", False):
            if getattr(case_vars, "DO_JEDIOCNVAR", False) and case_vars.RUN == "gdas":
                case_vars.START_ICE_FROM_ANA = True
            if getattr(case_vars, "DO_STARTMEM_FROM_JEDIICE", False) and case_vars.RUN == "enkfgdas":
                case_vars.START_ICE_FROM_ANA = True

        # Assign last_mem and first_mem to run members
        case_vars.last_mem = case_vars.NMEM_ENS
        if case_vars.RUN in ['enkfgdas']:
            case_vars.first_mem = 1
        elif case_vars.RUN in ['gefs']:  # GEFS Ensemble RUN (both regular and RT)
            case_vars.GEFSTYPE = self.task_config.get('GEFSTYPE', 'gefs-offline')
            case_vars.first_mem = 0
            if case_vars.GEFSTYPE == "gefs-offline":
                pass
            elif case_vars.GEFSTYPE == "gefs-real-time":
                # select the relevant member for each GEFS member from GFS outputs
                case_vars.cyc_ranges = [list(range(1, 31)), list(range(21, 51)),
                                        list(range(41, 71)), list(range(61, 81)) + list(range(1, 11))]
            else:
                # Error handling for unknown GEFSTYPE
                valid_types = ['gefs-offline', 'gefs-real-time']
                raise ValueError(f"Invalid GEFSTYPE '{case_vars.GEFSTYPE}' for RUN '{case_vars.RUN}'. "
                                 f"Valid options are: {valid_types}")
        else:  # Deterministic RUN (GFS and GCAFS)
            case_vars.first_mem = -1
            case_vars.last_mem = -1

        return case_vars

    @logit(logger)
    def calculate_general_cycle_variables(self) -> Dict[str, Any]:
        """Calculate cycle variables needed for master YAML templates

        This method replaces the Jinja template variables common across:
        - master_gfs.yaml.j2
        - master_gefs.yaml.j2
        - master_gefs_RT.yaml.j2
        - master_gcafs.yaml.j2

        Returns
        -------
        Dict[str, Any]
          Dictionary containing calculated cycle variables with metadata
        """
        # Initialize a dictionary using case specific variables
        cycle_vars = self.calculate_case_specific_variables()

        if "OCNRES" in cycle_vars:
            cycle_vars.OCNRES = f"{int(cycle_vars.OCNRES):03d}"
        # Calculate half window variables
        cycle_vars.half_window = cycle_vars.assim_freq // 2
        cycle_vars.half_window_begin = timedelta(hours=-cycle_vars.half_window)
        cycle_vars.half_window_end = timedelta(hours=cycle_vars.half_window)

        # Calculate current cycle variables
        if cycle_vars.current_cycle:
            if cycle_vars.DOIAU and cycle_vars.MODE == "cycled":
                cycle_vars.model_start_date_current_cycle = cycle_vars.current_cycle + cycle_vars.half_window_begin
            else:
                if cycle_vars.REPLAY_ICS:
                    cycle_vars.model_start_date_current_cycle = cycle_vars.current_cycle + cycle_vars.half_window_end
                else:
                    cycle_vars.model_start_date_current_cycle = cycle_vars.current_cycle

            # Calculate YMD and HH formats
            cycle_vars.current_cycle_YMD = cycle_vars.current_cycle.strftime("%Y%m%d")
            cycle_vars.current_cycle_HH = cycle_vars.current_cycle.strftime("%H")
            cycle_vars.m_prefix = cycle_vars.model_start_date_current_cycle.strftime("%Y%m%d.%H0000")

        # Calculate previous cycle variables
        if cycle_vars.previous_cycle:
            cycle_vars.previous_cycle_YMD = cycle_vars.previous_cycle.strftime("%Y%m%d")
            cycle_vars.previous_cycle_HH = cycle_vars.previous_cycle.strftime("%H")
            cycle_vars.m_index = cycle_vars.current_cycle.hour // 6
            cycle_vars.p_prefix = cycle_vars.previous_cycle.strftime("%Y%m%d.%H0000")

        # Define cycle directories to update com paths
        cycle_vars.current_cycle_dict = {
            "${ROTDIR}": cycle_vars.ROTDIR,
            "${RUN}": cycle_vars.RUN,
            "${YMD}": cycle_vars.current_cycle_YMD,
            "${HH}": cycle_vars.current_cycle_HH,
        }
        cycle_vars.previous_cycle_dict = {
            "${ROTDIR}": cycle_vars.ROTDIR,
            "${RUN}": cycle_vars.RUN,
            "${YMD}": cycle_vars.previous_cycle_YMD,
            "${HH}": cycle_vars.previous_cycle_HH,
        }
        return cycle_vars

    @logit(logger)
    def calculate_member_com_paths_gfs(self, memdir) -> Dict[str, Any]:
        """
        Calculate member COM paths for GFS

        Parameters
        ----------
        memdir : int
          The member directory number

        Returns
        -------
        Dict[str, Any]
          Dictionary containing member COM paths with metadata
        """
        com_vars = self.calculate_general_cycle_variables()
        memdir = f"mem{memdir:03d}" if memdir >= 0 else ''
        current_cycle = {**com_vars.current_cycle_dict, "${MEMDIR}": memdir}
        previous_cycle = {**com_vars.previous_cycle_dict, "${MEMDIR}": memdir, "${RUN}": com_vars.rRUN}

        com_vars['COMIN_ATMOS_INPUT_MEM'] = self._replace_template_vars(getattr(com_vars, 'COM_ATMOS_INPUT_TMPL', ''), current_cycle)
        com_vars['COMOUT_ATMOS_INPUT_MEM'] = self._replace_template_vars(getattr(com_vars, 'COM_ATMOS_INPUT_TMPL', ''), current_cycle)
        com_vars['COMOUT_ATMOS_RESTART_PREV_MEM'] = self._replace_template_vars(getattr(com_vars, 'COM_ATMOS_RESTART_TMPL', ''), previous_cycle)
        com_vars['COMOUT_ATMOS_RESTART_MEM'] = self._replace_template_vars(getattr(com_vars, 'COM_ATMOS_RESTART_TMPL', ''), current_cycle)
        com_vars['COMOUT_ATMOS_ANALYSIS_MEM'] = self._replace_template_vars(getattr(com_vars, 'COM_ATMOS_ANALYSIS_TMPL', ''), current_cycle)
        com_vars['COMOUT_ICE_ANALYSIS_MEM'] = self._replace_template_vars(getattr(com_vars, 'COM_ICE_ANALYSIS_TMPL', ''), current_cycle)
        com_vars['COMOUT_ICE_RESTART_PREV_MEM'] = self._replace_template_vars(getattr(com_vars, 'COM_ICE_RESTART_TMPL', ''), previous_cycle)
        com_vars['COMOUT_OCEAN_RESTART_PREV_MEM'] = self._replace_template_vars(getattr(com_vars, 'COM_OCEAN_RESTART_TMPL', ''), previous_cycle)
        com_vars['COMOUT_OCEAN_ANALYSIS_MEM'] = self._replace_template_vars(getattr(com_vars, 'COM_OCEAN_ANALYSIS_TMPL', ''), current_cycle)
        com_vars['COMOUT_MED_RESTART_PREV_MEM'] = self._replace_template_vars(getattr(com_vars, 'COM_MED_RESTART_TMPL', ''), previous_cycle)
        com_vars['COMOUT_CHEM_ANALYSIS_MEM'] = self._replace_template_vars(getattr(com_vars, 'COM_CHEM_ANALYSIS_TMPL', ''), current_cycle)
        com_vars['COMOUT_WAVE_RESTART_PREV_MEM'] = self._replace_template_vars(getattr(com_vars, 'COM_WAVE_RESTART_TMPL', ''), previous_cycle)

        return com_vars

    @logit(logger)
    def calculate_member_com_paths_gefs_offline(self, memdir) -> Dict[str, Any]:
        """
        Calculate member COM paths for GEFS offline

        Parameters
        ----------
        memdir : int
          The member directory number

        Returns
        -------
        Dict[str, Any]
          Dictionary containing member COM paths with metadata
        """
        com_vars = self.calculate_general_cycle_variables()
        memdir = f"mem{memdir:03d}" if memdir >= 0 else ''
        current_cycle = {**com_vars.current_cycle_dict, "${MEMDIR}": memdir}
        previous_cycle = {**com_vars.previous_cycle_dict, "${MEMDIR}": memdir}

        com_vars['COMIN_ATMOS_INPUT_MEM'] = self._replace_template_vars(getattr(com_vars, 'COM_ATMOS_INPUT_TMPL', ''), current_cycle)
        com_vars['COMOUT_ATMOS_INPUT_MEM'] = self._replace_template_vars(getattr(com_vars, 'COM_ATMOS_INPUT_TMPL', ''), current_cycle)
        com_vars['COMOUT_ATMOS_RESTART_PREV_MEM'] = self._replace_template_vars(getattr(com_vars, 'COM_ATMOS_RESTART_TMPL', ''), previous_cycle)
        com_vars['COMOUT_ATMOS_RESTART_MEM'] = self._replace_template_vars(getattr(com_vars, 'COM_ATMOS_RESTART_TMPL', ''), current_cycle)
        com_vars['COMOUT_ATMOS_ANALYSIS_MEM'] = self._replace_template_vars(getattr(com_vars, 'COM_ATMOS_ANALYSIS_TMPL', ''), current_cycle)
        com_vars['COMOUT_ATMOS_HISTORY_MEM'] = self._replace_template_vars(getattr(com_vars, 'COM_ATMOS_HISTORY_TMPL', ''), previous_cycle)
        com_vars['COMOUT_ICE_ANALYSIS_MEM'] = self._replace_template_vars(getattr(com_vars, 'COM_ICE_ANALYSIS_TMPL', ''), current_cycle)
        com_vars['COMOUT_ICE_RESTART_PREV_MEM'] = self._replace_template_vars(getattr(com_vars, 'COM_ICE_RESTART_TMPL', ''), previous_cycle)
        com_vars['COMOUT_OCEAN_RESTART_PREV_MEM'] = self._replace_template_vars(getattr(com_vars, 'COM_OCEAN_RESTART_TMPL', ''), previous_cycle)
        com_vars['COMOUT_OCEAN_ANALYSIS_MEM'] = self._replace_template_vars(getattr(com_vars, 'COM_OCEAN_ANALYSIS_TMPL', ''), current_cycle)
        com_vars['COMOUT_MED_RESTART_PREV_MEM'] = self._replace_template_vars(getattr(com_vars, 'COM_MED_RESTART_TMPL', ''), previous_cycle)
        com_vars['COMOUT_WAVE_RESTART_PREV_MEM'] = self._replace_template_vars(getattr(com_vars, 'COM_WAVE_RESTART_TMPL', ''), previous_cycle)

        return com_vars

    @logit(logger)
    def calculate_member_com_paths_gefs_rt(self, memdir) -> Dict[str, Any]:
        """
        Calculate member COM paths for GEFS real-time

        Parameters
        ----------
        memdir : int
          The member directory number

        Returns
        -------
        Dict[str, Any]
          Dictionary containing member COM paths with metadata
        """
        com_vars = self.calculate_general_cycle_variables()
        if memdir != 0:
            com_vars.gfs_member = com_vars.cyc_ranges[com_vars.m_index][(memdir - 1)]
        memdir = f"mem{memdir:03d}" if memdir >= 0 else ''
        current_cycle = {**com_vars.current_cycle_dict, "${MEMDIR}": memdir}
        previous_cycle = {**com_vars.previous_cycle_dict, "${MEMDIR}": memdir}

        com_vars['COMIN_ATMOS_INPUT_MEM'] = self._replace_template_vars(getattr(com_vars, 'COM_ATMOS_INPUT_TMPL', ''), current_cycle)
        com_vars['COMOUT_ATMOS_INPUT_MEM'] = self._replace_template_vars(getattr(com_vars, 'COM_ATMOS_INPUT_TMPL', ''), current_cycle)
        com_vars['COMOUT_ATMOS_RESTART_PREV_MEM'] = self._replace_template_vars(getattr(com_vars, 'COM_ATMOS_RESTART_TMPL', ''), previous_cycle)
        com_vars['COMOUT_ATMOS_ANALYSIS_MEM'] = self._replace_template_vars(getattr(com_vars, 'COM_ATMOS_ANALYSIS_TMPL', ''), current_cycle)
        com_vars['COMOUT_ATMOS_HISTORY_MEM'] = self._replace_template_vars(getattr(com_vars, 'COM_ATMOS_HISTORY_TMPL', ''), previous_cycle)
        com_vars['COMOUT_ICE_ANALYSIS_MEM'] = self._replace_template_vars(getattr(com_vars, 'COM_ICE_ANALYSIS_TMPL', ''), current_cycle)
        com_vars['COMOUT_ICE_RESTART_PREV_MEM'] = self._replace_template_vars(getattr(com_vars, 'COM_ICE_RESTART_TMPL', ''), previous_cycle)
        com_vars['COMOUT_OCEAN_RESTART_PREV_MEM'] = self._replace_template_vars(getattr(com_vars, 'COM_OCEAN_RESTART_TMPL', ''), previous_cycle)
        com_vars['COMOUT_OCEAN_ANALYSIS_MEM'] = self._replace_template_vars(getattr(com_vars, 'COM_OCEAN_ANALYSIS_TMPL', ''), current_cycle)
        com_vars['COMOUT_MED_RESTART_PREV_MEM'] = self._replace_template_vars(getattr(com_vars, 'COM_MED_RESTART_TMPL', ''), previous_cycle)
        com_vars['COMOUT_WAVE_RESTART_PREV_MEM'] = self._replace_template_vars(getattr(com_vars, 'COM_WAVE_RESTART_TMPL', ''), previous_cycle)

        return com_vars

    @logit(logger)
    def calculate_member_com_paths_gcafs(self, memdir) -> Dict[str, Any]:
        """
        Calculate member COM paths for GCAFS

        Parameters
        ----------
        memdir : int
          The member directory number

        Returns
        -------
        Dict[str, Any]
          Dictionary containing member COM paths with metadata
        """
        com_vars = self.calculate_general_cycle_variables()
        memdir = f"mem{memdir:03d}" if memdir >= 0 else ''

        # Three contexts:
        # - current (RUN) for outputs
        current_cycle_in = {**com_vars.current_cycle_dict, "${MEMDIR}": memdir, "${RUN}": com_vars.rRUN}
        # - current (rRUN) for inputs
        current_cycle = {**current_cycle_in, "${RUN}": com_vars.RUN}
        # - previous (rRUN) for prev-cycle restarts
        previous_cycle = {**com_vars.previous_cycle_dict, "${MEMDIR}": memdir, "${RUN}": com_vars.rRUN}

        com_vars['COMIN_ATMOS_INPUT_MEM'] = self._replace_template_vars(getattr(com_vars, 'COM_ATMOS_INPUT_TMPL', ''), current_cycle_in)
        com_vars['COMOUT_ATMOS_INPUT_MEM'] = self._replace_template_vars(getattr(com_vars, 'COM_ATMOS_INPUT_TMPL', ''), current_cycle)
        com_vars['COMOUT_ATMOS_RESTART_PREV_MEM'] = self._replace_template_vars(getattr(com_vars, 'COM_ATMOS_RESTART_TMPL', ''), previous_cycle)
        com_vars['COMOUT_ATMOS_RESTART_MEM'] = self._replace_template_vars(getattr(com_vars, 'COM_ATMOS_RESTART_TMPL', ''), current_cycle)
        com_vars['COMOUT_ATMOS_ANALYSIS_MEM'] = self._replace_template_vars(getattr(com_vars, 'COM_ATMOS_ANALYSIS_TMPL', ''), current_cycle)
        com_vars['COMOUT_ICE_ANALYSIS_MEM'] = self._replace_template_vars(getattr(com_vars, 'COM_ICE_ANALYSIS_TMPL', ''), current_cycle)
        com_vars['COMOUT_ICE_RESTART_PREV_MEM'] = self._replace_template_vars(getattr(com_vars, 'COM_ICE_RESTART_TMPL', ''), previous_cycle)
        com_vars['COMOUT_OCEAN_RESTART_PREV_MEM'] = self._replace_template_vars(getattr(com_vars, 'COM_OCEAN_RESTART_TMPL', ''), previous_cycle)
        com_vars['COMOUT_OCEAN_ANALYSIS_MEM'] = self._replace_template_vars(getattr(com_vars, 'COM_OCEAN_ANALYSIS_TMPL', ''), current_cycle)
        com_vars['COMOUT_MED_RESTART_PREV_MEM'] = self._replace_template_vars(getattr(com_vars, 'COM_MED_RESTART_TMPL', ''), previous_cycle)
        com_vars['COMOUT_WAVE_RESTART_PREV_MEM'] = self._replace_template_vars(getattr(com_vars, 'COM_WAVE_RESTART_TMPL', ''), previous_cycle)

        return com_vars

    def _replace_template_vars(self, template: str, var_dict: Dict[str, Any]) -> str:
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
    def calculate_stage_vars(self) -> None:
        """
        Prepare all required staging variables used to
        locate and sync files defined in the master YAML templates.
        """
        stage_vars = self.calculate_general_cycle_variables()
        run = getattr(stage_vars, 'RUN', None)
        for memdir in range(stage_vars.first_mem, stage_vars.last_mem + 1):
            stage_vars.memdir = memdir
            if run == 'gefs':
                gefstype = getattr(stage_vars, 'GEFSTYPE', None)
                if gefstype == 'gefs-real-time':
                    stage_vars.update(self.calculate_member_com_paths_gefs_rt(memdir))
                    self.execute_stage(stage_vars)
                elif gefstype == 'gefs-offline':
                    stage_vars.update(self.calculate_member_com_paths_gefs_offline(memdir))
                    self.execute_stage(stage_vars)
                else:
                    raise ValueError(f"Invalid GEFSTYPE '{gefstype}' for RUN 'gefs'.")
            elif run in ('gcafs', 'enkfgdas', 'gcdas'):
                stage_vars.update(self.calculate_member_com_paths_gcafs(memdir))
                self.execute_stage(stage_vars)
            elif run == 'gfs':
                stage_vars.update(self.calculate_member_com_paths_gfs(memdir))
                self.execute_stage(stage_vars)
            else:
                raise ValueError(f"Unknown RUN type: {run}")
