#!/usr/bin/env python3

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
    def calculate_cycle_variables(self) -> Dict[str, Any]:
        """Calculate cycle variables from master YAML template logic

        This method replaces the Jinja template variables common across:
        - master_gfs.yaml.j2
        - master_gefs.yaml.j2
        - master_gefs_RT.yaml.j2
        - master_gcafs.yaml.j2

        Returns
        -------
        Dict[str, Any]
          Dictionary containing calculated cycle variables
        """
        # Initialize a dictionary using config variables
        cycle_vars = self.task_config

        if "OCNRES" in cycle_vars:
            cycle_vars["OCNRES"] = f"{cycle_vars['OCNRES']:03d}"
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

        # application-specific logic and variables
        cycle_vars.rRUN = cycle_vars.RUN
        if cycle_vars.RUN in ['gfs', 'gcafs', 'enkfgdas']:
            cycle_vars.rRUN = "gdas"
            cycle_vars.first_mem = 1
            cycle_vars.last_mem = cycle_vars.NMEM_ENS
        elif cycle_vars.RUN in ['gefs']:  # GEFS Ensemble RUN (both regular and RT)
            cycle_vars.GEFSTYPE = self.task_config.get('GEFSTYPE', 'gefs-offline')
            if cycle_vars.GEFSTYPE == "gefs-offline":
                cycle_vars.first_mem = 0
                cycle_vars.last_mem = cycle_vars.NMEM_ENS
            elif cycle_vars.GEFSTYPE == "gefs-real-time":
                # select the relevant member for each GEFS member from GFS outputs
                cycle_vars.cyc_ranges = [list(range(1, 31)), list(range(21, 51)),
                                         list(range(41, 71)), list(range(61, 81)) + list(range(1, 11))]
                cycle_vars.ENSMEM = self.task_config.get('ENSMEM', 0)
            else:
                # Error handling for unknown GEFSTYPE
                valid_types = ['gefs-offline', 'gefs-real-time']
                raise ValueError(f"Invalid GEFSTYPE '{cycle_vars.GEFSTYPE}' for RUN '{cycle_vars.RUN}'. "
                                 f"Valid options are: {valid_types}")
        else:  # Deterministic RUN (GFS and GCAFS)
            cycle_vars.first_mem = -1
            cycle_vars.last_mem = -1

        com_templates = AttrDict()
        for k, v in getattr(self, 'task_config', {}).items():
            if isinstance(k, str) and k.startswith('COM_') and k.endswith('_TMPL'):
                com_templates[k] = v
        cycle_vars.update(com_templates)

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
        cycle_vars = self.calculate_cycle_variables()
        memdir = f"mem{memdir:03d}" if memdir >= 0 else ''
        current_cycle = {**cycle_vars.current_cycle_dict, "${MEMDIR}": memdir}
        previous_cycle = {**cycle_vars.previous_cycle_dict, "${MEMDIR}": memdir}

        cycle_vars['COMOUT_ATMOS_INPUT_MEM'] = self._replace_template_vars(getattr(cycle_vars, 'COM_ATMOS_INPUT_TMPL', ''), current_cycle)
        cycle_vars['COMOUT_ATMOS_RESTART_PREV_MEM'] = self._replace_template_vars(getattr(cycle_vars, 'COM_ATMOS_RESTART_TMPL', ''), previous_cycle)
        cycle_vars['COMOUT_ATMOS_RESTART_MEM'] = self._replace_template_vars(getattr(cycle_vars, 'COM_ATMOS_RESTART_TMPL', ''), current_cycle)
        cycle_vars['COMOUT_ATMOS_ANALYSIS_MEM'] = self._replace_template_vars(getattr(cycle_vars, 'COM_ATMOS_ANALYSIS_TMPL', ''), current_cycle)
        cycle_vars['COMOUT_ICE_ANALYSIS_MEM'] = self._replace_template_vars(getattr(cycle_vars, 'COM_ICE_ANALYSIS_TMPL', ''), current_cycle)
        cycle_vars['COMOUT_ICE_RESTART_PREV_MEM'] = self._replace_template_vars(getattr(cycle_vars, 'COM_ICE_RESTART_TMPL', ''), previous_cycle)
        cycle_vars['COMOUT_OCEAN_RESTART_PREV_MEM'] = self._replace_template_vars(getattr(cycle_vars, 'COM_OCEAN_RESTART_TMPL', ''), previous_cycle)
        cycle_vars['COMOUT_OCEAN_ANALYSIS_MEM'] = self._replace_template_vars(getattr(cycle_vars, 'COM_OCEAN_ANALYSIS_TMPL', ''), current_cycle)
        cycle_vars['COMOUT_MED_RESTART_PREV_MEM'] = self._replace_template_vars(getattr(cycle_vars, 'COM_MED_RESTART_TMPL', ''), previous_cycle)
        cycle_vars['COMOUT_CHEM_ANALYSIS_MEM'] = self._replace_template_vars(getattr(cycle_vars, 'COM_CHEM_ANALYSIS_TMPL', ''), current_cycle)
        cycle_vars['COMOUT_WAVE_RESTART_PREV_MEM'] = self._replace_template_vars(getattr(cycle_vars, 'COM_WAVE_RESTART_TMPL', ''), previous_cycle)

        return cycle_vars

    @logit(logger)
    def calculate_member_com_paths_gefs_offline(self, memdir) -> Dict[str, Any]:
        cycle_vars = self.calculate_cycle_variables()
        memdir = f"mem{memdir:03d}" if memdir >= 0 else ''
        current_cycle = {**cycle_vars.current_cycle_dict, "${MEMDIR}": memdir}
        previous_cycle = {**cycle_vars.previous_cycle_dict, "${MEMDIR}": memdir}

        cycle_vars['COMOUT_ATMOS_INPUT_MEM'] = self._replace_template_vars(getattr(cycle_vars, 'COM_ATMOS_INPUT_TMPL', ''), current_cycle)
        cycle_vars['COMOUT_ATMOS_RESTART_PREV_MEM'] = self._replace_template_vars(getattr(cycle_vars, 'COM_ATMOS_RESTART_TMPL', ''), previous_cycle)
        cycle_vars['COMOUT_ATMOS_RESTART_MEM'] = self._replace_template_vars(getattr(cycle_vars, 'COM_ATMOS_RESTART_TMPL', ''), current_cycle)
        cycle_vars['COMOUT_ATMOS_ANALYSIS_MEM'] = self._replace_template_vars(getattr(cycle_vars, 'COM_ATMOS_ANALYSIS_TMPL', ''), current_cycle)
        cycle_vars['COMOUT_ATMOS_HISTORY_MEM'] = self._replace_template_vars(getattr(cycle_vars, 'COM_ATMOS_HISTORY_TMPL', ''), previous_cycle)
        cycle_vars['COMOUT_ICE_ANALYSIS_MEM'] = self._replace_template_vars(getattr(cycle_vars, 'COM_ICE_ANALYSIS_TMPL', ''), current_cycle)
        cycle_vars['COMOUT_ICE_RESTART_PREV_MEM'] = self._replace_template_vars(getattr(cycle_vars, 'COM_ICE_RESTART_TMPL', ''), previous_cycle)
        cycle_vars['COMOUT_OCEAN_RESTART_PREV_MEM'] = self._replace_template_vars(getattr(cycle_vars, 'COM_OCEAN_RESTART_TMPL', ''), previous_cycle)
        cycle_vars['COMOUT_OCEAN_ANALYSIS_MEM'] = self._replace_template_vars(getattr(cycle_vars, 'COM_OCEAN_ANALYSIS_TMPL', ''), current_cycle)
        cycle_vars['COMOUT_MED_RESTART_PREV_MEM'] = self._replace_template_vars(getattr(cycle_vars, 'COM_MED_RESTART_TMPL', ''), previous_cycle)
        cycle_vars['COMOUT_WAVE_RESTART_PREV_MEM'] = self._replace_template_vars(getattr(cycle_vars, 'COM_WAVE_RESTART_TMPL', ''), previous_cycle)

        return cycle_vars

    @logit(logger)
    def calculate_member_com_paths_gefs_rt(self, memdir) -> Dict[str, Any]:
        cycle_vars = self.calculate_cycle_variables()
        if memdir != 0:
            cycle_vars.gfs_member = cycle_vars.cyc_ranges[cycle_vars.m_index][(memdir - 1)]
        memdir = f"mem{memdir:03d}" if memdir >= 0 else ''
        current_cycle = {**cycle_vars.current_cycle_dict, "${MEMDIR}": memdir}
        previous_cycle = {**cycle_vars.previous_cycle_dict, "${MEMDIR}": memdir}

        cycle_vars['COMOUT_ATMOS_INPUT_MEM'] = self._replace_template_vars(getattr(cycle_vars, 'COM_ATMOS_INPUT_TMPL', ''), current_cycle)
        cycle_vars['COMOUT_ATMOS_RESTART_PREV_MEM'] = self._replace_template_vars(getattr(cycle_vars, 'COM_ATMOS_RESTART_TMPL', ''), previous_cycle)
        cycle_vars['COMOUT_ATMOS_ANALYSIS_MEM'] = self._replace_template_vars(getattr(cycle_vars, 'COM_ATMOS_ANALYSIS_TMPL', ''), current_cycle)
        cycle_vars['COMOUT_ATMOS_HISTORY_MEM'] = self._replace_template_vars(getattr(cycle_vars, 'COM_ATMOS_HISTORY_TMPL', ''), previous_cycle)
        cycle_vars['COMOUT_ICE_ANALYSIS_MEM'] = self._replace_template_vars(getattr(cycle_vars, 'COM_ICE_ANALYSIS_TMPL', ''), current_cycle)
        cycle_vars['COMOUT_ICE_RESTART_PREV_MEM'] = self._replace_template_vars(getattr(cycle_vars, 'COM_ICE_RESTART_TMPL', ''), previous_cycle)
        cycle_vars['COMOUT_OCEAN_RESTART_PREV_MEM'] = self._replace_template_vars(getattr(cycle_vars, 'COM_OCEAN_RESTART_TMPL', ''), previous_cycle)
        cycle_vars['COMOUT_OCEAN_ANALYSIS_MEM'] = self._replace_template_vars(getattr(cycle_vars, 'COM_OCEAN_ANALYSIS_TMPL', ''), current_cycle)
        cycle_vars['COMOUT_MED_RESTART_PREV_MEM'] = self._replace_template_vars(getattr(cycle_vars, 'COM_MED_RESTART_TMPL', ''), previous_cycle)
        cycle_vars['COMOUT_WAVE_RESTART_PREV_MEM'] = self._replace_template_vars(getattr(cycle_vars, 'COM_WAVE_RESTART_TMPL', ''), previous_cycle)

        return cycle_vars

    @logit(logger)
    def calculate_member_com_paths_gcafs(self, memdir) -> Dict[str, Any]:
        cycle_vars = self.calculate_cycle_variables()
        memdir = f"mem{memdir:03d}" if memdir >= 0 else ''

        # Three contexts:
        # - current (RUN) for outputs
        current_cycle_out = {**cycle_vars.current_cycle_dict, "${MEMDIR}": memdir}
        # - current (rRUN) for inputs
        current_cycle_in = {**current_cycle_out, "${RUN}": cycle_vars.rRUN}
        # - previous (rRUN) for prev-cycle restarts
        previous_cycle = {**cycle_vars.previous_cycle_dict, "${MEMDIR}": memdir, "${RUN}": cycle_vars.rRUN}

        cycle_vars['COMIN_ATMOS_INPUT_MEM'] = self._replace_template_vars(getattr(cycle_vars, 'COM_ATMOS_INPUT_TMPL', ''), current_cycle_in)
        cycle_vars['COMOUT_ATMOS_INPUT_MEM'] = self._replace_template_vars(getattr(cycle_vars, 'COM_ATMOS_INPUT_TMPL', ''), current_cycle_out)
        cycle_vars['COMOUT_ATMOS_RESTART_PREV_MEM'] = self._replace_template_vars(getattr(cycle_vars, 'COM_ATMOS_RESTART_TMPL', ''), previous_cycle)
        cycle_vars['COMOUT_ATMOS_RESTART_MEM'] = self._replace_template_vars(getattr(cycle_vars, 'COM_ATMOS_RESTART_TMPL', ''), current_cycle_out)
        cycle_vars['COMOUT_ATMOS_ANALYSIS_MEM'] = self._replace_template_vars(getattr(cycle_vars, 'COM_ATMOS_ANALYSIS_TMPL', ''), current_cycle_out)
        cycle_vars['COMOUT_ICE_ANALYSIS_MEM'] = self._replace_template_vars(getattr(cycle_vars, 'COM_ICE_ANALYSIS_TMPL', ''), current_cycle_out)
        cycle_vars['COMOUT_ICE_RESTART_PREV_MEM'] = self._replace_template_vars(getattr(cycle_vars, 'COM_ICE_RESTART_TMPL', ''), previous_cycle)
        cycle_vars['COMOUT_OCEAN_RESTART_PREV_MEM'] = self._replace_template_vars(getattr(cycle_vars, 'COM_OCEAN_RESTART_TMPL', ''), previous_cycle)
        cycle_vars['COMOUT_OCEAN_ANALYSIS_MEM'] = self._replace_template_vars(getattr(cycle_vars, 'COM_OCEAN_ANALYSIS_TMPL', ''), current_cycle_out)
        cycle_vars['COMOUT_MED_RESTART_PREV_MEM'] = self._replace_template_vars(getattr(cycle_vars, 'COM_MED_RESTART_TMPL', ''), previous_cycle)
        cycle_vars['COMOUT_WAVE_RESTART_PREV_MEM'] = self._replace_template_vars(getattr(cycle_vars, 'COM_WAVE_RESTART_TMPL', ''), previous_cycle)

        return cycle_vars

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
        """Dispatch to per-master calculator to build COM paths."""
        cycle_vars = self.calculate_cycle_variables()
        run = getattr(cycle_vars, 'RUN', None)
        for memdir in range(cycle_vars.first_mem, cycle_vars.last_mem + 1):
            cycle_vars.memdir = memdir
            if run == 'gefs':
                gefstype = getattr(cycle_vars, 'GEFSTYPE', None)
                if gefstype == 'gefs-real-time':
                    cycle_vars.update(self.calculate_member_com_paths_gefs_rt(memdir))
                    self.execute_stage(cycle_vars)
                elif gefstype == 'gefs-offline':
                    cycle_vars.update(self.calculate_member_com_paths_gefs_offline(memdir))
                    self.execute_stage(cycle_vars)
                else:
                    raise ValueError(f"Invalid GEFSTYPE '{gefstype}' for RUN 'gefs'.")
            elif run in ('gcafs', 'enkfgdas'):
                cycle_vars.update(self.calculate_member_com_paths_gcafs(memdir))
                self.execute_stage(cycle_vars)
            elif run == 'gfs':
                cycle_vars.update(self.calculate_member_com_paths_gfs(memdir))
                self.execute_stage(cycle_vars)
            else:
                raise ValueError(f"Unknown RUN type: {run}")
