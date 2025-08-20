#!/usr/bin/env python3
# filepath: c:\Users\Anton.Fernando\Desktop\project1\global-workflow\ush\python\pygfs\task\stage_archive_utils.py

import os
from datetime import datetime, timedelta
from logging import getLogger
from typing import Any, Dict, List

from wxflow import Task, logit, AttrDict, to_timedelta

logger = getLogger(__name__.split('.')[-1])


class StageArchiveUtils(Task):
    """Utility class for Stage and Archive operations including template path generation
    and YAML variable calculations moved from master YAML templates.

    Supports all Global Workflow applications:
    - master_gfs.yaml.j2 (GFS deterministic)
    - master_gefs.yaml.j2 (GEFS ensemble)
    - master_gefs_RT.yaml.j2 (GEFS real-time)
    - master_gcafs.yaml.j2 (GCAFS climate analysis)

    This class implements the [Enabler] Refactor stage IC master YAMLs: Shift logic
    and shared variables to Python scripts initiative.
    """

    @logit(logger, name="StageArchiveUtils")
    def __init__(self, config: Dict[str, Any]) -> None:
        """Constructor for the StageArchiveUtils task

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
    def calculate_cycle_variables(self) -> Dict[str, Any]:
        """Calculate cycle variables from master YAML template logic

        This method replaces the Jinja template variables common across:
        - master_gfs.yaml.j2: Basic cycle logic for deterministic GFS
        - master_gefs.yaml.j2: Ensemble cycle logic with member handling
        - master_gefs_RT.yaml.j2: Real-time ensemble with modified restart logic
        - master_gcafs.yaml.j2: Climate analysis cycle logic with restart RUN handling

        Returns
        -------
        AttrDict
            AttrDict containing calculated cycle variables with dot notation access
        """
        # Get basic configuration with fallbacks using self.task_config dot notation
        assim_freq = self.task_config.get('assim_freq', 6)
        current_cycle = self.task_config.get('current_cycle', None)
        previous_cycle = self.task_config.get('previous_cycle', None)
        DOIAU = self.task_config.get('DOIAU', False)
        MODE = self.task_config.get('MODE', 'forecast-only')
        REPLAY_ICS = self.task_config.get('REPLAY_ICS', False)
        RUN = self.task_config.get('RUN', 'gfs')

        # Initialize AttrDict for cycle variables
        cycle_vars = AttrDict()

        # Calculate half window variables - replaces {% set half_window = assim_freq // 2 %}
        half_window = assim_freq // 2
        cycle_vars.half_window = half_window

        # Calculate time deltas - replaces half_window_begin/end Jinja logic
        half_window_begin = timedelta(hours=-half_window)
        half_window_end = timedelta(hours=half_window)
        cycle_vars.half_window_begin = half_window_begin
        cycle_vars.half_window_end = half_window_end

        # Calculate model start date for current cycle - replaces complex Jinja conditionals
        if current_cycle:
            if DOIAU and MODE == "cycled":
                model_start_date_current_cycle = current_cycle + half_window_begin
            else:
                if REPLAY_ICS:
                    model_start_date_current_cycle = current_cycle + half_window_end
                else:
                    model_start_date_current_cycle = current_cycle

            cycle_vars.model_start_date_current_cycle = model_start_date_current_cycle

            # Calculate YMD and HH formats - replaces Jinja strftime operations
            cycle_vars.current_cycle_YMD = current_cycle.strftime("%Y%m%d")
            cycle_vars.current_cycle_HH = current_cycle.strftime("%H")
            cycle_vars.m_prefix = model_start_date_current_cycle.strftime("%Y%m%d.%H0000")

        # Calculate previous cycle variables
        if previous_cycle:
            cycle_vars.previous_cycle_YMD = previous_cycle.strftime("%Y%m%d")
            cycle_vars.previous_cycle_HH = previous_cycle.strftime("%H")
            cycle_vars.p_prefix = previous_cycle.strftime("%Y%m%d.%H0000")

        # GCAFS-specific: Determine restart RUN - replaces {% set rRUN = "gdas" %}
        if RUN in ['gcafs', 'enkfgdas']:
            cycle_vars.rRUN = "gdas"  # Always use GDAS for restart in GCAFS
        else:
            cycle_vars.rRUN = RUN

        return cycle_vars

    @logit(logger)
    def calculate_member_variables(self) -> Dict[str, Any]:
        """Calculate member-related variables from master YAML template logic

        This method replaces the Jinja template variables across all master templates:
        - GFS: Deterministic (no members)
        - GEFS: Ensemble members 0-NMEM_ENS
        - GEFS RT: Real-time ensemble with modified member handling
        - GCAFS: Climate analysis with enkfgdas members 1-NMEM_ENS

        Returns
        -------
        AttrDict
            AttrDict containing member variables and COM directory lists with dot notation access
        """
        # Initialize AttrDict for member variables
        member_vars = AttrDict()

        # Get configuration using self.task_config dot notation
        RUN = self.task_config.get('RUN', 'gfs')
        NMEM_ENS = self.task_config.get('NMEM_ENS', 20)

        # Set first/last mem for loop - application-specific logic
        if RUN == "enkfgdas":  # GCAFS Ensemble RUN
            first_mem = 1
            last_mem = NMEM_ENS
        elif RUN in ['gefs']:  # GEFS Ensemble RUN (both regular and RT)
            GEFSTYPE = self.task_config.get('GEFSTYPE', 'gefs-offline')
            if GEFSTYPE == "gefs-offline":
                first_mem = 0
                last_mem = NMEM_ENS
            elif GEFSTYPE == "gefs-real-time":
                ENSMEM = self.task_config.get('ENSMEM', 0)
                # For real-time GEFS, process only the specific ensemble member
                first_mem = ENSMEM
                last_mem = ENSMEM
            else:
                # Error handling for unknown GEFSTYPE
                valid_types = ['gefs-offline', 'gefs-real-time']
                raise ValueError(f"Invalid GEFSTYPE '{GEFSTYPE}' for RUN '{RUN}'. "
                               f"Valid options are: {valid_types}")
        else:  # Deterministic RUN (GFS, GCAFS deterministic)
            first_mem = -1
            last_mem = -1

        member_vars.first_mem = first_mem
        member_vars.last_mem = last_mem
        member_vars.NMEM_ENS = NMEM_ENS

        # Initialize COM directory lists - comprehensive coverage for all applications
        com_dir_lists = AttrDict({
            # Input directories (GCAFS-specific but used across applications)
            'COMIN_ATMOS_INPUT_MEM_list': [],
            'COMIN_ATMOS_RESTART_PREV_MEM_list': [],
            'COMIN_ICE_RESTART_PREV_MEM_list': [],
            'COMIN_OCEAN_RESTART_PREV_MEM_list': [],
            'COMIN_MED_RESTART_PREV_MEM_list': [],
            'COMIN_WAVE_RESTART_PREV_MEM_list': [],

            # Output directories (all applications)
            'COMOUT_ATMOS_INPUT_MEM_list': [],
            'COMOUT_ATMOS_RESTART_PREV_MEM_list': [],
            'COMOUT_ATMOS_RESTART_MEM_list': [],
            'COMOUT_ATMOS_ANALYSIS_MEM_list': [],
            'COMOUT_ICE_ANALYSIS_MEM_list': [],
            'COMOUT_ICE_RESTART_PREV_MEM_list': [],
            'COMOUT_ICE_RESTART_MEM_list': [],
            'COMOUT_OCEAN_RESTART_PREV_MEM_list': [],
            'COMOUT_OCEAN_RESTART_MEM_list': [],
            'COMOUT_OCEAN_ANALYSIS_MEM_list': [],
            'COMOUT_MED_RESTART_PREV_MEM_list': [],
            'COMOUT_MED_RESTART_MEM_list': [],
            'COMOUT_WAVE_RESTART_PREV_MEM_list': [],
            'COMOUT_WAVE_RESTART_MEM_list': [],

            # GEFS-specific additional directories
            'COMOUT_ATMOS_ENSSTAT_MEM_list': [],
            'COMOUT_OCEAN_ENSSTAT_MEM_list': [],
            'COMOUT_ICE_ENSSTAT_MEM_list': [],
        })

        # Get cycle variables for path construction
        cycle_vars = self.calculate_cycle_variables()
        current_cycle_YMD = cycle_vars.get('current_cycle_YMD', '')
        current_cycle_HH = cycle_vars.get('current_cycle_HH', '')
        previous_cycle_YMD = cycle_vars.get('previous_cycle_YMD', '')
        previous_cycle_HH = cycle_vars.get('previous_cycle_HH', '')
        rRUN = cycle_vars.get('rRUN', RUN)

        # Get template paths from configuration
        ROTDIR = self.task_config.get('ROTDIR', '')

        # Comprehensive COM template patterns supporting all applications
        com_templates = self._get_com_templates()

        # Only process members if we have ensemble members to process
        if first_mem >= 0 and last_mem >= first_mem:
            # Construct member COM directory lists - replaces Jinja for loop
            for mem in range(first_mem, last_mem + 1):
                # Handle member directory naming - application-specific logic
                if mem >= 0:
                    memdir = f"mem{mem:03d}"  # Standard ensemble member format
                else:
                    memdir = ""  # Deterministic run (no member directory)

                # GCAFS-specific: current_cycle_dict_in for COMIN directories (uses rRUN)
                current_cycle_dict_in = {
                    '${ROTDIR}': ROTDIR,
                    '${RUN}': rRUN,           # Use restart RUN for input
                    '${YMD}': current_cycle_YMD,
                    '${HH}': current_cycle_HH,
                    '${MEMDIR}': memdir
                }

                # Current cycle dictionary for COMOUT directories (uses actual RUN)
                current_cycle_dict = {
                    '${ROTDIR}': ROTDIR,
                    '${RUN}': RUN,
                    '${YMD}': current_cycle_YMD,
                    '${HH}': current_cycle_HH,
                    '${MEMDIR}': memdir
                }

                # Previous cycle dictionary for restart files (uses rRUN for GCAFS)
                previous_cycle_dict = {
                    '${ROTDIR}': ROTDIR,
                    '${RUN}': rRUN,           # Use restart RUN for previous cycle
                    '${YMD}': previous_cycle_YMD,
                    '${HH}': previous_cycle_HH,
                    '${MEMDIR}': memdir
                }

                # Generate all COM directory paths
                self._generate_member_com_paths(com_dir_lists, com_templates,
                                              current_cycle_dict_in, current_cycle_dict,
                                              previous_cycle_dict)

        # Add COM directory lists to member_vars using AttrDict update
        member_vars.update(com_dir_lists)

        return member_vars

    def _get_com_templates(self) -> Dict[str, str]:
        """Get COM directory templates for all applications

        Returns
        -------
        Dict[str, str]
            Dictionary of COM template paths
        """
        return {
            'COM_ATMOS_INPUT_TMPL': getattr(self.task_config, 'COM_ATMOS_INPUT_TMPL',
                                          "${ROTDIR}/${RUN}.${YMD}/${HH}/model_data/atmos/input/${MEMDIR}"),
            'COM_ATMOS_RESTART_TMPL': getattr(self.task_config, 'COM_ATMOS_RESTART_TMPL',
                                            "${ROTDIR}/${RUN}.${YMD}/${HH}/model_data/atmos/restart/${MEMDIR}"),
            'COM_ATMOS_ANALYSIS_TMPL': getattr(self.task_config, 'COM_ATMOS_ANALYSIS_TMPL',
                                             "${ROTDIR}/${RUN}.${YMD}/${HH}/analysis/atmos/${MEMDIR}"),
            'COM_ICE_ANALYSIS_TMPL': getattr(self.task_config, 'COM_ICE_ANALYSIS_TMPL',
                                           "${ROTDIR}/${RUN}.${YMD}/${HH}/analysis/ice/${MEMDIR}"),
            'COM_ICE_RESTART_TMPL': getattr(self.task_config, 'COM_ICE_RESTART_TMPL',
                                          "${ROTDIR}/${RUN}.${YMD}/${HH}/model_data/ice/restart/${MEMDIR}"),
            'COM_OCEAN_RESTART_TMPL': getattr(self.task_config, 'COM_OCEAN_RESTART_TMPL',
                                            "${ROTDIR}/${RUN}.${YMD}/${HH}/model_data/ocean/restart/${MEMDIR}"),
            'COM_OCEAN_ANALYSIS_TMPL': getattr(self.task_config, 'COM_OCEAN_ANALYSIS_TMPL',
                                             "${ROTDIR}/${RUN}.${YMD}/${HH}/analysis/ocean/${MEMDIR}"),
            'COM_MED_RESTART_TMPL': getattr(self.task_config, 'COM_MED_RESTART_TMPL',
                                          "${ROTDIR}/${RUN}.${YMD}/${HH}/model_data/med/restart/${MEMDIR}"),
            'COM_WAVE_RESTART_TMPL': getattr(self.task_config, 'COM_WAVE_RESTART_TMPL',
                                           "${ROTDIR}/${RUN}.${YMD}/${HH}/model_data/wave/restart/${MEMDIR}"),
            'COM_ATMOS_ENSSTAT_TMPL': getattr(self.task_config, 'COM_ATMOS_ENSSTAT_TMPL',
                                            "${ROTDIR}/${RUN}.${YMD}/${HH}/products/atmos/ensstat/${MEMDIR}"),
            'COM_OCEAN_ENSSTAT_TMPL': getattr(self.task_config, 'COM_OCEAN_ENSSTAT_TMPL',
                                            "${ROTDIR}/${RUN}.${YMD}/${HH}/products/ocean/ensstat/${MEMDIR}"),
            'COM_ICE_ENSSTAT_TMPL': getattr(self.task_config, 'COM_ICE_ENSSTAT_TMPL',
                                          "${ROTDIR}/${RUN}.${YMD}/${HH}/products/ice/ensstat/${MEMDIR}")
        }

    def _generate_member_com_paths(self, com_dir_lists: Dict[str, List[str]],
                                 com_templates: Dict[str, str],
                                 current_cycle_dict_in: Dict[str, str],
                                 current_cycle_dict: Dict[str, str],
                                 previous_cycle_dict: Dict[str, str]) -> None:
        """Generate COM directory paths for a single member

        Parameters
        ----------
        com_dir_lists : Dict[str, List[str]]
            Dictionary of COM directory lists to populate
        com_templates : Dict[str, str]
            Dictionary of COM template paths
        current_cycle_dict_in : Dict[str, str]
            Variable substitution dict for current cycle input (COMIN)
        current_cycle_dict : Dict[str, str]
            Variable substitution dict for current cycle output (COMOUT)
        previous_cycle_dict : Dict[str, str]
            Variable substitution dict for previous cycle
        """
        # COMIN directories (input) - use rRUN for GCAFS
        comin_atmos_input = self._replace_template_vars(com_templates['COM_ATMOS_INPUT_TMPL'], current_cycle_dict_in)
        comin_atmos_restart_prev = self._replace_template_vars(com_templates['COM_ATMOS_RESTART_TMPL'], previous_cycle_dict)
        comin_ice_restart_prev = self._replace_template_vars(com_templates['COM_ICE_RESTART_TMPL'], previous_cycle_dict)
        comin_ocean_restart_prev = self._replace_template_vars(com_templates['COM_OCEAN_RESTART_TMPL'], previous_cycle_dict)
        comin_med_restart_prev = self._replace_template_vars(com_templates['COM_MED_RESTART_TMPL'], previous_cycle_dict)
        comin_wave_restart_prev = self._replace_template_vars(com_templates['COM_WAVE_RESTART_TMPL'], previous_cycle_dict)

        # COMOUT directories (output) - use actual RUN
        comout_atmos_input = self._replace_template_vars(com_templates['COM_ATMOS_INPUT_TMPL'], current_cycle_dict)
        comout_atmos_restart_prev = self._replace_template_vars(com_templates['COM_ATMOS_RESTART_TMPL'], previous_cycle_dict)
        comout_atmos_restart = self._replace_template_vars(com_templates['COM_ATMOS_RESTART_TMPL'], current_cycle_dict)
        comout_atmos_analysis = self._replace_template_vars(com_templates['COM_ATMOS_ANALYSIS_TMPL'], current_cycle_dict)
        comout_ice_analysis = self._replace_template_vars(com_templates['COM_ICE_ANALYSIS_TMPL'], current_cycle_dict)
        comout_ice_restart_prev = self._replace_template_vars(com_templates['COM_ICE_RESTART_TMPL'], previous_cycle_dict)
        comout_ice_restart = self._replace_template_vars(com_templates['COM_ICE_RESTART_TMPL'], current_cycle_dict)
        comout_ocean_restart_prev = self._replace_template_vars(com_templates['COM_OCEAN_RESTART_TMPL'], previous_cycle_dict)
        comout_ocean_restart = self._replace_template_vars(com_templates['COM_OCEAN_RESTART_TMPL'], current_cycle_dict)
        comout_ocean_analysis = self._replace_template_vars(com_templates['COM_OCEAN_ANALYSIS_TMPL'], current_cycle_dict)
        comout_med_restart_prev = self._replace_template_vars(com_templates['COM_MED_RESTART_TMPL'], previous_cycle_dict)
        comout_med_restart = self._replace_template_vars(com_templates['COM_MED_RESTART_TMPL'], current_cycle_dict)
        comout_wave_restart_prev = self._replace_template_vars(com_templates['COM_WAVE_RESTART_TMPL'], previous_cycle_dict)
        comout_wave_restart = self._replace_template_vars(com_templates['COM_WAVE_RESTART_TMPL'], current_cycle_dict)

        # GEFS-specific ensemble statistics directories
        comout_atmos_ensstat = self._replace_template_vars(com_templates['COM_ATMOS_ENSSTAT_TMPL'], current_cycle_dict)
        comout_ocean_ensstat = self._replace_template_vars(com_templates['COM_OCEAN_ENSSTAT_TMPL'], current_cycle_dict)
        comout_ice_ensstat = self._replace_template_vars(com_templates['COM_ICE_ENSSTAT_TMPL'], current_cycle_dict)

        # Append to lists - replaces Jinja {% do list.append() %} operations
        com_dir_lists['COMIN_ATMOS_INPUT_MEM_list'].append(comin_atmos_input)
        com_dir_lists['COMIN_ATMOS_RESTART_PREV_MEM_list'].append(comin_atmos_restart_prev)
        com_dir_lists['COMIN_ICE_RESTART_PREV_MEM_list'].append(comin_ice_restart_prev)
        com_dir_lists['COMIN_OCEAN_RESTART_PREV_MEM_list'].append(comin_ocean_restart_prev)
        com_dir_lists['COMIN_MED_RESTART_PREV_MEM_list'].append(comin_med_restart_prev)
        com_dir_lists['COMIN_WAVE_RESTART_PREV_MEM_list'].append(comin_wave_restart_prev)

        com_dir_lists['COMOUT_ATMOS_INPUT_MEM_list'].append(comout_atmos_input)
        com_dir_lists['COMOUT_ATMOS_RESTART_PREV_MEM_list'].append(comout_atmos_restart_prev)
        com_dir_lists['COMOUT_ATMOS_RESTART_MEM_list'].append(comout_atmos_restart)
        com_dir_lists['COMOUT_ATMOS_ANALYSIS_MEM_list'].append(comout_atmos_analysis)
        com_dir_lists['COMOUT_ICE_ANALYSIS_MEM_list'].append(comout_ice_analysis)
        com_dir_lists['COMOUT_ICE_RESTART_PREV_MEM_list'].append(comout_ice_restart_prev)
        com_dir_lists['COMOUT_ICE_RESTART_MEM_list'].append(comout_ice_restart)
        com_dir_lists['COMOUT_OCEAN_RESTART_PREV_MEM_list'].append(comout_ocean_restart_prev)
        com_dir_lists['COMOUT_OCEAN_RESTART_MEM_list'].append(comout_ocean_restart)
        com_dir_lists['COMOUT_OCEAN_ANALYSIS_MEM_list'].append(comout_ocean_analysis)
        com_dir_lists['COMOUT_MED_RESTART_PREV_MEM_list'].append(comout_med_restart_prev)
        com_dir_lists['COMOUT_MED_RESTART_MEM_list'].append(comout_med_restart)
        com_dir_lists['COMOUT_WAVE_RESTART_PREV_MEM_list'].append(comout_wave_restart_prev)
        com_dir_lists['COMOUT_WAVE_RESTART_MEM_list'].append(comout_wave_restart)

        com_dir_lists['COMOUT_ATMOS_ENSSTAT_MEM_list'].append(comout_atmos_ensstat)
        com_dir_lists['COMOUT_OCEAN_ENSSTAT_MEM_list'].append(comout_ocean_ensstat)
        com_dir_lists['COMOUT_ICE_ENSSTAT_MEM_list'].append(comout_ice_ensstat)

    def _replace_template_vars(self, template: str, var_dict: Dict[str, str]) -> str:
        """Replace template variables in string with actual values

        This replaces the Jinja replace_tmpl filter functionality

        Parameters
        ----------
        template : str
            Template string with variables to replace
        var_dict : Dict[str, str]
            Dictionary of variable names and values

        Returns
        -------
        str
            String with variables replaced
        """
        result = template
        for var, value in var_dict.items():
            result = result.replace(var, value)
        return result

    @logit(logger)
    def calculate_application_specific_variables(self) -> Dict[str, Any]:
        """Calculate application-specific variables for different master YAML templates

        Returns
        -------
        Dict[str, Any]
            Dictionary containing application-specific variables
        """
        app_vars = {}

        RUN = getattr(self.task_config, 'RUN', 'gfs')
        MODE = getattr(self.task_config, 'MODE', 'forecast-only')

        # Application detection flags
        app_vars['is_gfs'] = (RUN == 'gfs')
        app_vars['is_gefs'] = (RUN == 'gefs')
        app_vars['is_gefs_rt'] = (RUN == 'gefs' and MODE == 'forecast-only')
        app_vars['is_gcafs'] = (RUN in ['gcafs', 'enkfgdas'])
        app_vars['is_ensemble'] = (RUN in ['gefs', 'enkfgdas'])

        # Grid configuration - supporting all applications
        app_vars['ntiles'] = getattr(self.task_config, 'ntiles', 6)
        app_vars['CASE'] = getattr(self.task_config, 'CASE', 'C48')
        app_vars['LEVS'] = getattr(self.task_config, 'LEVS', 127)
        app_vars['OCNRES'] = getattr(self.task_config, 'OCNRES', 100)
        app_vars['waveGRD'] = getattr(self.task_config, 'waveGRD', 'mx025')

        # GCAFS-specific configurations
        if RUN in ['gcafs', 'enkfgdas']:
            app_vars['rRUN'] = "gdas"  # Always use GDAS for restart
            app_vars['gcafs_mode'] = True
        else:
            app_vars['gcafs_mode'] = False

        return app_vars

    @logit(logger)
    def generate_stage_template_paths(self) -> List[str]:
        """Generate template paths for stage IC processing

        Returns
        -------
        List[str]
            List of template paths for stage YAML templates
        """
        template_paths = []

        RUN = getattr(self.task_config, 'RUN', 'gfs')
        MODE = getattr(self.task_config, 'MODE', 'forecast-only')

        # Application-specific master templates following build_all.sh patterns
        if RUN == 'gfs':
            master_template = os.path.join(self.task_config.HOMEgfs, 'parm', 'stage', 'master_gfs.yaml.j2')
        elif RUN == 'gefs':
            if MODE == 'forecast-only':
                # Check for RT variant first
                rt_template = os.path.join(self.task_config.HOMEgfs, 'parm', 'stage', 'master_gefs_RT.yaml.j2')
                if os.path.exists(rt_template):
                    master_template = rt_template
                else:
                    master_template = os.path.join(self.task_config.HOMEgfs, 'parm', 'stage', 'master_gefs.yaml.j2')
            else:
                master_template = os.path.join(self.task_config.HOMEgfs, 'parm', 'stage', 'master_gefs.yaml.j2')
        elif RUN in ['gcafs', 'enkfgdas']:
            master_template = os.path.join(self.task_config.HOMEgfs, 'parm', 'stage', 'master_gcafs.yaml.j2')
        else:
            # Fallback to GFS template for unknown RUN types
            master_template = os.path.join(self.task_config.HOMEgfs, 'parm', 'stage', 'master_gfs.yaml.j2')

        template_paths.append(master_template)

        # RUN-specific template paths
        run_specific_template = os.path.join(
            self.task_config.HOMEgfs,
            'parm',
            'stage',
            f'{RUN}.yaml.j2'
        )
        if os.path.exists(run_specific_template):
            template_paths.append(run_specific_template)

        return template_paths

    @logit(logger)
    def generate_archive_template_paths(self) -> List[str]:
        """Generate template paths for archive processing

        Returns
        -------
        List[str]
            List of template paths for archive YAML templates
        """
        template_paths = []

        # Base archive template path
        base_template = os.path.join(self.task_config.HOMEgfs, 'parm', 'archive', 'master_archive.yaml.j2')
        template_paths.append(base_template)

        # RUN-specific template paths
        RUN = getattr(self.task_config, 'RUN', 'gfs')
        run_specific_template = os.path.join(
            self.task_config.HOMEgfs,
            'parm',
            'archive',
            f'{RUN}_archive.yaml.j2'
        )
        if os.path.exists(run_specific_template):
            template_paths.append(run_specific_template)

        # Application-specific archive templates
        MODE = getattr(self.task_config, 'MODE', 'forecast-only')
        app_template = os.path.join(
            self.task_config.HOMEgfs,
            'parm',
            'archive',
            f'{RUN}_{MODE}_archive.yaml.j2'
        )
        if os.path.exists(app_template):
            template_paths.append(app_template)

        return template_paths

    @logit(logger)
    def calculate_yaml_variables(self) -> Dict[str, Any]:
        """Calculate all variables needed for YAML template processing

        Combines cycle variables, member variables, application-specific variables,
        and standard configuration. This replaces the complex Jinja logic across:
        - master_gfs.yaml.j2: GFS deterministic forecast system
        - master_gefs.yaml.j2: GEFS ensemble forecast system
        - master_gefs_RT.yaml.j2: GEFS real-time ensemble system
        - master_gcafs.yaml.j2: GCAFS climate analysis system

        Returns
        -------
        Dict[str, Any]
            Dictionary containing all calculated variables for YAML processing
        """
        yaml_vars = {}

        # Basic cycle information
        yaml_vars['CDATE'] = self.task_config.CDATE
        yaml_vars['PDY'] = self.task_config.PDY
        yaml_vars['cyc'] = self.task_config.cyc
        yaml_vars['RUN'] = self.task_config.RUN

        # Directory paths
        yaml_vars['ROTDIR'] = self.task_config.ROTDIR
        yaml_vars['DATAROOT'] = self.task_config.DATAROOT
        yaml_vars['HOMEgfs'] = self.task_config.HOMEgfs
        yaml_vars['ICSDIR'] = getattr(self.task_config, 'ICSDIR', '')

        if hasattr(self.task_config, 'EXPDIR'):
            yaml_vars['EXPDIR'] = self.task_config.EXPDIR

        # Archive-specific paths
        yaml_vars['ARCDIR'] = getattr(self.task_config, 'ARCDIR', '')
        yaml_vars['ATARDIR'] = getattr(self.task_config, 'ATARDIR', '')

        # Application-specific variables
        yaml_vars['MODE'] = getattr(self.task_config, 'MODE', 'forecast-only')
        yaml_vars['EXP_WARM_START'] = getattr(self.task_config, 'EXP_WARM_START', False)

        # Comprehensive component flags for all applications
        component_flags = [
            'DO_ATM', 'DO_OCN', 'DO_ICE', 'DO_WAVE', 'DO_AERO',
            'DO_NEST', 'REPLAY_ICS', 'DO_JEDIOCNVAR', 'DO_AERO_ANL',
            'DO_VRFY', 'DO_METP', 'DO_FIT2OBS', 'DO_VERFOZN', 'DO_VERFRAD',
            'USE_ATM_ENS_PERTURB_FILES', 'USE_OCN_ENS_PERTURB_FILES',
            'DOIAU', 'DO_JEDIATMVAR', 'DO_STARTMEM_FROM_JEDIICE', 'DO_CA',
            'DO_LAND_DA', 'DO_SNOW_DA'  # Additional GCAFS flags
        ]

        for flag in component_flags:
            if hasattr(self.task_config, flag):
                yaml_vars[flag] = getattr(self.task_config, flag)

        # Ensemble configuration
        yaml_vars['NMEM_ENS'] = getattr(self.task_config, 'NMEM_ENS', 20)
        yaml_vars['assim_freq'] = getattr(self.task_config, 'assim_freq', 6)

        # Add cycle variables from master YAML logic
        cycle_vars = self.calculate_cycle_variables()
        yaml_vars.update(cycle_vars)

        # Add member variables from master YAML logic
        member_vars = self.calculate_member_variables()
        yaml_vars.update(member_vars)

        # Add application-specific variables
        app_vars = self.calculate_application_specific_variables()
        yaml_vars.update(app_vars)

        # Archive frequency and retention settings
        yaml_vars['HPSSARCH'] = getattr(self.task_config, 'HPSSARCH', 'NO')
        yaml_vars['LOCALARCH'] = getattr(self.task_config, 'LOCALARCH', 'NO')
        yaml_vars['FHMAX_GFS'] = getattr(self.task_config, 'FHMAX_GFS', 384)

        # Add path existence function for YAML processing
        yaml_vars['path_exists'] = os.path.exists

        # Add COM template variables - comprehensive set for all applications
        com_templates = self._get_com_templates()
        yaml_vars.update(com_templates)

        return yaml_vars

    @logit(logger)
    def get_stage_template_path(self) -> str:
        """Get the primary stage template path based on RUN

        Returns
        -------
        str
            Path to the primary stage YAML template
        """
        RUN = getattr(self.task_config, 'RUN', 'gfs')
        MODE = getattr(self.task_config, 'MODE', 'forecast-only')

        # Follow build_all.sh application patterns
        if RUN == 'gfs':
            template_name = 'master_gfs.yaml.j2'
        elif RUN == 'gefs':
            if MODE == 'forecast-only':
                # Check for RT variant
                rt_template = os.path.join(self.task_config.HOMEgfs, 'parm', 'stage', 'master_gefs_RT.yaml.j2')
                if os.path.exists(rt_template):
                    template_name = 'master_gefs_RT.yaml.j2'
                else:
                    template_name = 'master_gefs.yaml.j2'
            else:
                template_name = 'master_gefs.yaml.j2'
        elif RUN in ['gcafs', 'enkfgdas']:
            template_name = 'master_gcafs.yaml.j2'
        else:
            template_name = 'master_gfs.yaml.j2'  # Fallback

        return os.path.join(
            self.task_config.HOMEgfs,
            'parm',
            'stage',
            template_name
        )

    @logit(logger)
    def get_archive_template_path(self) -> str:
        """Get the primary archive template path

        Returns
        -------
        str
            Path to the primary archive YAML template
        """
        return os.path.join(
            self.task_config.HOMEgfs,
            'parm',
            'archive',
            'master_archive.yaml.j2'
        )

    @logit(logger)
    def validate_template_paths(self, template_paths: List[str]) -> List[str]:
        """Validate that template paths exist

        Parameters
        ----------
        template_paths : List[str]
            List of template paths to validate

        Returns
        -------
        List[str]
            List of validated template paths that exist

        Raises
        ------
        FileNotFoundError
            If no valid template paths found
        """
        validated_paths = []

        for path in template_paths:
            if os.path.exists(path):
                validated_paths.append(path)
            else:
                logger.warning(f"Template path does not exist: {path}")

        if not validated_paths:
            raise FileNotFoundError("No valid template paths found for stage/archive processing")

        return validated_paths

    @logit(logger)
    def prepare_stage_configuration(self) -> Dict[str, Any]:
        """Prepare configuration for stage operations

        Returns
        -------
        Dict[str, Any]
            Configuration dictionary with calculated variables for stage processing
        """
        config = self.calculate_yaml_variables()
        template_paths = self.generate_stage_template_paths()
        validated_paths = self.validate_template_paths(template_paths)

        config['template_paths'] = validated_paths
        return config

    @logit(logger)
    def prepare_archive_configuration(self) -> Dict[str, Any]:
        """Prepare configuration for archive operations

        Returns
        -------
        Dict[str, Any]
            Configuration dictionary with calculated variables for archive processing
        """
        config = self.calculate_yaml_variables()
        template_paths = self.generate_archive_template_paths()
        validated_paths = self.validate_template_paths(template_paths)

        config['template_paths'] = validated_paths
        return config
