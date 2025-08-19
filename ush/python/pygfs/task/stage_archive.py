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
    and YAML variable calculations moved from master_gefs.yaml.j2 and other templates

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
        """Calculate cycle variables from master_gefs.yaml.j2 template logic

        This method replaces the Jinja template variables:
        - half_window
        - half_window_begin/end
        - model_start_date_current_cycle
        - current_cycle_YMD/HH
        - previous_cycle_YMD/HH
        - p_prefix, m_prefix

        Returns
        -------
        Dict[str, Any]
            Dictionary containing calculated cycle variables
        """
        cycle_vars = {}

        # Get basic configuration with fallbacks
        assim_freq = getattr(self.task_config, 'assim_freq', 6)
        current_cycle = getattr(self.task_config, 'current_cycle', None)
        previous_cycle = getattr(self.task_config, 'previous_cycle', None)
        DOIAU = getattr(self.task_config, 'DOIAU', False)
        MODE = getattr(self.task_config, 'MODE', 'forecast-only')
        REPLAY_ICS = getattr(self.task_config, 'REPLAY_ICS', False)

        # Calculate half window variables - replaces {% set half_window = assim_freq // 2 %}
        half_window = assim_freq // 2
        cycle_vars['half_window'] = half_window

        # Calculate time deltas - replaces half_window_begin/end Jinja logic
        half_window_begin = timedelta(hours=-half_window)
        half_window_end = timedelta(hours=half_window)
        cycle_vars['half_window_begin'] = half_window_begin
        cycle_vars['half_window_end'] = half_window_end

        # Calculate model start date for current cycle - replaces complex Jinja conditionals
        if current_cycle:
            if DOIAU and MODE == "cycled":
                model_start_date_current_cycle = current_cycle + half_window_begin
            else:
                if REPLAY_ICS:
                    model_start_date_current_cycle = current_cycle + half_window_end
                else:
                    model_start_date_current_cycle = current_cycle

            cycle_vars['model_start_date_current_cycle'] = model_start_date_current_cycle

            # Calculate YMD and HH formats - replaces Jinja strftime operations
            cycle_vars['current_cycle_YMD'] = current_cycle.strftime("%Y%m%d")
            cycle_vars['current_cycle_HH'] = current_cycle.strftime("%H")
            cycle_vars['m_prefix'] = model_start_date_current_cycle.strftime("%Y%m%d.%H0000")

        # Calculate previous cycle variables
        if previous_cycle:
            cycle_vars['previous_cycle_YMD'] = previous_cycle.strftime("%Y%m%d")
            cycle_vars['previous_cycle_HH'] = previous_cycle.strftime("%H")
            cycle_vars['p_prefix'] = previous_cycle.strftime("%Y%m%d.%H0000")

        return cycle_vars

    @logit(logger)
    def calculate_member_variables(self) -> Dict[str, Any]:
        """Calculate member-related variables from master_gefs.yaml.j2 template logic

        This method replaces the Jinja template variables:
        - first_mem, last_mem
        - COMOUT_*_MEM_list arrays
        - Member loop logic and COM directory construction

        Returns
        -------
        Dict[str, Any]
            Dictionary containing member variables and COM directory lists
        """
        member_vars = {}

        # Get ensemble configuration - replaces {% set first_mem = 0 %} etc.
        NMEM_ENS = getattr(self.task_config, 'NMEM_ENS', 20)
        first_mem = 0  # Always 0 per master_gefs.yaml.j2
        last_mem = NMEM_ENS

        member_vars['first_mem'] = first_mem
        member_vars['last_mem'] = last_mem
        member_vars['NMEM_ENS'] = NMEM_ENS

        # Initialize COM directory lists - replaces Jinja list declarations
        com_dir_lists = {
            'COMOUT_ATMOS_INPUT_MEM_list': [],
            'COMOUT_ATMOS_RESTART_PREV_MEM_list': [],
            'COMOUT_ATMOS_ANALYSIS_MEM_list': [],
            'COMOUT_ICE_ANALYSIS_MEM_list': [],
            'COMOUT_ICE_RESTART_PREV_MEM_list': [],
            'COMOUT_OCEAN_RESTART_PREV_MEM_list': [],
            'COMOUT_OCEAN_ANALYSIS_MEM_list': [],
            'COMOUT_MED_RESTART_PREV_MEM_list': [],
            'COMOUT_WAVE_RESTART_PREV_MEM_list': []
        }

        # Get cycle variables for path construction
        cycle_vars = self.calculate_cycle_variables()
        current_cycle_YMD = cycle_vars.get('current_cycle_YMD', '')
        current_cycle_HH = cycle_vars.get('current_cycle_HH', '')
        previous_cycle_YMD = cycle_vars.get('previous_cycle_YMD', '')
        previous_cycle_HH = cycle_vars.get('previous_cycle_HH', '')

        # Get template paths from configuration
        ROTDIR = getattr(self.task_config, 'ROTDIR', '')
        RUN = getattr(self.task_config, 'RUN', 'gefs')

        # Template patterns from COM directory configuration
        COM_ATMOS_INPUT_TMPL = getattr(self.task_config, 'COM_ATMOS_INPUT_TMPL',
                                     "${ROTDIR}/${RUN}.${YMD}/${HH}/model_data/atmos/input/${MEMDIR}")
        COM_ATMOS_RESTART_TMPL = getattr(self.task_config, 'COM_ATMOS_RESTART_TMPL',
                                       "${ROTDIR}/${RUN}.${YMD}/${HH}/model_data/atmos/restart/${MEMDIR}")
        COM_ATMOS_ANALYSIS_TMPL = getattr(self.task_config, 'COM_ATMOS_ANALYSIS_TMPL',
                                        "${ROTDIR}/${RUN}.${YMD}/${HH}/analysis/atmos/${MEMDIR}")
        COM_ICE_ANALYSIS_TMPL = getattr(self.task_config, 'COM_ICE_ANALYSIS_TMPL',
                                      "${ROTDIR}/${RUN}.${YMD}/${HH}/analysis/ice/${MEMDIR}")
        COM_ICE_RESTART_TMPL = getattr(self.task_config, 'COM_ICE_RESTART_TMPL',
                                     "${ROTDIR}/${RUN}.${YMD}/${HH}/model_data/ice/restart/${MEMDIR}")
        COM_OCEAN_RESTART_TMPL = getattr(self.task_config, 'COM_OCEAN_RESTART_TMPL',
                                       "${ROTDIR}/${RUN}.${YMD}/${HH}/model_data/ocean/restart/${MEMDIR}")
        COM_OCEAN_ANALYSIS_TMPL = getattr(self.task_config, 'COM_OCEAN_ANALYSIS_TMPL',
                                        "${ROTDIR}/${RUN}.${YMD}/${HH}/analysis/ocean/${MEMDIR}")
        COM_MED_RESTART_TMPL = getattr(self.task_config, 'COM_MED_RESTART_TMPL',
                                     "${ROTDIR}/${RUN}.${YMD}/${HH}/model_data/med/restart/${MEMDIR}")
        COM_WAVE_RESTART_TMPL = getattr(self.task_config, 'COM_WAVE_RESTART_TMPL',
                                      "${ROTDIR}/${RUN}.${YMD}/${HH}/model_data/wave/restart/${MEMDIR}")

        # Construct member COM directory lists - replaces Jinja for loop
        for mem in range(first_mem, last_mem + 1):
            memdir = f"mem{mem:03d}"  # Replaces 'mem%03d' | format(mem)

            # Current cycle dictionary for path substitution - replaces Jinja current_cycle_dict
            current_cycle_dict = {
                '${ROTDIR}': ROTDIR,
                '${RUN}': RUN,
                '${YMD}': current_cycle_YMD,
                '${HH}': current_cycle_HH,
                '${MEMDIR}': memdir
            }

            # Previous cycle dictionary for path substitution - replaces Jinja previous_cycle_dict
            previous_cycle_dict = {
                '${ROTDIR}': ROTDIR,
                '${RUN}': RUN,
                '${YMD}': previous_cycle_YMD,
                '${HH}': previous_cycle_HH,
                '${MEMDIR}': memdir
            }

            # Replace template variables in paths - replaces Jinja replace_tmpl filter
            comout_atmos_input_mem = self._replace_template_vars(COM_ATMOS_INPUT_TMPL, current_cycle_dict)
            comout_atmos_restart_prev_mem = self._replace_template_vars(COM_ATMOS_RESTART_TMPL, previous_cycle_dict)
            comout_atmos_analysis_mem = self._replace_template_vars(COM_ATMOS_ANALYSIS_TMPL, current_cycle_dict)
            comout_ice_analysis_mem = self._replace_template_vars(COM_ICE_ANALYSIS_TMPL, current_cycle_dict)
            comout_ice_restart_prev_mem = self._replace_template_vars(COM_ICE_RESTART_TMPL, previous_cycle_dict)
            comout_ocean_restart_prev_mem = self._replace_template_vars(COM_OCEAN_RESTART_TMPL, previous_cycle_dict)
            comout_ocean_analysis_mem = self._replace_template_vars(COM_OCEAN_ANALYSIS_TMPL, current_cycle_dict)
            comout_med_restart_prev_mem = self._replace_template_vars(COM_MED_RESTART_TMPL, previous_cycle_dict)
            comout_wave_restart_prev_mem = self._replace_template_vars(COM_WAVE_RESTART_TMPL, previous_cycle_dict)

            # Append to lists - replaces Jinja {% do list.append() %} operations
            com_dir_lists['COMOUT_ATMOS_INPUT_MEM_list'].append(comout_atmos_input_mem)
            com_dir_lists['COMOUT_ATMOS_RESTART_PREV_MEM_list'].append(comout_atmos_restart_prev_mem)
            com_dir_lists['COMOUT_ATMOS_ANALYSIS_MEM_list'].append(comout_atmos_analysis_mem)
            com_dir_lists['COMOUT_ICE_ANALYSIS_MEM_list'].append(comout_ice_analysis_mem)
            com_dir_lists['COMOUT_ICE_RESTART_PREV_MEM_list'].append(comout_ice_restart_prev_mem)
            com_dir_lists['COMOUT_OCEAN_RESTART_PREV_MEM_list'].append(comout_ocean_restart_prev_mem)
            com_dir_lists['COMOUT_OCEAN_ANALYSIS_MEM_list'].append(comout_ocean_analysis_mem)
            com_dir_lists['COMOUT_MED_RESTART_PREV_MEM_list'].append(comout_med_restart_prev_mem)
            com_dir_lists['COMOUT_WAVE_RESTART_PREV_MEM_list'].append(comout_wave_restart_prev_mem)

        # Add COM directory lists to member_vars
        member_vars.update(com_dir_lists)

        return member_vars

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
    def generate_stage_template_paths(self) -> List[str]:
        """Generate template paths for stage IC processing

        Returns
        -------
        List[str]
            List of template paths for stage YAML templates
        """
        template_paths = []

        # Base stage template path
        base_template = os.path.join(self.task_config.HOMEgfs, 'parm', 'stage', 'master_gefs.yaml.j2')
        template_paths.append(base_template)

        # RUN-specific template paths
        run_specific_template = os.path.join(
            self.task_config.HOMEgfs,
            'parm',
            'stage',
            f'{self.task_config.RUN}.yaml.j2'
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
        run_specific_template = os.path.join(
            self.task_config.HOMEgfs,
            'parm',
            'archive',
            f'{self.task_config.RUN}_archive.yaml.j2'
        )
        if os.path.exists(run_specific_template):
            template_paths.append(run_specific_template)

        # Application-specific archive templates
        app_template = os.path.join(
            self.task_config.HOMEgfs,
            'parm',
            'archive',
            f'{self.task_config.RUN}_{self.task_config.MODE}_archive.yaml.j2'
        )
        if os.path.exists(app_template):
            template_paths.append(app_template)

        return template_paths

    @logit(logger)
    def calculate_yaml_variables(self) -> Dict[str, Any]:
        """Calculate all variables needed for YAML template processing

        Combines cycle variables, member variables, and standard configuration
        This replaces the complex Jinja logic in master_gefs.yaml.j2

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

        if hasattr(self.task_config, 'EXPDIR'):
            yaml_vars['EXPDIR'] = self.task_config.EXPDIR

        # Archive-specific paths
        yaml_vars['ARCDIR'] = getattr(self.task_config, 'ARCDIR', '')
        yaml_vars['ATARDIR'] = getattr(self.task_config, 'ATARDIR', '')

        # Application-specific variables
        if hasattr(self.task_config, 'MODE'):
            yaml_vars['MODE'] = self.task_config.MODE

        if hasattr(self.task_config, 'EXP_WARM_START'):
            yaml_vars['EXP_WARM_START'] = self.task_config.EXP_WARM_START

        # Component flags for conditional processing
        component_flags = [
            'DO_ATM', 'DO_OCN', 'DO_ICE', 'DO_WAVE', 'DO_AERO',
            'DO_NEST', 'REPLAY_ICS', 'DO_JEDIOCNVAR', 'DO_AERO_ANL',
            'DO_VRFY', 'DO_METP', 'DO_FIT2OBS', 'DO_VERFOZN', 'DO_VERFRAD',
            'USE_ATM_ENS_PERTURB_FILES', 'USE_OCN_ENS_PERTURB_FILES',
            'DOIAU'
        ]

        for flag in component_flags:
            if hasattr(self.task_config, flag):
                yaml_vars[flag] = getattr(self.task_config, flag)

        # Add cycle variables from master_gefs.yaml.j2 logic
        cycle_vars = self.calculate_cycle_variables()
        yaml_vars.update(cycle_vars)

        # Add member variables from master_gefs.yaml.j2 logic
        member_vars = self.calculate_member_variables()
        yaml_vars.update(member_vars)

        # Resolution and grid information
        if hasattr(self.task_config, 'CASE'):
            yaml_vars['CASE'] = self.task_config.CASE

        if hasattr(self.task_config, 'OCNRES'):
            yaml_vars['OCNRES'] = self.task_config.OCNRES

        if hasattr(self.task_config, 'LEVS'):
            yaml_vars['LEVS'] = self.task_config.LEVS

        # Archive frequency and retention settings
        yaml_vars['HPSSARCH'] = getattr(self.task_config, 'HPSSARCH', 'NO')
        yaml_vars['LOCALARCH'] = getattr(self.task_config, 'LOCALARCH', 'NO')
        yaml_vars['FHMAX_GFS'] = getattr(self.task_config, 'FHMAX_GFS', 384)

        # Add path existence function for YAML processing
        yaml_vars['path_exists'] = os.path.exists

        return yaml_vars

    @logit(logger)
    def get_stage_template_path(self) -> str:
        """Get the primary stage template path

        Returns
        -------
        str
            Path to the primary stage YAML template
        """
        return os.path.join(
            self.task_config.HOMEgfs,
            'parm',
            'stage',
            'master_gefs.yaml.j2'
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
