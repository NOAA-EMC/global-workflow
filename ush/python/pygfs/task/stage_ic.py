#!/usr/bin/env python3
"""
Stage Initial Conditions (IC) Task

Overview
--------
This module constructs cycle and member-specific COM directory path variables
required for initial conditions for the Stage IC task.
"""
import os
from logging import getLogger
from typing import Any, Dict, Tuple, Optional
from wxflow import FileHandler, Task, logit, parse_j2yaml, AttrDict, to_YMD, to_fv3time, add_to_datetime, to_timedelta

logger = getLogger(__name__.split('.')[-1])


class Stage(Task):
    """Task to stage initial conditions"""

    @logit(logger, name="Stage")
    def __init__(self, config: Dict[str, Any]) -> None:
        """Constructor for the Stage task

        Parameters
        ----------
        config : Dict[str, Any]
            Incoming configuration for the task from the environment
        """
        super().__init__(config)

    @logit(logger)
    def _copy_base_config(self) -> Dict[str, Any]:
        """Copy essential base configuration from task_config

        Returns
        -------
        Dict[str, Any]
            Dictionary with base configuration values
        """
        base_keys = [
            'RUN', 'MODE', 'EXP_WARM_START', 'NMEM_ENS',
            'assim_freq', 'current_cycle', 'previous_cycle',
            'ROTDIR', 'ICSDIR', 'STAGE_IC_YAML_TMPL', 'DO_JEDIATMVAR',
            'OCNRES', 'waveGRD', 'ntiles', 'DOIAU', 'ATMINC_GRID',
            'DO_JEDIOCNVAR', 'DO_STARTMEM_FROM_JEDIICE',
            'DO_WAVE', 'DO_OCN', 'DO_ICE', 'DO_NEST', 'DO_CA', 'DO_AERO_ANL',
            'USE_ATM_ENS_PERTURB_FILES', 'USE_OCN_ENS_PERTURB_FILES', 'DO_GSISOILDA', 'DO_LAND_IAU'
        ]

        if self.task_config.get('NET') == 'gfs':
            base_keys.append('DOIAU_ENKF')

        return {key: self.task_config[key] for key in base_keys if key in self.task_config}

    @logit(logger)
    def _get_config_vars(self) -> Dict[str, Any]:
        """Calculate derived configuration variables

        Returns
        -------
        Dict[str, Any]
            Dictionary with derived configuration values (rRUN, OCNRES, START_ICE_FROM_ANA)
        """
        config_vars = {}

        # Determine rRUN
        # For GFS/GCAFS/GCDAS, always use 'gdas' for initial conditions from previous cycles
        # TODO: Update when GCDAS/GCAFS-specific ICs become available
        config_vars['rRUN'] = "gdas" if self.task_config.RUN in ['gfs', 'gcafs', 'gcdas'] else self.task_config.RUN

        # OCNRES formatting
        if "OCNRES" in self.task_config:
            config_vars['OCNRES'] = f"{int(self.task_config.OCNRES):03d}"

        # START_ICE_FROM_ANA logic
        if self.task_config.get("DO_ICE", False):
            config_vars['START_ICE_FROM_ANA'] = False
            if self.task_config.get("DO_JEDIOCNVAR", False) and self.task_config.RUN == "gdas":
                config_vars['START_ICE_FROM_ANA'] = True
            if self.task_config.get("DO_STARTMEM_FROM_JEDIICE", False) and self.task_config.RUN == "enkfgdas":
                config_vars['START_ICE_FROM_ANA'] = True

        return config_vars

    @logit(logger)
    def _get_cycle_vars(self) -> Dict[str, Any]:
        """Calculate current and previous cycle variables

        Returns
        -------
        Dict[str, Any]
            Dictionary with current and previous cycle variables including half_window
        """
        cycle_vars = {}
        half_window = self.task_config.assim_freq // 2
        cycle_vars['half_window'] = half_window

        # Current cycle variables
        cycle_vars['current_cycle_HH'] = self.task_config.current_cycle.strftime("%H")
        cycle_vars['current_cycle_YMD'] = to_YMD(self.task_config.current_cycle)

        if self.task_config.DOIAU and self.task_config.MODE == "cycled":
            cycle_vars['model_start_date_current_cycle'] = add_to_datetime(self.task_config.current_cycle, to_timedelta(f"-{half_window}H"))
        else:
            if self.task_config.get('REPLAY_ICS', False):
                cycle_vars['model_start_date_current_cycle'] = add_to_datetime(self.task_config.current_cycle, to_timedelta(f"{half_window}H"))
            else:
                cycle_vars['model_start_date_current_cycle'] = self.task_config.current_cycle

        cycle_vars['m_prefix'] = to_fv3time(cycle_vars['model_start_date_current_cycle'])

        # Previous cycle variables
        previous_cycle_HH = self.task_config.previous_cycle.strftime("%H")
        cycle_vars['m_index'] = self.task_config.current_cycle.hour // self.task_config.assim_freq
        cycle_vars['p_prefix'] = to_fv3time(self.task_config.previous_cycle)
        cycle_vars['previous_cycle_HH'] = previous_cycle_HH
        cycle_vars['previous_cycle_YMD'] = to_YMD(self.task_config.previous_cycle)
        cycle_vars['mid_cyc'] = int(previous_cycle_HH) + int(half_window)

        return cycle_vars

    @logit(logger)
    def _create_cycle_dicts(self, rotdir: str, run: str) -> Dict[str, Dict[str, str]]:
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
            'current_cycle_dict': {
                "${ROTDIR}": rotdir,
                "${RUN}": run,
                "${YMD}": to_YMD(self.task_config.current_cycle),
                "${HH}": self.task_config.current_cycle.strftime("%H"),
            },
            'previous_cycle_dict': {
                "${ROTDIR}": rotdir,
                "${RUN}": run,
                "${YMD}": to_YMD(self.task_config.previous_cycle),
                "${HH}": self.task_config.previous_cycle.strftime("%H"),
            }
        }

    @logit(logger)
    def _copy_com_templates(self) -> Dict[str, str]:
        """Copy COM templates from task_config

        Returns
        -------
        Dict[str, str]
            Dictionary with COM template paths
        """
        return {key: self.task_config[key] for key in self.task_config.keys()
                if key.startswith('COM_') and key.endswith('_TMPL')}

    @logit(logger)
    def create_stage_dict(self) -> AttrDict:
        """Create staging dictionary with all necessary variables for YAML templates

        Returns
        -------
        AttrDict
            Dictionary containing all variables needed for staging YAML templates
        """
        stage_dict = AttrDict()

        # Copy essential base configuration
        stage_dict.update(self._copy_base_config())

        # Calculate derived configuration
        stage_dict.update(self._get_config_vars())

        # Calculate current and previous cycle variables
        stage_dict.update(self._get_cycle_vars())

        # Create cycle directories for template substitution
        stage_dict.update(self._create_cycle_dicts(stage_dict.ROTDIR, stage_dict.RUN))

        # Copy COM templates
        stage_dict.update(self._copy_com_templates())

        # Add GEFSTYPE if available (for GEFS runs)
        if 'GEFSTYPE' in self.task_config:
            stage_dict['GEFSTYPE'] = self.task_config.GEFSTYPE

        # Calculate member list based on RUN type
        stage_dict['member_list'] = self.get_member_list(
            stage_dict.RUN,
            stage_dict.NMEM_ENS,
            m_index=stage_dict.get('m_index', 0),
            gefstype=stage_dict.get('GEFSTYPE', None)
        )

        # Add os.path.exists function for template use
        stage_dict['path_exists'] = os.path.exists

        return stage_dict

    @logit(logger)
    def execute_stage(self, stage_dict: AttrDict) -> None:
        """Perform local staging of initial condition files

        Parameters
        ----------
        stage_dict : AttrDict
            Configuration dictionary with all necessary staging variables
        """
        if not os.path.isdir(stage_dict.ROTDIR):
            raise FileNotFoundError(f"FATAL ERROR: The ROTDIR ({stage_dict.ROTDIR}) does not exist!")

        stage_set = parse_j2yaml(stage_dict.STAGE_IC_YAML_TMPL, stage_dict, allow_missing=False)

        for key in stage_set.keys():
            FileHandler(stage_set[key]).sync()

    @staticmethod
    @logit(logger)
    def get_member_com_paths(stage_dict: AttrDict, member: int) -> Dict[str, Any]:
        """Get member-specific COM paths based on RUN type

        Parameters
        ----------
        stage_dict : AttrDict
            Staging configuration dictionary
        member : int
            Member directory number

        Returns
        -------
        Dict[str, Any]
            Dictionary of member-specific COM paths

        Raises
        ------
        ValueError
            If RUN type is unknown or GEFSTYPE is invalid for GEFS runs
        """

        if stage_dict.RUN == 'gefs':
            gefstype = stage_dict.get('GEFSTYPE', None)
            if gefstype == 'gefs-real-time':
                com_path = Stage._get_member_com_paths_gefs_rt(stage_dict, member)
            elif gefstype == 'gefs-offline':
                com_path = Stage._get_member_com_paths_gefs_offline(stage_dict, member)
            else:
                raise ValueError(f"Invalid GEFSTYPE '{gefstype}' for RUN 'gefs'.")
        elif stage_dict.RUN == 'sfs':
            com_path = Stage._get_member_com_paths_gefs_offline(stage_dict, member)
        elif stage_dict.RUN in ('gcafs', 'enkfgdas', 'gcdas', 'gdas'):
            com_path = Stage._get_member_com_paths_gcafs(stage_dict, member)
        elif stage_dict.RUN == 'gfs':
            com_path = Stage._get_member_com_paths_gfs(stage_dict, member)
        else:
            raise ValueError(f"Unknown RUN type: {stage_dict.RUN}")

        return Stage._paths_from_templates(stage_dict, com_path)

    @staticmethod
    @logit(logger)
    def get_member_list(run: str, nmem_ens: int, m_index: int = 0, gefstype: Optional[str] = None) -> list[int]:
        """Get list of member indices based on RUN type

        Parameters
        ----------
        run : str
            RUN type (e.g., 'gfs', 'gefs', 'enkfgdas', 'gdas', 'gcafs')
        nmem_ens : int
            Total number of ensemble members
        m_index : int, optional
            Cycle index for GEFS real-time mapping (default: 0)
        gefstype : Optional[str], optional
            GEFS type ('gefs-real-time' or 'gefs-offline')

        Returns
        -------
        list[int]
            List of member indices to process
        """
        if run in ['enkfgdas']:
            return list(range(1, nmem_ens + 1))
        if run in ['sfs']:
            return list(range(0, nmem_ens + 1))
        elif run in ['gefs']:
            if gefstype == 'gefs-real-time':
                # Map GEFS members to GFS member numbers
                # Cycle ranges determine which 30-member range to use based on cycle index
                cyc_ranges = {'00': list(range(1, 31)),
                              '06': list(range(21, 51)),
                              '12': list(range(41, 71)),
                              '18': list(range(61, 81)) + list(range(1, 11))}
                member_list = [0] + cyc_ranges[f'{m_index:02}']
                return member_list
            else:
                # GEFS offline uses sequential member numbering
                return list(range(0, nmem_ens + 1))
        else:
            # Deterministic runs (GFS, GDAS, GCAFS, GCDAS)
            # Only member -1 is processed (empty string for MEMDIR)
            return [-1]

    @staticmethod
    @logit(logger)
    def _paths_from_templates(stage_dict: AttrDict, com_path_tuples: Tuple[Tuple[str, str, Dict[str, Any]], ...]) -> Dict[str, str]:
        """Generate COM paths from template configurations

        Parameters
        ----------
        stage_dict : AttrDict
            Staging configuration dictionary
        com_path_tuples : Tuple[Tuple[str, str, Dict[str, Any]], ...]
            Tuple of tuples containing path key, template key, and substitution dict

        Returns
        -------
        Dict[str, str]
            Dictionary mapping COM path keys to resolved file paths
        """
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

        path_dict = {}
        for com_key, template_key, substitution_dict in com_path_tuples:
            template_str = stage_dict.get(template_key, '')
            if not template_str:
                logger.warning("Template key '%s' not found for COM key '%s'", template_key, com_key)
                path_dict[com_key] = ''
            else:
                path_dict[com_key] = _replace_template_vars(template_str, substitution_dict)
        return path_dict

    @staticmethod
    @logit(logger)
    def _get_member_com_paths_gfs(stage_dict: AttrDict, member: int) -> Tuple[Tuple[str, str, Dict[str, Any]], ...]:
        """Get member COM paths for GFS

        Parameters
        ----------
        stage_dict : AttrDict
            Staging configuration dictionary
        member : int
            The member directory number

        Returns
        -------
        Tuple[Tuple[str, str, Dict[str, Any]], ...]
            Tuple of tuples containing path key, template key, and substitution dict
        """
        member_str = f"mem{member:03d}" if member >= 0 else ''
        current_cycle_mem_dict = {**stage_dict.current_cycle_dict, "${MEMDIR}": member_str}
        previous_cycle_mem_dict = {**stage_dict.previous_cycle_dict, "${MEMDIR}": member_str, "${RUN}": stage_dict.rRUN}

        return (
            ('COMIN_ATMOS_INPUT', 'COM_ATMOS_INPUT_TMPL', current_cycle_mem_dict),
            ('COMOUT_ATMOS_INPUT', 'COM_ATMOS_INPUT_TMPL', current_cycle_mem_dict),
            ('COMOUT_ATMOS_RESTART_PREV', 'COM_ATMOS_RESTART_TMPL', previous_cycle_mem_dict),
            ('COMOUT_ATMOS_RESTART', 'COM_ATMOS_RESTART_TMPL', current_cycle_mem_dict),
            ('COMOUT_ATMOS_ANALYSIS', 'COM_ATMOS_ANALYSIS_TMPL', current_cycle_mem_dict),
            ('COMOUT_ICE_ANALYSIS', 'COM_ICE_ANALYSIS_TMPL', current_cycle_mem_dict),
            ('COMOUT_ICE_RESTART_PREV', 'COM_ICE_RESTART_TMPL', previous_cycle_mem_dict),
            ('COMOUT_OCEAN_RESTART_PREV', 'COM_OCEAN_RESTART_TMPL', previous_cycle_mem_dict),
            ('COMOUT_OCEAN_ANALYSIS', 'COM_OCEAN_ANALYSIS_TMPL', current_cycle_mem_dict),
            ('COMOUT_MED_RESTART_PREV', 'COM_MED_RESTART_TMPL', previous_cycle_mem_dict),
            ('COMOUT_CHEM_ANALYSIS', 'COM_CHEM_ANALYSIS_TMPL', current_cycle_mem_dict),
            ('COMOUT_WAVE_RESTART_PREV', 'COM_WAVE_RESTART_TMPL', previous_cycle_mem_dict),
        )

    @staticmethod
    @logit(logger)
    def _get_member_com_paths_gefs_offline(stage_dict: AttrDict, member: int) -> Tuple[Tuple[str, str, Dict[str, Any]], ...]:
        """Get member COM paths for GEFS offline

        Parameters
        ----------
        stage_dict : AttrDict
            Staging configuration dictionary
        member : int
            The member directory number

        Returns
        -------
        Tuple[Tuple[str, str, Dict[str, Any]], ...]
            Tuple of tuples containing path key, template key, and substitution dict
        """
        member_str = f"mem{member:03d}" if member >= 0 else ''
        current_cycle = {**stage_dict.current_cycle_dict, "${MEMDIR}": member_str}
        previous_cycle = {**stage_dict.previous_cycle_dict, "${MEMDIR}": member_str}

        return (
            ('COMIN_ATMOS_INPUT', 'COM_ATMOS_INPUT_TMPL', current_cycle),
            ('COMOUT_ATMOS_INPUT', 'COM_ATMOS_INPUT_TMPL', current_cycle),
            ('COMOUT_ATMOS_RESTART_PREV', 'COM_ATMOS_RESTART_TMPL', previous_cycle),
            ('COMOUT_ATMOS_RESTART', 'COM_ATMOS_RESTART_TMPL', current_cycle),
            ('COMOUT_ATMOS_ANALYSIS', 'COM_ATMOS_ANALYSIS_TMPL', current_cycle),
            ('COMOUT_ATMOS_HISTORY', 'COM_ATMOS_HISTORY_TMPL', previous_cycle),
            ('COMOUT_ICE_ANALYSIS', 'COM_ICE_ANALYSIS_TMPL', current_cycle),
            ('COMOUT_ICE_RESTART_PREV', 'COM_ICE_RESTART_TMPL', previous_cycle),
            ('COMOUT_OCEAN_RESTART_PREV', 'COM_OCEAN_RESTART_TMPL', previous_cycle),
            ('COMOUT_OCEAN_ANALYSIS', 'COM_OCEAN_ANALYSIS_TMPL', current_cycle),
            ('COMOUT_MED_RESTART_PREV', 'COM_MED_RESTART_TMPL', previous_cycle),
            ('COMOUT_WAVE_RESTART_PREV', 'COM_WAVE_RESTART_TMPL', previous_cycle),
        )

    @staticmethod
    @logit(logger)
    def _get_member_com_paths_gefs_rt(stage_dict: AttrDict, member: int) -> Tuple[Tuple[str, str, Dict[str, Any]], ...]:
        """Get member COM paths for GEFS real-time

        Parameters
        ----------
        stage_dict : AttrDict
            Staging configuration dictionary
        member : int
            The GFS member directory number (already mapped from GEFS)

        Returns
        -------
        Tuple[Tuple[str, str, Dict[str, Any]], ...]
            Tuple of tuples containing path key, template key, and substitution dict
        """
        member_str = f"mem{member:03d}" if member >= 0 else ''
        current_cycle = {**stage_dict.current_cycle_dict, "${MEMDIR}": member_str}
        previous_cycle = {**stage_dict.previous_cycle_dict, "${MEMDIR}": member_str}

        return (
            ('COMIN_ATMOS_INPUT', 'COM_ATMOS_INPUT_TMPL', current_cycle),
            ('COMOUT_ATMOS_INPUT', 'COM_ATMOS_INPUT_TMPL', current_cycle),
            ('COMOUT_ATMOS_RESTART_PREV', 'COM_ATMOS_RESTART_TMPL', previous_cycle),
            ('COMOUT_ATMOS_ANALYSIS', 'COM_ATMOS_ANALYSIS_TMPL', current_cycle),
            ('COMOUT_ATMOS_HISTORY', 'COM_ATMOS_HISTORY_TMPL', previous_cycle),
            ('COMOUT_ICE_ANALYSIS', 'COM_ICE_ANALYSIS_TMPL', current_cycle),
            ('COMOUT_ICE_RESTART_PREV', 'COM_ICE_RESTART_TMPL', previous_cycle),
            ('COMOUT_OCEAN_RESTART_PREV', 'COM_OCEAN_RESTART_TMPL', previous_cycle),
            ('COMOUT_OCEAN_ANALYSIS', 'COM_OCEAN_ANALYSIS_TMPL', current_cycle),
            ('COMOUT_MED_RESTART_PREV', 'COM_MED_RESTART_TMPL', previous_cycle),
            ('COMOUT_WAVE_RESTART_PREV', 'COM_WAVE_RESTART_TMPL', previous_cycle),
        )

    @staticmethod
    @logit(logger)
    def _get_member_com_paths_gcafs(stage_dict: AttrDict, member: int) -> Tuple[Tuple[str, str, Dict[str, Any]], ...]:
        """Get member COM paths for GCAFS

        Parameters
        ----------
        stage_dict : AttrDict
            Staging configuration dictionary
        member : int
            The member directory number

        Returns
        -------
        Tuple[Tuple[str, str, Dict[str, Any]], ...]
            Tuple of tuples containing path key, template key, and substitution dict
        """
        member_str = f"mem{member:03d}" if member >= 0 else ''
        current_cycle_in = {**stage_dict.current_cycle_dict, "${MEMDIR}": member_str, "${RUN}": stage_dict.rRUN}
        current_cycle = {**current_cycle_in, "${RUN}": stage_dict.RUN}
        previous_cycle = {**stage_dict.previous_cycle_dict, "${MEMDIR}": member_str, "${RUN}": stage_dict.rRUN}

        return (
            ('COMIN_ATMOS_INPUT', 'COM_ATMOS_INPUT_TMPL', current_cycle_in),
            ('COMOUT_ATMOS_INPUT', 'COM_ATMOS_INPUT_TMPL', current_cycle),
            ('COMOUT_ATMOS_RESTART_PREV', 'COM_ATMOS_RESTART_TMPL', previous_cycle),
            ('COMOUT_ATMOS_RESTART', 'COM_ATMOS_RESTART_TMPL', current_cycle),
            ('COMOUT_ATMOS_ANALYSIS', 'COM_ATMOS_ANALYSIS_TMPL', current_cycle),
            ('COMOUT_ICE_ANALYSIS', 'COM_ICE_ANALYSIS_TMPL', current_cycle),
            ('COMOUT_ICE_RESTART_PREV', 'COM_ICE_RESTART_TMPL', previous_cycle),
            ('COMOUT_OCEAN_RESTART_PREV', 'COM_OCEAN_RESTART_TMPL', previous_cycle),
            ('COMOUT_OCEAN_ANALYSIS', 'COM_OCEAN_ANALYSIS_TMPL', current_cycle),
            ('COMOUT_MED_RESTART_PREV', 'COM_MED_RESTART_TMPL', previous_cycle),
            ('COMOUT_WAVE_RESTART_PREV', 'COM_WAVE_RESTART_TMPL', previous_cycle),
        )
