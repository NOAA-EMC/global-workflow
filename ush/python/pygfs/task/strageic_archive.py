#!/usr/bin/env python3

import os
from logging import getLogger
from typing import Any, Dict, List

from wxflow import Task, logit, AttrDict

logger = getLogger(__name__.split('.')[-1])


class StageIC_Archive_Utils(Task):
    """Utility class for Archive operations including template path generation
    and YAML variable calculations moved from archive.py
    """

    @logit(logger, name="StageIC_Archive_Utils")
    def __init__(self, config: Dict[str, Any]) -> None:
        """Constructor for the StageIC_Archive_Utils task

        Parameters
        ----------
        config : Dict[str, Any]
            Incoming configuration for the task from the environment

        Returns
        -------
        None
        """
        super().__init__(config)

        rotdir = self.task_config.ROTDIR + os.sep

        # Find all absolute paths in the environment and get their relative paths from ${ROTDIR}
        path_dict = self._gen_relative_paths(rotdir)

        # Extend task_config with path_dict
        self.task_config = AttrDict(**self.task_config, **path_dict)

        # Boolean used for cleanup if the EXPDIR was archived
        self.archive_expdir = False

    @staticmethod
    def _gen_relative_paths(rotdir: str) -> Dict[str, str]:
        """Generate relative paths from ROTDIR for archive operations

        Parameters
        ----------
        rotdir : str
            Root directory path with trailing separator

        Returns
        -------
        Dict[str, str]
            Dictionary of relative paths for archive template processing
        """
        # This method would contain the relative path generation logic
        # from the original archive.py implementation
        return {}

    @logit(logger)
    def generate_template_paths(self) -> List[str]:
        """Generate template paths for archive processing (moved from archive.py)

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
        """Calculate variables needed for YAML template processing (moved from archive.py)

        Returns
        -------
        Dict[str, Any]
            Dictionary containing calculated variables for archive YAML processing
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
        yaml_vars['EXPDIR'] = self.task_config.EXPDIR

        # Archive-specific paths
        yaml_vars['ARCDIR'] = getattr(self.task_config, 'ARCDIR', '')
        yaml_vars['ATARDIR'] = getattr(self.task_config, 'ATARDIR', '')

        # Application-specific variables
        if hasattr(self.task_config, 'MODE'):
            yaml_vars['MODE'] = self.task_config.MODE

        if hasattr(self.task_config, 'EXP_WARM_START'):
            yaml_vars['EXP_WARM_START'] = self.task_config.EXP_WARM_START

        # Component flags for conditional archiving
        component_flags = [
            'DO_ATM', 'DO_OCN', 'DO_ICE', 'DO_WAVE', 'DO_AERO',
            'DO_NEST', 'REPLAY_ICS', 'DO_JEDIOCNVAR', 'DO_AERO_ANL',
            'DO_VRFY', 'DO_METP', 'DO_FIT2OBS', 'DO_VERFOZN', 'DO_VERFRAD'
        ]

        for flag in component_flags:
            if hasattr(self.task_config, flag):
                yaml_vars[flag] = getattr(self.task_config, flag)

        # Ensemble-specific variables
        if self.task_config.RUN in ['gefs']:
            if hasattr(self.task_config, 'NMEM_ENS'):
                yaml_vars['NMEM_ENS'] = self.task_config.NMEM_ENS
                yaml_vars['first_mem'] = 1
                yaml_vars['last_mem'] = self.task_config.NMEM_ENS

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

        # Include relative paths for archive operations
        for key, value in self.task_config.items():
            if key.endswith('_rel'):
                yaml_vars[key] = value

        return yaml_vars

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
            If primary template path does not exist
        """
        validated_paths = []

        for path in template_paths:
            if os.path.exists(path):
                validated_paths.append(path)
            else:
                logger.warning(f"Archive template path does not exist: {path}")

        if not validated_paths:
            raise FileNotFoundError("No valid archive template paths found")

        return validated_paths

    @logit(logger)
    def prepare_archive_configuration(self) -> Dict[str, Any]:
        """Prepare configuration for archive operations

        Returns
        -------
        Dict[str, Any]
            Configuration dictionary with calculated variables for archive processing
        """
        config = self.calculate_yaml_variables()
        template_paths = self.generate_template_paths()
        validated_paths = self.validate_template_paths(template_paths)

        config['template_paths'] = validated_paths
        return config
