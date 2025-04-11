"""
GCAFS forecast-only application configuration module.

This module defines the configuration for running the Global Chemical Aerosol
Forecast System (GCAFS) in forecast-only mode without data assimilation cycling.
"""

from applications.applications import AppConfig
from typing import Dict, Any
from wxflow import Configuration


class GCAFSForecastOnlyAppConfig(AppConfig):
    """
    Class to define GCAFS (Global Chemical Aerosol Forecast System) configurations.

    This class handles configuration management for the GCAFS application in
    forecast-only mode, including task scheduling, run options, and
    application-specific settings.

    Parameters
    ----------
    conf : Configuration
        The configuration object containing all GCAFS settings

    Attributes
    ----------
    run : str
        The name of the current run
    runs : list
        List of all available runs
    """

    def __init__(self, conf: Configuration):
        """
        Initialize the GCAFS application configuration.

        Parameters
        ----------
        conf : Configuration
            Configuration object containing GCAFS settings
        """
        super().__init__(conf)

        base = conf.parse_config('config.base')
        self.run = base.get('RUN', 'gcafs')
        self.runs = [self.run]

    def _get_run_options(self, conf: Configuration) -> Dict[str, Any]:
        """
        Get run-specific options for GCAFS.

        Parameters
        ----------
        conf : Configuration
            Configuration object containing run settings

        Returns
        -------
        Dict[str, Any]
            Dictionary containing run options including ensemble settings
        """
        run_options = super()._get_run_options(conf)
        run_options[self.run]['nens'] = conf.parse_config('config.base').get('NMEM_ENS', 0)
        return run_options

    def _get_app_configs(self, run):
        """
        Get configuration files involved in GCAFS execution.

        Parameters
        ----------
        run : str
            Name of the run configuration to process

        Returns
        -------
        list
            List of configuration file names needed for the GCAFS run
        """
        options = self.run_options[run]
        configs = ['stage_ic', 'fcst', 'atmos_products', 'aero', 'aerosol_init', 'prep_emissions']

        if options['do_wave']:
            configs += ['waveinit', 'wavepostsbs', 'wavepostpnt']

        if options['do_archcom']:
            configs += ['arch_tars']
            if options['do_globusarch']:
                configs += ['globus']

        configs += ['arch_vrfy', 'cleanup']
        return configs

    @staticmethod
    def _update_base(base_in):
        """
        Update base configuration with GCAFS-specific settings.

        Parameters
        ----------
        base_in : dict
            Input base configuration dictionary

        Returns
        -------
        dict
            Updated base configuration with GCAFS settings
        """
        base_out = base_in.copy()
        base_out['RUN'] = 'gcafs'
        return base_out

    def get_task_names(self):
        """
        Get ordered list of tasks for GCAFS workflow.

        This method determines which tasks should be run based on the configuration
        options, including aerosol forecasting, ensemble processing, and archival tasks.

        Returns
        -------
        dict
            Dictionary with run name as key and list of task names as value
        """
        options = self.run_options[self.run]
        tasks = ['stage_ic']

        if options['do_aero_fcst']:
            tasks += ['prep_emissions']
            tasks += ['aerosol_init']

        tasks += ['fcst', 'atmos_prod']

        # Only add ensemble tasks if nens > 0
        if int(options.get('nens', 0)) > 0:
            tasks += ['atmos_ensstat']

        # if options['do_archcom']:
        #     tasks += ['arch_tars']
        #     if options['do_globusarch']:
        #         tasks += ['globus_arch']

        # tasks += ['arch_vrfy', 'cleanup']

        return {f"{self.run}": tasks}
