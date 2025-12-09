"""
Seasonal Forecast System (SFS) application configuration module.

This module defines the SFS application configuration classes used for
configuring and running the Sea Forecast System.
"""

from applications.applications import AppConfig
from typing import Dict, Any
from wxflow import Configuration


class SFSAppConfig(AppConfig):
    """
    Class to define Seasonal Forecast System (SFS) configurations.

    This class handles the SFS-specific application settings and workflows,
    including task management and configuration options.

    Parameters
    ----------
    conf : Configuration
        The configuration object containing all settings

    Attributes
    ----------
    run : str
        The name of the current run
    runs : list
        List of all available runs
    """

    def __init__(self, conf: Configuration):
        super().__init__(conf)

        base = conf.parse_config('config.base')
        self.run = base.get('RUN', 'sfs')
        self.runs = [self.run]

    def _get_run_options(self, conf: Configuration) -> Dict[str, Any]:
        """
        Get run-specific options for SFS.

        Parameters
        ----------
        conf : Configuration
            Configuration object containing run settings

        Returns
        -------
        Dict[str, Any]
            Dictionary containing run options
        """
        run_options = super()._get_run_options(conf)

        run_options[self.run]['nens'] = conf.parse_config('config.base').get('NMEM_ENS', 0)

        return run_options

    def _get_app_configs(self, run):
        """
        Returns the config files that are involved in SFS.

        Parameters
        ----------
        run : str
            Name of the run configuration to process

        Returns
        -------
        list
            List of configuration file names needed for the SFS run
        """
        options = self.run_options[run]
        configs = ['stage_ic', 'fcst', 'atmos_products']

        if options['nens'] > 0:
            configs += ['efcs', 'atmos_ensstat']

        if options['do_wave']:
            configs += ['waveinit', 'wavepostsbs', 'wavepostpnt']
            if options['do_wave_bnd']:
                configs += ['wavepostbndpnt', 'wavepostbndpntbll']

        if options['do_ocean'] or options['do_ice']:
            configs += ['oceanice_products']

        if options['do_aero_fcst']:
            configs += ['prep_emissions']

        if options['do_extractvars']:
            configs += ['extractvars']

        if options['do_archcom']:
            configs += ['arch_tars']
            if options['do_globusarch']:
                configs += ['globus']

        configs += ['arch_vrfy', 'cleanup']

        return configs

    @staticmethod
    def _update_base(base_in):
        """
        Update base configuration with SFS-specific settings.

        Parameters
        ----------
        base_in : dict
            Input base configuration dictionary

        Returns
        -------
        dict
            Updated base configuration with SFS settings
        """
        base_out = base_in.copy()
        base_out['RUN'] = 'sfs'

        return base_out

    def get_task_names(self):
        """
        Get ordered list of tasks for SFS workflow.

        This method determines which tasks should be run based on the
        configuration options.

        Returns
        -------
        dict
            Dictionary with run name as key and list of task names as value
        """
        options = self.run_options[self.run]
        tasks = ['stage_ic']

        if options['do_wave']:
            tasks += ['waveinit']

        if options['do_aero_fcst']:
            tasks += ['prep_emissions']

        tasks += ['fcst']

        if options['nens'] > 0:
            tasks += ['efcs']

        tasks += ['atmos_prod']

        if options['nens'] > 0:
            tasks += ['atmos_ensstat']

        if options['do_ocean']:
            tasks += ['ocean_prod']

        if options['do_ice']:
            tasks += ['ice_prod']

        if options['do_wave']:
            tasks += ['wavepostsbs']
            if options['do_wave_bnd']:
                tasks += ['wavepostbndpnt', 'wavepostbndpntbll']
            tasks += ['wavepostpnt']

        if options['do_extractvars']:
            tasks += ['extractvars']

        # TODO add archiving for SFS
        # if options['do_archcom']:
        #     tasks += ['arch_tars']
        #     if options['do_globusarch']:
        #         tasks += ['globus']

        tasks += ['cleanup']

        return {f"{self.run}": tasks}
