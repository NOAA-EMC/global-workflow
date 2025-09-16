"""
GCAFS cycled application configuration module.

This module defines the configuration for running the Global Chemical Aerosol
Forecast System (GCAFS) in cycled mode with data assimilation.
"""

from applications.applications import AppConfig
from typing import Dict, Any
from wxflow import Configuration


class GCAFSCycledAppConfig(AppConfig):
    """
    Class to define GCAFS cycled configurations.

    This class handles configuration management for the GCAFS application in
    cycled mode with data assimilation, including ensemble configurations.

    Parameters
    ----------
    conf : Configuration
        The configuration object containing all GCAFS settings

    Attributes
    ----------
    runs : list
        List of all available runs (gcafs, enkfgcafs)
    ens_runs : list
        List of runs that include ensemble configurations
    """

    def __init__(self, conf: Configuration):
        """
        Initialize the GCAFS cycled application configuration.

        Parameters
        ----------
        conf : Configuration
            Configuration object containing GCAFS settings
        """
        super().__init__(conf)
        # Re-read config.base without RUN specified to get the basic settings for
        # cycled cases to be able to determine valid runs
        base = conf.parse_config('config.base')

        self.ens_runs = []

        if base.get('DOHYBVAR', False):
            self.ens_runs = ['gcafs']

        # Now construct self.runs the desired XML order (gcafs, enkfgcafs, gcdas, enkfgcdas)
        self.runs = []
        self.runs.append('gcafs') if base['INTERVAL_GFS'] > 0 else 0
        self.runs.append('enkfgcafs') if 'gcafs' in self.ens_runs and 'gcafs' in self.runs else 0
        self.runs.append('gcdas')  # We always have a 'gcdas' run
        self.runs.append('enkfgcdas') if 'gcdas' in self.ens_runs else 0

    def _get_run_options(self, conf: Configuration) -> Dict[str, Any]:
        """
        Get run-specific options for GCAFS cycled mode.

        Parameters
        ----------
        conf : Configuration
            Configuration object containing run settings

        Returns
        -------
        Dict[str, Any]
            Dictionary containing run options for each configured run
        """
        run_options = super()._get_run_options(conf)

        for run in self.runs:
            base = conf.parse_config('config.base', RUN=run)

            run_options[run]['do_hybvar'] = base.get('DOHYBVAR', False)
            run_options[run]['nens'] = base.get('NMEM_ENS', 0)
            if run_options[run]['do_hybvar']:
                run_options[run]['lobsdiag_forenkf'] = base.get('lobsdiag_forenkf', False)

            run_options[run]['do_jediatmvar'] = base.get('DO_JEDIATMVAR', False)
            run_options[run]['do_jediatmens'] = base.get('DO_JEDIATMENS', False)
            run_options[run]['do_mergensst'] = base.get('DO_MERGENSST', False)

        return run_options

    def _get_app_configs(self, run):
        """
        Returns the config files that are involved in the GCAFS cycled app.

        Parameters
        ----------
        run : str
            Name of the run configuration to process

        Returns
        -------
        list
            List of configuration file names needed for the specified run
        """
        options = self.run_options[run]

        configs = ['fetch']

        configs += ['offlineanl']

        # Add GCAFS-specific aerosol configs by default
        if options['do_aero_fcst']:
            configs += ['aero', 'prep_emissions']
            # Don't include aerosol_init for cycled runs
            # aerosol_init is only needed for forecast-only mode

        configs += ['stage_ic', 'sfcanl', 'fcst', 'upp', 'atmos_products', 'arch_vrfy', 'cleanup']

        if options['do_archcom']:
            configs += ['arch_tars']

        if options['do_hybvar']:
            print("WARNING: Hyb-Var not yet supported for GCAFS")
        # if options['do_hybvar']:
        #     if options['do_jediatmens']:
        #         configs += ['atmensanlinit', 'atmensanlobs', 'atmensanlsol',
        #                     'atmensanlletkf', 'atmensanlfv3inc', 'atmensanlfinal',
        #                     'ecen_fv3jedi']

        #     configs += ['esfc', 'efcs', 'epos', 'earc_vrfy']

        #     if options['do_archcom']:
        #         configs += ['earc_tars']

        if options['do_metp']:
            configs += ['metp']

        if options['do_aero_anl']:
            configs += ['aeroanlgenb', 'aeroanlinit', 'aeroanlvar', 'aeroanlfinal']
            configs += ['prepobsaero']

        if options['do_anlstat']:
            configs += ['anlstat']

        if options['do_globusarch']:
            configs += ['globus']
            # TODO Enable when a Hyb-Var capability is available for GCAFS
            # if options['do_hybvar']:
            #     print("WARNING Globus archiving is currently only possible for deterministic members")
            #     print("        Ensemble members will NOT be archived with this option!!")
            # if options['do_hybvar']:
            #     configs += ['globus_earc']

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
        Get the task names for each valid run in this GCAFS cycled configuration.

        This method determines which tasks should be run for each configured run.
        The order of the tasks is important for XML configuration generation.

        Returns
        -------
        dict
            Dictionary with run names as keys and ordered lists of task names as values

        Notes
        -----
        The order of the task names matters in the XML generation.
        """
        # Start with a dictionary of empty task lists for each valid run
        task_names = {run: [] for run in self.runs}

        for run in self.runs:
            options = self.run_options[run]

            # Most tasks are only in the cycling gcdas
            if run == 'gcdas':
                task_names[run] += ['stage_ic']

                task_names[run] += ['fetch']
                task_names[run] += ['offlineanl']
                task_names[run] += ['sfcanl']

                if options['do_aero_anl']:
                    task_names[run] += ['aeroanlgenb']
                    task_names[run] += ['aeroanlinit', 'aeroanlvar', 'aeroanlfinal']
                    task_names[run] += ['prepobsaero']

                if options['do_anlstat']:
                    task_names[run] += ['anlstat']

            # some are common across both
            if run in ['gcdas', 'gcafs']:

                if options['do_aero_fcst']:
                    task_names[run] += ['prep_emissions']

                task_names[run] += ['fcst']

                if options['do_upp']:
                    task_names[run] += ['atmupp']
                task_names[run] += ['atmos_prod']

                if run == 'gcafs':
                    if options['do_goes']:
                        task_names[run] += ['goesupp']

                # gcafs-only verification/tracking
                if run == 'gcafs':
                    if options['do_metp']:
                        task_names[run] += ['metp']

                # Last items
                task_names[run] += ['arch_vrfy']
                if options['do_archcom']:
                    task_names[run] += ['arch_tars']
                    if options['do_globusarch']:
                        task_names[run] += ['globus_arch']

                task_names[run] += ['cleanup']

        return task_names
