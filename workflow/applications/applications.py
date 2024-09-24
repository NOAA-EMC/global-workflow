#!/usr/bin/env python3

from typing import Dict, List, Any
from datetime import timedelta
from hosts import Host
from wxflow import Configuration, to_timedelta
from abc import ABC, ABCMeta, abstractmethod

__all__ = ['AppConfig']


class AppConfigInit(ABCMeta):
    def __call__(cls, *args, **kwargs):
        '''
        We want the child classes to be able to define additional settings
          before we source the configs and complete the rest of the process,
          so break init up into two methods, one to run first (both in the
          base class and the child class) and one to finalize the initiali-
          zation after both have completed.
        '''
        obj = type.__call__(cls, *args, **kwargs)
        obj._init_finalize(*args, **kwargs)
        return obj


class AppConfig(ABC, metaclass=AppConfigInit):

    VALID_MODES = ['cycled', 'forecast-only']

    def __init__(self, conf: Configuration) -> None:

        self.scheduler = Host().scheduler

        # Get the most basic settings from config.base to determine
        # experiment type ({NET}_{MODE})
        base = conf.parse_config('config.base')

        self.mode = base['MODE']
        if self.mode not in self.VALID_MODES:
            raise NotImplementedError(f'{self.mode} is not a valid application mode.\n'
                                      f'Valid application modes are:\n'
                                      f'{", ".join(self.VALID_MODES)}\n')

        self.net = base['NET']
        self.gfs_cyc = base.get('gfs_cyc')

        print(f"Generating the XML for a {self.mode}_{self.net} case")

    def _init_finalize(self, conf: Configuration):
        '''
        Finalize object initialization calling subclass methods
        '''

        # Get run-, net-, and mode-based options
        self.run_options = self._get_run_options(conf)

        # Get task names and runs for the application
        self.task_names = self.get_task_names()

        # Initialize the configs and model_apps dictionaries
        self.configs = dict.fromkeys(self.runs)

        # Now configure the experiment for each valid run
        for run in self.runs:
            self.configs[run] = self._source_configs(conf, run=run, log=False)

    def _get_run_options(self, conf: Configuration) -> Dict[str, Any]:
        '''
        Determine the do_* and APP options for each RUN by sourcing config.base
        for each RUN and collecting the flags into self.run_options
        '''

        run_options = {run: {} for run in dict.fromkeys(self.runs)}
        for run in self.runs:
            # Read config.base with RUN specified
            run_base = conf.parse_config('config.base', RUN=run)

            run_options[run]['app'] = run_base.get('APP', 'ATM')
            run_options[run]['do_wave_bnd'] = run_base.get('DOBNDPNT_WAVE', False)
            run_options[run]['do_bufrsnd'] = run_base.get('DO_BUFRSND', False)
            run_options[run]['do_gempak'] = run_base.get('DO_GEMPAK', False)
            run_options[run]['do_awips'] = run_base.get('DO_AWIPS', False)
            run_options[run]['do_verfozn'] = run_base.get('DO_VERFOZN', True)
            run_options[run]['do_verfrad'] = run_base.get('DO_VERFRAD', True)
            run_options[run]['do_vminmon'] = run_base.get('DO_VMINMON', True)
            run_options[run]['do_tracker'] = run_base.get('DO_TRACKER', True)
            run_options[run]['do_genesis'] = run_base.get('DO_GENESIS', True)
            run_options[run]['do_genesis_fsu'] = run_base.get('DO_GENESIS_FSU', False)
            run_options[run]['do_metp'] = run_base.get('DO_METP', False)
            run_options[run]['do_upp'] = not run_base.get('WRITE_DOPOST', True)
            run_options[run]['do_goes'] = run_base.get('DO_GOES', False)
            run_options[run]['do_mos'] = run_base.get('DO_MOS', False)
            run_options[run]['do_extractvars'] = run_base.get('DO_EXTRACTVARS', False)

            run_options[run]['do_atm'] = run_base.get('DO_ATM', True)
            run_options[run]['do_wave'] = run_base.get('DO_WAVE', False)
            run_options[run]['do_ocean'] = run_base.get('DO_OCN', False)
            run_options[run]['do_ice'] = run_base.get('DO_ICE', False)
            run_options[run]['do_aero'] = run_base.get('DO_AERO', False)
            run_options[run]['do_prep_obs_aero'] = run_base.get('DO_PREP_OBS_AERO', False)

            run_options[run]['do_hpssarch'] = run_base.get('HPSSARCH', False)
            run_options[run]['fcst_segments'] = run_base.get('FCST_SEGMENTS', None)

            if not AppConfig.is_monotonic(run_options[run]['fcst_segments']):
                raise ValueError(f'Forecast segments do not increase monotonically: {",".join(self.fcst_segments)}')

            wave_runs = []
            if run_options[run]['do_wave']:
                wave_run = run_base.get('WAVE_RUN', 'BOTH').lower()
                if wave_run in ['both']:
                    wave_runs = ['gfs', 'gdas']
                elif wave_run in ['gfs', 'gdas']:
                    wave_runs = [wave_run]

            aero_anl_runs = []
            aero_fcst_runs = []
            if run_options[run]['do_aero']:
                aero_anl_run = run_base.get('AERO_ANL_RUN', 'BOTH').lower()
                if aero_anl_run in ['both']:
                    aero_anl_runs = ['gfs', 'gdas']
                elif aero_anl_run in ['gfs', 'gdas']:
                    aero_anl_runs = [aero_anl_run]

                aero_fcst_run = base.get('AERO_FCST_RUN', None).lower()
                if aero_fcst_run in ['both']:
                    aero_fcst_runs = ['gfs', 'gdas']
                elif aero_fcst_run in ['gfs', 'gdas']:
                    aero_fcst_runs = [aero_fcst_run]

                run_options[run]['do_aero_anl'] = True if run in aero_anl_runs else False
                run_options[run]['do_aero_fcst'] = True if run in aero_fcst_runs else False

            # Append any MODE-specific options
            run_options = self._netmode_run_options(run_base, run_options)

        # Return the dictionary of run options
        return run_options

    @abstractmethod
    def _netmode_run_options(self, base: Dict[str, Any], run_options: Dict[str, Any]) -> Dict[str, Any]:
        '''
        Defines run-based options for a given NET_MODE case.

        Parameters
        ----------
        base: Dict
              Parsed config.base settings

        run_options: Dict
              A dictionary with valid RUN-based sub-dictionaries containing generic options.

        Returns
        -------
        run_options: Dict
              Output dictionary with additional options valid for the given NET and MODE.
        '''

        # Valid NET_MODE options are defined in the appropriate subclass.

        pass

    @abstractmethod
    def _get_app_configs(self):
        pass

    @staticmethod
    @abstractmethod
    def _update_base(base_in: Dict[str, Any]) -> Dict[str, Any]:
        '''
        Make final updates to base and return an updated copy

        Parameters
        ----------
        base_in: Dict
                 Beginning base settings

        Returns
        -------
        Dict: A copy of base_in with possible modifications based on the
              net and mode.

        '''
        pass

    def _source_configs(self, conf: Configuration, run: str = "gfs", log: bool = True) -> Dict[str, Any]:
        """
        Given the configuration object used to initialize this application,
        source the configurations for each config and return a dictionary
        Every config depends on "config.base"
        """

        # Include config.base by its lonesome and update it
        configs = {'base': conf.parse_config('config.base', RUN=run)}
        configs['base'] = self._update_base(configs['base'])

        # Source the list of all config_files involved in the application
        for config in self._get_app_configs(run):

            # All must source config.base first
            files = ['config.base']

            if config in ['eobs', 'eomg']:
                files += ['config.anal', 'config.eobs']
            elif config in ['eupd']:
                files += ['config.anal', 'config.eupd']
            elif config in ['efcs']:
                files += ['config.fcst', 'config.efcs']
            elif config in ['atmanlinit', 'atmanlvar', 'atmanlfv3inc']:
                files += ['config.atmanl', f'config.{config}']
            elif config in ['atmensanlinit', 'atmensanlobs', 'atmensanlsol', 'atmensanlletkf', 'atmensanlfv3inc']:
                files += ['config.atmensanl', f'config.{config}']
            elif 'wave' in config:
                files += ['config.wave', f'config.{config}']
            else:
                files += [f'config.{config}']

            print(f'sourcing config.{config}') if log else 0
            configs[config] = conf.parse_config(files, RUN=run)

        return configs

    @abstractmethod
    def get_task_names(self, run: str) -> Dict[str, List[str]]:
        '''
        Create a list of valid RUNs and a dict of task names for each RUN valid for the configuation.

        Parameters
        ----------
        None

        Returns
        -------
        Dict[str, List[str]]: Lists of all tasks for each RUN.

        '''
        pass

    @staticmethod
    def get_gfs_interval(gfs_cyc: int) -> timedelta:
        """
        return interval in hours based on gfs_cyc
        """

        gfs_internal_map = {'1': '24H', '2': '12H', '4': '6H'}

        try:
            return to_timedelta(gfs_internal_map[str(gfs_cyc)])
        except KeyError:
            raise KeyError(f'Invalid gfs_cyc = {gfs_cyc}')

    @staticmethod
    def is_monotonic(test_list: List, check_decreasing: bool = False) -> bool:
        """
        Determine if an array is monotonically increasing or decreasing

        TODO: Move this into wxflow somewhere

        Inputs
          test_list: List
            A list of comparable values to check
          check_decreasing: bool [default: False]
            Check whether list is monotonically decreasing

        Returns
          bool: Whether the list is monotonically increasing (if check_decreasing
                if False) or decreasing (if check_decreasing is True)

        """
        if check_decreasing:
            return all(x > y for x, y in zip(test_list, test_list[1:]))
        else:
            return all(x < y for x, y in zip(test_list, test_list[1:]))
