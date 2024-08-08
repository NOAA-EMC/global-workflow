#!/usr/bin/env python3

from typing import Dict, List, Any
from datetime import timedelta
from hosts import Host
from pathlib import Path
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

        # Save the configuration so we can source the config files when
        # determining task resources
        self.conf = conf

        _base = self.conf.parse_config('config.base')
        # Define here so the child __init__ functions can use it; will
        # be overwritten later during _init_finalize().
        self._base = _base

        self.mode = _base['MODE']

        if self.mode not in self.VALID_MODES:
            raise NotImplementedError(f'{self.mode} is not a valid application mode.\n' +
                                      'Valid application modes are:\n' +
                                      f'{", ".join(self.VALID_MODES)}')

        self.net = _base['NET']
        self.model_app = _base.get('APP', 'ATM')
        self.do_atm = _base.get('DO_ATM', True)
        self.do_wave = _base.get('DO_WAVE', False)
        self.do_wave_bnd = _base.get('DOBNDPNT_WAVE', False)
        self.do_ocean = _base.get('DO_OCN', False)
        self.do_ice = _base.get('DO_ICE', False)
        self.do_aero = _base.get('DO_AERO', False)
        self.do_prep_obs_aero = _base.get('DO_PREP_OBS_AERO', False)
        self.do_bufrsnd = _base.get('DO_BUFRSND', False)
        self.do_gempak = _base.get('DO_GEMPAK', False)
        self.do_awips = _base.get('DO_AWIPS', False)
        self.do_verfozn = _base.get('DO_VERFOZN', True)
        self.do_verfrad = _base.get('DO_VERFRAD', True)
        self.do_vminmon = _base.get('DO_VMINMON', True)
        self.do_tracker = _base.get('DO_TRACKER', True)
        self.do_genesis = _base.get('DO_GENESIS', True)
        self.do_genesis_fsu = _base.get('DO_GENESIS_FSU', False)
        self.do_metp = _base.get('DO_METP', False)
        self.do_upp = not _base.get('WRITE_DOPOST', True)
        self.do_goes = _base.get('DO_GOES', False)
        self.do_mos = _base.get('DO_MOS', False)
        self.do_extractvars = _base.get('DO_EXTRACTVARS', False)

        self.do_hpssarch = _base.get('HPSSARCH', False)

        self.nens = _base.get('NMEM_ENS', 0)
        self.fcst_segments = _base.get('FCST_SEGMENTS', None)

        if not AppConfig.is_monotonic(self.fcst_segments):
            raise ValueError(f'Forecast segments do not increase monotonically: {",".join(self.fcst_segments)}')

        self.wave_runs = None
        if self.do_wave:
            wave_run = _base.get('WAVE_RUN', 'BOTH').lower()
            if wave_run in ['both']:
                self.wave_runs = ['gfs', 'gdas']
            elif wave_run in ['gfs', 'gdas']:
                self.wave_runs = [wave_run]

        self.aero_anl_runs = None
        self.aero_fcst_runs = None
        if self.do_aero:
            aero_anl_run = _base.get('AERO_ANL_RUN', 'BOTH').lower()
            if aero_anl_run in ['both']:
                self.aero_anl_runs = ['gfs', 'gdas']
            elif aero_anl_run in ['gfs', 'gdas']:
                self.aero_anl_runs = [aero_anl_run]
            aero_fcst_run = _base.get('AERO_FCST_RUN', None).lower()
            if aero_fcst_run in ['both']:
                self.aero_fcst_runs = ['gfs', 'gdas']
            elif aero_fcst_run in ['gfs', 'gdas']:
                self.aero_fcst_runs = [aero_fcst_run]

    def _init_finalize(self, *args):
        print("Finalizing initialize")

        # Get a list of all possible config_files that would be part of the application
        self.configs_names = self._get_app_configs()

        # Source the config_files for the jobs in the application
        self.configs = self.source_configs()

        # Update the base config dictionary base on application
        self.configs['base'] = self.update_base(self.configs['base'])

        # Save base in the internal state since it is often needed
        self._base = self.configs['base']

        # Get more configuration options into the class attributes
        self.gfs_cyc = self._base.get('gfs_cyc')

        # Finally get task names for the application
        self.task_names = self.get_task_names()

    @abstractmethod
    def _get_app_configs(self):
        pass

    @staticmethod
    @abstractmethod
    def update_base(base_in: Dict[str, Any]) -> Dict[str, Any]:
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

    def source_configs(self, run: str = "gfs", log: bool = True) -> Dict[str, Any]:
        """
        Given the configuration object used to initialize this application,
        source the configurations for each config and return a dictionary
        Every config depends on "config.base"
        """

        configs = dict()

        # Return config.base as well
        configs['base'] = self.conf.parse_config('config.base')

        # Source the list of all config_files involved in the application
        for config in self.configs_names:

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
            elif config in ['atmensanlinit', 'atmensanlletkf', 'atmensanlfv3inc']:
                files += ['config.atmensanl', f'config.{config}']
            elif 'wave' in config:
                files += ['config.wave', f'config.{config}']
            else:
                files += [f'config.{config}']

            print(f'sourcing config.{config}') if log else 0
            configs[config] = self.conf.parse_config(files, RUN=run)

        return configs

    @abstractmethod
    def get_task_names(self) -> Dict[str, List[str]]:
        '''
        Create a list of task names for each RUN valid for the configuation.

        Parameters
        ----------
        None

        Returns
        -------
        Dict[str, List[str]]: Lists of tasks for each RUN.

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
