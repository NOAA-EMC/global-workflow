#!/usr/bin/env python

from typing import Dict, Any
from configuration import Configuration
import workflow_utils as wfu


class Application:
    VALID_MODES = ['cycled', 'forecast-only']
    SERVICE_TASKS = ['arch', 'earc', 'getic']

    def __init__(self, mode: str, configuration: Configuration) -> None:

        self.task_resources = None
        self.task_names = None
        if mode not in self.VALID_MODES:
            raise NotImplementedError(f'{mode} is not a valid application mode.\n' +
                                      'Valid application modes are:\n' +
                                      f'{", ".join(self.VALID_MODES)}')

        self.mode = mode
        self.cfg = configuration

        _base = self.cfg.parse_config('config.base')

        self.model_app = _base.get('APP', 'ATM')
        self.do_hybvar = _base.get('DOHYBVAR', False)
        self.do_wave = _base.get('DO_WAVE', False)
        self.do_ocean = _base.get('DO_OCEAN', False)
        self.do_ice = _base.get('DO_ICE', False)
        self.do_aero = _base.get('DO_AERO', False)
        self.do_gldas = _base.get('DO_GLDAS', False)
        self.do_bufrsnd = _base.get('DO_BUFRSND', False)
        self.do_gempak = _base.get('DO_GEMPAK', False)
        self.do_awips = _base.get('DO_AWIPS', False)
        self.do_wafs = _base.get('DO_WAFS', False)
        self.do_vrfy = _base.get('DO_VRFY', True)
        self.do_metp = _base.get('DO_METP', False)

        self.do_hpssarch = _base.get('HPSSARCH', False)

        # Get a list of all possible jobs that would be part of the application
        jobs_map = {'cycled': self._cycled_jobs,
                    'forecast-only': self._forecast_only_jobs}
        try:
            self.job_names = jobs_map[self.mode]
        except KeyError:
            raise NotImplementedError(f'{self.mode} is not a valid application mode.\n' +
                                      'Valid application modes are:\n' +
                                      f'{", ".join(self.VALID_MODES)}')

        # Source the configs for all the jobs in the application
        self.job_configs = self.source_configs

        # Update the base config dictionary based on application
        upd_base_map = {'cycled': self._cycled_upd_base,
                        'forecast-only': self._forecast_only_upd_base}
        try:
            self.job_configs['base'] = upd_base_map[self.mode](self.job_configs['base'])
        except KeyError:
            raise NotImplementedError(f'{self.mode} is not a valid application mode.\n' +
                                      'Valid application modes are:\n' +
                                      f'{", ".join(self.VALID_MODES)}')

        self._base = self.job_configs['base']

        if self.do_hybvar:
            self.lobsdiag_forenkf = self._base.get('lobsdiag_forenkf', False)
            eupd_cdump = self._base.get('EUPD_CYC', 'gdas').lower()
            if eupd_cdump in ['both']:
                self.eupd_cdumps = ['gfs', 'gdas']
            elif eupd_cdump in ['gfs', 'gdas']:
                self.eupd_cdumps = [eupd_cdump]

        if self.do_wave:
            wave_cdump = self._base.get('WAVE_CDUMP', 'BOTH').lower()
            if wave_cdump in ['both']:
                self.wave_cdumps = ['gfs', 'gdas']
            elif wave_cdump in ['gfs', 'gdas']:
                self.wave_cdumps = [wave_cdump]
            self.do_wave_bnd = self.job_configs['wavepostsbs'].get('DOBNDPNT_WAVE', False)

    @property
    def _cycled_jobs(self):
        """
        Returns the jobs that are involved in the cycled app
        """

        jobs = ['prep',
                'anal', 'analdiag', 'analcalc', 'gldas',
                'fcst', 'post', 'vrfy', 'arch']

        if self.do_hybvar:
            jobs += ['eobs', 'eomg', 'ediag', 'eupd', 'ecen', 'esfc', 'efcs', 'echgres', 'epos', 'earc']

        if self.do_metp:
            jobs += ['metp']

        if self.do_gempak:
            jobs += ['gempak']

        if self.do_awips:
            jobs += ['awips']

        if self.do_wave:
            jobs += ['waveinit', 'waveprep', 'wavepostsbs', 'wavepostbndpnt', 'wavepostbndpntbll', 'wavepostpnt']
            if self.do_gempak:
                jobs += ['wavegempak']
            if self.do_awips:
                jobs += ['waveawipsbulls', 'waveawipsgridded']

        if self.do_wafs:
            jobs += ['wafs', 'wafsgrib2', 'wafsblending', 'wafsgcip', 'wafsgrib20p25', 'wafsblending0p25']

        return jobs

    @property
    def _forecast_only_jobs(self):
        """
        Returns the jobs that are involved in the forecast-only app
        """

        jobs = ['fcst', 'post', 'vrfy', 'arch']

        if self.model_app in ['S2S', 'S2SW']:
            jobs += ['coupled_ic']
        else:
            jobs += ['init']
            if self.do_hpssarch:
                jobs += ['getic']

        if self.do_aero:
            jobs += ['aerosol_init']

        if self.do_ocean or self.do_ice:
            jobs += ['ocnpost']

        if self.do_metp:
            jobs += ['metp']

        if self.do_gempak:
            jobs += ['gempak']

        if self.do_awips:
            jobs += ['awips']

        if self.do_wave:
            jobs += ['waveinit', 'waveprep', 'wavepostsbs', 'wavepostbndpnt', 'wavepostbndpntbll', 'wavepostpnt']
            if self.do_gempak:
                jobs += ['wavegempak']
            if self.do_awips:
                jobs += ['waveawipsbulls', 'waveawipsgridded']

        if self.do_wafs:
            jobs += ['wafs', 'wafsgrib2', 'wafsblending', 'wafsgcip', 'wafsgrib20p25', 'wafsblending0p25']

        return jobs

    @staticmethod
    def _cycled_upd_base(base_in):

        return wfu.get_gfs_cyc_dates(base_in)

    @staticmethod
    def _forecast_only_upd_base(base_in):

        base_out = base_in.copy()
        base_out['INTERVAL'] = wfu.get_gfs_interval(base_in['gfs_cyc'])

        return base_out

    @property
    def source_configs(self) -> Dict[str, Any]:
        """
        Given the configuration object and jobs,
        source the configurations for each job and return a dictionary
        Every job depends on "config.base"
        """

        dict_jobs = dict()

        # Return config.base as well
        dict_jobs['base'] = self.cfg.parse_config('config.base')

        # Source the list of input tasks
        for job in self.job_names:

            # All tasks must source config.base first
            files = ['config.base']

            if job in ['eobs', 'eomg']:
                files += ['config.anal', 'config.eobs']
            elif job in ['eupd']:
                files += ['config.anal', 'config.eupd']
            elif job in ['efcs']:
                files += ['config.fcst', 'config.efcs']
            elif 'wave' in job:
                files += ['config.wave', f'config.{job}']
            else:
                files += [f'config.{job}']

            print(f'sourcing config.{job}')
            dict_jobs[job] = self.cfg.parse_config(files)

        return dict_jobs

    def get_task_names(self):

        # Get a list of all possible tasks that would be part of the application
        tasks_map = {'cycled': self._get_cycled_task_names,
                     'forecast-only': self._get_forecast_only_task_names}
        try:
            self.task_names = tasks_map[self.mode]()
        except KeyError:
            raise NotImplementedError(f'{self.mode} is not a valid application mode.\n' +
                                      'Valid application modes are:\n' +
                                      f'{", ".join(self.VALID_MODES)}')

    def _get_cycled_task_names(self):

        tasks = ['prep', 'anal', 'analcalc', 'fcst', 'post', 'vrfy', 'arch']

        gdas_only_tasks = ['analdiag']

        gldas_tasks = ['gldas']
        wave_tasks = ['waveinit', 'waveprep', 'wavepostsbs', 'wavepostbndpnt', 'wavepostbndpntbll', 'wavepostpnt']

        hybrid_tasks = ['eobs', 'eupd', 'echgres']
        hybrid_tasks += ['ediag'] if self.lobsdiag_forenkf else ['eomg']
        hybrid_gdas_tasks = ['ecen', 'esfc', 'efcs', 'epos', 'earc']

        # First collect all gdas tasks
        gdas_tasks = tasks + gdas_only_tasks

        if self.do_gldas:
            gdas_tasks += gldas_tasks

        if self.do_wave:
            gdas_tasks += wave_tasks

        if self.do_hybvar:
            if 'gdas' in self.eupd_cdumps:
                gdas_tasks += hybrid_tasks
                gdas_tasks += hybrid_gdas_tasks

        # Now collect gfs tasks
        gfs_tasks = tasks

        if self.do_hybvar:
            if 'gfs' in self.eupd_cdumps:
                gfs_tasks += hybrid_tasks

        if self.do_wave and 'gfs' in self.wave_cdumps:
            gfs_tasks += wave_tasks
            if self.do_gempak:
                gfs_tasks += ['wavegempak']
            if self.do_awips:
                gfs_tasks += ['waveawipsbulls', 'waveawipsgridded']

        if self.do_bufrsnd:
            gfs_tasks += ['postsnd']

        if self.do_gempak:
            gfs_tasks += ['gempak']

        if self.do_awips:
            gfs_tasks += ['awips']

        if self.do_wafs:
            gfs_tasks += ['wafs', 'wafsgcip', 'wafsgrib2', 'wafsgrib20p25', 'wafsblending', 'wafsblending0p25']

        if self.do_metp:
            gfs_tasks += ['metp']

        tasks = {'gdas': gdas_tasks, 'gfs': gfs_tasks}

        return tasks

    def _get_forecast_only_task_names(self):

        tasks = ['fcst', 'post', 'vrfy', 'arch']

        if 'S2S' in self.model_app:
            tasks += ['coupled_ic', 'ocnpost']
        else:
            if self.do_hpssarch:
                tasks += ['getic']
            tasks += ['init']

        if 'AERO' in self.model_app:
            tasks += ['aerosol_init']

        if self.do_wave:
            tasks += ['waveinit', 'waveprep', 'wavepostsbs', 'wavepostbndpnt', 'wavepostbndpntbll', 'wavepostpnt']
            if self.do_gempak:
                tasks += ['wavegempak']
            if self.do_awips:
                tasks += ['waveawipsbulls', 'waveawipsgridded']

        if self.do_bufrsnd:
            tasks += ['postsnd']

        if self.do_gempak:
            tasks += ['gempak']

        if self.do_awips:
            tasks += ['awips']

        if self.do_wafs:
            tasks += ['wafs', 'wafsgcip', 'wafsgrib2', 'wafsgrib20p25', 'wafsblending', 'wafsblending0p25']

        if self.do_metp:
            tasks += ['metp']

        return {'cdump': tasks}

    def get_resources(self):

        self.task_resources = dict()
        for cdump, cdump_tasks in self.task_names.items():
            self.task_resources[cdump] = dict()
            for task in cdump_tasks:
                self.task_resources[cdump][task] = wfu.get_resource(self.job_configs[task], task, cdump=cdump)
