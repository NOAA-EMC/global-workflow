#!/usr/bin/env python3

from typing import Dict, Any
from datetime import timedelta
from configuration import Configuration
from hosts import Host

__all__ = ['AppConfig']


def get_gfs_interval(gfs_cyc: int) -> str:
    """
    return interval in hours based on gfs_cyc
    """

    gfs_internal_map = {'0': None, '1': '24:00:00', '2': '12:00:00', '4': '06:00:00'}

    try:
        return gfs_internal_map[str(gfs_cyc)]
    except KeyError:
        raise KeyError(f'Invalid gfs_cyc = {gfs_cyc}')


def get_gfs_cyc_dates(base: Dict[str, Any]) -> Dict[str, Any]:
    """
    Generate GFS dates from experiment dates and gfs_cyc choice
    """

    base_out = base.copy()

    gfs_cyc = base['gfs_cyc']
    sdate = base['SDATE']
    edate = base['EDATE']
    base_out['INTERVAL'] = '06:00:00'  # Cycled interval is 6 hours

    interval_gfs = get_gfs_interval(gfs_cyc)

    # Set GFS cycling dates
    hrinc = 0
    hrdet = 0
    if gfs_cyc == 0:
        return base_out
    elif gfs_cyc == 1:
        hrinc = 24 - sdate.hour
        hrdet = edate.hour
    elif gfs_cyc == 2:
        if sdate.hour in [0, 12]:
            hrinc = 12
        elif sdate.hour in [6, 18]:
            hrinc = 6
        if edate.hour in [6, 18]:
            hrdet = 6
    elif gfs_cyc == 4:
        hrinc = 6
    sdate_gfs = sdate + timedelta(hours=hrinc)
    edate_gfs = edate - timedelta(hours=hrdet)
    if sdate_gfs > edate:
        print('W A R N I N G!')
        print('Starting date for GFS cycles is after Ending date of experiment')
        print(f'SDATE = {sdate.strftime("%Y%m%d%H")},     EDATE = {edate.strftime("%Y%m%d%H")}')
        print(f'SDATE_GFS = {sdate_gfs.strftime("%Y%m%d%H")}, EDATE_GFS = {edate_gfs.strftime("%Y%m%d%H")}')
        gfs_cyc = 0

    base_out['gfs_cyc'] = gfs_cyc
    base_out['SDATE_GFS'] = sdate_gfs
    base_out['EDATE_GFS'] = edate_gfs
    base_out['INTERVAL_GFS'] = interval_gfs

    fhmax_gfs = {}
    for hh in ['00', '06', '12', '18']:
        fhmax_gfs[hh] = base.get(f'FHMAX_GFS_{hh}', base.get('FHMAX_GFS_00', 120))
    base_out['FHMAX_GFS'] = fhmax_gfs

    return base_out


class AppConfig:

    VALID_MODES = ['cycled', 'forecast-only']

    def __init__(self, mode: str, configuration: Configuration) -> None:

        if mode not in self.VALID_MODES:
            raise NotImplementedError(f'{mode} is not a valid application mode.\n' +
                                      'Valid application modes are:\n' +
                                      f'{", ".join(self.VALID_MODES)}')

        self.mode = mode

        self.scheduler = Host().scheduler

        _base = configuration.parse_config('config.base')

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

        # Get a list of all possible config_files that would be part of the application
        self.configs_names = self._get_app_configs()

        # Source the config_files for the jobs in the application
        self.configs = self._source_configs(configuration)

        # Update the base config dictionary based on application
        upd_base_map = {'cycled': self._cycled_upd_base,
                        'forecast-only': self._forecast_only_upd_base}
        try:
            self.configs['base'] = upd_base_map[self.mode](self.configs['base'])
        except KeyError:
            raise NotImplementedError(f'{self.mode} is not a valid application mode.\n' +
                                      'Valid application modes are:\n' +
                                      f'{", ".join(self.VALID_MODES)}')

        # Save base in the internal state since it is often needed
        self._base = self.configs['base']

        # Get more configuration options into the class attributes
        self.gfs_cyc = self._base.get('gfs_cyc')

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
            self.do_wave_bnd = self.configs['wavepostsbs'].get('DOBNDPNT_WAVE', False)

        # Finally get task names for the application
        self.task_names = self.get_task_names()

    def _get_app_configs(self):

        configs_map = {'cycled': self._cycled_configs,
                       'forecast-only': self._forecast_only_configs}
        try:
            configs_names = configs_map[self.mode]
        except KeyError:
            raise NotImplementedError(f'{self.mode} is not a valid application mode.\n' +
                                      'Valid application modes are:\n' +
                                      f'{", ".join(self.VALID_MODES)}')

        return configs_names

    @property
    def _cycled_configs(self):
        """
        Returns the config_files that are involved in the cycled app
        """

        configs = ['prep',
                   'anal', 'analdiag', 'analcalc',
                   'fcst', 'post', 'vrfy', 'arch']

        if self.do_gldas:
            configs += ['gldas']

        if self.do_hybvar:
            configs += ['eobs', 'eomg', 'ediag', 'eupd', 'ecen', 'esfc', 'efcs', 'echgres', 'epos', 'earc']

        if self.do_metp:
            configs += ['metp']

        if self.do_gempak:
            configs += ['gempak']

        if self.do_awips:
            configs += ['awips']

        if self.do_wave:
            configs += ['waveinit', 'waveprep', 'wavepostsbs', 'wavepostbndpnt', 'wavepostbndpntbll', 'wavepostpnt']
            if self.do_gempak:
                configs += ['wavegempak']
            if self.do_awips:
                configs += ['waveawipsbulls', 'waveawipsgridded']

        if self.do_wafs:
            configs += ['wafs', 'wafsgrib2', 'wafsblending', 'wafsgcip', 'wafsgrib20p25', 'wafsblending0p25']

        return configs

    @property
    def _forecast_only_configs(self):
        """
        Returns the config_files that are involved in the forecast-only app
        """

        configs = ['fcst', 'post', 'vrfy', 'arch']

        if self.model_app in ['S2S', 'S2SW']:
            configs += ['coupled_ic']
        else:
            configs += ['init']
            if self.do_hpssarch:
                configs += ['getic']

        if self.do_aero:
            configs += ['aerosol_init']

        if self.do_ocean or self.do_ice:
            configs += ['ocnpost']

        if self.do_metp:
            configs += ['metp']

        if self.do_gempak:
            configs += ['gempak']

        if self.do_awips:
            configs += ['awips']

        if self.do_wave:
            configs += ['waveinit', 'waveprep', 'wavepostsbs', 'wavepostbndpnt', 'wavepostbndpntbll', 'wavepostpnt']
            if self.do_gempak:
                configs += ['wavegempak']
            if self.do_awips:
                configs += ['waveawipsbulls', 'waveawipsgridded']

        if self.do_wafs:
            configs += ['wafs', 'wafsgrib2', 'wafsblending', 'wafsgcip', 'wafsgrib20p25', 'wafsblending0p25']

        return configs

    @staticmethod
    def _cycled_upd_base(base_in):

        return get_gfs_cyc_dates(base_in)

    @staticmethod
    def _forecast_only_upd_base(base_in):

        base_out = base_in.copy()
        base_out['INTERVAL'] = get_gfs_interval(base_in['gfs_cyc'])
        base_out['CDUMP'] = 'gfs'  # TODO - is there a use case for CDUMP=gdas?

        return base_out

    def _source_configs(self, configuration: Configuration) -> Dict[str, Any]:
        """
        Given the configuration object and jobs,
        source the configurations for each config and return a dictionary
        Every config depends on "config.base"
        """

        configs = dict()

        # Return config.base as well
        configs['base'] = configuration.parse_config('config.base')

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
            elif 'wave' in config:
                files += ['config.wave', f'config.{config}']
            else:
                files += [f'config.{config}']

            print(f'sourcing config.{config}')
            configs[config] = configuration.parse_config(files)

        return configs

    def get_task_names(self):

        # Get a list of all possible tasks that would be part of the application
        tasks_map = {'cycled': self._get_cycled_task_names,
                     'forecast-only': self._get_forecast_only_task_names}
        try:
            task_names = tasks_map[self.mode]()
        except KeyError:
            raise NotImplementedError(f'{self.mode} is not a valid application mode.\n' +
                                      'Valid application modes are:\n' +
                                      f'{", ".join(self.VALID_MODES)}')

        return task_names

    def _get_cycled_task_names(self):

        tasks = ['prep', 'anal', 'analcalc', 'fcst', 'post', 'vrfy', 'arch']

        gdas_only_tasks = ['analdiag']

        gldas_tasks = ['gldas']
        wave_tasks = ['waveinit', 'waveprep', 'wavepostsbs', 'wavepostbndpnt', 'wavepostbndpntbll', 'wavepostpnt']

        if self.do_hybvar:
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

        return {f"{self._base['CDUMP']}": tasks}
