#!/usr/bin/env python3

from typing import Dict, Any
from datetime import timedelta
from hosts import Host
from pygw.configuration import Configuration

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

    def __init__(self, conf: Configuration) -> None:

        self.scheduler = Host().scheduler

        _base = conf.parse_config('config.base')

        self.mode = _base['MODE']

        if self.mode not in self.VALID_MODES:
            raise NotImplementedError(f'{self.mode} is not a valid application mode.\n' +
                                      'Valid application modes are:\n' +
                                      f'{", ".join(self.VALID_MODES)}')

        self.model_app = _base.get('APP', 'ATM')
        self.do_hybvar = _base.get('DOHYBVAR', False)
        self.do_atm = _base.get('DO_ATM', True)
        self.do_wave = _base.get('DO_WAVE', False)
        self.do_wave_bnd = _base.get('DOBNDPNT_WAVE', False)
        self.do_ocean = _base.get('DO_OCN', False)
        self.do_ice = _base.get('DO_ICE', False)
        self.do_aero = _base.get('DO_AERO', False)
        self.do_land = _base.get('DO_LAND', False)
        self.do_gldas = _base.get('DO_GLDAS', False)
        self.do_bufrsnd = _base.get('DO_BUFRSND', False)
        self.do_gempak = _base.get('DO_GEMPAK', False)
        self.do_awips = _base.get('DO_AWIPS', False)
        self.do_wafs = _base.get('WAFSF', False)
        self.do_vrfy = _base.get('DO_VRFY', True)
        self.do_fit2obs = _base.get('DO_FIT2OBS', True)
        self.do_metp = _base.get('DO_METP', False)
        self.do_jediatmvar = _base.get('DO_JEDIVAR', False)
        self.do_jediens = _base.get('DO_JEDIENS', False)
        self.do_jediocnvar = _base.get('DO_JEDIOCNVAR', False)

        self.do_hpssarch = _base.get('HPSSARCH', False)

        self.wave_cdumps = None
        if self.do_wave:
            wave_cdump = _base.get('WAVE_CDUMP', 'BOTH').lower()
            if wave_cdump in ['both']:
                self.wave_cdumps = ['gfs', 'gdas']
            elif wave_cdump in ['gfs', 'gdas']:
                self.wave_cdumps = [wave_cdump]

        self.lobsdiag_forenkf = False
        self.eupd_cdumps = None
        if self.do_hybvar:
            self.lobsdiag_forenkf = _base.get('lobsdiag_forenkf', False)
            eupd_cdump = _base.get('EUPD_CYC', 'gdas').lower()
            if eupd_cdump in ['both']:
                self.eupd_cdumps = ['gfs', 'gdas']
            elif eupd_cdump in ['gfs', 'gdas']:
                self.eupd_cdumps = [eupd_cdump]

        # Get a list of all possible config_files that would be part of the application
        self.configs_names = self._get_app_configs()

        # Source the config_files for the jobs in the application
        self.configs = self._source_configs(conf)

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

        configs = ['prep']

        if self.do_jediatmvar:
            configs += ['atmanlinit', 'atmanlrun', 'atmanlfinal']
        else:
            configs += ['anal', 'analdiag']

        if self.do_jediocnvar:
            configs += ['ocnanalprep', 'ocnanalbmat', 'ocnanalrun', 'ocnanalpost']
        if self.do_ocean:
            configs += ['ocnpost']

        configs += ['sfcanl', 'analcalc', 'fcst', 'post', 'vrfy', 'fit2obs', 'arch']

        if self.do_gldas:
            configs += ['gldas']

        if self.do_hybvar:
            if self.do_jediens:
                configs += ['atmensanalprep', 'atmensanalrun', 'atmensanalpost']
            else:
                configs += ['eobs', 'eomg', 'ediag', 'eupd']
            configs += ['ecen', 'esfc', 'efcs', 'echgres', 'epos', 'earc']

        if self.do_metp:
            configs += ['metp']

        if self.do_gempak:
            configs += ['gempak']

        if self.do_bufrsnd:
            configs += ['postsnd']

        if self.do_awips:
            configs += ['awips']

        if self.do_wave:
            configs += ['waveinit', 'waveprep', 'wavepostsbs', 'wavepostpnt']
            if self.do_wave_bnd:
                configs += ['wavepostbndpnt', 'wavepostbndpntbll']
            if self.do_gempak:
                configs += ['wavegempak']
            if self.do_awips:
                configs += ['waveawipsbulls', 'waveawipsgridded']

        if self.do_wafs:
            configs += ['wafs', 'wafsgrib2', 'wafsblending', 'wafsgcip', 'wafsgrib20p25', 'wafsblending0p25']

        if self.do_aero:
            configs += ['aeroanlinit', 'aeroanlrun', 'aeroanlfinal']

        if self.do_land:
            configs += ['landanlinit', 'landanlrun', 'landanlfinal']

        return configs

    @property
    def _forecast_only_configs(self):
        """
        Returns the config_files that are involved in the forecast-only app
        """

        configs = ['fcst']

        if self.do_atm:
            configs += ['post', 'vrfy']

        configs += ['arch']

        if self.model_app in ['S2S', 'S2SW', 'S2SWA', 'NG-GODAS']:
            configs += ['coupled_ic']
        else:
            configs += ['init']
            if self.do_hpssarch:
                configs += ['getic']

        if self.do_aero:
            configs += ['aerosol_init']

        if self.do_ocean or self.do_ice:
            configs += ['ocnpost']

        if self.do_atm and self.do_metp:
            configs += ['metp']

        if self.do_gempak:
            configs += ['gempak']

        if self.do_awips:
            configs += ['awips']

        if self.do_wave:
            configs += ['waveinit', 'waveprep', 'wavepostsbs', 'wavepostpnt']
            if self.do_wave_bnd:
                configs += ['wavepostbndpnt', 'wavepostbndpntbll']
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
        base_out['INTERVAL_GFS'] = get_gfs_interval(base_in['gfs_cyc'])
        base_out['CDUMP'] = 'gfs'

        return base_out

    def _source_configs(self, conf: Configuration) -> Dict[str, Any]:
        """
        Given the configuration object and jobs,
        source the configurations for each config and return a dictionary
        Every config depends on "config.base"
        """

        configs = dict()

        # Return config.base as well
        configs['base'] = conf.parse_config('config.base')

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
            configs[config] = conf.parse_config(files)

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
        """
        Get the task names for all the tasks in the cycled application.
        Note that the order of the task names matters in the XML.
        This is the place where that order is set.
        """

        gdas_gfs_common_tasks_before_fcst = ['prep']
        gdas_gfs_common_tasks_after_fcst = ['post']
        # if self.do_ocean:  # TODO: uncomment when ocnpost is fixed in cycled mode
        #    gdas_gfs_common_tasks_after_fcst += ['ocnpost']
        gdas_gfs_common_tasks_after_fcst += ['vrfy']

        gdas_gfs_common_cleanup_tasks = ['arch']

        if self.do_jediatmvar:
            gdas_gfs_common_tasks_before_fcst += ['atmanlinit', 'atmanlrun', 'atmanlfinal']
        else:
            gdas_gfs_common_tasks_before_fcst += ['anal']

        if self.do_jediocnvar:
            gdas_gfs_common_tasks_before_fcst += ['ocnanalprep', 'ocnanalbmat', 'ocnanalrun', 'ocnanalpost']

        gdas_gfs_common_tasks_before_fcst += ['sfcanl', 'analcalc']

        if self.do_aero:
            gdas_gfs_common_tasks_before_fcst += ['aeroanlinit', 'aeroanlrun', 'aeroanlfinal']

        if self.do_land:
            gdas_gfs_common_tasks_before_fcst += ['landanlinit', 'landanlrun', 'landanlfinal']

        gldas_tasks = ['gldas']
        wave_prep_tasks = ['waveinit', 'waveprep']
        wave_bndpnt_tasks = ['wavepostbndpnt', 'wavepostbndpntbll']
        wave_post_tasks = ['wavepostsbs', 'wavepostpnt']

        hybrid_tasks = []
        hybrid_after_eupd_tasks = []
        if self.do_hybvar:
            if self.do_jediens:
                hybrid_tasks += ['atmensanalprep', 'atmensanalrun', 'atmensanalpost', 'echgres']
            else:
                hybrid_tasks += ['eobs', 'eupd', 'echgres']
                hybrid_tasks += ['ediag'] if self.lobsdiag_forenkf else ['eomg']
            hybrid_after_eupd_tasks += ['ecen', 'esfc', 'efcs', 'epos', 'earc']

        # Collect all "gdas" cycle tasks
        gdas_tasks = gdas_gfs_common_tasks_before_fcst.copy()
        if not self.do_jediatmvar:
            gdas_tasks += ['analdiag']

        if self.do_gldas:
            gdas_tasks += gldas_tasks

        if self.do_wave and 'gdas' in self.wave_cdumps:
            gdas_tasks += wave_prep_tasks

        gdas_tasks += ['fcst']

        gdas_tasks += gdas_gfs_common_tasks_after_fcst

        if self.do_wave and 'gdas' in self.wave_cdumps:
            if self.do_wave_bnd:
                gdas_tasks += wave_bndpnt_tasks
            gdas_tasks += wave_post_tasks

        if self.do_fit2obs:
            gdas_tasks += ['fit2obs']

        gdas_tasks += gdas_gfs_common_cleanup_tasks

        # Collect "gfs" cycle tasks
        gfs_tasks = gdas_gfs_common_tasks_before_fcst

        if self.do_wave and 'gfs' in self.wave_cdumps:
            gfs_tasks += wave_prep_tasks

        gfs_tasks += ['fcst']

        gfs_tasks += gdas_gfs_common_tasks_after_fcst

        if self.do_metp:
            gfs_tasks += ['metp']

        if self.do_wave and 'gfs' in self.wave_cdumps:
            if self.do_wave_bnd:
                gfs_tasks += wave_bndpnt_tasks
            gfs_tasks += wave_post_tasks
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

        gfs_tasks += gdas_gfs_common_cleanup_tasks

        tasks = dict()
        tasks['gdas'] = gdas_tasks

        if self.do_hybvar and 'gdas' in self.eupd_cdumps:
            enkfgdas_tasks = hybrid_tasks + hybrid_after_eupd_tasks
            tasks['enkfgdas'] = enkfgdas_tasks

        # Add CDUMP=gfs tasks if running early cycle
        if self.gfs_cyc > 0:
            tasks['gfs'] = gfs_tasks

            if self.do_hybvar and 'gfs' in self.eupd_cdumps:
                enkfgfs_tasks = hybrid_tasks + hybrid_after_eupd_tasks
                enkfgfs_tasks.remove("echgres")
                tasks['enkfgfs'] = enkfgfs_tasks

        return tasks

    def _get_forecast_only_task_names(self):
        """
        Get the task names for all the tasks in the forecast-only application.
        Note that the order of the task names matters in the XML.
        This is the place where that order is set.
        """

        tasks = []

        if self.model_app in ['S2S', 'S2SW', 'S2SWA', 'NG-GODAS']:
            tasks += ['coupled_ic']
        else:
            if self.do_hpssarch:
                tasks += ['getic']
            tasks += ['init']

        if self.do_aero:
            tasks += ['aerosol_init']

        if self.do_wave:
            tasks += ['waveinit']
            # tasks += ['waveprep']  # TODO - verify if waveprep is executed in forecast-only mode when APP=ATMW|S2SW

        tasks += ['fcst']

        if self.do_atm:
            tasks += ['post']

        if self.model_app in ['S2S', 'S2SW', 'S2SWA', 'NG-GODAS']:
            tasks += ['ocnpost']

        if self.do_atm:
            tasks += ['vrfy']

        if self.do_atm and self.do_metp:
            tasks += ['metp']

        if self.do_wave:
            if self.do_wave_bnd:
                tasks += ['wavepostbndpnt', 'wavepostbndpntbll']
            tasks += ['wavepostsbs', 'wavepostpnt']
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

        tasks += ['arch']  # arch **must** be the last task

        return {f"{self._base['CDUMP']}": tasks}
