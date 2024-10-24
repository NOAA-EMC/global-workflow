from applications.applications import AppConfig
from wxflow import Configuration


class GFSForecastOnlyAppConfig(AppConfig):
    '''
    Class to define GFS forecast-only configurations
    '''

    def __init__(self, conf: Configuration):
        super().__init__(conf)

        base = conf.parse_config('config.base')
        self.aero_fcst_run = base.get('AERO_FCST_RUN', 'BOTH').lower()
        self.run = base.get('RUN', 'gfs')
        self.exp_warm_start = base.get('EXP_WARM_START', False)

    def _get_app_configs(self):
        """
        Returns the config_files that are involved in the forecast-only app
        """

        configs = ['stage_ic', 'fcst', 'arch', 'cleanup']

        if self.do_atm:

            if self.do_upp or self.do_goes:
                configs += ['upp']

            configs += ['atmos_products']

            if self.do_aero:
                if not self.exp_warm_start:
                    configs += ['aerosol_init']

            if self.do_tracker:
                configs += ['tracker']

            if self.do_genesis:
                configs += ['genesis']

            if self.do_genesis_fsu:
                configs += ['genesis_fsu']

            if self.do_metp:
                configs += ['metp']

            if self.do_bufrsnd:
                configs += ['postsnd']

            if self.do_gempak:
                configs += ['gempak']

            if self.do_awips:
                configs += ['awips']

        if self.do_ocean or self.do_ice:
            configs += ['oceanice_products']

        if self.do_wave:
            configs += ['waveinit', 'waveprep', 'wavepostsbs', 'wavepostpnt']
            if self.do_wave_bnd:
                configs += ['wavepostbndpnt', 'wavepostbndpntbll']
            if self.do_gempak:
                configs += ['wavegempak']
            if self.do_awips:
                configs += ['waveawipsbulls', 'waveawipsgridded']

        if self.do_mos:
            configs += ['mos_stn_prep', 'mos_grd_prep', 'mos_ext_stn_prep', 'mos_ext_grd_prep',
                        'mos_stn_fcst', 'mos_grd_fcst', 'mos_ext_stn_fcst', 'mos_ext_grd_fcst',
                        'mos_stn_prdgen', 'mos_grd_prdgen', 'mos_ext_stn_prdgen', 'mos_ext_grd_prdgen',
                        'mos_wx_prdgen', 'mos_wx_ext_prdgen']

        return configs

    @staticmethod
    def _update_base(base_in):

        base_out = base_in.copy()
        base_out['RUN'] = 'gfs'

        return base_out

    def get_task_names(self):
        """
        Get the task names for all the tasks in the forecast-only application.
        Note that the order of the task names matters in the XML.
        This is the place where that order is set.
        """

        tasks = ['stage_ic']

        if self.do_aero:
            aero_fcst_run = self.aero_fcst_run
            if self.run in aero_fcst_run or aero_fcst_run == "both":
                if not self.exp_warm_start:
                    tasks += ['aerosol_init']

        if self.do_wave:
            tasks += ['waveinit']
            # tasks += ['waveprep']  # TODO - verify if waveprep is executed in forecast-only mode when APP=ATMW|S2SW

        tasks += ['fcst']

        if self.do_atm:

            if self.do_upp:
                tasks += ['atmupp']

            tasks += ['atmos_prod']

            if self.do_goes:
                tasks += ['goesupp']

            if self.do_tracker:
                tasks += ['tracker']

            if self.do_genesis:
                tasks += ['genesis']

            if self.do_genesis_fsu:
                tasks += ['genesis_fsu']

            if self.do_metp:
                tasks += ['metp']

            if self.do_bufrsnd:
                tasks += ['postsnd']

            if self.do_gempak:
                tasks += ['gempak', 'gempakmeta', 'gempakncdcupapgif', 'gempakpgrb2spec']

            if self.do_awips:
                tasks += ['awips_20km_1p0deg', 'fbwind']

        if self.do_ocean:
            tasks += ['ocean_prod']

        if self.do_ice:
            tasks += ['ice_prod']

        if self.do_wave:
            if self.do_wave_bnd:
                tasks += ['wavepostbndpnt', 'wavepostbndpntbll']
            tasks += ['wavepostsbs', 'wavepostpnt']
            if self.do_gempak:
                tasks += ['wavegempak']
            if self.do_awips:
                tasks += ['waveawipsbulls', 'waveawipsgridded']

        if self.do_mos:
            tasks += ['mos_stn_prep', 'mos_grd_prep', 'mos_ext_stn_prep', 'mos_ext_grd_prep',
                      'mos_stn_fcst', 'mos_grd_fcst', 'mos_ext_stn_fcst', 'mos_ext_grd_fcst',
                      'mos_stn_prdgen', 'mos_grd_prdgen', 'mos_ext_stn_prdgen', 'mos_ext_grd_prdgen',
                      'mos_wx_prdgen', 'mos_wx_ext_prdgen']

        tasks += ['arch', 'cleanup']  # arch and cleanup **must** be the last tasks

        return {f"{self.run}": tasks}
