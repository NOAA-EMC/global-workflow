from applications.applications import AppConfig
from wxflow import Configuration
from typing import Dict, Any


class GFSForecastOnlyAppConfig(AppConfig):
    '''
    Class to define GFS forecast-only configurations
    '''

    def __init__(self, conf: Configuration):
        super().__init__(conf)

        base = conf.parse_config('config.base')
        self.run = base.get('RUN', 'gfs')
        self.runs = [self.run]

    def _get_run_options(self, conf: Configuration) -> Dict[str, Any]:

        run_options = super()._get_run_options(conf)

        run_options[self.run]['exp_warm_start'] = conf.parse_config('config.base').get('EXP_WARM_START', False)

        return run_options

    def _get_app_configs(self, run):
        """
        Returns the config_files that are involved in the forecast-only app
        """

        options = self.run_options[run]
        configs = ['stage_ic', 'fcst', 'arch', 'cleanup']

        if options['do_atm']:

            if options['do_upp'] or options['do_goes']:
                configs += ['upp']

            configs += ['atmos_products']

            if options['do_aero_fcst']:
                if not options['exp_warm_start']:
                    configs += ['aerosol_init']

            if options['do_tracker']:
                configs += ['tracker']

            if options['do_genesis']:
                configs += ['genesis']

            if options['do_genesis_fsu']:
                configs += ['genesis_fsu']

            if options['do_metp']:
                configs += ['metp']

            if options['do_bufrsnd']:
                configs += ['postsnd']

            if options['do_gempak']:
                configs += ['gempak']

            if options['do_awips']:
                configs += ['awips']

        if options['do_ocean'] or options['do_ice']:
            configs += ['oceanice_products']

        if options['do_wave']:
            configs += ['waveinit', 'waveprep', 'wavepostsbs', 'wavepostpnt']
            if options['do_wave_bnd']:
                configs += ['wavepostbndpnt', 'wavepostbndpntbll']
            if options['do_gempak']:
                configs += ['wavegempak']
            if options['do_awips']:
                configs += ['waveawipsbulls', 'waveawipsgridded']

        if options['do_mos']:
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
        options = self.run_options[self.run]

        if options['do_aero_fcst'] and not options['exp_warm_start']:
            tasks += ['aerosol_init']

        if options['do_wave']:
            tasks += ['waveinit']
            # tasks += ['waveprep']  # TODO - verify if waveprep is executed in forecast-only mode when APP=ATMW|S2SW

        tasks += ['fcst']

        if options['do_atm']:

            if options['do_upp']:
                tasks += ['atmupp']

            tasks += ['atmos_prod']

            if options['do_goes']:
                tasks += ['goesupp']

            if options['do_tracker']:
                tasks += ['tracker']

            if options['do_genesis']:
                tasks += ['genesis']

            if options['do_genesis_fsu']:
                tasks += ['genesis_fsu']

            if options['do_metp']:
                tasks += ['metp']

            if options['do_bufrsnd']:
                tasks += ['postsnd']

            if options['do_gempak']:
                tasks += ['gempak', 'gempakmeta', 'gempakncdcupapgif', 'gempakpgrb2spec']

            if options['do_awips']:
                tasks += ['awips_20km_1p0deg', 'fbwind']

        if options['do_ocean']:
            tasks += ['ocean_prod']

        if options['do_ice']:
            tasks += ['ice_prod']

        if options['do_wave']:
            if options['do_wave_bnd']:
                tasks += ['wavepostbndpnt', 'wavepostbndpntbll']
            tasks += ['wavepostsbs', 'wavepostpnt']
            if options['do_gempak']:
                tasks += ['wavegempak']
            if options['do_awips']:
                tasks += ['waveawipsbulls', 'waveawipsgridded']

        if options['do_mos']:
            tasks += ['mos_stn_prep', 'mos_grd_prep', 'mos_ext_stn_prep', 'mos_ext_grd_prep',
                      'mos_stn_fcst', 'mos_grd_fcst', 'mos_ext_stn_fcst', 'mos_ext_grd_fcst',
                      'mos_stn_prdgen', 'mos_grd_prdgen', 'mos_ext_stn_prdgen', 'mos_ext_grd_prdgen',
                      'mos_wx_prdgen', 'mos_wx_ext_prdgen']

        tasks += ['arch', 'cleanup']  # arch and cleanup **must** be the last tasks

        return {f"{self.run}": tasks}
