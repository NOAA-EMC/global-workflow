from applications.applications import AppConfig
from wxflow import Configuration


class GFSForecastOnlyAppConfig(AppConfig):
    '''
    Class to define GFS forecast-only configurations
    '''

    def __init__(self, conf: Configuration):
        super().__init__(conf)

    def _get_app_configs(self):
        """
        Returns the config_files that are involved in the forecast-only app
        """

        configs = ['stage_ic', 'fcst', 'arch', 'cleanup']

        if self.do_atm:
            configs += ['post']

        if self.do_aero:
            configs += ['aerosol_init']

        if self.do_ocean or self.do_ice:
            configs += ['ocnpost']

        if self.do_atm and self.do_tracker:
            configs += ['tracker']

        if self.do_atm and self.do_genesis:
            configs += ['genesis']

        if self.do_atm and self.do_genesis_fsu:
            configs += ['genesis_fsu']

        if self.do_atm and self.do_metp:
            configs += ['metp']

        if self.do_bufrsnd:
            configs += ['postsnd']

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

        return configs

    @staticmethod
    def _update_base(base_in):

        base_out = base_in.copy()
        base_out['INTERVAL_GFS'] = AppConfig.get_gfs_interval(base_in['gfs_cyc'])
        base_out['CDUMP'] = 'gfs'

        return base_out

    def get_task_names(self):
        """
        Get the task names for all the tasks in the forecast-only application.
        Note that the order of the task names matters in the XML.
        This is the place where that order is set.
        """

        tasks = ['stage_ic']

        if self.do_aero:
            tasks += ['aerosol_init']

        if self.do_wave:
            tasks += ['waveinit']
            # tasks += ['waveprep']  # TODO - verify if waveprep is executed in forecast-only mode when APP=ATMW|S2SW

        tasks += ['fcst']

        if self.do_atm:
            tasks += ['post']

        if self.do_ocean:
            tasks += ['ocnpost']

        if self.do_atm and self.do_tracker:
            tasks += ['tracker']

        if self.do_atm and self.do_genesis:
            tasks += ['genesis']

        if self.do_atm and self.do_genesis_fsu:
            tasks += ['genesis_fsu']

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
            tasks += ['awips_20km_1p0deg']
            tasks += ['awips_g2']
            tasks += ['fbwinds']

        if self.do_wafs:
            tasks += ['wafs', 'wafsgcip', 'wafsgrib2', 'wafsgrib20p25', 'wafsblending', 'wafsblending0p25']

        tasks += ['arch', 'cleanup']  # arch and cleanup **must** be the last tasks

        return {f"{self._base['CDUMP']}": tasks}
