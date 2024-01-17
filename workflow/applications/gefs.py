from applications.applications import AppConfig
from wxflow import Configuration


class GEFSAppConfig(AppConfig):
    '''
    Class to define GEFS configurations
    '''

    def __init__(self, conf: Configuration):
        super().__init__(conf)

    def _get_app_configs(self):
        """
        Returns the config_files that are involved in gefs
        """
        configs = ['stage_ic', 'fcst', 'atmos_products']

        if self.nens > 0:
            configs += ['efcs']

        if self.do_wave:
            configs += ['waveinit']

        return configs

    @staticmethod
    def _update_base(base_in):

        base_out = base_in.copy()
        base_out['INTERVAL_GFS'] = AppConfig.get_gfs_interval(base_in['gfs_cyc'])
        base_out['CDUMP'] = 'gefs'

        return base_out

    def get_task_names(self):

        tasks = ['stage_ic']

        if self.do_wave:
            tasks += ['waveinit']

        tasks += ['fcst']

        if self.nens > 0:
            tasks += ['efcs']

        tasks += ['atmprod']

        return {f"{self._base['CDUMP']}": tasks}
