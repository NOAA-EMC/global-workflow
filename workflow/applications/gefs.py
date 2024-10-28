from applications.applications import AppConfig
from wxflow import Configuration


class GEFSAppConfig(AppConfig):
    '''
    Class to define GEFS configurations
    '''

    def __init__(self, conf: Configuration):
        super().__init__(conf)

        base = conf.parse_config('config.base')
        self.run = base.get('RUN', 'gefs')

    def _get_app_configs(self):
        """
        Returns the config_files that are involved in gefs
        """
        configs = ['stage_ic', 'fcst', 'atmos_products', 'arch', 'cleanup']

        if self.nens > 0:
            configs += ['efcs', 'atmos_ensstat']

        if self.do_wave:
            configs += ['waveinit', 'wavepostsbs', 'wavepostpnt']
            if self.do_wave_bnd:
                configs += ['wavepostbndpnt', 'wavepostbndpntbll']

        if self.do_ocean or self.do_ice:
            configs += ['oceanice_products']

        if self.do_aero:
            configs += ['prep_emissions']

        if self.do_extractvars:
            configs += ['extractvars']

        return configs

    @staticmethod
    def _update_base(base_in):

        base_out = base_in.copy()
        base_out['RUN'] = 'gefs'

        return base_out

    def get_task_names(self):

        tasks = ['stage_ic']

        if self.do_wave:
            tasks += ['waveinit']

        if self.do_aero:
            tasks += ['prep_emissions']

        tasks += ['fcst']

        if self.nens > 0:
            tasks += ['efcs']

        tasks += ['atmos_prod']

        if self.nens > 0:
            tasks += ['atmos_ensstat']

        if self.do_ocean:
            tasks += ['ocean_prod']

        if self.do_ice:
            tasks += ['ice_prod']

        if self.do_wave:
            tasks += ['wavepostsbs']
            if self.do_wave_bnd:
                tasks += ['wavepostbndpnt', 'wavepostbndpntbll']
            tasks += ['wavepostpnt']

        if self.do_extractvars:
            tasks += ['extractvars', 'arch']

        tasks += ['cleanup']

        return {f"{self.run}": tasks}
