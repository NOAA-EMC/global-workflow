from applications.applications import AppConfig
from typing import Dict, Any
from wxflow import Configuration


class GEFSAppConfig(AppConfig):
    '''
    Class to define GEFS configurations
    '''

    def __init__(self, conf: Configuration):
        super().__init__(conf)

        base = conf.parse_config('config.base')
        self.run = base.get('RUN', 'gefs')
        self.runs = [self.run]

    def _get_run_options(self, conf: Configuration) -> Dict[str, Any]:

        run_options = super()._get_run_options(conf)

        run_options[self.run]['nens'] = conf.parse_config('config.base').get('NMEM_ENS', 0)

        return run_options

    def _get_app_configs(self, run):
        """
        Returns the config_files that are involved in gefs
        """
        options = self.run_options[run]
        configs = ['stage_ic', 'fcst', 'atmos_products', 'arch', 'cleanup']

        if options['nens'] > 0:
            configs += ['efcs', 'atmos_ensstat']

        if options['do_wave']:
            configs += ['waveinit', 'wavepostsbs', 'wavepostpnt']
            if options['do_wave_bnd']:
                configs += ['wavepostbndpnt', 'wavepostbndpntbll']

        if options['do_ocean'] or options['do_ice']:
            configs += ['oceanice_products']

        if options['do_aero_fcst']:
            configs += ['prep_emissions']

        if options['do_extractvars']:
            configs += ['extractvars']

        return configs

    @staticmethod
    def _update_base(base_in):

        base_out = base_in.copy()
        base_out['RUN'] = 'gefs'

        return base_out

    def get_task_names(self):

        options = self.run_options[self.run]
        tasks = ['stage_ic']

        if options['do_wave']:
            tasks += ['waveinit']

        if options['do_aero_fcst']:
            tasks += ['prep_emissions']

        tasks += ['fcst']

        if options['nens'] > 0:
            tasks += ['efcs']

        tasks += ['atmos_prod']

        if options['nens'] > 0:
            tasks += ['atmos_ensstat']

        if options['do_ocean']:
            tasks += ['ocean_prod']

        if options['do_ice']:
            tasks += ['ice_prod']

        if options['do_wave']:
            tasks += ['wavepostsbs']
            if options['do_wave_bnd']:
                tasks += ['wavepostbndpnt', 'wavepostbndpntbll']
            tasks += ['wavepostpnt']

        if options['do_extractvars']:
            tasks += ['extractvars', 'arch']

        tasks += ['cleanup']

        return {f"{self.run}": tasks}
