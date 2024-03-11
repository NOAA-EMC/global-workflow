#!/usr/bin/env python3

import numpy as np
from applications.applications import AppConfig
import rocoto.rocoto as rocoto
from wxflow import Template, TemplateConstants, to_timedelta
from typing import List

__all__ = ['Tasks']


class Tasks:
    SERVICE_TASKS = ['arch', 'earc']
    VALID_TASKS = ['aerosol_init', 'stage_ic',
                   'prep', 'anal', 'sfcanl', 'analcalc', 'analdiag', 'arch', "cleanup",
                   'prepatmiodaobs', 'atmanlinit', 'atmanlrun', 'atmanlfinal',
                   'prepoceanobs',
                   'ocnanalprep', 'ocnanalbmat', 'ocnanalrun', 'ocnanalchkpt', 'ocnanalpost', 'ocnanalvrfy',
                   'earc', 'ecen', 'echgres', 'ediag', 'efcs',
                   'eobs', 'eomg', 'epos', 'esfc', 'eupd',
                   'atmensanlinit', 'atmensanlrun', 'atmensanlfinal',
                   'aeroanlinit', 'aeroanlrun', 'aeroanlfinal',
                   'prepsnowobs', 'snowanl',
                   'fcst',
                   'atmanlupp', 'atmanlprod', 'atmupp', 'goesupp',
                   'atmosprod', 'oceanprod', 'iceprod',
                   'verfozn', 'verfrad', 'vminmon',
                   'metp',
                   'tracker', 'genesis', 'genesis_fsu',
                   'postsnd', 'awips_g2', 'awips_20km_1p0deg', 'fbwind',
                   'gempak', 'gempakmeta', 'gempakmetancdc', 'gempakncdcupapgif', 'gempakpgrb2spec', 'npoess_pgrb2_0p5deg'
                   'waveawipsbulls', 'waveawipsgridded', 'wavegempak', 'waveinit',
                   'wavepostbndpnt', 'wavepostbndpntbll', 'wavepostpnt', 'wavepostsbs', 'waveprep',
                   'npoess',
                   'mos_stn_prep', 'mos_grd_prep', 'mos_ext_stn_prep', 'mos_ext_grd_prep',
                   'mos_stn_fcst', 'mos_grd_fcst', 'mos_ext_stn_fcst', 'mos_ext_grd_fcst',
                   'mos_stn_prdgen', 'mos_grd_prdgen', 'mos_ext_stn_prdgen', 'mos_ext_grd_prdgen', 'mos_wx_prdgen', 'mos_wx_ext_prdgen']

    def __init__(self, app_config: AppConfig, cdump: str) -> None:

        self.app_config = app_config
        self.cdump = cdump

        # Save dict_configs and base in the internal state (never know where it may be needed)
        self._configs = self.app_config.configs
        self._base = self._configs['base']
        self.HOMEgfs = self._base['HOMEgfs']
        self.rotdir = self._base['ROTDIR']
        self.pslot = self._base['PSLOT']
        self.nmem = int(self._base['NMEM_ENS'])
        self._base['cycle_interval'] = to_timedelta(f'{self._base["assim_freq"]}H')

        self.n_tiles = 6  # TODO - this needs to be elsewhere

        envar_dict = {'RUN_ENVIR': self._base.get('RUN_ENVIR', 'emc'),
                      'HOMEgfs': self.HOMEgfs,
                      'EXPDIR': self._base.get('EXPDIR'),
                      'NET': self._base.get('NET'),
                      'CDUMP': self.cdump,
                      'RUN': self.cdump,
                      'CDATE': '<cyclestr>@Y@m@d@H</cyclestr>',
                      'PDY': '<cyclestr>@Y@m@d</cyclestr>',
                      'cyc': '<cyclestr>@H</cyclestr>',
                      'COMROOT': self._base.get('COMROOT'),
                      'DATAROOT': self._base.get('DATAROOT')}

        self.envars = self._set_envars(envar_dict)

    @staticmethod
    def _set_envars(envar_dict) -> list:

        envars = []
        for key, value in envar_dict.items():
            envars.append(rocoto.create_envar(name=key, value=str(value)))

        return envars

    def _template_to_rocoto_cycstring(self, template: str, subs_dict: dict = {}) -> str:
        '''
        Takes a string templated with ${ } and converts it into a string suitable
          for use in a rocoto <cyclestr>. Some common substitutions are defined by
          default. Any additional variables in the template and overrides of the
          defaults can be passed in by an optional dict.

          Variables substitued by default:
            ${ROTDIR} -> '&ROTDIR;'
            ${RUN}    -> self.cdump
            ${DUMP}   -> self.cdump
            ${MEMDIR} -> ''
            ${YMD}    -> '@Y@m@d'
            ${HH}     -> '@H'

        Parameters
        ----------
        template: str
                  Template string with variables to be replaced
        subs_dict: dict, optional
                   Dictionary containing substitutions

        Returns
        -------
        str
            Updated string with variables substituted

        '''

        # Defaults
        rocoto_conversion_dict = {
            'ROTDIR': '&ROTDIR;',
            'RUN': self.cdump,
            'DUMP': self.cdump,
            'MEMDIR': '',
            'YMD': '@Y@m@d',
            'HH': '@H'
        }

        rocoto_conversion_dict.update(subs_dict)

        return Template.substitute_structure(template,
                                             TemplateConstants.DOLLAR_CURLY_BRACE,
                                             rocoto_conversion_dict.get)

    @staticmethod
    def _get_forecast_hours(cdump, config, component='atmos') -> List[str]:
        # Make a local copy of the config to avoid modifying the original
        local_config = config.copy()

        # Ocean/Ice components do not have a HF output option like the atmosphere
        if component in ['ocean', 'ice']:
            local_config['FHMAX_HF_GFS'] = config['FHMAX_GFS']
            local_config['FHOUT_HF_GFS'] = config['FHOUT_OCNICE_GFS']
            local_config['FHOUT_GFS'] = config['FHOUT_OCNICE_GFS']
            local_config['FHOUT'] = config['FHOUT_OCNICE']

        fhmin = local_config['FHMIN']

        # Get a list of all forecast hours
        fhrs = []
        if cdump in ['gdas']:
            fhmax = local_config['FHMAX']
            fhout = local_config['FHOUT']
            fhrs = list(range(fhmin, fhmax + fhout, fhout))
        elif cdump in ['gfs', 'gefs']:
            fhmax = local_config['FHMAX_GFS']
            fhout = local_config['FHOUT_GFS']
            fhmax_hf = local_config['FHMAX_HF_GFS']
            fhout_hf = local_config['FHOUT_HF_GFS']
            fhrs_hf = range(fhmin, fhmax_hf + fhout_hf, fhout_hf)
            fhrs = list(fhrs_hf) + list(range(fhrs_hf[-1] + fhout, fhmax + fhout, fhout))

        # ocean/ice components do not have fhr 0 as they are averaged output
        if component in ['ocean', 'ice']:
            fhrs.remove(0)

        return fhrs

    def get_resource(self, task_name):
        """
        Given a task name (task_name) and its configuration (task_names),
        return a dictionary of resources (task_resource) used by the task.
        Task resource dictionary includes:
        account, walltime, cores, nodes, ppn, threads, memory, queue, partition, native
        """

        scheduler = self.app_config.scheduler

        task_config = self._configs[task_name]

        account = task_config['ACCOUNT']

        walltime = task_config[f'wtime_{task_name}']
        if self.cdump in ['gfs'] and f'wtime_{task_name}_gfs' in task_config.keys():
            walltime = task_config[f'wtime_{task_name}_gfs']

        cores = task_config[f'npe_{task_name}']
        if self.cdump in ['gfs'] and f'npe_{task_name}_gfs' in task_config.keys():
            cores = task_config[f'npe_{task_name}_gfs']

        ppn = task_config[f'npe_node_{task_name}']
        if self.cdump in ['gfs'] and f'npe_node_{task_name}_gfs' in task_config.keys():
            ppn = task_config[f'npe_node_{task_name}_gfs']

        nodes = int(np.ceil(float(cores) / float(ppn)))

        threads = task_config[f'nth_{task_name}']
        if self.cdump in ['gfs'] and f'nth_{task_name}_gfs' in task_config.keys():
            threads = task_config[f'nth_{task_name}_gfs']

        memory = task_config.get(f'memory_{task_name}', None)
        if scheduler in ['pbspro']:
            if task_config.get('prepost', False):
                memory += ':prepost=true'

        native = None
        if scheduler in ['pbspro']:
            # Set place=vscatter by default and debug=true if DEBUG_POSTSCRIPT="YES"
            if self._base['DEBUG_POSTSCRIPT']:
                native = '-l debug=true,place=vscatter'
            else:
                native = '-l place=vscatter'
            # Set either exclusive or shared - default on WCOSS2 is exclusive when not set
            if task_config.get('is_exclusive', False):
                native += ':exclhost'
            else:
                native += ':shared'
        elif scheduler in ['slurm']:
            native = '--export=NONE'

        queue = task_config['QUEUE_SERVICE'] if task_name in Tasks.SERVICE_TASKS else task_config['QUEUE']

        partition = None
        if scheduler in ['slurm']:
            partition = task_config['PARTITION_SERVICE'] if task_name in Tasks.SERVICE_TASKS else task_config[
                'PARTITION_BATCH']

        task_resource = {'account': account,
                         'walltime': walltime,
                         'nodes': nodes,
                         'cores': cores,
                         'ppn': ppn,
                         'threads': threads,
                         'memory': memory,
                         'native': native,
                         'queue': queue,
                         'partition': partition}

        return task_resource

    def get_task(self, task_name, *args, **kwargs):
        """
        Given a task_name, call the method for that task
        """
        try:
            return getattr(self, task_name, *args, **kwargs)()
        except AttributeError:
            raise AttributeError(f'"{task_name}" is not a valid task.\n' +
                                 'Valid tasks are:\n' +
                                 f'{", ".join(Tasks.VALID_TASKS)}')
