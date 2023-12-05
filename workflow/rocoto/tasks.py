#!/usr/bin/env python3

import numpy as np
from applications.applications import AppConfig
import rocoto.rocoto as rocoto
from wxflow import Template, TemplateConstants, to_timedelta

__all__ = ['Tasks', 'create_wf_task']


class Tasks:
    SERVICE_TASKS = ['arch', 'earc']
    VALID_TASKS = ['aerosol_init', 'stage_ic',
                   'prep', 'anal', 'sfcanl', 'analcalc', 'analdiag', 'arch', "cleanup",
                   'prepatmiodaobs', 'atmanlinit', 'atmanlrun', 'atmanlfinal',
                   'ocnanalprep', 'ocnanalbmat', 'ocnanalrun', 'ocnanalchkpt', 'ocnanalpost', 'ocnanalvrfy',
                   'earc', 'ecen', 'echgres', 'ediag', 'efcs',
                   'eobs', 'eomg', 'epos', 'esfc', 'eupd',
                   'atmensanlinit', 'atmensanlrun', 'atmensanlfinal',
                   'aeroanlinit', 'aeroanlrun', 'aeroanlfinal',
                   'preplandobs', 'landanl',
                   'fcst', 'post', 'ocnpost',
                   'verfozn', 'verfrad', 'vminmon', 'metp',
                   'tracker', 'genesis', 'genesis_fsu',
                   'postsnd', 'awips_g2', 'awips_20km_1p0deg', 'fbwinds', 'gempak', 'gempakmeta', 'gempakmetancdc',
                   'gempakncdcupapgif', 'gempakpgrb2spec',
                   'waveawipsbulls', 'waveawipsgridded', 'wavegempak', 'waveinit',
                   'wavepostbndpnt', 'wavepostbndpntbll', 'wavepostpnt', 'wavepostsbs', 'waveprep',
                   'npoess']

    def __init__(self, app_config: AppConfig, cdump: str) -> None:

        self.app_config = app_config
        self.cdump = cdump

        # Save dict_configs and base in the internal state (never know where it may be needed)
        self._configs = self.app_config.configs
        self._base = self._configs['base']
        self._base['cycle_interval'] = to_timedelta(f'{self._base["assim_freq"]}H')

        self.n_tiles = 6  # TODO - this needs to be elsewhere

        envar_dict = {'RUN_ENVIR': self._base.get('RUN_ENVIR', 'emc'),
                      'HOMEgfs': self._base.get('HOMEgfs'),
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

    @staticmethod
    def _get_hybgroups(nens: int, nmem_per_group: int, start_index: int = 1):
        ngrps = nens / nmem_per_group
        groups = ' '.join([f'{x:02d}' for x in range(start_index, int(ngrps) + 1)])
        return groups

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

        native = None
        if scheduler in ['pbspro']:
            native = '-l debug=true,place=vscatter'
            if task_config.get('is_exclusive', False):
                native += ':exclhost'
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


def create_wf_task(task_name, resources,
                   cdump='gdas', cycledef=None, envar=None, dependency=None,
                   metatask=None, varname=None, varval=None, vardict=None,
                   final=False):
    tasknamestr = f'{cdump}{task_name}'
    metatask_dict = None
    if metatask is not None:
        tasknamestr = f'{tasknamestr}#{varname}#'
        metatask_dict = {'metataskname': f'{cdump}{metatask}',
                         'varname': f'{varname}',
                         'varval': f'{varval}',
                         'vardict': vardict}

    cycledefstr = cdump.replace('enkf', '') if cycledef is None else cycledef

    task_dict = {'taskname': f'{tasknamestr}',
                 'cycledef': f'{cycledefstr}',
                 'maxtries': '&MAXTRIES;',
                 'command': f'&JOBS_DIR;/{task_name}.sh',
                 'jobname': f'&PSLOT;_{tasknamestr}_@H',
                 'resources': resources,
                 'log': f'&ROTDIR;/logs/@Y@m@d@H/{tasknamestr}.log',
                 'envars': envar,
                 'dependency': dependency,
                 'final': final}

    task = rocoto.create_task(task_dict) if metatask is None else rocoto.create_metatask(task_dict, metatask_dict)

    return ''.join(task)
