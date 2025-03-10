#!/usr/bin/env python3

import numpy as np
from applications.applications import AppConfig
import rocoto.rocoto as rocoto
from wxflow import Template, TemplateConstants, to_timedelta, timedelta_to_HMS
from typing import List, Union
from bisect import bisect_right

__all__ = ['Tasks']


class Tasks:
    SERVICE_TASKS = ['arch_vrfy', 'earc_vrfy', 'stage_ic', 'cleanup']
    DTN_TASKS = ['arch_tars', 'earc_tars', 'fetch']
    VALID_TASKS = ['aerosol_init', 'stage_ic', 'fetch',
                   'prep', 'anal', 'sfcanl', 'analcalc', 'analdiag', 'arch_vrfy', 'arch_tars', "cleanup",
                   'prepatmiodaobs', 'atmanlinit', 'atmanlvar', 'atmanlfv3inc', 'atmanlfinal',
                   'prep_emissions', 'prepoceanobs',
                   'marineanlinit', 'marineanlletkf', 'marinebmat', 'marineanlvar', 'ocnanalecen', 'marineanlchkpt', 'marineanlfinal', 'ocnanalvrfy',
                   'earc_vrfy', 'earc_tars', 'ecen', 'echgres', 'ediag', 'efcs',
                   'eobs', 'eomg', 'epos', 'esfc', 'eupd',
                   'atmensanlinit', 'atmensanlobs', 'atmensanlsol', 'atmensanlletkf', 'atmensanlfv3inc', 'atmensanlfinal', 'atmos_ensstat',
                   'aeroanlinit', 'aeroanlvar', 'aeroanlfinal', 'aeroanlgenb',
                   'snowanl', 'esnowanl',
                   'fcst',
                   'upp', 'atmanlprod', 'atmupp', 'goesupp',
                   'atmos_products', 'oceanice_products',
                   'verfozn', 'verfrad', 'vminmon',
                   'metp', 'fit2obs', 'extractvars',
                   'tracker', 'genesis', 'genesis_fsu',
                   'postsnd', 'awips', 'awips_20km_1p0deg', 'fbwind',
                   'gempak', 'gempakmeta', 'gempakmetancdc', 'gempakncdcupapgif', 'gempakpgrb2spec', 'npoess_pgrb2_0p5deg',
                   'waveawipsbulls', 'waveawipsgridded', 'wavegempak', 'waveinit',
                   'wavepostbndpnt', 'wavepostbndpntbll', 'wavepostpnt', 'wavepostsbs', 'waveprep',
                   'npoess',
                   'mos_stn_prep', 'mos_grd_prep', 'mos_ext_stn_prep', 'mos_ext_grd_prep',
                   'mos_stn_fcst', 'mos_grd_fcst', 'mos_ext_stn_fcst', 'mos_ext_grd_fcst',
                   'mos_stn_prdgen', 'mos_grd_prdgen', 'mos_ext_stn_prdgen', 'mos_ext_grd_prdgen', 'mos_wx_prdgen', 'mos_wx_ext_prdgen']

    def __init__(self, app_config: AppConfig, run: str) -> None:

        self.app_config = app_config
        self.run = run

        # Get the configs for the specified RUN
        self._configs = self.app_config.configs[run]

        # Get the workflow options for the specified RUN
        self.options = self.app_config.run_options[run]

        # Update the base config for the application
        self._configs['base'] = self.app_config._update_base(self._configs['base'])

        # Save base in the internal state (never know where it may be needed)
        self._base = self._configs['base']

        self.HOMEgfs = self._base['HOMEgfs']
        self.rotdir = self._base['ROTDIR']
        self.pslot = self._base['PSLOT']
        if self.run == "enkfgfs":
            self.nmem = int(self._base['NMEM_ENS_GFS'])
        else:
            self.nmem = int(self._base['NMEM_ENS'])
        self._base['interval_gdas'] = to_timedelta(f'{self._base["assim_freq"]}H')
        self._base['interval_gfs'] = to_timedelta(f'{self._base["INTERVAL_GFS"]}H')

        # DATAROOT is set by prod_envir in ops.  Here, we use `STMP` to construct DATAROOT
        dataroot_str = f"{self._base.get('STMP')}/RUNDIRS/{self._base.get('PSLOT')}/{self.run}.<cyclestr>@Y@m@d@H</cyclestr>"
        envar_dict = {'RUN_ENVIR': self._base.get('RUN_ENVIR', 'emc'),
                      'HOMEgfs': self.HOMEgfs,
                      'EXPDIR': self._base.get('EXPDIR'),
                      'NET': self._base.get('NET'),
                      'RUN': self.run,
                      'CDATE': '<cyclestr>@Y@m@d@H</cyclestr>',
                      'PDY': '<cyclestr>@Y@m@d</cyclestr>',
                      'cyc': '<cyclestr>@H</cyclestr>',
                      'COMROOT': self._base.get('COMROOT'),
                      'DATAROOT': dataroot_str}

        self._set_envars(envar_dict)

        self._set_system_settings()

    def _set_envars(self, envar_dict) -> None:

        envars = []
        for key, value in envar_dict.items():
            envars.append(rocoto.create_envar(name=key, value=str(value)))

        self.envars = envars

    def _set_system_settings(self) -> None:

        def _validate_system_key(input_dict, key):
            # This helper function returns None if
            # 1) the value held in 'key' matches '@' + key + '@'
            # 2) the value is an empty string, or
            # 3) the key does not exist in the dictionary.

            value = input_dict.get(key, None)
            value = None if value == '' else value
            return None if value == f'@{key}@' else value

        # Check the system configuration
        base = self._base
        self.clusters_batch = _validate_system_key(base, 'CLUSTERS')
        self.clusters_service = _validate_system_key(base, 'CLUSTERS_SERVICE')
        self.clusters_dtn = _validate_system_key(base, 'CLUSTERS_DTN')

        # Reservations are only valid for batch nodes (at least for now)
        self.reservation_batch = _validate_system_key(base, 'RESERVATION')

        self.partition_batch = _validate_system_key(base, 'PARTITION_BATCH')
        self.partition_service = _validate_system_key(base, 'PARTITION_SERVICE')
        self.partition_dtn = _validate_system_key(base, 'PARTITION_DTN')

        self.queue_batch = _validate_system_key(base, 'QUEUE')
        self.queue_service = _validate_system_key(base, 'QUEUE_SERVICE')
        self.queue_dtn = _validate_system_key(base, 'QUEUE_DTN')

    def _template_to_rocoto_cycstring(self, template: str, subs_dict: dict = {}) -> str:
        '''
        Takes a string templated with ${ } and converts it into a string suitable
          for use in a rocoto <cyclestr>. Some common substitutions are defined by
          default. Any additional variables in the template and overrides of the
          defaults can be passed in by an optional dict.

          Variables substitued by default:
            ${ROTDIR} -> '&ROTDIR;'
            ${RUN}    -> self.run
            ${DUMP}   -> self.run
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
            'RUN': self.run,
            'DUMP': self.run,
            'MEMDIR': '',
            'YMD': '@Y@m@d',
            'HH': '@H'
        }

        rocoto_conversion_dict.update(subs_dict)

        return Template.substitute_structure(template,
                                             TemplateConstants.DOLLAR_CURLY_BRACE,
                                             rocoto_conversion_dict.get)

    @staticmethod
    def _get_forecast_hours(run, config, component='atmos') -> List[str]:
        # Make a local copy of the config to avoid modifying the original
        local_config = config.copy()
        # Ocean/Ice components do not have a HF output option like the atmosphere
        if component in ['ocean', 'ice']:
            local_config['FHMAX_HF_GFS'] = 0

        if component in ['ocean']:
            local_config['FHOUT_HF_GFS'] = config['FHOUT_OCN_GFS']
            local_config['FHOUT_GFS'] = config['FHOUT_OCN_GFS']
            local_config['FHOUT'] = config['FHOUT_OCN']

        if component in ['ice']:
            local_config['FHOUT_HF_GFS'] = config['FHOUT_ICE_GFS']
            local_config['FHOUT_GFS'] = config['FHOUT_ICE_GFS']
            local_config['FHOUT'] = config['FHOUT_ICE']

        if component in ['wave']:
            local_config['FHOUT_HF_GFS'] = config['FHOUT_HF_WAV']
            local_config['FHMAX_HF_GFS'] = config['FHMAX_HF_WAV']
            local_config['FHOUT_GFS'] = config['FHOUT_WAV']
            local_config['FHOUT'] = config['FHOUT_WAV']

        fhmin = local_config['FHMIN']

        # Get a list of all forecast hours
        fhrs = []
        if run in ['gdas']:
            fhmax = local_config['FHMAX']
            fhout = local_config['FHOUT']
            fhrs = list(range(fhmin, fhmax + fhout, fhout))
        elif run in ['gfs', 'gefs', 'sfs']:
            fhmax = local_config['FHMAX_GFS']
            fhout = local_config['FHOUT_GFS']
            fhout_hf = local_config['FHOUT_HF_GFS']
            fhmax_hf = local_config['FHMAX_HF_GFS']
            fhrs_hf = range(fhmin, fhmax_hf + fhout_hf, fhout_hf)
            fhrs = list(fhrs_hf) + list(range(fhrs_hf[-1] + fhout, fhmax + fhout, fhout))

        return fhrs

    @staticmethod
    def get_job_groups(fhrs: List[int], ngroups: int, breakpoints: List[int] = None) -> List[dict]:
        '''
        Split forecast hours into a number of groups, obeying a list of pre-set breakpoints.

        Takes a list of forecast hours and splits it into a number of groups while obeying
        a list of pre-set breakpoints and recording which segment each belongs to.

        Parameters
        ----------
        fhrs: List[int]
                List of forecast hours to break into groups
        ngroups: int
                 Number of groups to split the forecast hours into
        breakpoints: List[int]
                     List of preset forecast hour break points to use (default: [])

        Returns
        -------
        List[dict]: List of dicts, where each dict contains two keys:
                    'fhrs': the forecast hours for that group
                    'seg': the forecast segment (from the original breakpoint list)
                           the group belong to
        '''
        if breakpoints is None:
            breakpoints = []

        num_segs = len(breakpoints) + 1
        if num_segs > ngroups:
            raise ValueError(f"Number of segments ({num_segs}) is greater than the number of groups ({ngroups}")

        if ngroups > len(fhrs):
            ngroups = len(fhrs)

        # First, split at segment boundaries
        fhrs_segs = [grp.tolist() for grp in np.array_split(fhrs, [bisect_right(fhrs, bpnt) for bpnt in breakpoints if bpnt < max(fhrs)])]
        seg_lens = [len(seg) for seg in fhrs_segs]

        # Initialize each segment to be split into one job group
        ngroups_segs = [1 for _ in range(0, len(fhrs_segs))]

        # For remaining job groups, iteratively assign to the segment with the most
        # hours per group
        for _ in range(0, ngroups - len(fhrs_segs)):
            current_lens = [size / weight for size, weight in zip(seg_lens, ngroups_segs)]
            index_max = max(range(len(current_lens)), key=current_lens.__getitem__)
            ngroups_segs[index_max] += 1

        # Now that we know how many groups each forecast segment should be split into,
        # Split them and flatten to a single list.
        groups = []
        for seg_num, (fhrs_seg, ngroups_seg) in enumerate(zip(fhrs_segs, ngroups_segs)):
            [groups.append({'fhrs': grp.tolist(), 'seg': seg_num}) for grp in np.array_split(fhrs_seg, ngroups_seg)]

        return groups

    def get_grouped_fhr_dict(self, fhrs: List[int], ngroups: int) -> dict:
        '''
        Prepare a metatask dictionary for forecast hour groups.

        Takes a list of forecast hours and splits it into a number of groups while not
        crossing forecast segment boundaries. Then use that to prepare a dict with key
        variable lists for use in a rocoto metatask.

        Parameters
        ----------
        fhrs: List[int]
              List of forecast hours to break into groups
        ngroups: int
                 Number of groups to split the forecast hours into

        Returns
        -------
        dict: Several variable lists for use in rocoto metatasks:
              fhr_list: list of comma-separated lists of fhr groups
              fhr_label: list of labels corresponding to the fhr range
              fhr3_last: list of the last fhr in each group, formatted to three digits
              fhr3_next: list of the fhr that would follow each group, formatted to
                         three digits
              seg_dep: list of segments each group belongs to
        '''
        fhr_breakpoints = self.options['fcst_segments'][1:-1]
        group_dicts = Tasks.get_job_groups(fhrs=fhrs, ngroups=ngroups, breakpoints=fhr_breakpoints)

        fhrs_group = [dct['fhrs'] for dct in group_dicts]
        fhrs_first = [grp[0] for grp in fhrs_group]
        fhrs_last = [grp[-1] for grp in fhrs_group]
        fhrs_next = fhrs_first[1:] + [fhrs_last[-1] + (fhrs[-1] - fhrs[-2])]
        grp_str = [f'f{grp[0]:03d}-f{grp[-1]:03d}' if len(grp) > 1 else f'f{grp[0]:03d}' for grp in fhrs_group]
        seg_deps = [f'seg{dct["seg"]}' for dct in group_dicts]

        fhr_var_dict = {'fhr_list': ' '.join(([','.join(str(fhr) for fhr in grp) for grp in fhrs_group])),
                        'fhr_label': ' '.join(grp_str),
                        'seg_dep': ' '.join(seg_deps),
                        'fhr3_last': ' '.join([f'{fhr:03d}' for fhr in fhrs_last]),
                        'fhr3_next': ' '.join([f'{fhr:03d}' for fhr in fhrs_next])
                        }

        return fhr_var_dict

    @staticmethod
    def multiply_HMS(hms_timedelta: str, multiplier: Union[int, float]) -> str:
        '''
        Multiplies an HMS timedelta string

        Parameters
        ----------
        hms_timedelta: str
                       String representing a time delta in HH:MM:SS format
        multiplier: int | float
                    Value to multiply the time delta by

        Returns
        -------
        str: String representing a time delta in HH:MM:SS format

        '''
        input_timedelta = to_timedelta(hms_timedelta)
        output_timedelta = input_timedelta * multiplier
        return timedelta_to_HMS(output_timedelta)

    def get_resource(self, task_name):
        """
        Given a task name (task_name) and its configuration (task_names),
        return a dictionary of resources (task_resource) used by the task.
        Task resource dictionary includes:
        account, walltime, ntasks, nodes, ppn, threads, memory, queue, partition, native
        """

        scheduler = self.app_config.scheduler

        task_config = self._configs[task_name]

        account = task_config['ACCOUNT']

        walltime = task_config[f'walltime']
        ntasks = task_config[f'ntasks']
        ppn = task_config[f'tasks_per_node']

        nodes = int(np.ceil(float(ntasks) / float(ppn)))

        threads = task_config[f'threads_per_task']

        # Memory is not required
        memory = task_config.get(f'memory', None)

        dtn_task = task_name in Tasks.DTN_TASKS
        service_task = task_name in Tasks.SERVICE_TASKS

        if task_name not in Tasks.VALID_TASKS:
            raise KeyError(f"ERROR {task_name} is not a valid tasks!")

        # Combine the task configuration with the system configuration
        if service_task:
            task_queue = self.queue_service if self.queue_service else self.queue_batch
            task_partition = self.partition_service if self.partition_service else self.partition_batch
            task_clusters = self.clusters_service if self.clusters_service else self.clusters_batch
            task_reservation = None  # Reservations are only for batch nodes
        elif dtn_task:
            # First check if there is a DTN queue, partition, or clusters
            # If not, then try SERVICE queue, partition, clusters
            if self.queue_dtn:
                task_queue = self.queue_dtn
            elif self.queue_service:
                task_queue = self.queue_service
            else:
                task_queue = self.queue_batch

            if self.partition_dtn:
                task_partition = self.partition_dtn
            elif self.partition_service:
                task_partition = self.partition_service
            else:
                task_partition = self.partition_batch

            if self.clusters_dtn:
                task_clusters = self.clusters_dtn
            elif self.clusters_service:
                task_clusters = self.clusters_service
            else:
                task_clusters = self.clusters_batch

            task_reservation = None

        else:  # This is a batch task
            task_partition = self.partition_batch
            task_queue = self.queue_batch
            task_clusters = self.clusters_batch
            task_reservation = self.reservation_batch

        # Scheduler-specific configurations
        native = None
        if scheduler in ['pbspro']:

            # Check memory usage at the end
            if task_config.get('prepost', False):
                memory += ':prepost=true'

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
            if task_config.get('is_exclusive', False):
                native = '--exclusive'
            else:
                native = '--export=NONE'

            if task_reservation:
                native += ' --reservation=' + task_reservation

            if task_clusters:
                native += ' --clusters=' + task_clusters

        # Finally, construct and return the task resource dictionary
        task_resource = {'account': account,
                         'walltime': walltime,
                         'nodes': nodes,
                         'ntasks': ntasks,
                         'ppn': ppn,
                         'threads': threads,
                         'memory': memory,
                         'native': native,
                         'queue': task_queue,
                         'partition': task_partition}

        return task_resource

    def get_task(self, task_name, *args, **kwargs):
        """
        Given a task_name, call the method for that task
        """
        try:
            return getattr(self, task_name, *args, **kwargs)()
        except AttributeError:
            raise AttributeError(f'"{task_name}" is not a valid task.\n'
                                 f'Valid tasks are:\n'
                                 f'{", ".join(Tasks.VALID_TASKS)}')
