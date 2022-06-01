#!/usr/bin/env python

"""
    Module containing functions all workflow setups require
"""
import os
import numpy as np
from distutils.spawn import find_executable
from typing import List, Dict, Any
from rocoto import create_task, create_metatask
from hosts import Host
from configuration import Configuration

SERVICE_TASKS = ['arch', 'earc', 'getic']


def source_configs(config: Configuration, tasks) -> dict:
    """
        Given the configuration object and tasks,
        return a dictionary for each task
        Every task depends on "config.base"
    """

    dict_configs = dict()

    # Return config.base as well
    dict_configs['base'] = config.parse_config('config.base')

    # Source the list of input tasks
    for task in tasks:

        # All tasks must source config.base first
        files = ['config.base']

        if task in ['eobs', 'eomg']:
            files.append('config.anal')
            files.append('config.eobs')
        elif task in ['eupd']:
            files.append('config.anal')
            files.append('config.eupd')
        elif task in ['efcs']:
            files.append('config.fcst')
            files.append('config.efcs')
        elif 'wave' in task:
            files.append('config.wave')
            files.append(f'config.{task}')
        else:
            files.append(f'config.{task}')

        print(f'sourcing config.{task}')
        dict_configs[task] = config.parse_config(files)

    return dict_configs


def create_wf_task(task, cdump='gdas', cycledef=None, envar=None, dependency=None,
                   metatask=None, varname=None, varval=None, vardict=None,
                   final=False):
    if metatask is None:
        taskstr = f'{task}'
    else:
        taskstr = f'{task}#{varname}#'
        metataskstr = f'{cdump}{metatask}'
        metatask_dict = {'metataskname': metataskstr,
                         'varname': f'{varname}',
                         'varval': f'{varval}',
                         'vardict': vardict}

    taskstr = f'{cdump}{taskstr}'
    cycledefstr = cdump if cycledef is None else cycledef

    task_dict = {'taskname': f'{taskstr}',
                 'cycledef': f'{cycledefstr}',
                 'maxtries': '&MAXTRIES;',
                 'command': f'&JOBS_DIR;/{task}.sh',
                 'jobname': f'&PSLOT;_{taskstr}_@H',
                 'account': '&ACCOUNT;',
                 'queue': f'&QUEUE_{task.upper()}_{cdump.upper()};',
                 'walltime': f'&WALLTIME_{task.upper()}_{cdump.upper()};',
                 'native': f'&NATIVE_{task.upper()}_{cdump.upper()};',
                 'memory': f'&MEMORY_{task.upper()}_{cdump.upper()};',
                 'resources': f'&RESOURCES_{task.upper()}_{cdump.upper()};',
                 'log': f'&ROTDIR;/logs/@Y@m@d@H/{taskstr}.log',
                 'envars': envar,
                 'dependency': dependency,
                 'final': final}

    # Add partition for machines using slurm
    if Host.get_scheduler in ['slurm']:
        task_dict['partition'] = f'&PARTITION_{task.upper()}_{cdump.upper()};'

    if metatask is None:
        task = create_task(task_dict)
    else:
        task = create_metatask(task_dict, metatask_dict)
    task = ''.join(task)

    return task


def get_gfs_interval(gfs_cyc: int) -> str:
    """
    return interval in hours based on gfs_cyc
    """

    gfs_internal_map = {'0': None, '1': '24:00:00', '2': '12:00:00', '4': '06:00:00'}

    try:
        return gfs_internal_map[str(gfs_cyc)]
    except KeyError:
        raise KeyError(f'Invalid gfs_cyc = {gfs_cyc}')


def get_resource(task_dict: dict, task_name: str, cdump: str = 'gdas') -> dict:
    """
    Given a task name (task_name) and its configuration (task_dict),
    return a dictionary of resources (task_resource) used by the task.
    Task resource dictionary includes:
    account, walltime, cores, nodes, ppn, threads, memory, queue, partition, native
    """

    scheduler = Host.get_scheduler

    account = task_dict['ACCOUNT']

    walltime = task_dict[f'wtime_{task_name}']
    if cdump in ['gfs'] and f'wtime_{task_name}_gfs' in task_dict.keys():
        walltime = task_dict[f'wtime_{task_name}_gfs']

    cores = task_dict[f'npe_{task_name}']
    if cdump in ['gfs'] and f'npe_{task_name}_gfs' in task_dict.keys():
        cores = task_dict[f'npe_{task_name}_gfs']

    ppn = task_dict[f'npe_node_{task_name}']
    if cdump in ['gfs'] and f'npe_node_{task_name}_gfs' in task_dict.keys():
        ppn = task_dict[f'npe_node_{task_name}_gfs']

    nodes = np.int(np.ceil(np.float(cores) / np.float(ppn)))

    threads = task_dict[f'nth_{task_name}']
    if cdump in ['gfs'] and f'nth_{task_name}_gfs' in task_dict.keys():
        threads = task_dict[f'nth_{task_name}_gfs']

    compute = f'<nodes>{nodes}:ppn={ppn}:tpp={threads}</nodes>'  # TODO - remove dependence on compute

    memory = task_dict.get(f'memory_{task_name}', None)

    native = '--export=NONE' if scheduler in ['slurm'] else None

    queue = task_dict['QUEUE']
    if task_name in SERVICE_TASKS and scheduler not in ['slurm']:
        queue = task_dict['QUEUE_SERVICE']

    partition = None
    if scheduler in ['slurm']:
        partition = task_dict['PARTITION_SERVICE'] if task_name in SERVICE_TASKS else task_dict['PARTITION_BATCH']

    task_resource = dict()
    task_resource['account'] = account
    task_resource['walltime'] = walltime
    task_resource['nodes'] = nodes
    task_resource['cores'] = cores
    task_resource['ppn'] = ppn
    task_resource['threads'] = threads
    task_resource['compute'] = compute  # TODO - remove in the future
    task_resource['memory'] = memory
    task_resource['native'] = native
    task_resource['queue'] = queue
    task_resource['partition'] = partition

    return task_resource


def get_taskplan_resources(dict_configs: List[Dict[str, Any]], task_plan: List[str], cdump='gdas') -> Dict[str, Any]:
    """
    Given a list of dictionary containing tasks, and a task plan,
    return the resource dictionary for the tasks in the task plan
    """
    resources = dict()
    for task in task_plan:
        task_name = cdump + task
        resources[task_name] = get_resource(dict_configs[task], task, cdump=cdump)
    return resources


def create_crontab(base, cronint=5):
    """
    Create crontab to execute rocotorun every cronint (5) minutes
    """

    # No point creating a crontab if rocotorun is not available.
    rocotoruncmd = find_executable('rocotorun')
    if rocotoruncmd is None:
        print('Failed to find rocotorun, crontab will not be created')
        return

    rocotorunstr = f'''{rocotoruncmd} -d {base['EXPDIR']}/{base['PSLOT']}.db -w {base['EXPDIR']}/{base['PSLOT']}.xml'''
    cronintstr = f'*/{cronint} * * * *'

    try:
        replyto = os.environ['REPLYTO']
    except KeyError:
        replyto = ''

    strings = ['',
               f'#################### {base["PSLOT"]} ####################',
               f'MAILTO="{replyto}"',
               f'{cronintstr} {rocotorunstr}',
               '#################################################################',
               '']

    with open(os.path.join(base['EXPDIR'], f'{base["PSLOT"]}.crontab'), 'w') as fh:
        fh.write('\n'.join(strings))

    return
