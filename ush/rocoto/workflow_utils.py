#!/usr/bin/env python

"""
    Module containing functions all workflow setups require
"""
import os
import numpy as np
from distutils.spawn import find_executable
from datetime import timedelta
from typing import Dict, Any
from rocoto import create_task, create_metatask
from hosts import Host

#SERVICE_TASKS = ['arch', 'earc', 'getic']


def create_wf_task(task, cdump='gdas', cycledef=None, envar=None, dependency=None,
                   metatask=None, varname=None, varval=None, vardict=None,
                   final=False):

    taskstr = f'{cdump}{task}'
    metatask_dict = None
    if metatask is not None:
        taskstr = f'{taskstr}#{varname}#'
        metatask_dict = {'metataskname': f'{cdump}{metatask}',
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


def check_expdir(cmd_expdir, cfg_expdir):

    if not os.path.samefile(cmd_expdir, cfg_expdir):
        print('MISMATCH in experiment directories!')
        print(f'config.base: EXPDIR = {cfg_expdir}')
        print(f'input arg:     --expdir = {cmd_expdir}')
        raise ValueError('Abort!')


def get_gfs_interval(gfs_cyc: int) -> str:
    """
    return interval in hours based on gfs_cyc
    """

    gfs_internal_map = {'0': None, '1': '24:00:00', '2': '12:00:00', '4': '06:00:00'}

    try:
        return gfs_internal_map[str(gfs_cyc)]
    except KeyError:
        raise KeyError(f'Invalid gfs_cyc = {gfs_cyc}')


def get_gfs_cyc_dates(base: Dict[str, Any]) -> Dict[str, Any]:
    """
    Generate GFS dates from experiment dates and gfs_cyc choice
    """

    base_out = base.copy()

    gfs_cyc = base['gfs_cyc']
    sdate = base['SDATE']
    edate = base['EDATE']

    interval_gfs = get_gfs_interval(gfs_cyc)

    # Set GFS cycling dates
    hrinc = 0
    hrdet = 0
    if gfs_cyc == 0:
        return base_out
    elif gfs_cyc == 1:
        hrinc = 24 - sdate.hour
        hrdet = edate.hour
    elif gfs_cyc == 2:
        if sdate.hour in [0, 12]:
            hrinc = 12
        elif sdate.hour in [6, 18]:
            hrinc = 6
        if edate.hour in [6, 18]:
            hrdet = 6
    elif gfs_cyc == 4:
        hrinc = 6
    sdate_gfs = sdate + timedelta(hours=hrinc)
    edate_gfs = edate - timedelta(hours=hrdet)
    if sdate_gfs > edate:
        print('W A R N I N G!')
        print('Starting date for GFS cycles is after Ending date of experiment')
        print(f'SDATE = {sdate.strftime("%Y%m%d%H")},     EDATE = {edate.strftime("%Y%m%d%H")}')
        print(f'SDATE_GFS = {sdate_gfs.strftime("%Y%m%d%H")}, EDATE_GFS = {edate_gfs.strftime("%Y%m%d%H")}')
        gfs_cyc = 0

    base_out['gfs_cyc'] = gfs_cyc
    base_out['SDATE_GFS'] = sdate_gfs
    base_out['EDATE_GFS'] = edate_gfs
    base_out['INTERVAL_GFS'] = interval_gfs

    fhmax_gfs = {}
    for hh in ['00', '06', '12', '18']:
        fhmax_gfs[hh] = base.get(f'FHMAX_GFS_{hh}', base.get('FHMAX_GFS_00', 120))
    base_out['FHMAX_GFS'] = fhmax_gfs

    return base_out


def get_resource(task_dict: dict, task_name: str, cdump: str = 'gdas') -> dict:
    """
    Given a task name (task_name) and its configuration (task_names),
    return a dictionary of resources (task_resource) used by the task.
    Task resource dictionary includes:
    account, walltime, cores, nodes, ppn, threads, memory, queue, partition, native
    """

    SERVICE_TASKS = ['arch', 'earc', 'getic']
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
        partition = task_dict['QUEUE_SERVICE'] if task_name in SERVICE_TASKS else task_dict['PARTITION_BATCH']

    task_resource = {'account': account,
                     'walltime': walltime,
                     'nodes': nodes,
                     'cores': cores,
                     'ppn': ppn,
                     'threads': threads,
                     'compute': compute,
                     'memory': memory,
                     'native': native,
                     'queue': queue,
                     'partition': partition}

    return task_resource


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
