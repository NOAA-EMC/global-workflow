#!/usr/bin/env python

'''
    Module containing functions all workflow setups require
'''
import random
import re
import os, sys, stat
import socket
import glob
import subprocess
import numpy as np
from distutils.spawn import find_executable
from datetime import datetime, timedelta
import rocoto

DATE_ENV_VARS=['CDATE','SDATE','EDATE']
SCHEDULER_MAP={'HERA':'slurm',
               'ORION':'slurm',
               'WCOSS':'lsf',
               'WCOSS_DELL_P3':'lsf',
               'WCOSS_C':'lsfcray',
               'WCOSS2':'pbspro'}

class UnknownMachineError(Exception): pass
class UnknownConfigError(Exception): pass
class ShellScriptException(Exception):
    def __init__(self,scripts,errors):
        self.scripts = scripts
        self.errors = errors
        super(ShellScriptException,self).__init__(
            str(errors)+
            ': error processing'+
            (' '.join(scripts)))

def get_shell_env(scripts):
    vars=dict()
    runme=''.join([ f'source {s} ; ' for s in scripts ])
    magic=f'--- ENVIRONMENT BEGIN {random.randint(0,64**5)} ---'
    runme+=f'/bin/echo -n "{magic}" ; /usr/bin/env -0'
    with open('/dev/null','w') as null:
        env=subprocess.Popen(runme,shell=True,stdin=null.fileno(),
                       stdout=subprocess.PIPE)
        (out,err)=env.communicate()
    out = out.decode()
    begin=out.find(magic)
    if begin<0:
        raise ShellScriptException(scripts,'Cannot find magic string; '
                                   'at least one script failed: '+repr(out))
    for entry in out[begin+len(magic):].split('\x00'):
        iequal=entry.find('=')
        vars[entry[0:iequal]] = entry[iequal+1:]
    return vars

def get_script_env(scripts):
    default_env=get_shell_env([])
    and_script_env=get_shell_env(scripts)
    vars_just_in_script=set(and_script_env)-set(default_env)
    union_env=dict(default_env)
    union_env.update(and_script_env)
    return dict([ (v,union_env[v]) for v in vars_just_in_script ])

def cast_or_not(type,value):
    try:
        return type(value)
    except ValueError:
        return value

def get_configs(expdir):
    """
        Given an experiment directory containing config files,
        return a list of configs minus the ones ending with ".default"
    """
    result=list()
    for config in glob.glob(f'{expdir}/config.*'):
        if not config.endswith('.default'):
            result.append(config)
    return result

def find_config(config_name, configs):

    for config in configs:
        if config_name == os.path.basename(config):
            return config

    raise UnknownConfigError(f'{config_name} does not exist (known: {repr(config_name)}), ABORT!')

def source_configs(configs, tasks):
    '''
        Given list of config files, source them
        and return a dictionary for each task
        Every task depends on config.base
    '''

    dict_configs = {}

    # Return config.base as well
    dict_configs['base'] = config_parser([find_config('config.base', configs)])

    # Source the list of input tasks
    for task in tasks:

        files = []

        files.append(find_config('config.base', configs))

        if task in ['eobs', 'eomg']:
            files.append(find_config('config.anal', configs))
            files.append(find_config('config.eobs', configs))
        elif task in ['eupd']:
            files.append(find_config('config.anal', configs))
            files.append(find_config('config.eupd', configs))
        elif task in ['efcs']:
            files.append(find_config('config.fcst', configs))
            files.append(find_config('config.efcs', configs))
        else:
            files.append(find_config(f'config.{task}', configs))

        print(f'sourcing config.{task}')
        dict_configs[task] = config_parser(files)

    return dict_configs


def config_parser(files):
    """
    Given the name of config file, key-value pair of all variables in the config file is returned as a dictionary
    :param files: config file or list of config files
    :type files: list or str or unicode
    :return: Key value pairs representing the environment variables defined
            in the script.
    :rtype: dict
    """
    if isinstance(files,(str, bytes)):
        files=[files]
    varbles=dict()
    for key,value in get_script_env(files).items():
        if key in DATE_ENV_VARS: # likely a date, convert to datetime
            varbles[key] = datetime.strptime(value,'%Y%m%d%H')
        elif '.' in value: # Likely a number and that too a float
            varbles[key] = cast_or_not(float,value)
        else: # Still could be a number, may be an integer
            varbles[key] = cast_or_not(int,value)

    return varbles

def detectMachine():

    machines = ['HERA', 'ORION', 'WCOSS_C', 'WCOSS_DELL_P3', 'WCOSS2']

    if os.path.exists('/scratch1/NCEPDEV'):
        return 'HERA'
    elif os.path.exists('/work/noaa'):
        return 'ORION'
    elif os.path.exists('/gpfs') and os.path.exists('/etc/SuSE-release'):
        return 'WCOSS_C'
    elif os.path.exists('/gpfs/dell2'):
        return 'WCOSS_DELL_P3'
    elif os.path.exists('/lfs/h2'):
        return 'WCOSS2'
    else:
        print(f'workflow is currently only supported on: {machines}')
        raise NotImplementedError('Cannot auto-detect platform, ABORT!')

def get_scheduler(machine):
    try:
        return SCHEDULER_MAP[machine]
    except KeyError:
        raise UnknownMachineError(f'Unknown machine: {machine}, ABORT!')

def create_wf_task(task, cdump='gdas', cycledef=None, envar=None, dependency=None, \
                   metatask=None, varname=None, varval=None, vardict=None, \
                   final=False):

    if metatask is None:
        taskstr = f'{task}'
    else:
        taskstr = f'{task}#{varname}#'
        metataskstr = f'{cdump}{metatask}'
        metatask_dict = {'metataskname': metataskstr, \
                         'varname': f'{varname}', \
                         'varval': f'{varval}', \
                         'vardict': vardict}

    taskstr = f'{cdump}{taskstr}'
    cycledefstr = cdump if cycledef is None else cycledef

    task_dict = {'taskname': f'{taskstr}', \
                 'cycledef': f'{cycledefstr}', \
                 'maxtries': '&MAXTRIES;', \
                 'command': f'&JOBS_DIR;/{task}.sh', \
                 'jobname': f'&PSLOT;_{taskstr}_@H', \
                 'account': '&ACCOUNT;', \
                 'queue': f'&QUEUE_{task.upper()}_{cdump.upper()};', \
                 'walltime': f'&WALLTIME_{task.upper()}_{cdump.upper()};', \
                 'native': f'&NATIVE_{task.upper()}_{cdump.upper()};', \
                 'memory': f'&MEMORY_{task.upper()}_{cdump.upper()};', \
                 'resources': f'&RESOURCES_{task.upper()}_{cdump.upper()};', \
                 'log': f'&ROTDIR;/logs/@Y@m@d@H/{taskstr}.log', \
                 'envar': envar, \
                 'dependency': dependency, \
                 'final': final}

    # Add PARTITION_BATCH to all non-service jobs on Orion (SLURM)
    if get_scheduler(detectMachine()) in ['slurm'] and detectMachine() in ['ORION']:
        task_dict['partition'] = '&PARTITION_BATCH;'
    # Add PARTITION_SERVICE to all service jobs (SLURM)
    if get_scheduler(detectMachine()) in ['slurm'] and task in ['getic','arch','earc']:
        task_dict['partition'] = f'&PARTITION_{task.upper()}_{cdump.upper()};'

    if metatask is None:
        task = rocoto.create_task(task_dict)
    else:
        task = rocoto.create_metatask(task_dict, metatask_dict)
    task = ''.join(task)

    return task


def create_firstcyc_task(cdump='gdas'):
    '''
    This task is needed to run to finalize the first half cycle
    '''

    task = 'firstcyc'
    taskstr = f'{task}'

    deps = []
    data = '&EXPDIR;/logs/@Y@m@d@H.log'
    dep_dict = {'type':'data', 'data':data, 'offset':'24:00:00'}
    deps.append(rocoto.add_dependency(dep_dict))
    dep_dict = {'type':'cycleexist', 'condition':'not', 'offset':'-06:00:00'}
    deps.append(rocoto.add_dependency(dep_dict))
    dependencies = rocoto.create_dependency(dep_condition='and', dep=deps)

    task_dict = {'taskname': f'{taskstr}' , \
                 'cycledef': 'first', \
                 'maxtries': '&MAXTRIES;', \
                 'final' : True, \
                 'command': 'sleep 1', \
                 'jobname': f'&PSLOT;_{taskstr}_@H', \
                 'account': '&ACCOUNT;', \
                 'queue': '&QUEUE_SERVICE;', \
                 'walltime': f'&WALLTIME_ARCH_{cdump.upper()};', \
                 'native': f'&NATIVE_ARCH_{cdump.upper()};', \
                 'resources': f'&RESOURCES_ARCH_{cdump.upper()};', \
                 'log': f'&ROTDIR;/logs/@Y@m@d@H/{taskstr}.log', \
                 'dependency': dependencies}

    if get_scheduler(detectMachine()) in ['slurm']:
        task_dict['queue'] = '&QUEUE;'
        task_dict['partition'] = '&PARTITION_SERVICE;'

    task = rocoto.create_task(task_dict)

    return ''.join(task)


def get_gfs_interval(gfs_cyc):
    '''
        return interval in hours based on gfs_cyc
    '''

    # Get interval from cyc_input
    if gfs_cyc == 0:
        interval = None
    if gfs_cyc == 1:
        interval = '24:00:00'
    elif gfs_cyc == 2:
        interval = '12:00:00'
    elif gfs_cyc == 4:
        interval = '06:00:00'

    return interval


def get_resources(machine, cfg, task, reservation, cdump='gdas'):

    scheduler = get_scheduler(machine)

    if cdump in ['gfs'] and f'wtime_{task}_gfs' in cfg.keys():
        wtimestr = cfg[f'wtime_{task}_gfs']
    else:
        wtimestr = cfg[f'wtime_{task}']

    ltask = 'eobs' if task in ['eomg'] else task

    if cdump in ['gfs'] and f'memory_{task}_gfs' in cfg.keys():
        memory = cfg.get(f'memory_{ltask}_gfs', None)
    else:
        memory = cfg.get(f'memory_{ltask}', None)

    if cdump in ['gfs'] and f'npe_{task}_gfs' in cfg.keys():
        tasks = cfg[f'npe_{ltask}_gfs']
    else:
        tasks = cfg[f'npe_{ltask}']

    if cdump in ['gfs'] and f'npe_node_{task}_gfs' in cfg.keys():
        ppn = cfg[f'npe_node_{ltask}_gfs']
    else:
        ppn = cfg[f'npe_node_{ltask}']

    if machine in [ 'WCOSS2', 'WCOSS_DELL_P3', 'HERA', 'ORION' ]:
        if cdump in ['gfs'] and f'nth_{task}_gfs' in cfg.keys():
            threads = cfg[f'nth_{ltask}_gfs']
        else:
            threads = cfg[f'nth_{ltask}']

    nodes = np.int(np.ceil(np.float(tasks) / np.float(ppn)))

    memstr = '' if memory is None else str(memory)
    natstr = ''

    if scheduler in ['slurm']:
        natstr = '--export=NONE'

    if machine in ['HERA', 'ORION', 'WCOSS_C', 'WCOSS_DELL_P3', 'WCOSS2' ]:

        if machine in ['HERA', 'ORION', 'WCOSS2']:
            resstr = f'<nodes>{nodes}:ppn={ppn}:tpp={threads}</nodes>'
        else:
            resstr = f'<nodes>{nodes}:ppn={ppn}</nodes>'

        if machine in ['WCOSS_C'] and task in ['arch', 'earc', 'getic']:
            resstr += '<shared></shared>'

        if machine in ['WCOSS_DELL_P3']:
            if not reservation in ['NONE']:
               natstr = f"-U {reservation} -R 'affinity[core({threads})]'"
            else:
               natstr = f"-R 'affinity[core({threads})]'"

            if task in ['arch', 'earc', 'getic']:
                  natstr = "-R 'affinity[core(1)]'"

        if machine in ['WCOSS2']:
            natstr = "-l debug=true"

            # Add place settings
            if task in ['arch', 'earc', 'getic']: # dev_transfer shared only
               natstr += ",place=shared"
            else:
               natstr += ",place=vscatter"
               # Set either exclusive or shared - default on WCOSS2 is exclusive when not set
               if memory is None:
                  natstr += ":exclhost"
               else:
                  natstr += ":shared"

    elif machine in ['WCOSS']:
        resstr = f'<cores>{tasks}</cores>'

    if task in ['arch', 'earc', 'getic']:
        queuestr = '&QUEUE;' if scheduler in ['slurm'] else '&QUEUE_SERVICE;'
    else:
        queuestr = '&QUEUE;'

    return wtimestr, resstr, queuestr, memstr, natstr


def create_crontab(base, cronint=5):
    '''
        Create crontab to execute rocotorun every cronint (5) minutes
    '''

    # No point creating a crontab if rocotorun is not available.
    rocotoruncmd = find_executable('rocotorun')
    if rocotoruncmd is None:
        print('Failed to find rocotorun, crontab will not be created')
        return

# Leaving the code for a wrapper around crontab file if needed again later
#    if check_slurm():
#
#        cronintstr = '*/%d * * * *' % cronint
#        rocotorunstr = '%s -d %s/%s.db -w %s/%s.xml' % (rocotoruncmd, base['EXPDIR'], base['PSLOT'], base['EXPDIR'], base['PSLOT'])
#
#        wrapper_strings = []
#        wrapper_strings.append('#!/bin/env tcsh\n')
#        wrapper_strings.append('\n')
#        wrapper_strings.append('module load slurm\n')
#        wrapper_strings.append('module load rocoto/1.3.0-RC4\n')
#        wrapper_strings.append('\n')
#        wrapper_strings.append(rocotorunstr)
#
#        hostname = 'tfe02'
#        script_file = os.path.join(base['EXPDIR'], '%s.sh' % base['PSLOT'])
#
#        fh = open(script_file, 'w')
#        fh.write(''.join(wrapper_strings))
#        os.chmod(script_file,stat.S_IRWXU|stat.S_IRWXG|stat.S_IRWXO)
#        fh.close()
#
#        rocotorunstr = 'ssh %s %s/%s.sh' % (socket.gethostname(), base['EXPDIR'], base['PSLOT'])
#
#    else:

    rocotorunstr = f'''{rocotoruncmd} -d {base['EXPDIR']}/{base['PSLOT']}.db -w {base['EXPDIR']}/{base['PSLOT']}.xml'''
    cronintstr = f'*/{cronint} * * * *'

    # On WCOSS, rocoto module needs to be loaded everytime cron runs
    if base['machine'] in ['WCOSS']:
        rocotoloadstr = '. /usrx/local/Modules/default/init/sh; module use -a /usrx/local/emc_rocoto/modulefiles; module load rocoto/1.3.0rc2)'
        rocotorunstr = f'({rocotoloadstr} {rocotorunstr})'

    try:
        REPLYTO = os.environ['REPLYTO']
    except:
        REPLYTO = ''

    strings = []

    strings.append('\n')
    strings.append(f'''#################### {base['PSLOT']} ####################\n''')
    strings.append(f'MAILTO="{REPLYTO}"\n')
    strings.append(f'{cronintstr} {rocotorunstr}\n')
    strings.append('#################################################################\n')
    strings.append('\n')

    fh = open(os.path.join(base['EXPDIR'], f'''{base['PSLOT']}.crontab'''), 'w')
    fh.write(''.join(strings))
    fh.close()

    return
