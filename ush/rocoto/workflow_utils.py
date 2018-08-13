#!/usr/bin/env python

###############################################################
# < next few lines under version control, D O  N O T  E D I T >
# $Date$
# $Revision$
# $Author$
# $Id$
###############################################################
'''
    Module containing functions all workflow setups require
'''
import random
import os
import sys
import glob
import subprocess
import numpy as np
from distutils.spawn import find_executable
from datetime import datetime, timedelta
import rocoto

DATE_ENV_VARS=['CDATE','SDATE','EDATE']
SCHEDULER_MAP={'ZEUS':'moabtorque',
               'THEIA':'moabtorque',
               'WCOSS':'lsf',
               'WCOSS_DELL_P3':'lsf',
               'WCOSS_C':'lsfcray'}

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
    runme=''.join([ 'source %s ; '%(s,) for s in scripts ])
    magic='--- ENVIRONMENT BEGIN %d ---'%random.randint(0,64**5)
    runme+='/bin/echo -n "%s" ; /usr/bin/env -0'%(magic,)
    with open('/dev/null','wb+') as null:
        env=subprocess.Popen(runme,shell=True,stdin=null.fileno(),
                       stdout=subprocess.PIPE)
        (out,err)=env.communicate()
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
    for config in glob.glob('%s/config.*' % expdir):
        if not config.endswith('.default'):
            result.append(config)
    return result

def find_config(config_name, configs):

    for config in configs:
        if config_name == os.path.basename(config):
            return config

    raise UnknownConfigError("%s does not exist (known: %s), ABORT!" % (
        config_name,repr(config_name)))

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
            files.append(find_config('config.%s' % task, configs))

        print 'sourcing config.%s' % task
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
    if isinstance(files,basestring):
        files=[files]
    varbles=dict()
    for key,value in get_script_env(files).iteritems():
        if key in DATE_ENV_VARS: # likely a date, convert to datetime
            varbles[key] = datetime.strptime(value,'%Y%m%d%H')
        elif '.' in value: # Likely a number and that too a float
            varbles[key] = cast_or_not(float,value)
        else: # Still could be a number, may be an integer
            varbles[key] = cast_or_not(int,value)

    return varbles

def get_scheduler(machine):
    """Determine the scheduler"""
    try:
        return SCHEDULER_MAP[machine]
    except KeyError:
        raise UnknownMachineError('Unknown machine: %s'%(machine,))


def create_wf_task(task, cdump='gdas', cycledef=None, envar=None, dependency=None, \
                   metatask=None, varname=None, varval=None, vardict=None, \
                   final=False):

    if metatask is None:
        taskstr = '%s' % task
    else:
        taskstr = '%s#%s#' % (task, varname)
        metataskstr = '%s%s' % (cdump, metatask)
        metatask_dict = {'metataskname': metataskstr, \
                         'varname': '%s' % varname, \
                         'varval': '%s' % varval, \
                         'vardict': vardict}

    taskstr = '%s%s' % (cdump, taskstr)
    cycledefstr = cdump if cycledef is None else cycledef

    task_dict = {'taskname': '%s' % taskstr, \
                 'cycledef': '%s' % cycledefstr, \
                 'maxtries': '&MAXTRIES;', \
                 'command': '&JOBS_DIR;/%s.sh' % task, \
                 'jobname': '&PSLOT;_%s_@H' % taskstr, \
                 'account': '&ACCOUNT;', \
                 'queue': '&QUEUE_%s_%s;' % (task.upper(), cdump.upper()), \
                 'walltime': '&WALLTIME_%s_%s;' % (task.upper(), cdump.upper()), \
                 'native': '&NATIVE_%s_%s;' % (task.upper(), cdump.upper()), \
                 'memory': '&MEMORY_%s_%s;' % (task.upper(), cdump.upper()), \
                 'resources': '&RESOURCES_%s_%s;' % (task.upper(), cdump.upper()), \
                 'log': '&ROTDIR;/logs/@Y@m@d@H/%s.log' % taskstr, \
                 'envar': envar, \
                 'dependency': dependency, \
                 'final': final}

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
    taskstr = '%s' % task

    deps = []
    data = '&EXPDIR;/logs/@Y@m@d@H.log'
    dep_dict = {'type':'data', 'data':data, 'offset':'24:00:00'}
    deps.append(rocoto.add_dependency(dep_dict))
    dep_dict = {'type':'cycleexist', 'condition':'not', 'offset':'-06:00:00'}
    deps.append(rocoto.add_dependency(dep_dict))
    dependencies = rocoto.create_dependency(dep_condition='and', dep=deps)

    task_dict = {'taskname': '%s' % taskstr, \
                 'cycledef': 'first', \
                 'maxtries': '&MAXTRIES;', \
                 'final' : True, \
                 'command': 'sleep 1', \
                 'jobname': '&PSLOT;_%s_@H' % taskstr, \
                 'account': '&ACCOUNT;', \
                 'queue': '&QUEUE_ARCH;', \
                 'walltime': '&WALLTIME_ARCH_%s;' % cdump.upper(), \
                 'native': '&NATIVE_ARCH_%s;' % cdump.upper(), \
                 'resources': '&RESOURCES_ARCH_%s;' % cdump.upper(), \
                 'log': '&ROTDIR;/logs/@Y@m@d@H/%s.log' % taskstr, \
                 'dependency': dependencies}

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


def get_resources(machine, cfg, task, cdump='gdas'):

    if cdump in ['gfs'] and 'wtime_%s_gfs' % task in cfg.keys():
        wtimestr = cfg['wtime_%s_gfs' % task]
    else:
        wtimestr = cfg['wtime_%s' % task]

    ltask = 'eobs' if task in ['eomg'] else task

    memory = cfg.get('memory_%s' % ltask, None)

    if cdump in ['gfs'] and 'npe_%s_gfs' % task in cfg.keys():
        tasks = cfg['npe_%s_gfs' % ltask]
    else:
        tasks = cfg['npe_%s' % ltask]

    if cdump in ['gfs'] and 'npe_node_%s_gfs' % task in cfg.keys():
        ppn = cfg['npe_node_%s_gfs' % ltask]
    else:
        ppn = cfg['npe_node_%s' % ltask]

    if machine in [ 'WCOSS_DELL_P3']:
        threads = cfg['nth_%s' % ltask]

    nodes = np.int(np.ceil(np.float(tasks) / np.float(ppn)))

    memstr = '' if memory is None else str(memory)
    natstr = ''

    if machine in ['ZEUS', 'THEIA', 'WCOSS_C', 'WCOSS_DELL_P3']:
        resstr = '<nodes>%d:ppn=%d</nodes>' % (nodes, ppn)

        if machine in ['WCOSS_C'] and task in ['arch', 'earc', 'getic']:
            resstr += '<shared></shared>'

        if machine in ['WCOSS_DELL_P3']:
            natstr = "-R 'affinity[core(%d)]'" % (threads)

            if task in ['arch', 'earc', 'getic']:
                 natstr = "-R 'affinity[core(1)]'"

    elif machine in ['WCOSS']:
        resstr = '<cores>%d</cores>' % tasks

    queuestr = '&QUEUE_ARCH;' if task in ['arch', 'earc', 'getic'] else '&QUEUE;'

    return wtimestr, resstr, queuestr, memstr, natstr


def create_crontab(base, cronint=5):
    '''
        Create crontab to execute rocotorun every cronint (5) minutes
    '''

    # No point creating a crontab if rocotorun is not available.
    rocotoruncmd = find_executable('rocotorun')
    if rocotoruncmd is None:
        print 'Failed to find rocotorun, crontab will not be created'
        return

    cronintstr = '*/%d * * * *' % cronint
    rocotorunstr = '%s -d %s/%s.db -w %s/%s.xml' % (rocotoruncmd, base['EXPDIR'], base['PSLOT'], base['EXPDIR'], base['PSLOT'])

    # On WCOSS, rocoto module needs to be loaded everytime cron runs
    if base['machine'] in ['WCOSS']:
        rocotoloadstr = '. /usrx/local/Modules/default/init/sh; module use -a /usrx/local/emc_rocoto/modulefiles; module load rocoto/20170119-master)'
        rocotorunstr = '(%s %s)' % (rocotoloadstr, rocotorunstr)

    try:
        REPLYTO = os.environ['REPLYTO']
    except:
        REPLYTO = ''

    strings = []

    strings.append('\n')
    strings.append('#################### %s ####################\n' % base['PSLOT'])
    strings.append('MAILTO="%s"\n' % REPLYTO)
    strings.append('%s %s\n' % (cronintstr, rocotorunstr))
    strings.append('#################################################################\n')
    strings.append('\n')

    fh = open(os.path.join(base['EXPDIR'], '%s.crontab' % base['PSLOT']), 'w')
    fh.write(''.join(strings))
    fh.close()

    return
