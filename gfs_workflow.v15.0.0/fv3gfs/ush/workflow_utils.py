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

import os
import sys
import glob
from distutils.spawn import find_executable
from datetime import datetime, timedelta
import shellvars
import rocoto


def get_configs(expdir):
    '''
        Given an experiment directory containing config files,
        return a list of configs minus the ones ending with ".default"
    '''

    configs = glob.glob('%s/config.*' % expdir)

    # remove any defaults from the list
    for c, config in enumerate(configs):
        if config.endswith('.default'):
            configs.pop(c)

    return configs


def find_config(config_name, configs):

    for config in configs:
        if config_name == os.path.basename(config):
            return config

    # no match found
    raise IOError("%s does not exist, ABORT!" % config_name)


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
    sv = shellvars.ShellVars(files)
    varbles = sv.get_vars()
    for key,value in varbles.iteritems():
        if any(x in key for x in ['CDATE','SDATE','EDATE']): # likely a date, convert to datetime
            varbles[key] = datetime.strptime(value,'%Y%m%d%H')
            continue
        if '.' in value: # Likely a number and that too a float
            try:
                varbles[key] = float(value)
            except ValueError:
                varbles[key] = value
        else: # Still could be a number, may be an integer
            try:
                varbles[key] = int(value)
            except ValueError:
                varbles[key] = value

    return varbles


def get_scheduler(machine):
    '''
        Determine the scheduler
    '''

    if machine in ['ZEUS', 'THEIA']:
        return 'moabtorque'
    elif machine in ['WCOSS']:
        return 'lsf'
    elif machine in ['WCOSS_C']:
        return 'lsfcray'
    else:
        msg = 'Unknown machine: %s, ABORT!' % machine
        Exception.__init__(self, msg)


def create_wf_task(task, cdump='gdas', envar=None, dependency=None, \
                   metatask=None, varname=None, varval=None, final=False):

    if metatask is None:
        taskstr = '%s' % task
    else:
        taskstr = '%s#%s#' % (task, varname)
        metataskstr = '%s%s' % (cdump, metatask)
        metatask_dict = {'metataskname': metataskstr, \
                         'varname': '%s' % varname, \
                         'varval': '%s' % varval}

    taskstr = '%s%s' % (cdump, taskstr)

    task_dict = {'taskname': '%s' % taskstr, \
                 'cycledef': '%s' % cdump, \
                 'maxtries': '&MAXTRIES;', \
                 'command': '&JOBS_DIR;/%s.sh' % task, \
                 'jobname': '&PSLOT;_%s_@H' % taskstr, \
                 'account': '&ACCOUNT;', \
                 'queue': '&QUEUE_%s_%s;' % (task.upper(), cdump.upper()), \
                 'walltime': '&WALLTIME_%s_%s;' % (task.upper(), cdump.upper()), \
                 'native': '&NATIVE_%s_%s;' % (task.upper(), cdump.upper()), \
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
    tasks = cfg['npe_%s' % ltask]
    ppn = cfg['npe_node_%s' % ltask]

    nodes = tasks / ppn

    if machine in ['ZEUS', 'THEIA']:
        resstr = '<nodes>%d:ppn=%d</nodes>' % (nodes, ppn)

    elif machine in ['WCOSS_C']:
        resstr = '<nodes>%d:ppn=%d</nodes>' % (nodes, ppn)
        if task in ['arch', 'earc', 'getic']:
            resstr += '<shared></shared>'
        else:
            if memory is not None:
                resstr += '<memory>%s</memory>' % str(memory)

    elif machine in ['WCOSS']:
        resstr = '<cores>%d</cores>' % tasks

    queuestr = '&QUEUE_ARCH;' if task in ['arch', 'earc', 'getic'] else '&QUEUE;'

    return wtimestr, resstr, queuestr


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
