#!/usr/bin/env python

###############################################################
# < next few lines under version control, D O  N O T  E D I T >
# $Date$
# $Revision$
# $Author$
# $Id$
###############################################################
'''
    PROGRAM:
        Create the ROCOTO workflow given the configuration of the GFS parallel

    AUTHOR:
        Rahul.Mahajan
        rahul.mahajan@noaa.gov

    FILE DEPENDENCIES:
        1. config files for the parallel; e.g. config.base, config.fcst[.gfs], etc.
        Without these dependencies, the script will fail

    OUTPUT:
        1. PSLOT.xml: XML workflow
        2. PSLOT.crontab: crontab for ROCOTO run command
'''

import os
import sys
import glob
from distutils.spawn import find_executable
from datetime import datetime, timedelta
from argparse import ArgumentParser, ArgumentDefaultsHelpFormatter
import shellvars
import rocoto

def main():
    parser = ArgumentParser(description='Setup XML workflow and CRONTAB for a GFS parallel.', formatter_class=ArgumentDefaultsHelpFormatter)
    parser.add_argument('--expdir',help='full path to experiment directory containing config files', type=str, required=False, default=os.environ['PWD'])
    args = parser.parse_args()

    # Source the configs in expdir once and return a dictionary
    dict_configs = source_configs(args.expdir)

    # First create workflow XML
    create_xml(dict_configs)

    # Next create the crontab
    create_crontab(dict_configs['base'])

    return


def source_configs(expdir):
    '''
        Given an experiment directory containing config files
        source the configs and return a dictionary for each task
    '''

    configs = glob.glob('%s/config.*' % expdir)

    dict_configs = {}

    # First read "config.base", gather basic info
    print 'sourcing config.base'
    dict_configs['base'] = config_parser(find_config('config.base', configs))
    base = dict_configs['base']

    if expdir != base['EXPDIR']:
        print 'MISMATCH in experiment directories!'
        print 'config.base: EXPDIR = %s' % base['EXPDIR']
        print 'input arg:     --expdir = %s' % expdir
        sys.exit(1)

    # GDAS/GFS tasks
    for task in ['prep', 'anal', 'fcst', 'post', 'vrfy', 'arch']:

        print 'sourcing config.%s' % task

        files = []
        files.append(find_config('config.base', configs))
        files.append(find_config('config.%s' % task, configs))

        dict_configs[task] = config_parser(files)

    # Hybrid tasks
    if dict_configs['base']['DOHYBVAR'] == 'YES':

        for task in ['eobs', 'eomg', 'eupd', 'ecen', 'efcs', 'epos', 'earc']:

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


def find_config(config_name,configs):
    if any(config_name in s for s in configs):
        config = filter(lambda x: config_name in x, configs)[0]
        return config
    else:
        raise IOError("config file does not exist: %s" % config_name)


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
        Exception.__init__(self,msg)


def get_gfs_cyc_dates(base):
    '''
        Generate GFS dates from experiment dates and gfs_cyc choice
    '''

    base_out = base.copy()

    gfs_cyc = base['gfs_cyc']
    sdate = base['SDATE']
    edate = base['EDATE']

    # Set GFS cycling dates
    hrdet = 0
    if gfs_cyc == 1:
        interval_gfs = '24:00:00'
        hrinc = 24 - sdate.hour
        hrdet = edate.hour
    elif gfs_cyc == 2:
        interval_gfs = '12:00:00'
        if sdate.hour in [0, 12]:
            hrinc = 12
        elif sdate.hour in [6, 18]:
            hrinc = 6
        if edate.hour in [6, 18]:
            hrdet = 6
    elif gfs_cyc == 4:
        interval_gfs = '06:00:00'
        hrinc = 6
    sdate_gfs = sdate + timedelta(hours=hrinc)
    edate_gfs = edate - timedelta(hours=hrdet)
    if sdate_gfs > edate:
        print 'W A R N I N G!'
        print 'Starting date for GFS cycles is after Ending date of experiment'
        print 'SDATE = %s,     EDATE = %s' % (sdate.strftime('%Y%m%d%H'), edate.strftime('%Y%m%d%H'))
        print 'SDATE_GFS = %s, EDATE_GFS = %s' % (sdate_gfs.strftime('%Y%m%d%H'), edate_gfs.strftime('%Y%m%d%H'))
        gfs_cyc = 0

    base_out['gfs_cyc'] = gfs_cyc
    base_out['SDATE_GFS'] = sdate_gfs
    base_out['EDATE_GFS'] = edate_gfs
    base_out['INTERVAL_GFS'] = interval_gfs

    return base_out


def get_preamble():
    '''
        Generate preamble for XML
    '''

    strings = []

    strings.append('<?xml version="1.0"?>\n')
    strings.append('<!DOCTYPE workflow\n')
    strings.append('[\n')
    strings.append('\t<!--\n')
    strings.append('\tPROGRAM\n')
    strings.append('\t\tMain workflow manager for cycling Global Forecast System\n')
    strings.append('\n')
    strings.append('\tAUTHOR:\n')
    strings.append('\t\tRahul Mahajan\n')
    strings.append('\t\trahul.mahajan@noaa.gov\n')
    strings.append('\n')
    strings.append('\tNOTES:\n')
    strings.append('\t\tThis workflow was automatically generated at %s\n' % datetime.now())
    strings.append('\t-->\n')

    return ''.join(strings)


def get_definitions(base):
    '''
        Create entities related to the experiment
    '''

    strings = []

    strings.append('\n')
    strings.append('\t<!-- User definitions -->\n')
    strings.append('\t<!ENTITY LOGNAME "%s">\n' % os.environ['USER'])
    strings.append('\n')
    strings.append('\t<!-- Experiment parameters such as name, starting, ending dates -->\n')
    strings.append('\t<!ENTITY PSLOT "%s">\n' % base['PSLOT'])
    strings.append('\t<!ENTITY SDATE "%s">\n' % base['SDATE'].strftime('%Y%m%d%H%M'))
    strings.append('\t<!ENTITY EDATE "%s">\n' % base['EDATE'].strftime('%Y%m%d%H%M'))
    strings.append('\n')
    strings.append('\t<!-- Experiment and Rotation directory -->\n')
    strings.append('\t<!ENTITY EXPDIR "%s">\n' % base['EXPDIR'])
    strings.append('\t<!ENTITY ROTDIR "%s">\n' % base['ROTDIR'])
    strings.append('\n')
    strings.append('\t<!-- Directories for driving the workflow -->\n')
    strings.append('\t<!ENTITY JOBS_DIR "%s/fv3gfs/jobs">\n' % base['BASE_WORKFLOW'])
    strings.append('\n')
    strings.append('\t<!-- Machine related entities -->\n')
    strings.append('\t<!ENTITY ACCOUNT    "%s">\n' % base['ACCOUNT'])
    strings.append('\t<!ENTITY QUEUE      "%s">\n' % base['QUEUE'])
    strings.append('\t<!ENTITY QUEUE_ARCH "%s">\n' % base['QUEUE_ARCH'])
    strings.append('\t<!ENTITY SCHEDULER  "%s">\n' % get_scheduler(base['machine']))
    strings.append('\n')
    strings.append('\t<!-- ROCOTO parameters that control workflow -->\n')
    strings.append('\t<!ENTITY CYCLETHROTTLE "3">\n')
    strings.append('\t<!ENTITY TASKTHROTTLE  "20">\n')
    strings.append('\t<!ENTITY MAXTRIES      "2">\n')
    strings.append('\n')

    return ''.join(strings)


def get_gfs_dates(base):
    '''
        Generate GFS dates entities
    '''

    strings = []

    strings.append('\n')
    strings.append('\t<!-- Starting and ending dates for GFS cycle -->\n')
    strings.append('\t<!ENTITY SDATE_GFS    "%s">\n' % base['SDATE_GFS'].strftime('%Y%m%d%H%M'))
    strings.append('\t<!ENTITY EDATE_GFS    "%s">\n' % base['EDATE_GFS'].strftime('%Y%m%d%H%M'))
    strings.append('\t<!ENTITY INTERVAL_GFS "%s">\n' % base['INTERVAL_GFS'])

    return ''.join(strings)


def get_gdasgfs_resources(dict_configs, cdump='gdas'):
    '''
        Create GDAS or GFS resource entities
    '''

    strings = []

    strings.append('\n')
    strings.append('\t<!-- BEGIN: Resource requirements for %s part of the workflow -->\n' % cdump.upper())
    strings.append('\n')

    base = dict_configs['base']

    tasks = ['prep', 'anal', 'fcst', 'post', 'vrfy', 'arch']
    for task in tasks:

        cfg = dict_configs[task]

        if cdump in ['gfs'] and 'wtime_%s_gfs' % task in cfg.keys():
            walltime = cfg['wtime_%s_gfs' % task]
        else:
            walltime = cfg['wtime_%s' % task]

        tasks = cfg['npe_%s' % task]
        ppn = cfg['npe_node_%s' % task]
        nodes = tasks / ppn
        memory = cfg['memory_%s' % task] if 'memory_%s' % task in cfg.keys() else None

        if base['machine'] in ['ZEUS', 'THEIA']:
            resstr = '<nodes>%d:ppn=%d</nodes>' % (nodes, ppn)

        elif base['machine'] in ['WCOSS_C']:
            resstr = '<nodes>%d:ppn=%d</nodes>' % (nodes, ppn)
            if task in ['arch']:
                resstr += '<shared></shared>'
            else:
                if memory is not None:
                    resstr += '<memory>%s</memory>' % str(memory)

        elif base['machine'] in ['WCOSS']:
            resstr = '<cores>%d</cores>' % (tasks)


        taskstr = '%s_GFS' % task.upper() if cdump == 'gfs' else '%s' % task.upper()
        queuestr = '&QUEUE_ARCH;' if task in ['arch'] else '&QUEUE;'

        strings.append('\t<!ENTITY QUEUE_%s     "%s">\n' % (taskstr, queuestr))
        strings.append('\t<!ENTITY WALLTIME_%s  "%s">\n' % (taskstr, walltime))
        strings.append('\t<!ENTITY RESOURCES_%s "%s">\n' % (taskstr, resstr))
        strings.append('\t<!ENTITY NATIVE_%s    "">\n'   % (taskstr))

        strings.append('\n')

    strings.append('\t<!-- END: Resource requirements for %s part of the workflow -->\n' % cdump.upper())

    return ''.join(strings)


def get_hyb_resources(dict_configs, cdump='gdas'):
    '''
        Create hybrid resource entities
    '''

    strings = []

    strings.append('\n')
    strings.append('\t<!-- BEGIN: Resource requirements for hybrid part of the workflow -->\n')
    strings.append('\n')

    base = dict_configs['base']

    hybrid_tasks = ['eobs', 'eomg', 'eupd', 'ecen', 'efcs', 'epos', 'earc']
    for task in hybrid_tasks:

        cfg = dict_configs['eobs'] if task in ['eomg'] else dict_configs[task]

        if task in ['eomg']:
            tasks = cfg['npe_eobs']
            ppn = cfg['npe_node_eobs']
            walltime = cfg['wtime_eomg']
            memory = cfg['memory_eobs'] if 'memory_%s' % task in cfg.keys() else None
        else:
            tasks = cfg['npe_%s' % task]
            ppn = cfg['npe_node_%s' % task]
            walltime = cfg['wtime_%s' % task]
            memory = cfg['memory_%s' % task] if 'memory_%s' % task in cfg.keys() else None

        nodes = tasks / ppn

        if base['machine'] in ['ZEUS', 'THEIA']:
            resstr = '<nodes>%d:ppn=%d</nodes>' % (nodes, ppn)

        elif base['machine'] in ['WCOSS_C']:
            resstr = '<nodes>%d:ppn=%d</nodes>' % (nodes, ppn)
            if task in ['earc']:
                resstr += '<shared></shared>'
            else:
                if memory is not None:
                    resstr += '<memory>%s</memory>' % str(memory)

        elif base['machine'] in ['WCOSS']:
            resstr = '<cores>%d</cores>' % (tasks)

        taskstr = task.upper()
        queuestr = '&QUEUE_ARCH;' if task in ['earc'] else '&QUEUE;'

        strings.append('\t<!ENTITY QUEUE_%s     "%s">\n' % (taskstr, queuestr))
        strings.append('\t<!ENTITY WALLTIME_%s  "%s">\n' % (taskstr, walltime))
        strings.append('\t<!ENTITY RESOURCES_%s "%s">\n' % (taskstr, resstr))
        strings.append('\t<!ENTITY NATIVE_%s    "">\n'   % (taskstr))

        strings.append('\n')

    strings.append('\t<!-- END: Resource requirements for hybrid part of the workflow-->\n')

    return ''.join(strings)


def create_wf_task(task, cdump='gdas', envar=None, dependency=None, \
                   metatask=None, varname=None, varval=None):

    if metatask is None:
        taskstr = '%s' % task
    else:
        taskstr = '%s#%s#' % (task, varname)
        metataskstr = '%s' % metatask
        metatask_dict = {'metataskname': metataskstr, \
                         'varname': '%s' % varname, \
                         'varval': '%s' % varval}

    if cdump in ['gfs']:
        cdumpstr = '_GFS'
        taskstr = 'gfs%s' % taskstr
    elif cdump in ['gdas']:
        cdumpstr = ''

    task_dict = {'taskname': '%s' % taskstr, \
                 'cycledef': '%s' % cdump, \
                 'maxtries': '&MAXTRIES;', \
                 'command': '&JOBS_DIR;/%s.sh' % task, \
                 'jobname': '&PSLOT;_%s_@H' % taskstr, \
                 'account': '&ACCOUNT;', \
                 'queue': '&QUEUE_%s%s;' % (task.upper(), cdumpstr), \
                 'walltime': '&WALLTIME_%s%s;' % (task.upper(), cdumpstr), \
                 'native': '&NATIVE_%s%s;' % (task.upper(), cdumpstr), \
                 'resources': '&RESOURCES_%s%s;' % (task.upper(), cdumpstr), \
                 'log': '&ROTDIR;/logs/@Y@m@d@H/%s.log' % taskstr, \
                 'envar': envar, \
                 'dependency': dependency}

    if metatask is None:
        task = rocoto.create_task(task_dict)
    else:
        task = rocoto.create_metatask(task_dict, metatask_dict)
    task = ''.join(task)

    return task


def create_firstcyc_task():
    '''
    This task is needed to run to finalize the first half cycle
    '''

    task = 'firstcyc'
    taskstr = '%s' % task

    deps = []
    data = '&EXPDIR;/logs/@Y@m@d@H.log'
    dep_dict = {'type':'data', 'data':data, 'offset':'24:00:00'}
    deps.append(rocoto.add_dependency(dep_dict))
    dep_dict = {'condition':'not', 'type':'cycleexist', 'offset':'-06:00:00'}
    deps.append(rocoto.add_dependency(dep_dict))
    dependencies = rocoto.create_dependency(dep_condition='and', dep=deps)

    task_dict = {'taskname': '%s' % taskstr, \
                 'cycledef': 'first', \
                 'maxtries': '&MAXTRIES;', \
                 'final' : 'true', \
                 'command': 'sleep 1', \
                 'jobname': '&PSLOT;_%s_@H' % taskstr, \
                 'account': '&ACCOUNT;', \
                 'queue': '&QUEUE_ARCH;', \
                 'walltime': '&WALLTIME_ARCH;', \
                 'native': '&NATIVE_ARCH;', \
                 'resources': '&RESOURCES_ARCH;', \
                 'log': '&ROTDIR;/logs/@Y@m@d@H/%s.log' % taskstr, \
                 'dependency': dependencies}

    task = rocoto.create_task(task_dict)

    return ''.join(task)


def get_gdasgfs_tasks(cdump='gdas', dohybvar='NO'):
    '''
        Create GDAS or GFS tasks
    '''

    envars = []
    envars.append(rocoto.create_envar(name='EXPDIR', value='&EXPDIR;'))
    envars.append(rocoto.create_envar(name='CDATE', value='<cyclestr>@Y@m@d@H</cyclestr>'))
    envars.append(rocoto.create_envar(name='CDUMP', value='%s' % cdump))

    tasks = []

    # prep
    taskname = 'prep'
    deps = []
    dep_dict = {'name':'post', 'type':'task', 'offset':'-06:00:00'}
    deps.append(rocoto.add_dependency(dep_dict))
    dependencies = rocoto.create_dependency(dep=deps)
    task = create_wf_task(taskname, cdump=cdump, envar=envars, dependency=dependencies)

    tasks.append(task)
    tasks.append('\n')

    # anal
    taskname = 'anal'
    deps = []
    if cdump in ['gdas']:
        dep_dict = {'name':'prep', 'type':'task'}
        deps.append(rocoto.add_dependency(dep_dict))
    elif cdump in ['gfs']:
        dep_dict = {'name':'gfsprep', 'type':'task'}
        deps.append(rocoto.add_dependency(dep_dict))
    if dohybvar in ['y', 'Y', 'yes', 'YES']:
        dep_dict = {'name':'epos', 'type':'task', 'offset':'-06:00:00'}
        deps.append(rocoto.add_dependency(dep_dict))
        dependencies = rocoto.create_dependency(dep_condition='and', dep=deps)
    else:
        dependencies = rocoto.create_dependency(dep=deps)
    task = create_wf_task(taskname, cdump=cdump, envar=envars, dependency=dependencies)

    tasks.append(task)
    tasks.append('\n')

    # fcst
    taskname = 'fcst'
    deps = []
    if cdump in ['gdas']:
        dep_dict = {'name':'anal', 'type':'task'}
        deps.append(rocoto.add_dependency(dep_dict))
        dep_dict = {'condition':'not', 'type':'cycleexist', 'offset':'-06:00:00'}
        deps.append(rocoto.add_dependency(dep_dict))
        dependencies = rocoto.create_dependency(dep_condition='or', dep=deps)
    elif cdump in ['gfs']:
        dep_dict = {'name':'gfsanal', 'type':'task'}
        deps.append(rocoto.add_dependency(dep_dict))
        dependencies = rocoto.create_dependency(dep=deps)
    task = create_wf_task(taskname, cdump=cdump, envar=envars, dependency=dependencies)

    tasks.append(task)
    tasks.append('\n')

    # post
    taskname = 'post'
    deps = []
    if cdump in ['gdas']:
        dep_dict = {'name':'fcst', 'type':'task'}
    elif cdump in ['gfs']:
        dep_dict = {'name':'gfsfcst', 'type':'task'}
    deps.append(rocoto.add_dependency(dep_dict))
    dependencies = rocoto.create_dependency(dep=deps)
    task = create_wf_task(taskname, cdump=cdump, envar=envars, dependency=dependencies)

    tasks.append(task)
    tasks.append('\n')

    # vrfy
    taskname = 'vrfy'
    deps = []
    if cdump in ['gdas']:
        dep_dict = {'name':'post', 'type':'task'}
    elif cdump in ['gfs']:
        dep_dict = {'name':'gfspost', 'type':'task'}
    deps.append(rocoto.add_dependency(dep_dict))
    dependencies = rocoto.create_dependency(dep=deps)
    task = create_wf_task(taskname, cdump=cdump, envar=envars, dependency=dependencies)

    tasks.append(task)
    tasks.append('\n')

    # arch
    taskname = 'arch'
    deps = []
    if cdump in ['gdas']:
        dep_dict = {'name':'vrfy', 'type':'task'}
    elif cdump in ['gfs']:
        dep_dict = {'name':'gfsvrfy', 'type':'task'}
    deps.append(rocoto.add_dependency(dep_dict))
    dependencies = rocoto.create_dependency(dep=deps)
    task = create_wf_task(taskname, cdump=cdump, envar=envars, dependency=dependencies)

    tasks.append(task)
    tasks.append('\n')

    return ''.join(tasks)


def get_hyb_tasks(EOMGGROUPS, EFCSGROUPS, cdump='gdas'):
    '''
        Create Hybrid tasks
    '''

    envars = []
    envars.append(rocoto.create_envar(name='EXPDIR', value='&EXPDIR;'))
    envars.append(rocoto.create_envar(name='CDATE', value='<cyclestr>@Y@m@d@H</cyclestr>'))
    envars.append(rocoto.create_envar(name='CDUMP', value='%s' % cdump))

    ensgrp = rocoto.create_envar(name='ENSGRP', value='#grp#')

    tasks = []

    # eobs
    taskname = 'eobs'
    deps = []
    dep_dict = {'name':'prep', 'type':'task'}
    deps.append(rocoto.add_dependency(dep_dict))
    dep_dict = {'name':'epos', 'type':'task', 'offset':'-06:00:00'}
    deps.append(rocoto.add_dependency(dep_dict))
    dependencies = rocoto.create_dependency(dep_condition='and', dep=deps)
    task = create_wf_task(taskname, cdump=cdump, envar=envars, dependency=dependencies)

    tasks.append(task)
    tasks.append('\n')

    # eomn, eomg
    metataskname = 'eomn'
    varname = 'grp'
    varval = EOMGGROUPS
    taskname = 'eomg'
    deps = []
    dep_dict = {'name':'eobs', 'type':'task'}
    deps.append(rocoto.add_dependency(dep_dict))
    dependencies = rocoto.create_dependency(dep=deps)
#    eomgenvars = envars + ['&eENSGRP;']
    eomgenvars = envars + [ensgrp]
    task = create_wf_task(taskname, cdump=cdump, envar=eomgenvars, dependency=dependencies, \
           metatask=metataskname, varname=varname, varval=varval)

    tasks.append(task)
    tasks.append('\n')

    # eupd
    taskname = 'eupd'
    deps = []
    dep_dict = {'name':'eomn', 'type':'metatask'}
    deps.append(rocoto.add_dependency(dep_dict))
    dependencies = rocoto.create_dependency(dep=deps)
    task = create_wf_task(taskname, cdump=cdump, envar=envars, dependency=dependencies)

    tasks.append(task)
    tasks.append('\n')

    # ecen
    taskname = 'ecen'
    deps = []
    dep_dict = {'name':'anal', 'type':'task'}
    deps.append(rocoto.add_dependency(dep_dict))
    dep_dict = {'name':'eupd', 'type':'task'}
    deps.append(rocoto.add_dependency(dep_dict))
    dependencies = rocoto.create_dependency(dep_condition='and', dep=deps)
    task = create_wf_task(taskname, cdump=cdump, envar=envars, dependency=dependencies)

    tasks.append(task)
    tasks.append('\n')

    # efmn
    metataskname = 'efmn'
    varname = 'grp'
    varval = EFCSGROUPS
    taskname = 'efcs'
    deps = []
    dep_dict = {'name':'ecen', 'type':'task'}
    deps.append(rocoto.add_dependency(dep_dict))
    dep_dict = {'condition':'not', 'type':'cycleexist', 'offset':'-06:00:00'}
    deps.append(rocoto.add_dependency(dep_dict))
    dependencies = rocoto.create_dependency(dep_condition='or', dep=deps)
#    efcsenvars = envars + ['&eENSGRP;']
    efcsenvars = envars + [ensgrp]
    task = create_wf_task(taskname, cdump=cdump, envar=efcsenvars, dependency=dependencies, \
           metatask=metataskname, varname=varname, varval=varval)

    tasks.append(task)
    tasks.append('\n')

    # epos
    taskname = 'epos'
    deps = []
    dep_dict = {'name':'efmn', 'type':'metatask'}
    deps.append(rocoto.add_dependency(dep_dict))
    dependencies = rocoto.create_dependency(dep=deps)
    task = create_wf_task(taskname, cdump=cdump, envar=envars, dependency=dependencies)

    tasks.append(task)
    tasks.append('\n')

    # earc
    taskname = 'earc'
    deps = []
    dep_dict = {'name':'epos', 'type':'task'}
    deps.append(rocoto.add_dependency(dep_dict))
    dependencies = rocoto.create_dependency(dep=deps)
    task = create_wf_task(taskname, cdump=cdump, envar=envars, dependency=dependencies)

    tasks.append(task)
    tasks.append('\n')

    return ''.join(tasks)


def get_workflow_header(base):
    '''
        Create the workflow header block
    '''

    strings = []

    strings.append('\n')
    strings.append(']>\n')
    strings.append('\n')
    strings.append('<workflow realtime="F" scheduler="&SCHEDULER;" cyclethrottle="&CYCLETHROTTLE;" taskthrottle="&TASKTHROTTLE;">\n')
    strings.append('\n')
    strings.append('\t<log verbosity="10"><cyclestr>&EXPDIR;/logs/@Y@m@d@H.log</cyclestr></log>\n')
    strings.append('\n')
    strings.append('\t<!-- Define the cycles -->\n')
    strings.append('\t<cycledef group="first">&SDATE;     &SDATE;     06:00:00</cycledef>\n')
    strings.append('\t<cycledef group="gdas" >&SDATE;     &EDATE;     06:00:00</cycledef>\n')
    if base['gfs_cyc'] != 0:
        strings.append('\t<cycledef group="gfs"  >&SDATE_GFS; &EDATE_GFS; &INTERVAL_GFS;</cycledef>\n')

    strings.append('\n')

    return ''.join(strings)


def get_workflow_footer():
    '''
        Generate workflow footer
    '''

    strings = []
    strings.append('\n</workflow>\n')

    return ''.join(strings)


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

    strings = []

    strings.append('# This is a basic crontab file and will execute rocotorun every 5 minutes\n')
    strings.append('# Usage: crontab %s.crontab\n' % base['PSLOT'])
    strings.append('#   list all crontabs:      crontab -l\n')
    strings.append('#   remove current crontab: crontab -r %s.crontab\n' % base['PSLOT'])
    strings.append('\n')

    try:
        REPLYTO = os.environ['REPLYTO']
    except:
        REPLYTO = ''
    strings.append('MAILTO="%s"\n' % REPLYTO)
    strings.append('%s %s\n' % (cronintstr, rocotorunstr))

    fh = open(os.path.join(base['EXPDIR'], '%s.crontab' % base['PSLOT']), 'w')
    fh.write(''.join(strings))
    fh.close()

    return


def create_xml(dict_configs):
    '''
        Given an dictionary of sourced config files,
        create the workflow XML
    '''

    if dict_configs['base']['gfs_cyc'] != 0:
        dict_configs['base'] = get_gfs_cyc_dates(dict_configs['base'])

    base = dict_configs['base']

    # Start collecting workflow pieces
    preamble = get_preamble()
    definitions = get_definitions(base)
    workflow_header = get_workflow_header(base)
    workflow_footer = get_workflow_footer()

    # Get GDAS related entities, resources, workflow
    gdas_resources = get_gdasgfs_resources(dict_configs)
    gdas_tasks = get_gdasgfs_tasks(dohybvar=base['DOHYBVAR'])

    # Get hybrid related entities, resources, workflow
    if base['DOHYBVAR'] == "YES":

        # Determine EOMG/EFCS groups based on ensemble size and grouping
        nens = base['NMEM_ENKF']
        eobs = dict_configs['eobs']
        efcs = dict_configs['efcs']
        nens_eomg = eobs['NMEM_EOMGGRP']
        nens_efcs = efcs['NMEM_EFCSGRP']
        neomg_grps = nens / nens_eomg
        nefcs_grps = nens / nens_efcs
        EOMGGROUPS = ' '.join(['%02d' % x for x in range(1, neomg_grps+1)])
        EFCSGROUPS = ' '.join(['%02d' % x for x in range(1, nefcs_grps+1)])

        hyb_resources = get_hyb_resources(dict_configs)
        hyb_tasks = get_hyb_tasks(EOMGGROUPS, EFCSGROUPS)

    # Get GFS cycle related entities, resources, workflow
    if base['gfs_cyc'] != 0:
        gfs_dates = get_gfs_dates(base)
        gfs_resources = get_gdasgfs_resources(dict_configs, cdump='gfs')
        gfs_tasks = get_gdasgfs_tasks(cdump='gfs', dohybvar=base['DOHYBVAR'])

    xmlfile = []
    xmlfile.append(preamble)
    xmlfile.append(definitions)
    if base['gfs_cyc'] != 0:
        xmlfile.append(gfs_dates)
    xmlfile.append(gdas_resources)
    if base['DOHYBVAR'] == "YES":
        xmlfile.append(hyb_resources)
    if base['gfs_cyc'] != 0:
        xmlfile.append(gfs_resources)
    xmlfile.append(workflow_header)
    xmlfile.append(gdas_tasks)
    if base['DOHYBVAR'] == 'YES':
        xmlfile.append(hyb_tasks)
    if base['gfs_cyc'] != 0:
        xmlfile.append(gfs_tasks)
    xmlfile.append(create_firstcyc_task())
    xmlfile.append(workflow_footer)

    # Write the XML file
    fh = open('%s/%s.xml' % (base['EXPDIR'], base['PSLOT']), 'w')
    fh.write(''.join(xmlfile))
    fh.close()

    return


if __name__ == '__main__':
    main()
    sys.exit(0)
