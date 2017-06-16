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
        Create the ROCOTO workflow for a forecast only experiment given the configuration of the GFS parallel

    AUTHOR:
        Rahul.Mahajan
        rahul.mahajan@noaa.gov

    FILE DEPENDENCIES:
        1. config files for the parallel; e.g. config.base, config.fcst[.gfs], etc.
        Without this dependency, the script will fail

    OUTPUT:
        1. PSLOT.xml: XML workflow
        2. PSLOT.crontab: crontab for ROCOTO run command

'''

import os
import sys
import glob
from distutils.spawn import find_executable
from datetime import datetime
from argparse import ArgumentParser, ArgumentDefaultsHelpFormatter
import shellvars
import rocoto


taskplan = ['getics', 'chgres', 'fcst', 'post', 'vrfy', 'arch']

def main():
    parser = ArgumentParser(description='Setup XML workflow and CRONTAB for a forecast only experiment.', formatter_class=ArgumentDefaultsHelpFormatter)
    parser.add_argument('--expdir',help='full path to experiment directory containing config files', type=str, required=False, default=os.environ['PWD'])

    args = parser.parse_args()

    configs = get_configs(args.expdir)

    dict_configs = source_configs(args.expdir, configs)

    # First create workflow XML
    create_xml(dict_configs)

    # Next create the crontab
    create_crontab(dict_configs['base'])

    return


def get_configs(expdir):
    '''
        Given an experiment directory containing config files,
        return a list of configs minus the ones ending with ".default"
    '''

    configs = glob.glob('%s/config.*' % expdir)

    # remove any defaults from the list
    for c, config in enumerate(configs):
        if config.endswith('.default'):
            print 'dropping %s' % config
            configs.pop(c)

    return configs


def find_config(config_name, configs):

    for config in configs:
        if config_name == os.path.basename(config):
            return config

    # no match found
    raise IOError("%s does not exist, ABORT!" % config_name)


def source_configs(expdir, configs):
    '''
        Given alist of config files
        source the configs and return a dictionary for each task
    '''

    dict_configs = {}

    # First read "config.base", gather basic info
    print 'sourcing config.%s' % 'base'
    dict_configs['base'] = config_parser(find_config('config.base', configs))
    base = dict_configs['base']

    if expdir != base['EXPDIR']:
        print 'MISMATCH in experiment directories!'
        print 'config.base: EXPDIR = %s' % base['EXPDIR']
        print 'input arg:     --expdir = %s' % expdir
        sys.exit(1)

    # GDAS/GFS tasks
    for task in taskplan:

        files = []
        files.append(find_config('config.base', configs))
        files.append(find_config('config.%s' % task, configs))

        print 'sourcing config.%s' % task
        dict_configs[task] = config_parser(files)

    return dict_configs


def config_parser(filename):
    """
    Given the name of config file, key-value pair of all variables in the config file is returned as a dictionary
    :param filename: config file
    :type filename: str or unicode
    :return: Key value pairs representing the environment variables defined
            in the script.
    :rtype: dict
    """
    sv = shellvars.ShellVars(filename)
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
        Exception.__init__(self,msg)


def get_interval(cyc_input):
    '''
        return interval in hours based on gfs_cyc like input
    '''

    # Get interval from cyc_input
    if cyc_input == 1:
        return '24:00:00'
    elif cyc_input == 2:
        return '12:00:00'
    elif cyc_input == 4:
        return '06:00:00'


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
    strings.append('\t\tMain workflow manager for Forecast only Global Forecast System\n')
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
    strings.append('\t<!ENTITY PSLOT    "%s">\n' % base['PSLOT'])
    strings.append('\t<!ENTITY CDUMP    "%s">\n' % base['CDUMP'])
    strings.append('\t<!ENTITY SDATE    "%s">\n' % base['SDATE'].strftime('%Y%m%d%H%M'))
    strings.append('\t<!ENTITY EDATE    "%s">\n' % base['EDATE'].strftime('%Y%m%d%H%M'))
    strings.append('\t<!ENTITY INTERVAL "%s">\n' % base['INTERVAL'])
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
    strings.append('\t<!ENTITY CYCLETHROTTLE "2">\n')
    strings.append('\t<!ENTITY TASKTHROTTLE  "20">\n')
    strings.append('\t<!ENTITY MAXTRIES      "2">\n')
    strings.append('\n')

    return ''.join(strings)


def get_resources(dict_configs, cdump='gdas'):
    '''
        Create resource entities
    '''

    strings = []

    strings.append('\n')

    base = dict_configs['base']

    for task in taskplan:

        cfg = dict_configs[task]

        if cdump in ['gfs'] and 'wtime_%s_gfs' % task in cfg.keys():
            walltime = cfg['wtime_%s_gfs' % task]
        else:
            walltime = cfg['wtime_%s' % task]

        tasks = cfg['npe_%s' % task]
        ppn = cfg['npe_node_%s' % task]
        nodes = tasks / ppn
        memory = cfg['memory_%s' % task] if 'memory_%s' % task in cfg.keys() else None

        if base['machine'] in ['THEIA']:
            resstr = '<nodes>%d:ppn=%d</nodes>' % (nodes, ppn)

        elif base['machine'] in ['WCOSS_C']:
            resstr = '<nodes>%d:ppn=%d</nodes>' % (nodes, ppn)
            if task in ['getics', 'arch']:
                resstr += '<shared></shared>'
            else:
                if memory is not None:
                    resstr += '<memory>%s</memory>' % str(memory)

        elif base['machine'] in ['WCOSS']:
            resstr = '<cores>%d</cores>' % (tasks)

        taskstr = '%s' % task.upper()
        queuestr = '&QUEUE_ARCH;' if task in ['getics', 'arch'] else '&QUEUE;'

        strings.append('\t<!ENTITY QUEUE_%s     "%s">\n' % (taskstr, queuestr))
        strings.append('\t<!ENTITY WALLTIME_%s  "%s">\n' % (taskstr, walltime))
        strings.append('\t<!ENTITY RESOURCES_%s "%s">\n' % (taskstr, resstr))
        strings.append('\t<!ENTITY NATIVE_%s    "">\n'   % (taskstr))

        strings.append('\n')

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


def get_fcstonly_workflow(cdump='gdas'):
    '''
        Create tasks for forecast only workflow
    '''

    envars = []
    envars.append(rocoto.create_envar(name='EXPDIR', value='&EXPDIR;'))
    envars.append(rocoto.create_envar(name='CDATE', value='<cyclestr>@Y@m@d@H</cyclestr>'))
    envars.append(rocoto.create_envar(name='CDUMP', value='%s' % cdump))

    tasks = []

    # getics
    taskname = 'getics'
    task = create_wf_task(taskname, cdump=cdump, envar=envars)

    tasks.append(task)
    tasks.append('\n')

    # chgres
    taskname = 'chgres'
    deps = []
    dep_dict = {'name':'getics', 'type':'task'}
    deps.append(rocoto.add_dependency(dep_dict))
    dependencies = rocoto.create_dependency(dep=deps)
    task = create_wf_task(taskname, cdump=cdump, envar=envars, dependency=dependencies)

    tasks.append(task)
    tasks.append('\n')

    # fcst
    taskname = 'fcst'
    deps = []
    dep_dict = {'name':'chgres', 'type':'task'}
    deps.append(rocoto.add_dependency(dep_dict))
    dependencies = rocoto.create_dependency(dep=deps)
    task = create_wf_task(taskname, cdump=cdump, envar=envars, dependency=dependencies)

    tasks.append(task)
    tasks.append('\n')

    # post
    taskname = 'post'
    deps = []
    dep_dict = {'name':'fcst', 'type':'task'}
    deps.append(rocoto.add_dependency(dep_dict))
    dependencies = rocoto.create_dependency(dep=deps)
    task = create_wf_task(taskname, cdump=cdump, envar=envars, dependency=dependencies)

    tasks.append(task)
    tasks.append('\n')

    # vrfy
    taskname = 'vrfy'
    deps = []
    dep_dict = {'name':'post', 'type':'task'}
    deps.append(rocoto.add_dependency(dep_dict))
    dependencies = rocoto.create_dependency(dep=deps)
    task = create_wf_task(taskname, cdump=cdump, envar=envars, dependency=dependencies)

    tasks.append(task)
    tasks.append('\n')

    # arch
    taskname = 'arch'
    deps = []
    dep_dict = {'name':'vrfy', 'type':'task'}
    deps.append(rocoto.add_dependency(dep_dict))
    dependencies = rocoto.create_dependency(dep=deps)
    task = create_wf_task(taskname, cdump=cdump, envar=envars, dependency=dependencies)

    tasks.append(task)
    tasks.append('\n')

    return ''.join(tasks)


def get_workflow_body():
    '''
        Create the workflow body
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
    strings.append('\t<cycledef group="fcstonly">&SDATE; &EDATE; &INTERVAL;</cycledef>\n')
    strings.append('\n')
    strings.append(get_fcstonly_workflow())
    strings.append('\n')
    strings.append('</workflow>\n')

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
        Given an experiment directory containing config files and
        XML directory containing XML templates, create the workflow XML
    '''


    dict_configs['base']['INTERVAL'] = get_interval(dict_configs['base']['gfs_cyc'])
    base = dict_configs['base']

    preamble = get_preamble()
    definitions = get_definitions(base)
    resources = get_resources(dict_configs, cdump=base['CDUMP'])
    workflow = get_workflow_body()

    # Start writing the XML file
    fh = open('%s/%s.xml' % (base['EXPDIR'], base['PSLOT']), 'w')

    fh.write(preamble)
    fh.write(definitions)
    fh.write(resources)
    fh.write(workflow)

    fh.close()

    return

if __name__ == '__main__':
    main()
    sys.exit(0)
