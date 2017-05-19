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
        2. XML templates for Forecast only tasksG fcstonly.xml
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


def main():
    parser = ArgumentParser(description='Setup XML workflow and CRONTAB for a forecast only experiment.', formatter_class=ArgumentDefaultsHelpFormatter)
    parser.add_argument('--expdir',help='full path to experiment directory containing config files', type=str, required=False, default=os.environ['PWD'])
    parser.add_argument('--xmldir', help='full path to directory containing XML templates', type=str, required=True)

    args = parser.parse_args()

    expdir = args.expdir
    xmldir = args.xmldir

    # Source the configs in expdir once and return a dictionary
    dict_configs = source_configs(expdir)

    # First create workflow XML
    create_xml(dict_configs, xmldir)

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
    dict_configs['base'] = config_parser(find_config('config.base', configs))
    base = dict_configs['base']

    if expdir != base['EXPDIR']:
        print 'MISMATCH in experiment directories!'
        print 'config.base: EXPDIR = %s' % base['EXPDIR']
        print 'input arg:     --expdir = %s' % expdir
        sys.exit(1)

    # GDAS/GFS tasks
    for task in ['prep', 'anal', 'fcst', 'post', 'vrfy', 'arch']:

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
    strings.append('    <!--\n')
    strings.append('        PROGRAM\n')
    strings.append('            Main workflow manager for Forecast only Global Forecast System\n')
    strings.append('\n')
    strings.append('        AUTHOR:\n')
    strings.append('            Rahul Mahajan\n')
    strings.append('            rahul.mahajan@noaa.gov\n')
    strings.append('\n')
    strings.append('        NOTES:\n')
    strings.append('            This workflow was automatically generated at %s\n' % datetime.now())
    strings.append('    -->\n')

    return ''.join(strings)


def get_definitions(base):
    '''
        Create entities related to the experiment
    '''

    strings = []

    strings.append('\n')
    strings.append('    <!-- User definitions -->\n')
    strings.append('    <!ENTITY LOGNAME "%s">\n' % os.environ['USER'])
    strings.append('\n')
    strings.append('    <!-- Experiment parameters such as name, starting, ending dates -->\n')
    strings.append('    <!ENTITY PSLOT    "%s">\n' % base['PSLOT'])
    strings.append('    <!ENTITY CDUMP    "%s">\n' % base['CDUMP'])
    strings.append('    <!ENTITY SDATE    "%s">\n' % base['SDATE'].strftime('%Y%m%d%H%M'))
    strings.append('    <!ENTITY EDATE    "%s">\n' % base['EDATE'].strftime('%Y%m%d%H%M'))
    strings.append('    <!ENTITY INTERVAL "%s">\n' % base['INTERVAL'])
    strings.append('\n')
    strings.append('    <!-- Experiment and Rotation directory -->\n')
    strings.append('    <!ENTITY EXPDIR "%s">\n' % base['EXPDIR'])
    strings.append('    <!ENTITY ROTDIR "%s">\n' % base['ROTDIR'])
    strings.append('\n')
    strings.append('    <!-- Directories for driving the workflow -->\n')
    strings.append('    <!ENTITY JOBS_DIR "%s/fv3gfs/jobs">\n' % base['BASE_WORKFLOW'])
    strings.append('\n')
    strings.append('    <!-- Machine related entities -->\n')
    strings.append('    <!ENTITY ACCOUNT    "%s">\n' % base['ACCOUNT'])
    strings.append('    <!ENTITY QUEUE      "%s">\n' % base['QUEUE'])
    strings.append('    <!ENTITY QUEUE_ARCH "%s">\n' % base['QUEUE_ARCH'])
    strings.append('    <!ENTITY SCHEDULER  "%s">\n' % get_scheduler(base['machine']))
    strings.append('\n')
    strings.append('    <!-- ROCOTO parameters that control workflow -->\n')
    strings.append('    <!ENTITY CYCLETHROTTLE "4">\n')
    strings.append('    <!ENTITY TASKTHROTTLE  "20">\n')
    strings.append('    <!ENTITY MAXTRIES      "2">\n')
    strings.append('\n')
    strings.append('    <!-- Environment variables used in tasks -->\n')
    strings.append('    <!ENTITY eEXPDIR "<envar><name>EXPDIR</name><value>&EXPDIR;</value></envar>">\n')
    strings.append('    <!ENTITY eCDATE  "<envar><name>CDATE</name><value><cyclestr>@Y@m@d@H</cyclestr></value></envar>">\n')
    strings.append('    <!ENTITY eCDUMP  "<envar><name>CDUMP</name><value>&CDUMP;</value></envar>">\n')

    return ''.join(strings)


def get_resources(dict_configs, cdump='gdas'):
    '''
        Create resource entities
    '''

    strings = []

    strings.append('\n')
    strings.append('    <!-- BEGIN: Resource requirements for forecast only workflow -->\n')
    strings.append('\n')

    base = dict_configs['base']

    tasks = ['getics', 'chgres', 'fcst', 'post', 'vrfy', 'arch']
    for task in tasks:

        cfg = dict_configs['task']

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

        strings.append('    <!ENTITY QUEUE_%s     "%s">\n' % (taskstr, queuestr))
        strings.append('    <!ENTITY WALLTIME_%s  "%s">\n' % (taskstr, walltime))
        strings.append('    <!ENTITY RESOURCES_%s "%s">\n' % (taskstr, resstr))
        strings.append('    <!ENTITY NATIVE_%s    "">\n'   % (taskstr))

        strings.append('\n')

    strings.append('    <!-- END: Resource requirements for forecast only workflow -->\n')

    return ''.join(strings)


def get_fcstonly_workflow(xmldir):
    '''
        Read forecast only XML template
    '''

    fr = open('%s/fcstonly.xml' % xmldir, 'r')
    lines = fr.readlines()
    fr.close()

    return ''.join(lines)


def get_workflow_body(xmldir):
    '''
        Create the workflow body
    '''

    strings = []

    strings.append('\n')
    strings.append(']>\n')
    strings.append('\n')
    strings.append('<workflow realtime="F" scheduler="&SCHEDULER;" cyclethrottle="&CYCLETHROTTLE;" taskthrottle="&TASKTHROTTLE;">\n')
    strings.append('\n')
    strings.append('    <log verbosity="10"><cyclestr>&EXPDIR;/logs/@Y@m@d@H.log</cyclestr></log>\n')
    strings.append('\n')
    strings.append('    <!-- Define the cycles -->\n')
    strings.append('    <cycledef group="fcstonly">&SDATE; &EDATE; &INTERVAL;</cycledef>\n')
    strings.append('\n')
    strings.append(get_fcstonly_workflow(xmldir))
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


def create_xml(dict_configs, xmldir):
    '''
        Given an experiment directory containing config files and
        XML directory containing XML templates, create the workflow XML
    '''


    dict_configs['base']['INTERVAL'] = get_interval(dict_configs['base']['gfs_cyc'])
    base = dict_configs['base']

    preamble = get_preamble()
    definitions = get_definitions(base)
    resources = get_resources(dict_configs, cdump=base['CDUMP'])
    workflow = get_workflow_body(xmldir)

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
