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

    THANKS:
        module shellvars.py from the Internet, adapted for local use.
'''

import os
import sys
import glob
from distutils.spawn import find_executable
from datetime import datetime
from argparse import ArgumentParser, ArgumentDefaultsHelpFormatter
from subprocess import Popen, PIPE


def main():
    parser = ArgumentParser(description='Setup XML workflow and CRONTAB for a forecast only experiment.', formatter_class=ArgumentDefaultsHelpFormatter)
    parser.add_argument('--expdir',help='full path to experiment directory containing config files', type=str, required=False, default=os.environ['PWD'])
    parser.add_argument('--xmldir', help='full path to directory containing XML templates', type=str, required=True)

    args = parser.parse_args()

    expdir = args.expdir
    xmldir = args.xmldir

    # First create workflow XML
    create_xml(expdir, xmldir)

    # Next create the crontab
    create_crontab(expdir)

    return


class ShellScriptException(Exception):
    def __init__(self, shell_script, shell_error):
        self.shell_script = shell_script
        self.shell_error = shell_error
        msg = "Error processing script %s: %s" % (shell_script, shell_error)
        Exception.__init__(self, msg)


class ShellVars():
    """
    Module that sources the shell script and returns an object that can be used to get a key
    value pair for a single variable or all variables or just a list of variables defined in the
    script.
    ShellVars.list_vars : return a list list all variables in the script
    ShellVars.get_vars  : return a dictionary of key value pairs for all variables in the script
    ShellVars.get_var   : return a key value pair for desired variable in the script
    """

    def __init__(self, script_path, ignore=None):
        """
        Given a shell script, initializes the class ShellVars
        :param script_path: Path to the shell script
        :type script_path: str or unicode
        :param ignore: variable names to ignore.  By default we ignore variables
                        that env injects into the script's environment.
                        See IGNORE_DEFAULT.
        :type ignore: iterable
        """
        IGNORE_DEFAULT = set(["SHLVL", "PWD", "_"])

        self.script_path = script_path
        self.ignore = IGNORE_DEFAULT if ignore is None else ignore

        return


    def _noscripterror(self):
        return IOError("File does not exist: %s" % self.script_path)


    def list_vars(self):
        """
        Given a shell script, returns a list of shell variable names.
        Note: this method executes the script, so beware if it contains side-effects.
        :return: Key value pairs representing the environment variables defined
                in the script.
        :rtype: list
        """
        if os.path.isfile(self.script_path):
            input = (""". "%s" > /dev/null 2>&1; env | awk -F = '/[a-zA-Z_][a-zA-Z_0-9]*=/ """ % self.script_path +
                     """{ if (!system("[ -n \\"${" $1 "}\\" ]")) print $1 }'""")
            cmd = "env -i bash".split()

            p = Popen(cmd, stdout=PIPE, stdin=PIPE, stderr=PIPE)
            stdout_data, stderr_data = p.communicate(input=input)
            if stderr_data:
                raise ShellScriptException(self.script_path, stderr_data)
            else:
                lines = stdout_data.split()
                return [elt for elt in lines if elt not in self.ignore]
        else:
            raise self._noscripterror()


    def get_vars(self):
        """
        Gets the values of environment variables defined in a shell script.
        Note: this method executes the script potentially many times.
        :return: Key value pairs representing the environment variables defined
                in the script.
        :rtype: dict
        """

        # Iterate over every var independently:
        # This is slower than using env, but enables us to capture multiline variables
        return dict((var, self.get_var(var)) for var in self.list_vars())


    def get_var(self, var):
        """
        Given the name of an environment variable, returns the
        value of the environment variable.
        :param var: environment variable name
        :type var: str or unicode
        :return: str
        """
        if os.path.isfile(self.script_path):
            input = '. "%s" > /dev/null 2>&1; echo -n "$%s"\n'% (self.script_path, var)
            pipe = Popen(["bash"], stdout=PIPE, stdin=PIPE, stderr=PIPE)
            stdout_data, stderr_data = pipe.communicate(input=input)
            if stderr_data:
                raise ShellScriptException(self.script_path, stderr_data)
            else:
                return stdout_data
        else:
            raise self._noscripterror()


def config_parser(filename):
    """
    Given the name of config file, key-value pair of all variables in the config file is returned as a dictionary
    :param filename: config file
    :type filename: str or unicode
    :return: Key value pairs representing the environment variables defined
            in the script.
    :rtype: dict
    """
    sv = ShellVars(filename)
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

    if machine in ['THEIA']:
        scheduler = 'moabtorque'
    elif machine in ['WCOSS']:
        scheduler = 'lsf'
    elif machine in ['WCOSS_C']:
        scheduler = 'lsfcray'

    return scheduler


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
    strings.append('    <!ENTITY SCHEDULER  "%s">\n' % base['SCHEDULER'])
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


def get_resources(configs, base, cdump='gdas'):
    '''
        Create resource entities
    '''

    strings = []

    strings.append('\n')
    strings.append('    <!-- BEGIN: Resource requirements for forecast only workflow -->\n')
    strings.append('\n')

    tasks = ['getics', 'chgres', 'fcst', 'post', 'vrfy', 'arch']
    for task in tasks:

        cfg = config_parser(find_config('config.%s' % task, configs))
        if cdump == 'gfs' and any('config.%s.gfs' % task in s for s in configs):
            cfg = config_parser('config.%s.gfs' % task)

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


def create_crontab(expdir, cronint=5):
    '''
        Create crontab to execute rocotorun every cronint (5) minutes
    '''

    # No point creating a crontab if rocotorun is not available.
    rocotoruncmd = find_executable('rocotorun')
    if rocotoruncmd is None:
        print 'Failed to find rocotorun, crontab will not be created'
        return

    configs = glob.glob('%s/config.*' % expdir)

    # read "config.base", gather basic info
    base = config_parser(find_config('config.base', configs))

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


def create_xml(expdir, xmldir):
    '''
        Given an experiment directory containing config files and
        XML directory containing XML templates, create the workflow XML
    '''

    configs = glob.glob('%s/config.*' % expdir)

    # First read "config.base", gather basic info needed for xml
    base = config_parser(find_config('config.base', configs))

    if expdir != base['EXPDIR']:
        print 'MISMATCH in experiment directories!'
        print 'config.base: EXPDIR = %s' % base['EXPDIR']
        print 'input arg:     --expdir = %s' % expdir
        sys.exit(1)

    base['SCHEDULER'] = get_scheduler(base['machine'])
    base['INTERVAL'] = get_interval(base['gfs_cyc'])

    preamble = get_preamble()
    definitions = get_definitions(base)
    resources = get_resources(configs, base, cdump=base['CDUMP'])
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
