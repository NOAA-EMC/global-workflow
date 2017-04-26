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
        2. XML templates for GDAS, GFS and Hybrid tasks; gdas.xml, gfs.xml, gdashyb.xml
        Without these dependencies, the script will fail

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
from datetime import datetime, timedelta
from argparse import ArgumentParser, ArgumentDefaultsHelpFormatter
from subprocess import Popen, PIPE


def main():
    parser = ArgumentParser(description='Setup XML workflow and CRONTAB for a GFS parallel.', formatter_class=ArgumentDefaultsHelpFormatter)
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

    if machine in ['ZEUS', 'THEIA']:
        scheduler = 'moabtorque'
    elif machine in ['WCOSS']:
        scheduler = 'lsf'
    elif machine in ['WCOSS_C']:
        scheduler = 'lsfcray'

    return scheduler


def get_gfs_cyc_dates(base_in):
    '''
        Generate GFS dates from experiment dates and gfs_cyc choice
    '''

    gfs_cyc = base_in['gfs_cyc']
    sdate = base_in['SDATE']
    edate = base_in['EDATE']

    base_out = base_in.copy()

    # Set GFS cycling dates
    hrdet = 0
    if gfs_cyc == 1:
        interval = '24:00:00'
        hrinc = 24 - sdate.hour
        hrdet = edate.hour
    elif gfs_cyc == 2:
        interval = '12:00:00'
        if sdate.hour in [0, 12]:
            hrinc = 12
        elif sdate.hour in [6, 18]:
            hrinc = 6
        if edate.hour in [6, 18]:
            hrdet = 6
    elif gfs_cyc == 4:
        interval = '06:00:00'
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
    base_out['INTERVAL_GFS'] = interval
    base_out['SDATE_GFS'] = sdate_gfs
    base_out['EDATE_GFS'] = edate_gfs

    return base_out


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
    strings.append('            Main workflow manager for cycling Global Forecast System\n')
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
    strings.append('    <!ENTITY PSLOT "%s">\n' % base['PSLOT'])
    strings.append('    <!ENTITY SDATE "%s">\n' % base['SDATE'].strftime('%Y%m%d%H%M'))
    strings.append('    <!ENTITY EDATE "%s">\n' % base['EDATE'].strftime('%Y%m%d%H%M'))
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
    strings.append('    <!ENTITY CYCLETHROTTLE "3">\n')
    strings.append('    <!ENTITY TASKTHROTTLE  "20">\n')
    strings.append('    <!ENTITY MAXTRIES      "2">\n')
    strings.append('\n')
    strings.append('    <!-- Environment variables used in tasks -->\n')
    strings.append('    <!ENTITY eEXPDIR    "<envar><name>EXPDIR</name><value>&EXPDIR;</value></envar>">\n')
    strings.append('    <!ENTITY eCDATE     "<envar><name>CDATE</name><value><cyclestr>@Y@m@d@H</cyclestr></value></envar>">\n')
    strings.append('    <!ENTITY eCDUMPGDAS "<envar><name>CDUMP</name><value>gdas</value></envar>">\n')
    strings.append('    <!ENTITY eCDUMPGFS  "<envar><name>CDUMP</name><value>gfs</value></envar>">\n')

    return ''.join(strings)


def get_gfs_dates(base):
    '''
        Generate GFS dates entities
    '''

    strings = []

    strings.append('\n')
    strings.append('    <!-- Starting and ending dates for GFS cycle -->\n')
    strings.append('    <!ENTITY SDATE_GFS    "%s">\n' % base['SDATE_GFS'].strftime('%Y%m%d%H%M'))
    strings.append('    <!ENTITY EDATE_GFS    "%s">\n' % base['EDATE_GFS'].strftime('%Y%m%d%H%M'))
    strings.append('    <!ENTITY INTERVAL_GFS "%s">\n' % base['INTERVAL_GFS'])

    return ''.join(strings)


def get_gdasgfs_resources(configs, base, cdump='gdas'):
    '''
        Create GDAS or GFS resource entities
    '''

    strings = []

    strings.append('\n')
    strings.append('    <!-- BEGIN: Resource requirements for %s part of the workflow -->\n' % cdump.upper())
    strings.append('\n')

    tasks = ['prep', 'anal', 'fcst', 'post', 'vrfy', 'arch']
    for task in tasks:

        cfg = config_parser(find_config('config.%s' % task, configs))
        if cdump == 'gfs' and any('config.%s.gfs' % task in s for s in configs):
            cfg = config_parser('config.%s.gfs' % task)

        if cdump in ['gfs'] and task in ['fcst', 'post']:
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

        strings.append('    <!ENTITY QUEUE_%s     "%s">\n' % (taskstr, queuestr))
        strings.append('    <!ENTITY WALLTIME_%s  "%s">\n' % (taskstr, walltime))
        strings.append('    <!ENTITY RESOURCES_%s "%s">\n' % (taskstr, resstr))
        strings.append('    <!ENTITY NATIVE_%s    "">\n'   % (taskstr))

        strings.append('\n')

    strings.append('    <!-- END: Resource requirements for %s part of the workflow -->\n' % cdump.upper())

    return ''.join(strings)


def get_gdashyb_resources(configs, base):
    '''
        Create hybrid resource entities
    '''

    strings = []

    strings.append('\n')
    strings.append('    <!-- BEGIN: Resource requirements for hybrid part of the workflow -->\n')
    strings.append('\n')

    hybrid_tasks = ['eobs', 'eomg', 'eupd', 'ecen', 'efcs', 'epos', 'earc']
    for task in hybrid_tasks:

        if task == 'eomg':
            cfg = config_parser(find_config('config.eobs', configs))
        else:
            cfg = config_parser(find_config('config.%s' % task, configs))

        if task == 'eomg':
            tasks = cfg['npe_eobs']
            ppn = cfg['npe_node_eobs']
            walltime = cfg['wtime_eomn']
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

        strings.append('    <!ENTITY QUEUE_%s     "%s">\n' % (taskstr, queuestr))
        strings.append('    <!ENTITY WALLTIME_%s  "%s">\n' % (taskstr, walltime))
        strings.append('    <!ENTITY RESOURCES_%s "%s">\n' % (taskstr, resstr))
        strings.append('    <!ENTITY NATIVE_%s    "">\n'   % (taskstr))

        strings.append('\n')

    strings.append('    <!-- END: Resource requirements for hybrid part of the workflow-->\n')

    return ''.join(strings)


def create_task():

    strings = []

    return strings


def create_firstcyc_task():

    strings = []

    strings.append('    <!-- BEGIN: FIRST cycle -->\n')
    strings.append('\n')
    strings.append('    <!-- BEGIN: FIRST firstcyc -->\n')
    strings.append('    <task name="firstcyc" cycledefs="first" maxtries="&MAXTRIES;" final="true">\n')
    strings.append('\n')
    strings.append('        <command>sleep 1</command>\n')
    strings.append('\n')
    strings.append('        <jobname><cyclestr>&PSLOT;_firstcyc_@H</cyclestr></jobname>\n')
    strings.append('        <account>&ACCOUNT;</account>\n')
    strings.append('        <queue>&QUEUE_ARCH;</queue>\n')
    strings.append('        &RESOURCES_ARCH;\n')
    strings.append('        <walltime>&WALLTIME_ARCH;</walltime>\n')
    strings.append('        <native>&NATIVE_ARCH;</native>\n')
    strings.append('\n')
    strings.append('        <join><cyclestr>&ROTDIR;/logs/@Y@m@d@H/firstcyc.log</cyclestr></join>\n')
    strings.append('\n')
    strings.append('        <dependency>\n')
    strings.append('            <and>\n')
    strings.append('                <datadep><cyclestr offset="24:00:00">&EXPDIR;/logs/@Y@m@d@H.log</cyclestr></datadep>\n')
    strings.append('                <not>\n')
    strings.append('                    <cycleexistdep cycle_offset="-6:00:00"/>\n')
    strings.append('                </not>\n')
    strings.append('            </and>\n')
    strings.append('        </dependency>\n')
    strings.append('\n')
    strings.append('    </task>\n')
    strings.append('    <!-- END: FIRST firstcyc -->\n')
    strings.append('\n')
    strings.append('    <!-- END: FIRST cycle -->\n')

    return ''.join(strings)


def get_gdasgfs_workflow(xmldir, cdump='gdas', dohybvar='NO'):
    '''
        Read GDAS or GFS XML template
    '''

    fr = open('%s/%s.xml' % (xmldir, cdump), 'r')
    lines = fr.readlines()
    fr.close()

    if dohybvar == 'YES':
        lines = [l.replace('@EPOSDEP@', '<taskdep task="epos" cycle_offset="-06:00:00"/>') for l in lines]
    else:
        lines = [l.replace('@EPOSDEP@', '') for l in lines]

    lines.append('\n')

    return ''.join(lines)


def get_gdashyb_workflow(xmldir, EOMNGROUPS, EFMNGROUPS):
    '''
        Read GDAS Hybrid XML template
    '''

    fr = open('%s/gdashyb.xml' % xmldir, 'r')
    lines = fr.readlines()
    fr.close()

    lines = [l.replace('@EOMNGROUPS@', EOMNGROUPS) for l in lines]
    lines = [l.replace('@EFMNGROUPS@', EFMNGROUPS) for l in lines]

    lines.append('\n')

    return ''.join(lines)


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
    strings.append('    <log verbosity="10"><cyclestr>&EXPDIR;/logs/@Y@m@d@H.log</cyclestr></log>\n')
    strings.append('\n')
    strings.append('    <!-- Define the cycles -->\n')
    strings.append('    <cycledef group="first">&SDATE;     &SDATE;     06:00:00</cycledef>\n')
    strings.append('    <cycledef group="gdas" >&SDATE;     &EDATE;     06:00:00</cycledef>\n')
    if base['gfs_cyc'] != 0:
        strings.append('    <cycledef group="gfs"  >&SDATE_GFS; &EDATE_GFS; &INTERVAL_GFS;</cycledef>\n')

    strings.append('\n')

    return ''.join(strings)


def get_workflow_footer():
    '''
        Generate workflow footer
    '''

    strings = []
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

    if base['gfs_cyc'] != 0:
        base = get_gfs_cyc_dates(base)

    # Determine EOMN/EFMN groups based on ensemble size and grouping
    if base['DOHYBVAR'] == "YES":
        nens = base['NMEM_ENKF']
        eobs = config_parser(find_config('config.eobs', configs))
        efcs = config_parser(find_config('config.efcs', configs))
        nens_eomn = eobs['NMEM_ENKF_GRP']
        nens_efmn = efcs['NMEM_ENKF_GRP']
        neomn_grps = nens / nens_eomn
        nefmn_grps = nens / nens_efmn
        EOMNGROUPS = ' '.join(['%02d' % x for x in range(1, neomn_grps+1)])
        EFMNGROUPS = ' '.join(['%02d' % x for x in range(1, nefmn_grps+1)])

    preamble = get_preamble()
    definitions = get_definitions(base)

    if base['gfs_cyc'] != 0:
        gfs_dates = get_gfs_dates(base)

    gdas_resources = get_gdasgfs_resources(configs, base)
    gdas_workflow = get_gdasgfs_workflow(xmldir, dohybvar=base['DOHYBVAR'])

    if base['DOHYBVAR'] == "YES":
        gdashyb_resources = get_gdashyb_resources(configs, base)
        gdashyb_workflow = get_gdashyb_workflow(xmldir, EOMNGROUPS, EFMNGROUPS)

    if base['gfs_cyc'] != 0:
        gfs_resources = get_gdasgfs_resources(configs, base, cdump='gfs')
        gfs_workflow = get_gdasgfs_workflow(xmldir, cdump='gfs', dohybvar=base['DOHYBVAR'])

    workflow_header = get_workflow_header(base)
    workflow_footer = get_workflow_footer()

    # Start writing the XML file
    fh = open('%s/%s.xml' % (base['EXPDIR'], base['PSLOT']), 'w')

    fh.write(preamble)

    fh.write(definitions)

    if base['gfs_cyc'] != 0:
        fh.write(gfs_dates)

    fh.write(gdas_resources)

    if base['DOHYBVAR'] == "YES":
        fh.write(gdashyb_resources)

    if base['gfs_cyc'] != 0:
        fh.write(gfs_resources)

    fh.write(workflow_header)

    fh.write(gdas_workflow)

    if base['DOHYBVAR'] == 'YES':
        fh.write(gdashyb_workflow)

    if base['gfs_cyc'] != 0:
        fh.write(gfs_workflow)

    fh.write(create_firstcyc_task())

    fh.write(workflow_footer)

    fh.close()

    return

if __name__ == '__main__':
    main()
    sys.exit(0)
