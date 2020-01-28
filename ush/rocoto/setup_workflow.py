#!/usr/bin/env python

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
import re
import numpy as np
from datetime import datetime, timedelta
from argparse import ArgumentParser, ArgumentDefaultsHelpFormatter
from collections import OrderedDict
import rocoto
import workflow_utils as wfu

def main():
    parser = ArgumentParser(description='Setup XML workflow and CRONTAB for a GFS parallel.', formatter_class=ArgumentDefaultsHelpFormatter)
    parser.add_argument('--expdir', help='full path to experiment directory containing config files', type=str, required=False, default=os.environ['PWD'])
    args = parser.parse_args()

    configs = wfu.get_configs(args.expdir)

    _base = wfu.config_parser([wfu.find_config('config.base', configs)])

    if not os.path.samefile(args.expdir, _base['EXPDIR']):
        print 'MISMATCH in experiment directories!'
        print 'config.base: EXPDIR = %s' % repr(_base['EXPDIR'])
        print 'input arg:     --expdir = %s' % repr(args.expdir)
        sys.exit(1)

    gfs_steps = ['prep', 'anal', 'gldas', 'fcst', 'postsnd', 'post', 'vrfy', 'metp', 'arch']
    gfs_steps_gempak = ['gempak']
    gfs_steps_awips = ['awips']
    #hyb_steps = ['eobs', 'eomg', 'eupd', 'ecen', 'efcs', 'epos', 'earc']
    wav_steps = ['waveinit', 'waveprep', 'wavepostsbs']
    #Implement additional wave jobs at later date
    #wav_steps = ['waveinit', 'waveprep', 'wavepostsbs', 'wavepost', 'wavestat']
    #wav_steps_gempak = ['wavegempaksbs']
    #wav_steps_awips = ['waveawipssbs', 'waveawips']
# From gfsv16b latest
#    gfs_steps = ['prep', 'anal', 'gldas', 'fcst', 'postsnd', 'post', 'awips', 'gempak', 'vrfy', 'metp', 'arch']
    hyb_steps = ['eobs', 'eomg', 'eupd', 'ecen', 'esfc', 'efcs', 'epos', 'earc']

    steps = gfs_steps + hyb_steps if _base.get('DOHYBVAR', 'NO') == 'YES' else gfs_steps
    steps = steps + gfs_steps_gempak if _base.get('DO_GEMPAK', 'NO') == 'YES' else steps
    steps = steps + gfs_steps_awips if _base.get('DO_AWIPS', 'NO') == 'YES' else steps
    steps = steps + wav_steps if _base.get('DO_WAVE', 'NO') == 'YES' else steps
    #steps = steps + wav_steps_gempak if _base.get('DO_GEMPAK', 'NO') == 'YES' else steps
    #steps = steps + wav_steps_awips if _base.get('DO_AWIPS', 'NO') == 'YES' else steps

    dict_configs = wfu.source_configs(configs, steps)

    # Check and set gfs_cyc specific variables
    if dict_configs['base']['gfs_cyc'] != 0:
        dict_configs['base'] = get_gfs_cyc_dates(dict_configs['base'])

    # First create workflow XML
    create_xml(dict_configs)

    # Next create the crontab
    wfu.create_crontab(dict_configs['base'])

    return


def get_gfs_cyc_dates(base):
    '''
        Generate GFS dates from experiment dates and gfs_cyc choice
    '''

    base_out = base.copy()

    gfs_cyc = base['gfs_cyc']
    sdate = base['SDATE']
    edate = base['EDATE']

    interval_gfs = wfu.get_gfs_interval(gfs_cyc)

    # Set GFS cycling dates
    hrdet = 0
    if gfs_cyc == 1:
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
        print 'W A R N I N G!'
        print 'Starting date for GFS cycles is after Ending date of experiment'
        print 'SDATE = %s,     EDATE = %s' % (sdate.strftime('%Y%m%d%H'), edate.strftime('%Y%m%d%H'))
        print 'SDATE_GFS = %s, EDATE_GFS = %s' % (sdate_gfs.strftime('%Y%m%d%H'), edate_gfs.strftime('%Y%m%d%H'))
        gfs_cyc = 0

    base_out['gfs_cyc'] = gfs_cyc
    base_out['SDATE_GFS'] = sdate_gfs
    base_out['EDATE_GFS'] = edate_gfs
    base_out['INTERVAL_GFS'] = interval_gfs

    fhmax_gfs = {}
    for hh in ['00', '06', '12', '18']:
        fhmax_gfs[hh] = base.get('FHMAX_GFS_%s' % hh, 'FHMAX_GFS_00')
    base_out['FHMAX_GFS'] = fhmax_gfs

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

    machine = base.get('machine', wfu.detectMachine())
    scheduler = wfu.get_scheduler(machine)

    strings = []

    strings.append('\n')
    strings.append('\t<!-- Experiment parameters such as name, starting, ending dates -->\n')
    strings.append('\t<!ENTITY PSLOT "%s">\n' % base['PSLOT'])
    strings.append('\t<!ENTITY SDATE "%s">\n' % base['SDATE'].strftime('%Y%m%d%H%M'))
    strings.append('\t<!ENTITY EDATE "%s">\n' % base['EDATE'].strftime('%Y%m%d%H%M'))

    if base['gfs_cyc'] != 0:
        strings.append(get_gfs_dates(base))
        strings.append('\n')

    strings.append('\t<!-- Run Envrionment -->\n')
    strings.append('\t<!ENTITY RUN_ENVIR "%s">\n' % base['RUN_ENVIR'])
    strings.append('\n')
    strings.append('\t<!-- Experiment and Rotation directory -->\n')
    strings.append('\t<!ENTITY EXPDIR "%s">\n' % base['EXPDIR'])
    strings.append('\t<!ENTITY ROTDIR "%s">\n' % base['ROTDIR'])
    strings.append('\n')
    strings.append('\t<!-- Directories for driving the workflow -->\n')
    strings.append('\t<!ENTITY HOMEgfs  "%s">\n' % base['HOMEgfs'])
    strings.append('\t<!ENTITY JOBS_DIR "%s">\n' % base['BASE_JOB'])
    strings.append('\t<!ENTITY DMPDIR   "%s">\n' % base['DMPDIR'])
    strings.append('\n')
    strings.append('\t<!-- Machine related entities -->\n')
    strings.append('\t<!ENTITY ACCOUNT    "%s">\n' % base['ACCOUNT'])

    strings.append('\t<!ENTITY QUEUE      "%s">\n' % base['QUEUE'])
    strings.append('\t<!ENTITY QUEUE_ARCH "%s">\n' % base['QUEUE_ARCH'])
    if scheduler in ['slurm']:
        strings.append('\t<!ENTITY PARTITION_ARCH "%s">\n' % base['QUEUE_ARCH'])
    strings.append('\t<!ENTITY SCHEDULER  "%s">\n' % scheduler)
    strings.append('\n')
    strings.append('\t<!-- Toggle HPSS archiving -->\n')
    strings.append('\t<!ENTITY ARCHIVE_TO_HPSS "YES">\n')
    strings.append('\n')
    strings.append('\t<!-- ROCOTO parameters that control workflow -->\n')
    strings.append('\t<!ENTITY CYCLETHROTTLE "3">\n')
    strings.append('\t<!ENTITY TASKTHROTTLE  "25">\n')
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

    base = dict_configs['base']
    machine = base.get('machine', wfu.detectMachine())
    scheduler = wfu.get_scheduler(machine)
    do_bufrsnd = base.get('DO_BUFRSND', 'NO').upper()
    do_gempak = base.get('DO_GEMPAK', 'NO').upper()
    do_awips = base.get('DO_AWIPS', 'NO').upper()
    do_gldas = base.get('DO_GLDAS', 'NO').upper()
    do_wave = base.get('DO_WAVE', 'NO').upper()

    #tasks = ['prep', 'anal', 'fcst', 'post', 'vrfy', 'arch']
    tasks = ['prep', 'anal']

    if cdump in ['gdas'] and do_gldas in ['Y', 'YES']:
        tasks += ['gldas']
    if cdump in ['gdas'] and do_wave in ['Y', 'YES']:
        #tasks += ['waveinit', 'waveprep', 'wavepostsbs', 'wavepost', 'wavestat']
        tasks += ['waveinit', 'waveprep', 'wavepostsbs']

    tasks += ['fcst', 'post', 'vrfy', 'metp', 'arch']

    if cdump in ['gfs'] and do_wave in ['Y', 'YES']:
        #tasks += ['waveinit', 'waveprep', 'wavepostsbs', 'wavepost', 'wavestat']
        tasks += ['waveinit', 'waveprep', 'wavepostsbs']
    if cdump in ['gfs'] and do_bufrsnd in ['Y', 'YES']:
        tasks += ['postsnd']
    if cdump in ['gfs'] and do_gempak in ['Y', 'YES']:
        tasks += ['gempak']
    #if cdump in ['gfs'] and do_wave in ['Y', 'YES'] and do_gempak in ['Y', 'YES']:
    #    tasks += ['wavegempaksbs']
    if cdump in ['gfs'] and do_awips in ['Y', 'YES']:
        tasks += ['awips']
    #if cdump in ['gfs'] and do_wave in ['Y', 'YES'] and do_awips in ['Y', 'YES']:
    #    tasks += ['waveawipssbs', 'waveawips']
 
    dict_resources = OrderedDict()

    for task in tasks:

        cfg = dict_configs[task]

        wtimestr, resstr, queuestr, memstr, natstr = wfu.get_resources(machine, cfg, task, cdump=cdump)
        taskstr = '%s_%s' % (task.upper(), cdump.upper())

        strings = []
        strings.append('\t<!ENTITY QUEUE_%s     "%s">\n' % (taskstr, queuestr))
        if scheduler in ['slurm'] and task in ['arch']:
            strings.append('\t<!ENTITY PARTITION_%s "&PARTITION_ARCH;">\n' % taskstr )
        strings.append('\t<!ENTITY WALLTIME_%s  "%s">\n' % (taskstr, wtimestr))
        strings.append('\t<!ENTITY RESOURCES_%s "%s">\n' % (taskstr, resstr))
        if len(memstr) != 0:
            strings.append('\t<!ENTITY MEMORY_%s    "%s">\n' % (taskstr, memstr))
        strings.append('\t<!ENTITY NATIVE_%s    "%s">\n' % (taskstr, natstr))

        dict_resources['%s%s' % (cdump, task)] = ''.join(strings)

    return dict_resources


def get_hyb_resources(dict_configs):
    '''
        Create hybrid resource entities
    '''

    base = dict_configs['base']
    machine = base.get('machine', wfu.detectMachine())
    scheduler = wfu.get_scheduler(machine)
    lobsdiag_forenkf = base.get('lobsdiag_forenkf', '.false.').upper()
    eupd_cyc= base.get('EUPD_CYC', 'gdas').upper()

    dict_resources = OrderedDict()

    # These tasks can be run in either or both cycles
    tasks1 = ['eobs', 'eomg', 'eupd']
    if lobsdiag_forenkf in ['.T.', '.TRUE.']:
        tasks1.remove('eomg')

    if eupd_cyc in ['BOTH']:
        cdumps = ['gfs', 'gdas']
    elif eupd_cyc in ['GFS']:
        cdumps = ['gfs']
    elif eupd_cyc in ['GDAS']:
        cdumps = ['gdas']

    for cdump in cdumps:
        for task in tasks1:

            cfg = dict_configs['eobs'] if task in ['eomg'] else dict_configs[task]

            wtimestr, resstr, queuestr, memstr, natstr = wfu.get_resources(machine, cfg, task, cdump=cdump)

            taskstr = '%s_%s' % (task.upper(), cdump.upper())

            strings = []

            strings.append('\t<!ENTITY QUEUE_%s     "%s">\n' % (taskstr, queuestr))
            strings.append('\t<!ENTITY WALLTIME_%s  "%s">\n' % (taskstr, wtimestr))
            strings.append('\t<!ENTITY RESOURCES_%s "%s">\n' % (taskstr, resstr))
            if len(memstr) != 0:
                strings.append('\t<!ENTITY MEMORY_%s    "%s">\n' % (taskstr, memstr))
            strings.append('\t<!ENTITY NATIVE_%s    "%s">\n' % (taskstr, natstr))

            dict_resources['%s%s' % (cdump, task)] = ''.join(strings)


    # These tasks are always run as part of the GDAS cycle
    cdump = 'gdas'
    tasks2 = ['ecen', 'esfc', 'efcs', 'epos', 'earc']
    for task in tasks2:

        cfg = dict_configs[task]

        wtimestr, resstr, queuestr, memstr, natstr = wfu.get_resources(machine, cfg, task, cdump=cdump)

        taskstr = '%s_%s' % (task.upper(), cdump.upper())

        strings = []
        strings.append('\t<!ENTITY QUEUE_%s     "%s">\n' % (taskstr, queuestr))
        if scheduler in ['slurm'] and task in ['earc']:
            strings.append('\t<!ENTITY PARTITION_%s "&PARTITION_ARCH;">\n' % taskstr )
        strings.append('\t<!ENTITY WALLTIME_%s  "%s">\n' % (taskstr, wtimestr))
        strings.append('\t<!ENTITY RESOURCES_%s "%s">\n' % (taskstr, resstr))
        if len(memstr) != 0:
            strings.append('\t<!ENTITY MEMORY_%s    "%s">\n' % (taskstr, memstr))
        strings.append('\t<!ENTITY NATIVE_%s    "%s">\n' % (taskstr, natstr))

        dict_resources['%s%s' % (cdump, task)] = ''.join(strings)

    return dict_resources


def get_gdasgfs_tasks(dict_configs, cdump='gdas'):
    '''
        Create GDAS or GFS tasks
    '''

    envars = []
    if wfu.get_scheduler(wfu.detectMachine()) in ['slurm']:
        envars.append(rocoto.create_envar(name='SLURM_SET', value='YES'))
    envars.append(rocoto.create_envar(name='RUN_ENVIR', value='&RUN_ENVIR;'))
    envars.append(rocoto.create_envar(name='HOMEgfs', value='&HOMEgfs;'))
    envars.append(rocoto.create_envar(name='EXPDIR', value='&EXPDIR;'))
    envars.append(rocoto.create_envar(name='CDATE', value='<cyclestr>@Y@m@d@H</cyclestr>'))
    envars.append(rocoto.create_envar(name='CDUMP', value='%s' % cdump))
    envars.append(rocoto.create_envar(name='PDY', value='<cyclestr>@Y@m@d</cyclestr>'))
    envars.append(rocoto.create_envar(name='cyc', value='<cyclestr>@H</cyclestr>'))

    base = dict_configs['base']
    gfs_cyc = base.get('gfs_cyc', 0)
    gldas_cyc = base.get('gldas_cyc', 0)
    dohybvar = base.get('DOHYBVAR', 'NO').upper()
    eupd_cyc = base.get('EUPD_CYC', 'gdas').upper()
    do_bufrsnd = base.get('DO_BUFRSND', 'NO').upper()
    do_gempak = base.get('DO_GEMPAK', 'NO').upper()
    do_awips = base.get('DO_AWIPS', 'NO').upper()
    do_gldas = base.get('DO_GLDAS', 'NO').upper()
    do_wave = base.get('DO_WAVE', 'NO').upper()
    dumpsuffix = base.get('DUMP_SUFFIX', '')
    gridsuffix = base.get('SUFFIX', '')

    dict_tasks = OrderedDict()

    # prep
    deps = []
    dep_dict = {'type': 'metatask', 'name': '%spost' % 'gdas', 'offset': '-06:00:00'}
    deps.append(rocoto.add_dependency(dep_dict))
    data = '&ROTDIR;/gdas.@Y@m@d/@H/gdas.t@Hz.atmf009%s' % (gridsuffix)
    dep_dict = {'type': 'data', 'data': data, 'offset': '-06:00:00'}
    deps.append(rocoto.add_dependency(dep_dict))
    data = '&DMPDIR;/%s%s.@Y@m@d/@H/%s.t@Hz.updated.status.tm00.bufr_d' % (cdump, dumpsuffix, cdump)
    dep_dict = {'type': 'data', 'data': data}
    deps.append(rocoto.add_dependency(dep_dict))
    dependencies = rocoto.create_dependency(dep_condition='and', dep=deps)

    gfs_enkf = True if eupd_cyc in ['BOTH', 'GFS'] and dohybvar in ['Y', 'YES'] else False

    if gfs_enkf and cdump in ['gfs']:
        if gfs_cyc == 4:
            task = wfu.create_wf_task('prep', cdump=cdump, envar=envars, dependency=dependencies)
        else:
            task = wfu.create_wf_task('prep', cdump=cdump, envar=envars, dependency=dependencies, cycledef='gdas')

    else:
        task = wfu.create_wf_task('prep', cdump=cdump, envar=envars, dependency=dependencies)

    dict_tasks['%sprep' % cdump] = task

    # waveinit
    if do_wave in ['Y', 'YES']:
        deps = []
        dep_dict = {'type': 'task', 'name': '%sprep' % cdump}
        deps.append(rocoto.add_dependency(dep_dict))
        dep_dict = {'type': 'cycleexist', 'condition': 'not', 'offset': '-06:00:00'}
        deps.append(rocoto.add_dependency(dep_dict))
        dependencies = rocoto.create_dependency(dep_condition='or', dep=deps)
        task = wfu.create_wf_task('waveinit', cdump=cdump, envar=envars, dependency=dependencies)
        dict_tasks['%swaveinit' % cdump] = task

    # waveprep
    if do_wave in ['Y', 'YES']:
        deps = []
        dep_dict = {'type': 'task', 'name': '%swaveinit' % cdump}
        deps.append(rocoto.add_dependency(dep_dict))
        dependencies = rocoto.create_dependency(dep=deps)
        task = wfu.create_wf_task('waveprep', cdump=cdump, envar=envars, dependency=dependencies)
        dict_tasks['%swaveprep' % cdump] = task

    # anal
    deps = []
    dep_dict = {'type': 'task', 'name': '%sprep' % cdump}
    deps.append(rocoto.add_dependency(dep_dict))
    if dohybvar in ['y', 'Y', 'yes', 'YES']:
        dep_dict = {'type': 'metatask', 'name': '%sepmn' % 'gdas', 'offset': '-06:00:00'}
        deps.append(rocoto.add_dependency(dep_dict))
        dependencies = rocoto.create_dependency(dep_condition='and', dep=deps)
    else:
        dependencies = rocoto.create_dependency(dep=deps)
    task = wfu.create_wf_task('anal', cdump=cdump, envar=envars, dependency=dependencies)

    dict_tasks['%sanal' % cdump] = task

    # gldas
    if cdump in ['gdas'] and do_gldas in ['Y', 'YES']:
        deps = []
        data = '&ROTDIR;/%s.@Y@m@d/@H/%s.t@Hz.loginc.txt' % (cdump, cdump)
        dep_dict = {'type': 'data', 'data': data}
        deps.append(rocoto.add_dependency(dep_dict))
        dep_dict = {'type': 'task', 'name': '%sanal' % cdump}
        deps.append(rocoto.add_dependency(dep_dict))
        dependencies = rocoto.create_dependency(dep_condition='or', dep=deps)
        task = wfu.create_wf_task('gldas', cdump=cdump, envar=envars, dependency=dependencies)

        dict_tasks['%sgldas' % cdump] = task

    # fcst
    deps = []
    data = '&ROTDIR;/%s.@Y@m@d/@H/%s.t@Hz.loginc.txt' % (cdump, cdump)
    dep_dict = {'type': 'data', 'data': data}
    deps.append(rocoto.add_dependency(dep_dict))
    if do_wave in ['Y', 'YES']:
        dep_dict = {'type': 'task', 'name': '%swaveprep' % cdump}
        deps.append(rocoto.add_dependency(dep_dict))
    #if cdump in ['gdas']:
    #    if do_gldas in ['Y', 'YES']:
    #        dep_dict = {'type': 'task', 'name': '%sgldas' % cdump}
    #        deps.append(rocoto.add_dependency(dep_dict))
    #    else:
    #        dep_dict = {'type': 'task', 'name': '%sanal' % cdump}
    #        deps.append(rocoto.add_dependency(dep_dict))
    #    dep_dict = {'type': 'cycleexist', 'condition': 'not', 'offset': '-06:00:00'}
    #    deps.append(rocoto.add_dependency(dep_dict))
    #    dependencies = rocoto.create_dependency(dep_condition='or',dep=deps)
    #elif cdump in ['gfs']:
    #    dep_dict = {'type': 'task', 'name': '%sanal' % cdump}
    #    deps.append(rocoto.add_dependency(dep_dict))
    dependencies = rocoto.create_dependency(dep_condition='and',dep=deps)
    task = wfu.create_wf_task('fcst', cdump=cdump, envar=envars, dependency=dependencies)

    dict_tasks['%sfcst' % cdump] = task

    # post
    deps = []
    data = '&ROTDIR;/%s.@Y@m@d/@H/%s.t@Hz.log#dep#.txt' % (cdump, cdump)
    dep_dict = {'type': 'data', 'data': data}
    deps.append(rocoto.add_dependency(dep_dict))
    dep_dict = {'type': 'task', 'name': '%sfcst' % cdump}
    deps.append(rocoto.add_dependency(dep_dict))
    dependencies = rocoto.create_dependency(dep_condition='or', dep=deps)
    fhrgrp = rocoto.create_envar(name='FHRGRP', value='#grp#')
    fhrlst = rocoto.create_envar(name='FHRLST', value='#lst#')
    ROTDIR = rocoto.create_envar(name='ROTDIR', value='&ROTDIR;')
    postenvars = envars + [fhrgrp] + [fhrlst] + [ROTDIR]
    varname1, varname2, varname3 = 'grp', 'dep', 'lst'
    varval1, varval2, varval3 = get_postgroups(dict_configs['post'], cdump=cdump)
    vardict = {varname2: varval2, varname3: varval3}
    task = wfu.create_wf_task('post', cdump=cdump, envar=postenvars, dependency=dependencies,
                              metatask='post', varname=varname1, varval=varval1, vardict=vardict)

    dict_tasks['%spost' % cdump] = task

    # wavepostsbs
    if do_wave in ['Y', 'YES']:
        deps = []
        data = '&ROTDIR;/%swave.@Y@m@d/@H/rundata/%swave.out_grd.glo_10m.@Y@m@d.@H0000' % (cdump,cdump)
        dep_dict = {'type': 'data', 'data': data}
        deps.append(rocoto.add_dependency(dep_dict))
        data = '&ROTDIR;/%swave.@Y@m@d/@H/rundata/%swave.out_grd.aoc_9km.@Y@m@d.@H0000' % (cdump,cdump)
        dep_dict = {'type': 'data', 'data': data}
        deps.append(rocoto.add_dependency(dep_dict))
        data = '&ROTDIR;/%swave.@Y@m@d/@H/rundata/%swave.out_grd.ant_9km.@Y@m@d.@H0000' % (cdump,cdump)
        dep_dict = {'type': 'data', 'data': data}
        deps.append(rocoto.add_dependency(dep_dict))
        dependencies = rocoto.create_dependency(dep_condition='and', dep=deps)
        task = wfu.create_wf_task('wavepostsbs', cdump=cdump, envar=envars, dependency=dependencies)
        dict_tasks['%swavepostsbs' % cdump] = task

    # wavegempaksbs
    #if do_wave in ['Y', 'YES'] and do_gempak in ['Y', 'YES'] and cdump in ['gfs']:
    #    deps = []
    #    data = '&ROTDIR;/%swave.@Y@m@d/@H/%swave.t@Hz.glo_10m.10m.f000.grib2' % (cdump,cdump)
    #    dep_dict = {'type': 'data', 'data': data}
    #    deps.append(rocoto.add_dependency(dep_dict))
    #    data = '&ROTDIR;/%swave.@Y@m@d/@H/%swave.t@Hz.aoc_9km.9km.f000.grib2' % (cdump,cdump)
    #    dep_dict = {'type': 'data', 'data': data}
    #    deps.append(rocoto.add_dependency(dep_dict))
    #    data = '&ROTDIR;/%swave.@Y@m@d/@H/%swave.t@Hz.ant_9km.9km.f000.grib2' % (cdump,cdump)
    #    dep_dict = {'type': 'data', 'data': data}
    #    deps.append(rocoto.add_dependency(dep_dict))
    #    dependencies = rocoto.create_dependency(dep_condition='and', dep=deps)
    #    task = wfu.create_wf_task('wavegempaksbs', cdump=cdump, envar=envars, dependency=dependencies)
    #    dict_tasks['%swavegempaksbs' % cdump] = task

    # waveawipssbs
    #if do_wave in ['Y', 'YES'] and do_awips in ['Y', 'YES'] and cdump in ['gfs']:
    #    deps = []
    #    data = '&ROTDIR;/%swave.@Y@m@d/@H/%swave.t@Hz.glo_10m.10m.f000.grib2' % (cdump,cdump)
    #    dep_dict = {'type': 'data', 'data': data}
    #    deps.append(rocoto.add_dependency(dep_dict))
    #    data = '&ROTDIR;/%swave.@Y@m@d/@H/%swave.t@Hz.aoc_9km.9km.f000.grib2' % (cdump,cdump)
    #    dep_dict = {'type': 'data', 'data': data}
    #    deps.append(rocoto.add_dependency(dep_dict))
    #    data = '&ROTDIR;/%swave.@Y@m@d/@H/%swave.t@Hz.ant_9km.9km.f000.grib2' % (cdump,cdump)
    #    dep_dict = {'type': 'data', 'data': data}
    #    deps.append(rocoto.add_dependency(dep_dict))
    #    dependencies = rocoto.create_dependency(dep_condition='and', dep=deps)
    #    task = wfu.create_wf_task('waveawipssbs', cdump=cdump, envar=envars, dependency=dependencies)
    #    dict_tasks['%swaveawipssbs' % cdump] = task

    # wavepost
    #if do_wave in ['Y', 'YES']:
    #    deps = []
    #    dep_dict = {'type':'task', 'name':'%sfcst' % cdump}
    #    deps.append(rocoto.add_dependency(dep_dict))
    #    dep_dict = {'type':'task', 'name':'%swavepostsbs' % cdump}
    #    deps.append(rocoto.add_dependency(dep_dict))
    #    dependencies = rocoto.create_dependency(dep_condition='and', dep=deps)
    #    task = wfu.create_wf_task('wavepost', cdump=cdump, envar=envars, dependency=dependencies)
    #    dict_tasks['%swavepost' % cdump] = task

    # waveawips
    #if do_wave in ['Y', 'YES'] and do_awips in ['Y', 'YES'] and cdump in ['gfs']:
    #    deps = []
    #    dep_dict = {'type':'task', 'name':'%swavepost' % cdump}
    #    deps.append(rocoto.add_dependency(dep_dict))
    #    dependencies = rocoto.create_dependency(dep=deps)
    #    task = wfu.create_wf_task('waveawips', cdump=cdump, envar=envars, dependency=dependencies)
    #    dict_tasks['%swaveawips' % cdump] = task

    # wavestat
    #if do_wave in ['Y', 'YES']:
    #    deps = []
    #    dep_dict = {'type':'task', 'name':'%swavepost' % cdump}
    #    deps.append(rocoto.add_dependency(dep_dict))
    #    dependencies = rocoto.create_dependency(dep=deps)
    #    task = wfu.create_wf_task('wavestat', cdump=cdump, envar=envars, dependency=dependencies)
    #    dict_tasks['%swavestat' % cdump] = task

    # vrfy
    deps = []
    dep_dict = {'type': 'metatask', 'name': '%spost' % cdump}
    deps.append(rocoto.add_dependency(dep_dict))
    dependencies = rocoto.create_dependency(dep=deps)
    task = wfu.create_wf_task('vrfy', cdump=cdump, envar=envars, dependency=dependencies)

    dict_tasks['%svrfy' % cdump] = task

    # metp
    if cdump in ['gfs']:
        deps = []
        dep_dict = {'type':'metatask', 'name':'%spost' % cdump}
        deps.append(rocoto.add_dependency(dep_dict))
        dep_dict = {'type':'task', 'name':'%sarch' % cdump, 'offset':'-&INTERVAL_GFS;'}
        deps.append(rocoto.add_dependency(dep_dict))
        dependencies = rocoto.create_dependency(dep_condition='and', dep=deps)
        metpcase = rocoto.create_envar(name='METPCASE', value='#metpcase#')
        metpenvars = envars + [metpcase]
        varname1 = 'metpcase'
        varval1 = 'g2g1 g2o1 pcp1'
        task = wfu.create_wf_task('metp', cdump=cdump, envar=metpenvars, dependency=dependencies,
                                   metatask='metp', varname=varname1, varval=varval1)
        dict_tasks['%smetp' % cdump] = task

    #postsnd
    if cdump in ['gfs'] and do_bufrsnd in ['Y', 'YES']:
        deps = []
        dep_dict = {'type': 'task', 'name': '%sfcst' % cdump}
        deps.append(rocoto.add_dependency(dep_dict))
        dependencies = rocoto.create_dependency(dep=deps)
        task = wfu.create_wf_task('postsnd', cdump=cdump, envar=envars, dependency=dependencies)

        dict_tasks['%spostsnd' % cdump] = task

    # awips
    if cdump in ['gfs'] and do_awips in ['Y', 'YES']:
        deps = []
        data = '&ROTDIR;/%s.@Y@m@d/@H/%s.t@Hz.sfluxgrb#dep#.grib2.idx' % (cdump, cdump)
        dep_dict = {'type': 'data', 'data': data}
        deps.append(rocoto.add_dependency(dep_dict))
        dep_dict = {'type': 'metatask', 'name': '%spost' % cdump}
        deps.append(rocoto.add_dependency(dep_dict))
        dependencies = rocoto.create_dependency(dep_condition='or', dep=deps)
        fhrgrp = rocoto.create_envar(name='FHRGRP', value='#grp#')
        fhrlst = rocoto.create_envar(name='FHRLST', value='#lst#')
        ROTDIR = rocoto.create_envar(name='ROTDIR', value='&ROTDIR;')
        awipsenvars = envars + [fhrgrp] + [fhrlst] + [ROTDIR]
        varname1, varname2, varname3 = 'grp', 'dep', 'lst'
        varval1, varval2, varval3 = get_awipsgroups(dict_configs['awips'], cdump=cdump)
        vardict = {varname2: varval2, varname3: varval3}
        task = wfu.create_wf_task('awips', cdump=cdump, envar=awipsenvars, dependency=dependencies,
                                  metatask='awips', varname=varname1, varval=varval1, vardict=vardict)

        dict_tasks['%sawips' % cdump] = task

    # gempak
    if cdump in ['gfs'] and do_gempak in ['Y', 'YES']:
        deps = []
        dep_dict = {'type': 'metatask', 'name': '%spost' % cdump}
        deps.append(rocoto.add_dependency(dep_dict))
        dependencies = rocoto.create_dependency(dep=deps)
        task = wfu.create_wf_task('gempak', cdump=cdump, envar=envars, dependency=dependencies)

        dict_tasks['%sgempak' % cdump] = task

    # arch
    deps = []
    dep_dict = {'type': 'task', 'name': '%svrfy' % cdump}
    deps.append(rocoto.add_dependency(dep_dict))
    dep_dict = {'type': 'streq', 'left': '&ARCHIVE_TO_HPSS;', 'right': 'YES'}
    deps.append(rocoto.add_dependency(dep_dict))
    dependencies = rocoto.create_dependency(dep_condition='and', dep=deps)
    task = wfu.create_wf_task('arch', cdump=cdump, envar=envars, dependency=dependencies)

    dict_tasks['%sarch' % cdump] = task

    return dict_tasks


def get_hyb_tasks(dict_configs, cycledef='enkf'):
    '''
        Create Hybrid tasks
    '''

    # Determine groups based on ensemble size and grouping
    base = dict_configs['base']
    nens = base['NMEM_ENKF']
    lobsdiag_forenkf = base.get('lobsdiag_forenkf', '.false.').upper()
    eupd_cyc = base.get('EUPD_CYC', 'gdas').upper()

    eobs = dict_configs['eobs']
    nens_eomg = eobs['NMEM_EOMGGRP']
    neomg_grps = nens / nens_eomg
    EOMGGROUPS = ' '.join(['%02d' % x for x in range(1, neomg_grps + 1)])

    efcs = dict_configs['efcs']
    nens_efcs = efcs['NMEM_EFCSGRP']
    nefcs_grps = nens / nens_efcs
    EFCSGROUPS = ' '.join(['%02d' % x for x in range(1, nefcs_grps + 1)])

    earc = dict_configs['earc']
    nens_earc = earc['NMEM_EARCGRP']
    nearc_grps = nens / nens_earc
    EARCGROUPS = ' '.join(['%02d' % x for x in range(0, nearc_grps + 1)])

    envars = []
    if wfu.get_scheduler(wfu.detectMachine()) in ['slurm']:
       envars.append(rocoto.create_envar(name='SLURM_SET', value='YES'))
    envars.append(rocoto.create_envar(name='RUN_ENVIR', value='&RUN_ENVIR;'))
    envars.append(rocoto.create_envar(name='HOMEgfs', value='&HOMEgfs;'))
    envars.append(rocoto.create_envar(name='EXPDIR', value='&EXPDIR;'))
    envars.append(rocoto.create_envar(name='CDATE', value='<cyclestr>@Y@m@d@H</cyclestr>'))
    #envars.append(rocoto.create_envar(name='CDUMP', value='%s' % cdump))
    envars.append(rocoto.create_envar(name='PDY', value='<cyclestr>@Y@m@d</cyclestr>'))
    envars.append(rocoto.create_envar(name='cyc', value='<cyclestr>@H</cyclestr>'))

    ensgrp = rocoto.create_envar(name='ENSGRP', value='#grp#')

    dict_tasks = OrderedDict()

    if eupd_cyc in ['BOTH']:
        cdumps = ['gfs', 'gdas']
    elif eupd_cyc in ['GFS']:
        cdumps = ['gfs']
    elif eupd_cyc in ['GDAS']:
        cdumps = ['gdas']

    for cdump in cdumps:

        envar_cdump = rocoto.create_envar(name='CDUMP', value='%s' % cdump)
        envars1 = envars + [envar_cdump]

        # eobs
        deps = []
        dep_dict = {'type': 'task', 'name': '%sprep' % cdump}
        deps.append(rocoto.add_dependency(dep_dict))
        dep_dict = {'type': 'metatask', 'name': '%sepmn' % 'gdas', 'offset': '-06:00:00'}
        deps.append(rocoto.add_dependency(dep_dict))
        dependencies = rocoto.create_dependency(dep_condition='and', dep=deps)
        task = wfu.create_wf_task('eobs', cdump=cdump, envar=envars1, dependency=dependencies, cycledef=cycledef)

        dict_tasks['%seobs' % cdump] = task

        # eomn, eomg
        if lobsdiag_forenkf in ['.F.', '.FALSE.']:
            deps = []
            dep_dict = {'type': 'task', 'name': '%seobs' % cdump}
            deps.append(rocoto.add_dependency(dep_dict))
            dependencies = rocoto.create_dependency(dep=deps)
            eomgenvars= envars1 + [ensgrp]
            task = wfu.create_wf_task('eomg', cdump=cdump, envar=eomgenvars, dependency=dependencies,
                                      metatask='eomn', varname='grp', varval=EOMGGROUPS, cycledef=cycledef)

            dict_tasks['%seomn' % cdump] = task

        # eupd
        deps = []
        if lobsdiag_forenkf in ['.F.', '.FALSE.']:
            dep_dict = {'type': 'metatask', 'name': '%seomn' % cdump}
        else:
            dep_dict = {'type': 'task', 'name': '%seobs' % cdump}
        deps.append(rocoto.add_dependency(dep_dict))
        dependencies = rocoto.create_dependency(dep=deps)
        task = wfu.create_wf_task('eupd', cdump=cdump, envar=envars1, dependency=dependencies, cycledef=cycledef)

        dict_tasks['%seupd' % cdump] = task

    # All hybrid tasks beyond this point are always executed in the GDAS cycle
    cdump = 'gdas'
    envar_cdump = rocoto.create_envar(name='CDUMP', value='%s' % cdump)
    envars1 = envars + [envar_cdump]
    cdump_eupd = 'gfs' if eupd_cyc in ['GFS'] else 'gdas'

    # ecmn, ecen
    deps1 = []
    data = '&ROTDIR;/%s.@Y@m@d/@H/%s.t@Hz.loganl.txt' % (cdump, cdump)
    dep_dict = {'type': 'data', 'data': data}
    deps1.append(rocoto.add_dependency(dep_dict))
    dep_dict = {'type': 'task', 'name': '%sanal' % cdump}
    deps1.append(rocoto.add_dependency(dep_dict))
    dependencies1 = rocoto.create_dependency(dep_condition='or', dep=deps1)

    deps2 = []
    deps2 = dependencies1    
    dep_dict = {'type': 'task', 'name': '%seupd' % cdump_eupd}
    deps2.append(rocoto.add_dependency(dep_dict))
    dependencies2 = rocoto.create_dependency(dep_condition='and', dep=deps2)

    fhrgrp = rocoto.create_envar(name='FHRGRP', value='#grp#')
    fhrlst = rocoto.create_envar(name='FHRLST', value='#lst#')
    ecenenvars = envars1 + [fhrgrp] + [fhrlst]
    varname1, varname2, varname3 = 'grp', 'dep', 'lst'
    varval1, varval2, varval3 = get_ecengroups(dict_configs, dict_configs['ecen'], cdump=cdump)
    vardict = {varname2: varval2, varname3: varval3}
    task = wfu.create_wf_task('ecen', cdump=cdump, envar=ecenenvars, dependency=dependencies2,
                              metatask='ecmn', varname=varname1, varval=varval1, vardict=vardict)

    dict_tasks['%secmn' % cdump] = task

    # esfc
    deps1 = []
    data = '&ROTDIR;/%s.@Y@m@d/@H/%s.t@Hz.loganl.txt' % (cdump, cdump)
    dep_dict = {'type': 'data', 'data': data}
    deps1.append(rocoto.add_dependency(dep_dict))
    dep_dict = {'type': 'task', 'name': '%sanal' % cdump}
    deps1.append(rocoto.add_dependency(dep_dict))
    dependencies1 = rocoto.create_dependency(dep_condition='or', dep=deps1)

    deps2 = []
    deps2 = dependencies1
    dep_dict = {'type': 'task', 'name': '%seupd' % cdump_eupd}
    deps2.append(rocoto.add_dependency(dep_dict))
    dependencies2 = rocoto.create_dependency(dep_condition='and', dep=deps2)
    task = wfu.create_wf_task('esfc', cdump=cdump, envar=envars1, dependency=dependencies2, cycledef=cycledef)

    dict_tasks['%sesfc' % cdump] = task


    # efmn, efcs
    deps1 = []
    dep_dict = {'type': 'metatask', 'name': '%secmn' % cdump}
    deps1.append(rocoto.add_dependency(dep_dict))
    dep_dict = {'type': 'task', 'name': '%sesfc' % cdump}
    deps1.append(rocoto.add_dependency(dep_dict))
    dependencies1 = rocoto.create_dependency(dep_condition='and', dep=deps1)
  
    deps2 = []
    deps2 = dependencies1
    dep_dict = {'type': 'cycleexist', 'condition': 'not', 'offset': '-06:00:00'}
    deps2.append(rocoto.add_dependency(dep_dict))
    dependencies2 = rocoto.create_dependency(dep_condition='or', dep=deps2)

    efcsenvars = envars1 + [ensgrp]
    task = wfu.create_wf_task('efcs', cdump=cdump, envar=efcsenvars, dependency=dependencies2,
                              metatask='efmn', varname='grp', varval=EFCSGROUPS, cycledef=cycledef)

    dict_tasks['%sefmn' % cdump] = task

    # epmn, epos
    deps = []
    dep_dict = {'type': 'metatask', 'name': '%sefmn' % cdump}
    deps.append(rocoto.add_dependency(dep_dict))
    dependencies = rocoto.create_dependency(dep=deps)
    fhrgrp = rocoto.create_envar(name='FHRGRP', value='#grp#')
    fhrlst = rocoto.create_envar(name='FHRLST', value='#lst#')
    eposenvars = envars1 + [fhrgrp] + [fhrlst]
    varname1, varname2, varname3 = 'grp', 'dep', 'lst'
    varval1, varval2, varval3 = get_eposgroups(dict_configs['epos'], cdump=cdump)
    vardict = {varname2: varval2, varname3: varval3}
    task = wfu.create_wf_task('epos', cdump=cdump, envar=eposenvars, dependency=dependencies,
                              metatask='epmn', varname=varname1, varval=varval1, vardict=vardict)

    dict_tasks['%sepmn' % cdump] = task

    # eamn, earc
    deps = []
    dep_dict = {'type': 'metatask', 'name': '%sepmn' % cdump}
    deps.append(rocoto.add_dependency(dep_dict))
    dependencies = rocoto.create_dependency(dep=deps)
    earcenvars = envars1 + [ensgrp]
    task = wfu.create_wf_task('earc', cdump=cdump, envar=earcenvars, dependency=dependencies,
                              metatask='eamn', varname='grp', varval=EARCGROUPS, cycledef=cycledef)

    dict_tasks['%seamn' % cdump] = task

    return  dict_tasks


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
    strings.append('\t<cycledef group="enkf" >&SDATE;     &EDATE;     06:00:00</cycledef>\n')
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


def get_postgroups(post, cdump='gdas'):

    fhmin = post['FHMIN']
    fhmax = post['FHMAX']
    fhout = post['FHOUT']

    # Get a list of all forecast hours
    if cdump in ['gdas']:
        fhrs = range(fhmin, fhmax+fhout, fhout)
    elif cdump in ['gfs']:
        fhmax = np.max([post['FHMAX_GFS_00'],post['FHMAX_GFS_06'],post['FHMAX_GFS_12'],post['FHMAX_GFS_18']])
        fhout = post['FHOUT_GFS']
        fhmax_hf = post['FHMAX_HF_GFS']
        fhout_hf = post['FHOUT_HF_GFS']
        fhrs_hf = range(fhmin, fhmax_hf+fhout_hf, fhout_hf)
        fhrs = fhrs_hf + range(fhrs_hf[-1]+fhout, fhmax+fhout, fhout)

    npostgrp = post['NPOSTGRP']
    ngrps = npostgrp if len(fhrs) > npostgrp else len(fhrs)

    fhrs = ['f%03d' % f for f in fhrs]
    fhrs = np.array_split(fhrs, ngrps)
    fhrs = [f.tolist() for f in fhrs]

    fhrgrp = ' '.join(['%03d' % x for x in range(0, ngrps+1)])
    fhrdep = ' '.join(['anl'] + [f[-1] for f in fhrs])
    fhrlst = ' '.join(['anl'] + ['_'.join(f) for f in fhrs])

    return fhrgrp, fhrdep, fhrlst

def get_awipsgroups(awips, cdump='gdas'):

    fhmin = awips['FHMIN']
    fhmax = awips['FHMAX']
    fhout = awips['FHOUT']

    # Get a list of all forecast hours
    if cdump in ['gdas']:
        fhrs = range(fhmin, fhmax+fhout, fhout)
    elif cdump in ['gfs']:
        fhmax = np.max([awips['FHMAX_GFS_00'],awips['FHMAX_GFS_06'],awips['FHMAX_GFS_12'],awips['FHMAX_GFS_18']])
        fhout = awips['FHOUT_GFS']
        fhmax_hf = awips['FHMAX_HF_GFS']
        fhout_hf = awips['FHOUT_HF_GFS']
        if fhmax > 240:
            fhmax = 240
        if fhmax_hf > 240:
            fhmax_hf = 240
        fhrs_hf = range(fhmin, fhmax_hf+fhout_hf, fhout_hf)
        fhrs = fhrs_hf + range(fhrs_hf[-1]+fhout, fhmax+fhout, fhout)

    nawipsgrp = awips['NAWIPSGRP']
    ngrps = nawipsgrp if len(fhrs) > nawipsgrp else len(fhrs)

    fhrs = ['f%03d' % f for f in fhrs]
    fhrs = np.array_split(fhrs, ngrps)
    fhrs = [f.tolist() for f in fhrs]

    fhrgrp = ' '.join(['%03d' % x for x in range(0, ngrps)])
    fhrdep = ' '.join([f[-1] for f in fhrs])
    fhrlst = ' '.join(['_'.join(f) for f in fhrs])

    return fhrgrp, fhrdep, fhrlst

def get_ecengroups(dict_configs, ecen, cdump='gdas'):

    base = dict_configs['base']

    if base.get('DOIAU_ENKF', 'NO') == 'YES' : 
        fhrs = list(base.get('IAUFHRS','6').split(','))
        ifhrs = ['f00%01s' % f for f in fhrs]
        ifhrs0 = ifhrs[0]
        nfhrs = len(fhrs)

        ifhrs = ['f00%01s' % f for f in fhrs]
        ifhrs0 = ifhrs[0]
        nfhrs = len(fhrs)

        necengrp = ecen['NECENGRP']
        ngrps = necengrp if len(fhrs) > necengrp else len(fhrs)

        ifhrs = np.array_split(ifhrs, ngrps)

        fhrgrp = ' '.join(['%03d' % x for x in range(0, ngrps)])
        fhrdep = ' '.join([f[-1] for f in ifhrs])
        fhrlst = ' '.join(['_'.join(f) for f in ifhrs])

    else:
        fhrgrp='000'
        fhrdep='f006'
        fhrlst='f006'

    return fhrgrp, fhrdep, fhrlst

def get_eposgroups(epos, cdump='gdas'):

    fhmin = epos['FHMIN_ENKF']
    fhmax = epos['FHMAX_ENKF']
    fhout = epos['FHOUT_ENKF']
    fhrs = range(fhmin, fhmax+fhout, fhout)

    neposgrp = epos['NEPOSGRP']
    ngrps = neposgrp if len(fhrs) > neposgrp else len(fhrs)

    fhrs = ['f%03d' % f for f in fhrs]
    fhrs = np.array_split(fhrs, ngrps)
    fhrs = [f.tolist() for f in fhrs]

    fhrgrp = ' '.join(['%03d' % x for x in range(0, ngrps)])
    fhrdep = ' '.join([f[-1] for f in fhrs])
    fhrlst = ' '.join(['_'.join(f) for f in fhrs])

    return fhrgrp, fhrdep, fhrlst


def dict_to_strings(dict_in):

    strings = []
    for key in dict_in.keys():
        strings.append(dict_in[key])
        strings.append('\n')

    return ''.join(strings)


def create_xml(dict_configs):
    '''
        Given an dictionary of sourced config files,
        create the workflow XML
    '''

    from  __builtin__ import any as b_any

    base = dict_configs['base']
    dohybvar = base.get('DOHYBVAR', 'NO').upper()
    gfs_cyc = base.get('gfs_cyc', 0)
    eupd_cyc = base.get('EUPD_CYC', 'gdas').upper()

    # Start collecting workflow pieces
    preamble = get_preamble()
    definitions = get_definitions(base)
    workflow_header = get_workflow_header(base)
    workflow_footer = get_workflow_footer()

    # Get GDAS related entities, resources, workflow
    dict_gdas_resources = get_gdasgfs_resources(dict_configs)
    dict_gdas_tasks = get_gdasgfs_tasks(dict_configs)

    # Get hybrid related entities, resources, workflow
    if dohybvar in ['Y', 'YES']:

        dict_hyb_resources = get_hyb_resources(dict_configs)
        dict_hyb_tasks = get_hyb_tasks(dict_configs)

        # Removes <memory>&MEMORY_JOB_DUMP</memory> post mortem from hyb tasks
        hyp_tasks = {'gdaseobs':'gdaseobs', 'gdaseomg':'gdaseomn', 'gdaseupd':'gdaseupd','gdasecen':'gdasecmn','gdasesfc':'gdasesfc','gdasefcs':'gdasefmn','gdasepos':'gdasepmn','gdasearc':'gdaseamn'}
        for each_task, each_resource_string in dict_hyb_resources.iteritems():
            #print each_task,hyp_tasks[each_task]
            #print dict_hyb_tasks[hyp_tasks[each_task]]
            if 'MEMORY' not in each_resource_string:
                if each_task in dict_hyb_tasks:
                    temp_task_string = []
                    for each_line in re.split(r'(\s+)', dict_hyb_tasks[each_task]):
                        if 'memory' not in each_line:
                             temp_task_string.append(each_line)
                    dict_hyb_tasks[each_task] = ''.join(temp_task_string)
                if hyp_tasks[each_task] in dict_hyb_tasks:
                    temp_task_string = []
                    for each_line in re.split(r'(\s+)', dict_hyb_tasks[hyp_tasks[each_task]]):
                        if 'memory' not in each_line:
                             temp_task_string.append(each_line)
                    dict_hyb_tasks[hyp_tasks[each_task]] = ''.join(temp_task_string)

    # Get GFS cycle related entities, resources, workflow
    dict_gfs_resources = get_gdasgfs_resources(dict_configs, cdump='gfs')
    dict_gfs_tasks = get_gdasgfs_tasks(dict_configs, cdump='gfs')

    # Removes <memory>&MEMORY_JOB_DUMP</memory> post mortem from gdas tasks
    for each_task, each_resource_string in dict_gdas_resources.iteritems():
        if each_task not in dict_gdas_tasks:
            continue
        if 'MEMORY' not in each_resource_string:
            temp_task_string = []
            for each_line in re.split(r'(\s+)', dict_gdas_tasks[each_task]):
                if 'memory' not in each_line:
                     temp_task_string.append(each_line)
            dict_gdas_tasks[each_task] = ''.join(temp_task_string)

    # Removes <memory>&MEMORY_JOB_DUMP</memory> post mortem from gfs tasks
    for each_task, each_resource_string in dict_gfs_resources.iteritems():
        if each_task not in dict_gfs_tasks:
            continue
        if 'MEMORY' not in each_resource_string:
            temp_task_string = []
            for each_line in re.split(r'(\s+)', dict_gfs_tasks[each_task]):
                if 'memory' not in each_line:
                     temp_task_string.append(each_line)
            dict_gfs_tasks[each_task] = ''.join(temp_task_string)

    # Put together the XML file
    xmlfile = []

    xmlfile.append(preamble)

    xmlfile.append(definitions)

    xmlfile.append(dict_to_strings(dict_gdas_resources))

    if dohybvar in ['Y', 'YES']:
        xmlfile.append(dict_to_strings(dict_hyb_resources))

    if gfs_cyc != 0:
        xmlfile.append(dict_to_strings(dict_gfs_resources))
    elif gfs_cyc == 0 and dohybvar in ['Y', 'YES'] and eupd_cyc in ['BOTH', 'GFS']:
        xmlfile.append(dict_gfs_resources['gfsprep'])

    xmlfile.append(workflow_header)

    xmlfile.append(dict_to_strings(dict_gdas_tasks))

    if dohybvar in ['Y', 'YES']:
        xmlfile.append(dict_to_strings(dict_hyb_tasks))

    if gfs_cyc != 0:
        xmlfile.append(dict_to_strings(dict_gfs_tasks))
    elif gfs_cyc == 0 and dohybvar in ['Y', 'YES'] and eupd_cyc in ['BOTH', 'GFS']:
        xmlfile.append(dict_gfs_tasks['gfsprep'])
        xmlfile.append('\n')

    xmlfile.append(wfu.create_firstcyc_task())

    xmlfile.append(workflow_footer)

    # Write the XML file
    fh = open('%s/%s.xml' % (base['EXPDIR'], base['PSLOT']), 'w')
    fh.write(''.join(xmlfile))
    fh.close()

    return


if __name__ == '__main__':
    main()
    sys.exit(0)
