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

    gfs_steps = ['prep', 'anal', 'fcst', 'postsnd', 'post', 'awips', 'gempak', 'vrfy', 'arch']
    hyb_steps = ['eobs', 'eomg', 'eupd', 'ecen', 'efcs', 'epos', 'earc']

    steps = gfs_steps + hyb_steps if _base.get('DOHYBVAR', 'NO') == 'YES' else gfs_steps

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
    strings.append('\t<!ENTITY SCHEDULER  "%s">\n' % wfu.get_scheduler(base['machine']))
    strings.append('\n')
    strings.append('\t<!-- Toggle HPSS archiving -->\n')
    strings.append('\t<!ENTITY ARCHIVE_TO_HPSS "YES">\n')
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

    base = dict_configs['base']
    machine = base.get('machine', 'WCOSS_C')
    do_bufrsnd = base.get('DO_BUFRSND', 'NO').upper()
    do_gempak = base.get('DO_GEMPAK', 'NO').upper()
    do_awips = base.get('DO_AWIPS', 'NO').upper()

    tasks = ['prep', 'anal', 'fcst', 'post', 'vrfy', 'arch']

    if cdump in ['gfs'] and do_bufrsnd in ['Y', 'YES']:
        tasks += ['postsnd']
    if cdump in ['gfs'] and do_gempak in ['Y', 'YES']:
        tasks += ['gempak']
    if cdump in ['gfs'] and do_awips in ['Y', 'YES']:
        tasks += ['awips']

    dict_resources = OrderedDict()

    for task in tasks:

        cfg = dict_configs[task]

        wtimestr, resstr, queuestr, memstr, natstr = wfu.get_resources(machine, cfg, task, cdump=cdump)
        taskstr = '%s_%s' % (task.upper(), cdump.upper())

        strings = []
        strings.append('\t<!ENTITY QUEUE_%s     "%s">\n' % (taskstr, queuestr))
        strings.append('\t<!ENTITY WALLTIME_%s  "%s">\n' % (taskstr, wtimestr))
        strings.append('\t<!ENTITY RESOURCES_%s "%s">\n' % (taskstr, resstr))
        strings.append('\t<!ENTITY MEMORY_%s    "%s">\n' % (taskstr, memstr))
        strings.append('\t<!ENTITY NATIVE_%s    "%s">\n' % (taskstr, natstr))

        dict_resources['%s%s' % (cdump, task)] = ''.join(strings)

    return dict_resources


def get_hyb_resources(dict_configs):
    '''
        Create hybrid resource entities
    '''

    base = dict_configs['base']
    machine = base.get('machine', 'WCOSS_C')
    lobsdiag_forenkf = base.get('lobsdiag_forenkf', '.false.').upper()
    eupd_cyc= base.get('EUPD_CYC', 'gdas').upper()

    dict_resources = OrderedDict()

    # These tasks can be run in either or both cycles
    tasks1 = ['eobs', 'eomg', 'eupd']
    if lobsdiag_forenkf in ['.T.', '.TRUE.']:
        tasks.remove('eomg')

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
            strings.append('\t<!ENTITY MEMORY_%s    "%s">\n' % (taskstr, memstr))
            strings.append('\t<!ENTITY NATIVE_%s    "%s">\n' % (taskstr, natstr))

            dict_resources['%s%s' % (cdump, task)] = ''.join(strings)


    # These tasks are always run as part of the GDAS cycle
    cdump = 'gdas'
    tasks2 = ['ecen', 'efcs', 'epos', 'earc']
    for task in tasks2:

        cfg = dict_configs[task]

        wtimestr, resstr, queuestr, memstr, natstr = wfu.get_resources(machine, cfg, task, cdump=cdump)

        taskstr = '%s_%s' % (task.upper(), cdump.upper())

        strings = []
        strings.append('\t<!ENTITY QUEUE_%s     "%s">\n' % (taskstr, queuestr))
        strings.append('\t<!ENTITY WALLTIME_%s  "%s">\n' % (taskstr, wtimestr))
        strings.append('\t<!ENTITY RESOURCES_%s "%s">\n' % (taskstr, resstr))
        strings.append('\t<!ENTITY MEMORY_%s    "%s">\n' % (taskstr, memstr))
        strings.append('\t<!ENTITY NATIVE_%s    "%s">\n' % (taskstr, natstr))

        dict_resources['%s%s' % (cdump, task)] = ''.join(strings)

    return dict_resources


def get_gdasgfs_tasks(dict_configs, cdump='gdas'):
    '''
        Create GDAS or GFS tasks
    '''

    envars = []
    envars.append(rocoto.create_envar(name='RUN_ENVIR', value='&RUN_ENVIR;'))
    envars.append(rocoto.create_envar(name='HOMEgfs', value='&HOMEgfs;'))
    envars.append(rocoto.create_envar(name='EXPDIR', value='&EXPDIR;'))
    envars.append(rocoto.create_envar(name='CDATE', value='<cyclestr>@Y@m@d@H</cyclestr>'))
    envars.append(rocoto.create_envar(name='CDUMP', value='%s' % cdump))
    envars.append(rocoto.create_envar(name='PDY', value='<cyclestr>@Y@m@d</cyclestr>'))
    envars.append(rocoto.create_envar(name='cyc', value='<cyclestr>@H</cyclestr>'))

    base = dict_configs['base']
    gfs_cyc = base.get('gfs_cyc', 0)
    dohybvar = base.get('DOHYBVAR', 'NO').upper()
    eupd_cyc = base.get('EUPD_CYC', 'gdas').upper()
    do_bufrsnd = base.get('DO_BUFRSND', 'NO').upper()
    do_gempak = base.get('DO_GEMPAK', 'NO').upper()
    do_awips = base.get('DO_AWIPS', 'NO').upper()
    dumpsuffix = base.get('DUMP_SUFFIX', '')

    dict_tasks = OrderedDict()

    # prep
    deps = []
    dep_dict = {'type': 'metatask', 'name': '%spost' % 'gdas', 'offset': '-06:00:00'}
    deps.append(rocoto.add_dependency(dep_dict))
    data = '&ROTDIR;/gdas.@Y@m@d/@H/gdas.t@Hz.atmf009.nemsio'
    dep_dict = {'type': 'data', 'data': data, 'offset': '-06:00:00'}
    deps.append(rocoto.add_dependency(dep_dict))
    data = '&DMPDIR;/@Y@m@d@H/%s%s/%s.t@Hz.updated.status.tm00.bufr_d' % (cdump, dumpsuffix, cdump)
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

    # fcst
    deps = []
    dep_dict = {'type': 'task', 'name': '%sanal' % cdump}
    deps.append(rocoto.add_dependency(dep_dict))
    if cdump in ['gdas']:
        dep_dict = {'type': 'cycleexist', 'condition': 'not', 'offset': '-06:00:00'}
        deps.append(rocoto.add_dependency(dep_dict))
        dependencies = rocoto.create_dependency(dep_condition='or', dep=deps)
    elif cdump in ['gfs']:
        dependencies = rocoto.create_dependency(dep=deps)
    task = wfu.create_wf_task('fcst', cdump=cdump, envar=envars, dependency=dependencies)

    dict_tasks['%sfcst' % cdump] = task

    # post
    deps = []
    data = '&ROTDIR;/%s.@Y@m@d/@H/%s.t@Hz.log#dep#.nemsio' % (cdump, cdump)
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

    # vrfy
    deps = []
    dep_dict = {'type': 'metatask', 'name': '%spost' % cdump}
    deps.append(rocoto.add_dependency(dep_dict))
    dependencies = rocoto.create_dependency(dep=deps)
    task = wfu.create_wf_task('vrfy', cdump=cdump, envar=envars, dependency=dependencies)

    dict_tasks['%svrfy' % cdump] = task


    if cdump in ['gfs'] and do_bufrsnd in ['Y', 'YES']:
        #postsnd
        deps = []
        dep_dict = {'type': 'task', 'name': '%sfcst' % cdump}
        deps.append(rocoto.add_dependency(dep_dict))
        dependencies = rocoto.create_dependency(dep=deps)
        task = wfu.create_wf_task('postsnd', cdump=cdump, envar=envars, dependency=dependencies)

        dict_tasks['%spostsnd' % cdump] = task

    if cdump in ['gfs'] and do_awips in ['Y', 'YES']:
        # awips
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

    if cdump in ['gfs'] and do_gempak in ['Y', 'YES']:
        # gempak
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

    # ecen
    deps = []
    dep_dict = {'type': 'task', 'name': '%sanal' % cdump}
    deps.append(rocoto.add_dependency(dep_dict))
    dep_dict = {'type': 'task', 'name': '%seupd' % cdump_eupd}
    deps.append(rocoto.add_dependency(dep_dict))
    dependencies = rocoto.create_dependency(dep_condition='and', dep=deps)
    task = wfu.create_wf_task('ecen', cdump=cdump, envar=envars1, dependency=dependencies, cycledef=cycledef)

    dict_tasks['%secen' % cdump] = task

    # efmn, efcs
    deps = []
    dep_dict = {'type': 'task', 'name': '%secen' % cdump}
    deps.append(rocoto.add_dependency(dep_dict))
    dep_dict = {'type': 'cycleexist', 'condition': 'not', 'offset': '-06:00:00'}
    deps.append(rocoto.add_dependency(dep_dict))
    dependencies = rocoto.create_dependency(dep_condition='or', dep=deps)
    efcsenvars = envars1 + [ensgrp]
    task = wfu.create_wf_task('efcs', cdump=cdump, envar=efcsenvars, dependency=dependencies,
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
    fhrdep = ' '.join(['f000'] + [f[-1] for f in fhrs])
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

    # Get GFS cycle related entities, resources, workflow
    dict_gfs_resources = get_gdasgfs_resources(dict_configs, cdump='gfs')
    dict_gfs_tasks = get_gdasgfs_tasks(dict_configs, cdump='gfs')

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
