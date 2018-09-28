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
import numpy as np
from datetime import datetime
from argparse import ArgumentParser, ArgumentDefaultsHelpFormatter
import rocoto
import workflow_utils as wfu


taskplan = ['getic', 'fv3ic', 'fcst', 'post', 'vrfy', 'arch']

def main():
    parser = ArgumentParser(description='Setup XML workflow and CRONTAB for a forecast only experiment.', formatter_class=ArgumentDefaultsHelpFormatter)
    parser.add_argument('--expdir',help='full path to experiment directory containing config files', type=str, required=False, default=os.environ['PWD'])
    parser.add_argument('--cdump',help='cycle to run forecasts', type=str, choices=['gdas', 'gfs'], default='gfs', required=False)

    args = parser.parse_args()

    configs = wfu.get_configs(args.expdir)

    _base = wfu.config_parser([wfu.find_config('config.base', configs)])

    if not os.path.samefile(args.expdir,_base['EXPDIR']):
        print 'MISMATCH in experiment directories!'
        print 'config.base: EXPDIR = %s' % repr(_base['EXPDIR'])
        print 'input arg:     --expdir = %s' % repr(args.expdir)
        sys.exit(1)

    dict_configs = wfu.source_configs(configs, taskplan)

    dict_configs['base']['CDUMP'] = args.cdump

    # First create workflow XML
    create_xml(dict_configs)

    # Next create the crontab
    wfu.create_crontab(dict_configs['base'])

    return


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
    strings.append('\t<!-- Experiment parameters such as name, cycle, resolution -->\n')
    strings.append('\t<!ENTITY PSLOT    "%s">\n' % base['PSLOT'])
    strings.append('\t<!ENTITY CDUMP    "%s">\n' % base['CDUMP'])
    strings.append('\t<!ENTITY CASE     "%s">\n' % base['CASE'])
    strings.append('\n')
    strings.append('\t<!-- Experiment parameters such as starting, ending dates -->\n')
    strings.append('\t<!ENTITY SDATE    "%s">\n' % base['SDATE'].strftime('%Y%m%d%H%M'))
    strings.append('\t<!ENTITY EDATE    "%s">\n' % base['EDATE'].strftime('%Y%m%d%H%M'))
    if base['INTERVAL'] is None:
        print 'cycle INTERVAL cannot be None'
        sys.exit(1)
    strings.append('\t<!ENTITY INTERVAL "%s">\n' % base['INTERVAL'])
    strings.append('\n')
    strings.append('\t<!-- Run Envrionment -->\n')
    strings.append('\t<!ENTITY RUN_ENVIR "%s">\n' % base['RUN_ENVIR'])
    strings.append('\n')
    strings.append('\t<!-- Experiment related directories -->\n')
    strings.append('\t<!ENTITY EXPDIR "%s">\n' % base['EXPDIR'])
    strings.append('\t<!ENTITY ROTDIR "%s">\n' % base['ROTDIR'])
    strings.append('\t<!ENTITY ICSDIR "%s">\n' % base['ICSDIR'])
    strings.append('\n')
    strings.append('\t<!-- Directories for driving the workflow -->\n')
    strings.append('\t<!ENTITY HOMEgfs  "%s">\n' % base['HOMEgfs'])
    strings.append('\t<!ENTITY JOBS_DIR "%s">\n' % base['BASE_JOB'])
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

    strings.append('\t<!-- BEGIN: Resource requirements for the workflow -->\n')
    strings.append('\n')

    machine = dict_configs['base']['machine']

    for task in taskplan:

        cfg = dict_configs[task]

        wtimestr, resstr, queuestr, memstr, natstr = wfu.get_resources(machine, cfg, task, cdump=cdump)

        taskstr = '%s_%s' % (task.upper(), cdump.upper())

        strings.append('\t<!ENTITY QUEUE_%s     "%s">\n' % (taskstr, queuestr))
        strings.append('\t<!ENTITY WALLTIME_%s  "%s">\n' % (taskstr, wtimestr))
        strings.append('\t<!ENTITY RESOURCES_%s "%s">\n' % (taskstr, resstr))
        strings.append('\t<!ENTITY MEMORY_%s    "%s">\n' % (taskstr, memstr))
        strings.append('\t<!ENTITY NATIVE_%s    "%s">\n' % (taskstr, natstr))

        strings.append('\n')

    strings.append('\t<!-- END: Resource requirements for the workflow -->\n')

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

    fhrgrp = ' '.join(['%03d' % x for x in range(1, ngrps+1)])
    fhrdep = ' '.join([f[-1] for f in fhrs])
    fhrlst = ' '.join(['_'.join(f) for f in fhrs])

    return fhrgrp, fhrdep, fhrlst


def get_workflow(dict_configs, cdump='gdas'):
    '''
        Create tasks for forecast only workflow
    '''

    envars = []
    envars.append(rocoto.create_envar(name='RUN_ENVIR', value='&RUN_ENVIR;'))
    envars.append(rocoto.create_envar(name='HOMEgfs', value='&HOMEgfs;'))
    envars.append(rocoto.create_envar(name='EXPDIR', value='&EXPDIR;'))
    envars.append(rocoto.create_envar(name='CDATE', value='<cyclestr>@Y@m@d@H</cyclestr>'))
    envars.append(rocoto.create_envar(name='CDUMP', value='&CDUMP;'))
    envars.append(rocoto.create_envar(name='PDY', value='<cyclestr>@Y@m@d</cyclestr>'))
    envars.append(rocoto.create_envar(name='cyc', value='<cyclestr>@H</cyclestr>'))

    tasks = []

    # getics
    deps = []
    data = '&ICSDIR;/@Y@m@d@H/&CDUMP;/pgbanl.&CDUMP;.@Y@m@d@H'
    dep_dict = {'type':'data', 'data':data}
    deps.append(rocoto.add_dependency(dep_dict))
    data = '&ICSDIR;/@Y@m@d@H/&CDUMP;/siganl.&CDUMP;.@Y@m@d@H'
    dep_dict = {'type':'data', 'data':data}
    deps.append(rocoto.add_dependency(dep_dict))
    data = '&ICSDIR;/@Y@m@d@H/&CDUMP;/sfcanl.&CDUMP;.@Y@m@d@H'
    dep_dict = {'type':'data', 'data':data}
    deps.append(rocoto.add_dependency(dep_dict))
    deps = rocoto.create_dependency(dep_condition='and', dep=deps)
    dependencies = rocoto.create_dependency(dep_condition='not', dep=deps)
    task = wfu.create_wf_task('getic', cdump=cdump, envar=envars, dependency=dependencies)
    tasks.append(task)
    tasks.append('\n')

    # chgres
    deps = []
    data = '&ICSDIR;/@Y@m@d@H/&CDUMP;/siganl.&CDUMP;.@Y@m@d@H'
    dep_dict = {'type':'data', 'data':data}
    deps.append(rocoto.add_dependency(dep_dict))
    data = '&ICSDIR;/@Y@m@d@H/&CDUMP;/sfcanl.&CDUMP;.@Y@m@d@H'
    dep_dict = {'type':'data', 'data':data}
    deps.append(rocoto.add_dependency(dep_dict))
    dependencies = rocoto.create_dependency(dep_condition='and', dep=deps)

    deps = []
    data = '&ICSDIR;/@Y@m@d@H/&CDUMP;/&CASE;/INPUT/gfs_data.tile6.nc'
    dep_dict = {'type':'data', 'data':data}
    deps.append(rocoto.add_dependency(dep_dict))
    data = '&ICSDIR;/@Y@m@d@H/&CDUMP;/&CASE;/INPUT/sfc_data.tile6.nc'
    dep_dict = {'type':'data', 'data':data}
    deps.append(rocoto.add_dependency(dep_dict))
    deps = rocoto.create_dependency(dep_condition='and', dep=deps)
    dependencies2 = rocoto.create_dependency(dep_condition='not', dep=deps)

    deps = []
    deps.append(dependencies)
    deps.append(dependencies2)
    dependencies = rocoto.create_dependency(dep_condition='and', dep=deps)

    task = wfu.create_wf_task('fv3ic', cdump=cdump, envar=envars, dependency=dependencies)
    tasks.append(task)
    tasks.append('\n')

    # fcst
    deps = []
    data = '&ICSDIR;/@Y@m@d@H/&CDUMP;/&CASE;/INPUT/gfs_data.tile6.nc'
    dep_dict = {'type':'data', 'data':data}
    deps.append(rocoto.add_dependency(dep_dict))
    data = '&ICSDIR;/@Y@m@d@H/&CDUMP;/&CASE;/INPUT/sfc_data.tile6.nc'
    dep_dict = {'type':'data', 'data':data}
    deps.append(rocoto.add_dependency(dep_dict))
    dependencies = rocoto.create_dependency(dep_condition='and', dep=deps)
    task = wfu.create_wf_task('fcst', cdump=cdump, envar=envars, dependency=dependencies)
    tasks.append(task)
    tasks.append('\n')

    # post
    deps = []
    data = '&ROTDIR;/%s.@Y@m@d/@H/%s.t@Hz.log#dep#.nemsio' % (cdump, cdump)
    dep_dict = {'type': 'data', 'data': data}
    deps.append(rocoto.add_dependency(dep_dict))
    dependencies = rocoto.create_dependency(dep=deps)
    fhrgrp = rocoto.create_envar(name='FHRGRP', value='#grp#')
    fhrlst = rocoto.create_envar(name='FHRLST', value='#lst#')
    ROTDIR = rocoto.create_envar(name='ROTDIR', value='&ROTDIR;')
    postenvars = envars + [fhrgrp] + [fhrlst] + [ROTDIR]
    varname1, varname2, varname3 = 'grp', 'dep', 'lst'
    varval1, varval2, varval3 = get_postgroups(dict_configs['post'], cdump=cdump)
    vardict = {varname2: varval2, varname3: varval3}
    task = wfu.create_wf_task('post', cdump=cdump, envar=postenvars, dependency=dependencies,
                              metatask='post', varname=varname1, varval=varval1, vardict=vardict)
    tasks.append(task)
    tasks.append('\n')

    # vrfy
    deps = []
    dep_dict = {'type':'metatask', 'name':'%spost' % cdump}
    deps.append(rocoto.add_dependency(dep_dict))
    dependencies = rocoto.create_dependency(dep=deps)
    task = wfu.create_wf_task('vrfy', cdump=cdump, envar=envars, dependency=dependencies)
    tasks.append(task)
    tasks.append('\n')

    # arch
    deps = []
    dep_dict = {'type':'task', 'name':'%svrfy' % cdump}
    deps.append(rocoto.add_dependency(dep_dict))
    dep_dict = {'type':'streq', 'left':'&ARCHIVE_TO_HPSS;', 'right':'YES'}
    deps.append(rocoto.add_dependency(dep_dict))
    dependencies = rocoto.create_dependency(dep_condition='and', dep=deps)
    task = wfu.create_wf_task('arch', cdump=cdump, envar=envars, dependency=dependencies, final=True)
    tasks.append(task)
    tasks.append('\n')

    return ''.join(tasks)


def get_workflow_body(dict_configs, cdump='gdas'):
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
    strings.append('\t<cycledef group="%s">&SDATE; &EDATE; &INTERVAL;</cycledef>\n' % cdump)
    strings.append('\n')
    strings.append(get_workflow(dict_configs, cdump=cdump))
    strings.append('\n')
    strings.append('</workflow>\n')

    return ''.join(strings)


def create_xml(dict_configs):
    '''
        Given an experiment directory containing config files and
        XML directory containing XML templates, create the workflow XML
    '''


    dict_configs['base']['INTERVAL'] = wfu.get_gfs_interval(dict_configs['base']['gfs_cyc'])
    base = dict_configs['base']

    preamble = get_preamble()
    definitions = get_definitions(base)
    resources = get_resources(dict_configs, cdump=base['CDUMP'])
    workflow = get_workflow_body(dict_configs, cdump=base['CDUMP'])

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
