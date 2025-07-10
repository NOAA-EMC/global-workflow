#!/usr/bin/env python

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
import re
import numpy as np
from datetime import datetime
from argparse import ArgumentParser, ArgumentDefaultsHelpFormatter
import rocoto
import workflow_utils as wfu

taskplan = ['getic', 'waveinit', 'waveprep', 'fcst', 'post', 'wavepostsbs', 'wavepostbndpnt', 'wavepostbndpntbll', 'wavepostpnt', 'wavegempak', 'waveawipsbulls', 'waveawipsgridded', 'postsnd', 'gempak', 'awips', 'vrfy', 'metp', 'arch']

def main():
    parser = ArgumentParser(description='Setup XML workflow and CRONTAB for a forecast only experiment.', formatter_class=ArgumentDefaultsHelpFormatter)
    parser.add_argument('--expdir',help='full path to experiment directory containing config files', type=str, required=False, default=os.environ['PWD'])
    parser.add_argument('--cdump',help='cycle to run forecasts', type=str, choices=['gdas', 'gfs'], default='gfs', required=False)

    args = parser.parse_args()

    configs = wfu.get_configs(args.expdir)

    _base = wfu.config_parser([wfu.find_config('config.base', configs)])

    if not os.path.samefile(args.expdir,_base['EXPDIR']):
        print('MISMATCH in experiment directories!')
        print(f'''config.base: EXPDIR = {repr(_base['EXPDIR'])}''')
        print(f'input arg:     --expdir = {repr(args.expdir)}')
        sys.exit(1)

    dict_configs = wfu.source_configs(configs, taskplan)

    dict_configs['base']['CDUMP'] = args.cdump

    # npe_node_max is the same for all tasks, so just use the one from fcst
    dict_configs['base']['npe_node_max'] = dict_configs['fcst']['npe_node_max']

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
    strings.append(f'\t\tThis workflow was automatically generated at {datetime.now()}\n')
    strings.append('\t-->\n')

    return ''.join(strings)


def get_definitions(base):
    '''
        Create entities related to the experiment
    '''

    machine = base.get('machine', wfu.detectMachine())
    scheduler = wfu.get_scheduler(machine)
    hpssarch = base.get('HPSSARCH', 'NO').upper()

    strings = []

    strings.append('\n')
    strings.append('\t<!-- Experiment parameters such as name, cycle, resolution -->\n')
    strings.append(f'''\t<!ENTITY PSLOT    "{base['PSLOT']}">\n''')
    strings.append(f'''\t<!ENTITY CDUMP    "{base['CDUMP']}">\n''')
    strings.append(f'''\t<!ENTITY CASE     "{base['CASE']}">\n''')
    strings.append('\n')
    strings.append('\t<!-- Experiment parameters such as starting, ending dates -->\n')
    strings.append(f'''\t<!ENTITY SDATE    "{base['SDATE'].strftime('%Y%m%d%H%M')}">\n''')
    strings.append(f'''\t<!ENTITY EDATE    "{base['EDATE'].strftime('%Y%m%d%H%M')}">\n''')
    if base['INTERVAL'] is None:
        print('cycle INTERVAL cannot be None')
        sys.exit(1)
    strings.append(f'''\t<!ENTITY INTERVAL "{base['INTERVAL']}">\n''')
    strings.append('\n')
    strings.append('\t<!-- Run Envrionment -->\n')
    strings.append(f'''\t<!ENTITY RUN_ENVIR "{base['RUN_ENVIR']}">\n''')
    strings.append('\n')
    strings.append('\t<!-- Experiment related directories -->\n')
    strings.append(f'''\t<!ENTITY EXPDIR "{base['EXPDIR']}">\n''')
    strings.append(f'''\t<!ENTITY ROTDIR "{base['ROTDIR']}">\n''')
    strings.append(f'''\t<!ENTITY ICSDIR "{base['ICSDIR']}">\n''')
    strings.append('\n')
    strings.append('\t<!-- Directories for driving the workflow -->\n')
    strings.append(f'''\t<!ENTITY HOMEgfs  "{base['HOMEgfs']}">\n''')
    strings.append(f'''\t<!ENTITY JOBS_DIR "{base['BASE_JOB']}">\n''')
    strings.append('\n')
    strings.append('\t<!-- Machine related entities -->\n')
    strings.append(f'''\t<!ENTITY ACCOUNT    "{base['ACCOUNT']}">\n''')
    strings.append(f'''\t<!ENTITY QUEUE      "{base['QUEUE']}">\n''')
    strings.append(f'''\t<!ENTITY QUEUE_SERVICE "{base['QUEUE_SERVICE']}">\n''')
    if scheduler in ['slurm'] and machine in ['ORION']:
       strings.append(f'''\t<!ENTITY PARTITION_BATCH "{base['PARTITION_BATCH']}">\n''')
    if scheduler in ['slurm']:
       strings.append(f'''\t<!ENTITY PARTITION_SERVICE "{base['QUEUE_SERVICE']}">\n''')
    strings.append(f'\t<!ENTITY SCHEDULER  "{scheduler}">\n')
    strings.append('\n')
    strings.append('\t<!-- Toggle HPSS archiving -->\n')
    strings.append(f'''\t<!ENTITY ARCHIVE_TO_HPSS "{base['HPSSARCH']}">\n''')
    strings.append('\n')
    strings.append('\t<!-- ROCOTO parameters that control workflow -->\n')
    strings.append('\t<!ENTITY CYCLETHROTTLE "2">\n')
    strings.append('\t<!ENTITY TASKTHROTTLE  "25">\n')
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

    base = dict_configs['base']
    machine = base.get('machine', wfu.detectMachine())
    reservation = base.get('RESERVATION', 'NONE').upper()
    scheduler = wfu.get_scheduler(machine)

    do_wave = base.get('DO_WAVE', 'NO').upper()
    do_bufrsnd = base.get('DO_BUFRSND', 'NO').upper()
    do_gempak = base.get('DO_GEMPAK', 'NO').upper()
    do_awips = base.get('DO_AWIPS', 'NO').upper()
    do_metp = base.get('DO_METP', 'NO').upper()

    for task in taskplan:

        cfg = dict_configs[task]

        wtimestr, resstr, queuestr, memstr, natstr = wfu.get_resources(machine, cfg, task, reservation, cdump=cdump)

        taskstr = f'{task.upper()}_{cdump.upper()}'

        strings.append(f'\t<!ENTITY QUEUE_{taskstr}     "{queuestr}">\n')
        if scheduler in ['slurm'] and machine in ['ORION'] and task not in ['getic', 'arch']:
            strings.append(f'\t<!ENTITY PARTITION_{taskstr} "&PARTITION_BATCH;">\n')
        if scheduler in ['slurm'] and task in ['getic', 'arch']:
            strings.append(f'\t<!ENTITY PARTITION_{taskstr} "&PARTITION_SERVICE;">\n')
        strings.append(f'\t<!ENTITY WALLTIME_{taskstr}  "{wtimestr}">\n')
        strings.append(f'\t<!ENTITY RESOURCES_{taskstr} "{resstr}">\n')
        if len(memstr) != 0:
            strings.append(f'\t<!ENTITY MEMORY_{taskstr}    "{memstr}">\n')
        strings.append(f'\t<!ENTITY NATIVE_{taskstr}    "{natstr}">\n')

        strings.append('\n')

    strings.append('\t<!-- END: Resource requirements for the workflow -->\n')

    return ''.join(strings)


def get_postgroups(post, cdump='gdas'):

    fhmin = post['FHMIN']
    fhmax = post['FHMAX']
    fhout = post['FHOUT']

    # Get a list of all forecast hours
    if cdump in ['gdas']:
        fhrs = list(range(fhmin, fhmax+fhout, fhout))
    elif cdump in ['gfs']:
        fhmax = np.max([post['FHMAX_GFS_00'],post['FHMAX_GFS_06'],post['FHMAX_GFS_12'],post['FHMAX_GFS_18']])
        fhout = post['FHOUT_GFS']
        fhmax_hf = post['FHMAX_HF_GFS']
        fhout_hf = post['FHOUT_HF_GFS']
        fhrs_hf = list(range(fhmin, fhmax_hf+fhout_hf, fhout_hf))
        fhrs = fhrs_hf + list(range(fhrs_hf[-1]+fhout, fhmax+fhout, fhout))

    npostgrp = post['NPOSTGRP']
    ngrps = npostgrp if len(fhrs) > npostgrp else len(fhrs)

    fhrs = [f'f{f:03d}' for f in fhrs]
    fhrs = np.array_split(fhrs, ngrps)
    fhrs = [f.tolist() for f in fhrs]

    fhrgrp = ' '.join([f'{x:03d}' for x in range(1, ngrps+1)])
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
    envars.append(rocoto.create_envar(name='GDATE', value='<cyclestr offset="-6:00:00">@Y@m@d@H</cyclestr>'))
    envars.append(rocoto.create_envar(name='GDUMP', value='gdas'))
    envars.append(rocoto.create_envar(name='gPDY', value='<cyclestr offset="-6:00:00">@Y@m@d</cyclestr>'))
    envars.append(rocoto.create_envar(name='gcyc', value='<cyclestr offset="-6:00:00">@H</cyclestr>'))

    base = dict_configs['base']
    machine = base.get('machine', wfu.detectMachine())
    hpssarch = base.get('HPSSARCH', 'NO').upper()
    do_wave = base.get('DO_WAVE', 'NO').upper()
    do_wave_cdump = base.get('WAVE_CDUMP', 'BOTH').upper()
    do_bufrsnd = base.get('DO_BUFRSND', 'NO').upper()
    do_gempak = base.get('DO_GEMPAK', 'NO').upper()
    do_awips = base.get('DO_AWIPS', 'NO').upper()
    do_vrfy = base.get('DO_VRFY', 'YES').upper()
    do_metp = base.get('DO_METP', 'NO').upper()

    tasks = []

    # getic
    if hpssarch in ['YES']:
      deps = []
      data = '&ROTDIR;/&CDUMP;.@Y@m@d/@H/atmos/INPUT/sfc_data.tile6.nc'
      dep_dict = {'type':'data', 'data':data}
      deps.append(rocoto.add_dependency(dep_dict))
      data = '&ROTDIR;/&CDUMP;.@Y@m@d/@H/atmos/RESTART/@Y@m@d.@H0000.sfcanl_data.tile6.nc'
      dep_dict = {'type':'data', 'data':data}
      deps.append(rocoto.add_dependency(dep_dict))
      dependencies = rocoto.create_dependency(dep_condition='nor', dep=deps)

      task = wfu.create_wf_task('getic', cdump=cdump, envar=envars, dependency=dependencies)
      tasks.append(task)
      tasks.append('\n')

    # waveinit
    if do_wave in ['Y', 'YES'] and do_wave_cdump in ['GFS', 'BOTH']:
        task = wfu.create_wf_task('waveinit', cdump=cdump, envar=envars)
        tasks.append(task)
        tasks.append('\n')

    # waveprep
    if do_wave in ['Y', 'YES'] and do_wave_cdump in ['GFS', 'BOTH']:
        deps = []
        dep_dict = {'type': 'task', 'name': f'{cdump}waveinit'}
        deps.append(rocoto.add_dependency(dep_dict))
        dependencies = rocoto.create_dependency(dep_condition='and', dep=deps)
        task = wfu.create_wf_task('waveprep', cdump=cdump, envar=envars, dependency=dependencies)
        tasks.append(task)
        tasks.append('\n')

    # fcst
    deps = []
    data = '&ROTDIR;/&CDUMP;.@Y@m@d/@H/atmos/INPUT/sfc_data.tile6.nc'
    dep_dict = {'type':'data', 'data':data}
    deps.append(rocoto.add_dependency(dep_dict))
    data = '&ROTDIR;/&CDUMP;.@Y@m@d/@H/atmos/RESTART/@Y@m@d.@H0000.sfcanl_data.tile6.nc'
    dep_dict = {'type':'data', 'data':data}
    deps.append(rocoto.add_dependency(dep_dict))
    dependencies = rocoto.create_dependency(dep_condition='or', dep=deps)

    if do_wave in ['Y', 'YES'] and do_wave_cdump in ['GFS', 'BOTH']:
        deps = []
        dep_dict = {'type': 'task', 'name': f'{cdump}waveprep'}
        deps.append(rocoto.add_dependency(dep_dict))
        dependencies2 = rocoto.create_dependency(dep_condition='and', dep=deps)

    deps = []
    deps.append(dependencies)
    if do_wave in ['Y', 'YES'] and do_wave_cdump in ['GFS', 'BOTH']:
        deps.append(dependencies2)
    dependencies = rocoto.create_dependency(dep_condition='and', dep=deps)

    task = wfu.create_wf_task('fcst', cdump=cdump, envar=envars, dependency=dependencies)
    tasks.append(task)
    tasks.append('\n')

    # post
    deps = []
    data = f'&ROTDIR;/{cdump}.@Y@m@d/@H/atmos/{cdump}.t@Hz.log#dep#.txt'
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

    # wavepostsbs
    if do_wave in ['Y', 'YES'] and do_wave_cdump in ['GFS', 'BOTH']:
        deps = []
        data = f'&ROTDIR;/{cdump}.@Y@m@d/@H/wave/rundata/{cdump}wave.out_grd.gnh_10m.@Y@m@d.@H0000'
        dep_dict = {'type': 'data', 'data': data}
        deps.append(rocoto.add_dependency(dep_dict))
        data = f'&ROTDIR;/{cdump}.@Y@m@d/@H/wave/rundata/{cdump}wave.out_grd.aoc_9km.@Y@m@d.@H0000'
        dep_dict = {'type': 'data', 'data': data}
        deps.append(rocoto.add_dependency(dep_dict))
        data = f'&ROTDIR;/{cdump}.@Y@m@d/@H/wave/rundata/{cdump}wave.out_grd.gsh_15m.@Y@m@d.@H0000'
        dep_dict = {'type': 'data', 'data': data}
        deps.append(rocoto.add_dependency(dep_dict))
        dependencies = rocoto.create_dependency(dep_condition='and', dep=deps)
        task = wfu.create_wf_task('wavepostsbs', cdump=cdump, envar=envars, dependency=dependencies)
        tasks.append(task)
        tasks.append('\n')

    # wavepostbndpnt
    if do_wave in ['Y', 'YES']:
        deps = []
        dep_dict = {'type':'task', 'name':f'{cdump}fcst'}
        deps.append(rocoto.add_dependency(dep_dict))
        dependencies = rocoto.create_dependency(dep=deps)
        task = wfu.create_wf_task('wavepostbndpnt', cdump=cdump, envar=envars, dependency=dependencies)
        tasks.append(task)
        tasks.append('\n')

    # wavepostbndpntbll
    if do_wave in ['Y', 'YES']:
        deps = []
        data = f'&ROTDIR;/{cdump}.@Y@m@d/@H/atmos/{cdump}.t@Hz.logf180.txt'
        dep_dict = {'type': 'data', 'data': data}
        deps.append(rocoto.add_dependency(dep_dict))
        dependencies = rocoto.create_dependency(dep=deps)
        task = wfu.create_wf_task('wavepostbndpntbll', cdump=cdump, envar=envars, dependency=dependencies)
        tasks.append(task)
        tasks.append('\n')

    # wavepostpnt
    if do_wave in ['Y', 'YES']:
        deps = []
        dep_dict = {'type':'task', 'name':f'{cdump}fcst'}
        deps.append(rocoto.add_dependency(dep_dict))
        dep_dict = {'type':'task', 'name':f'{cdump}wavepostbndpntbll'}
        deps.append(rocoto.add_dependency(dep_dict))
        dependencies = rocoto.create_dependency(dep_condition='and', dep=deps)
        task = wfu.create_wf_task('wavepostpnt', cdump=cdump, envar=envars, dependency=dependencies)
        tasks.append(task)
        tasks.append('\n')

    # wavegempak
    if do_wave in ['Y', 'YES'] and do_gempak in ['Y', 'YES']:
        deps = []
        dep_dict = {'type':'task', 'name':f'{cdump}wavepostsbs'}
        deps.append(rocoto.add_dependency(dep_dict))
        dependencies = rocoto.create_dependency(dep=deps)
        task = wfu.create_wf_task('wavegempak', cdump=cdump, envar=envars, dependency=dependencies)
        tasks.append(task)
        tasks.append('\n')

    # waveawipsbulls
    if do_wave in ['Y', 'YES'] and do_awips in ['Y', 'YES']:
        deps = []
        dep_dict = {'type':'task', 'name':f'{cdump}wavepostsbs'}
        deps.append(rocoto.add_dependency(dep_dict))
        dep_dict = {'type':'task', 'name':f'{cdump}wavepostpnt'}
        deps.append(rocoto.add_dependency(dep_dict))
        dependencies = rocoto.create_dependency(dep_condition='and', dep=deps)
        task = wfu.create_wf_task('waveawipsbulls', cdump=cdump, envar=envars, dependency=dependencies)
        tasks.append(task)
        tasks.append('\n')

    # waveawipsgridded
    if do_wave in ['Y', 'YES'] and do_awips in ['Y', 'YES']:
        deps = []
        dep_dict = {'type':'task', 'name':f'{cdump}wavepostsbs'}
        deps.append(rocoto.add_dependency(dep_dict))
        dependencies = rocoto.create_dependency(dep=deps)
        task = wfu.create_wf_task('waveawipsgridded', cdump=cdump, envar=envars, dependency=dependencies)
        tasks.append(task)
        tasks.append('\n')

    #postsnd
    if do_bufrsnd in ['Y', 'YES']:
        deps = []
        dep_dict = {'type': 'task', 'name': f'{cdump}fcst'}
        deps.append(rocoto.add_dependency(dep_dict))
        dependencies = rocoto.create_dependency(dep=deps)
        task = wfu.create_wf_task('postsnd', cdump=cdump, envar=envars, dependency=dependencies)
        tasks.append(task)
        tasks.append('\n')

    # awips
    if do_awips in ['Y', 'YES']:
        deps = []
        dep_dict = {'type': 'metatask', 'name': f'{cdump}post'}
        deps.append(rocoto.add_dependency(dep_dict))
        dependencies = rocoto.create_dependency(dep=deps)
        fhrgrp = rocoto.create_envar(name='FHRGRP', value='#grp#')
        fhrlst = rocoto.create_envar(name='FHRLST', value='#lst#')
        ROTDIR = rocoto.create_envar(name='ROTDIR', value='&ROTDIR;')
        awipsenvars = envars + [fhrgrp] + [fhrlst] + [ROTDIR]
        varname1, varname2, varname3 = 'grp', 'dep', 'lst'
        varval1, varval2, varval3 = get_awipsgroups(dict_configs['awips'], cdump=cdump)
        vardict = {varname2: varval2, varname3: varval3}
        task = wfu.create_wf_task('awips', cdump=cdump, envar=awipsenvars, dependency=dependencies,
                                  metatask='awips', varname=varname1, varval=varval1, vardict=vardict)
        tasks.append(task)
        tasks.append('\n')

    # gempak
    if do_gempak in ['Y', 'YES']:
        deps = []
        dep_dict = {'type': 'metatask', 'name': f'{cdump}post'}
        deps.append(rocoto.add_dependency(dep_dict))
        dependencies = rocoto.create_dependency(dep=deps)
        ROTDIR = rocoto.create_envar(name='ROTDIR', value='&ROTDIR;')
        gempakenvars = envars + [ROTDIR]
        task = wfu.create_wf_task('gempak', cdump=cdump, envar=gempakenvars, dependency=dependencies)
        tasks.append(task)
        tasks.append('\n')

    # vrfy
    if do_vrfy in ['Y', 'YES']:
        deps = []
        dep_dict = {'type':'metatask', 'name':f'{cdump}post'}
        deps.append(rocoto.add_dependency(dep_dict))
        dependencies = rocoto.create_dependency(dep=deps)
        task = wfu.create_wf_task('vrfy', cdump=cdump, envar=envars, dependency=dependencies)
        tasks.append(task)
        tasks.append('\n')

    # metp
    if do_metp in ['Y', 'YES']:
        deps = []
        dep_dict = {'type':'metatask', 'name':f'{cdump}post'}
        deps.append(rocoto.add_dependency(dep_dict))
        dependencies = rocoto.create_dependency(dep_condition='and', dep=deps)
        sdate_gfs = rocoto.create_envar(name='SDATE_GFS', value='&SDATE;')
        metpcase = rocoto.create_envar(name='METPCASE', value='#metpcase#')
        metpenvars = envars + [sdate_gfs] + [metpcase]
        varname1 = 'metpcase'
        varval1 = 'g2g1 g2o1 pcp1'
        task = wfu.create_wf_task('metp', cdump=cdump, envar=metpenvars, dependency=dependencies,
                                  metatask='metp', varname=varname1, varval=varval1)
        tasks.append(task)
        tasks.append('\n')

    # arch
    deps = []
    dep_dict = {'type':'metatask', 'name':f'{cdump}post'}
    deps.append(rocoto.add_dependency(dep_dict))
    if do_vrfy in ['Y', 'YES']:
        dep_dict = {'type':'task', 'name':f'{cdump}vrfy'}
        deps.append(rocoto.add_dependency(dep_dict))
    dep_dict = {'type':'streq', 'left':'&ARCHIVE_TO_HPSS;', 'right':f'{hpssarch}'}
    deps.append(rocoto.add_dependency(dep_dict))
    if do_wave in ['Y', 'YES']:
      dep_dict = {'type': 'task', 'name': f'{cdump}wavepostsbs'}
      deps.append(rocoto.add_dependency(dep_dict))
      dep_dict = {'type': 'task', 'name': f'{cdump}wavepostpnt'}
      deps.append(rocoto.add_dependency(dep_dict))
      dep_dict = {'type': 'task', 'name': f'{cdump}wavepostbndpnt'}
      deps.append(rocoto.add_dependency(dep_dict))
    dependencies = rocoto.create_dependency(dep_condition='and', dep=deps)
    task = wfu.create_wf_task('arch', cdump=cdump, envar=envars, dependency=dependencies, final=True)
    tasks.append(task)
    tasks.append('\n')

    return ''.join(tasks)


def get_awipsgroups(awips, cdump='gfs'):

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
        fhrs = list(fhrs_hf) + list(range(fhrs_hf[-1]+fhout, fhmax+fhout, fhout))

    nawipsgrp = awips['NAWIPSGRP']
    ngrps = nawipsgrp if len(fhrs) > nawipsgrp else len(fhrs)

    fhrs = [f'f{f:03d}' for f in fhrs]
    fhrs = np.array_split(fhrs, ngrps)
    fhrs = [f.tolist() for f in fhrs]

    fhrgrp = ' '.join([f'{x:03d}' for x in range(0, ngrps)])
    fhrdep = ' '.join([f[-1] for f in fhrs])
    fhrlst = ' '.join(['_'.join(f) for f in fhrs])

    return fhrgrp, fhrdep, fhrlst

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
    strings.append(f'\t<cycledef group="{cdump}">&SDATE; &EDATE; &INTERVAL;</cycledef>\n')
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

    # Removes <memory>&MEMORY_JOB_DUMP</memory> post mortem from gdas tasks
    temp_workflow = ''
    memory_dict = []
    for each_resource_string in re.split(r'(\s+)', resources):
        if 'MEMORY' in each_resource_string:
            memory_dict.append(each_resource_string)
    for each_line in re.split(r'(\s+)', workflow):
        if 'MEMORY' not in each_line:
            temp_workflow += each_line
        else:
            if any( substring in each_line for substring in memory_dict):
                temp_workflow += each_line
    workflow = temp_workflow

    # Start writing the XML file
    fh = open(f'{base["EXPDIR"]}/{base["PSLOT"]}.xml', 'w')

    fh.write(preamble)
    fh.write(definitions)
    fh.write(resources)
    fh.write(workflow)

    fh.close()

    return

if __name__ == '__main__':
    main()
    sys.exit(0)
