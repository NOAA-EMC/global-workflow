#!/usr/bin/env python3

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

taskplan = ['getic', 'init', 'coupled_ic', 'aerosol_init', 'waveinit', 'waveprep', 'fcst', 'post', 'wavepostsbs', 'wavepostbndpnt', 'wavepostbndpntbll', 'wavepostpnt', 'wavegempak', 'waveawipsbulls', 'waveawipsgridded', 'wafs', 'wafsgrib2', 'wafsblending', 'wafsgcip', 'wafsgrib20p25', 'wafsblending0p25', 'postsnd', 'gempak', 'awips', 'vrfy', 'metp', 'arch', 'ocnpost']

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
    if scheduler in ['slurm']:
       strings.append(f'''\t<!ENTITY PARTITION_BATCH "{base['PARTITION_BATCH']}">\n''')
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
        if scheduler in ['slurm']:
            if task in ['getic', 'arch']:
                strings.append(f'\t<!ENTITY PARTITION_{taskstr} "&PARTITION_SERVICE;">\n')
            else:
                strings.append(f'\t<!ENTITY PARTITION_{taskstr} "&PARTITION_BATCH;">\n')

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
        fhrs = list(range(fhmin, fhmax + fhout, fhout))
    elif cdump in ['gfs']:
        fhmax = np.max([post['FHMAX_GFS_00'], post['FHMAX_GFS_06'], post['FHMAX_GFS_12'], post['FHMAX_GFS_18']])
        fhout = post['FHOUT_GFS']
        fhmax_hf = post['FHMAX_HF_GFS']
        fhout_hf = post['FHOUT_HF_GFS']
        fhrs_hf = list(range(fhmin, fhmax_hf + fhout_hf, fhout_hf))
        fhrs = fhrs_hf + list(range(fhrs_hf[-1] + fhout, fhmax + fhout, fhout))

    npostgrp = post['NPOSTGRP']
    ngrps = npostgrp if len(fhrs) > npostgrp else len(fhrs)

    fhrs = [f'f{f:03d}' for f in fhrs]
    fhrs = np.array_split(fhrs, ngrps)
    fhrs = [f.tolist() for f in fhrs]

    fhrgrp = ' '.join([f'_{f[0]}-{f[-1]}' for f in fhrs])
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

    base = dict_configs['base']
    machine = base.get('machine', wfu.detectMachine())
    hpssarch = base.get('HPSSARCH', 'NO').upper()
    app = base.get('APP', "ATM").upper()
    atmtype = base.get('ATMTYPE', 'MODEL').upper()
    do_post = base.get('DO_POST', 'YES').upper()
    do_wave = base.get('DO_WAVE', 'NO').upper()
    do_ocean = base.get('DO_OCN', 'NO').upper()
    do_ice = base.get('DO_ICE', 'NO').upper()
    do_aero = base.get('DO_AERO', 'NO').upper()
    do_wave_cdump = base.get('WAVE_CDUMP', 'BOTH').upper()
    if do_wave in ['YES']:
        do_wave_bnd = dict_configs['wavepostsbs'].get('DOBNDPNT_WAVE', "YES").upper()
    do_bufrsnd = base.get('DO_BUFRSND', 'NO').upper()
    do_gempak = base.get('DO_GEMPAK', 'NO').upper()
    do_awips = base.get('DO_AWIPS', 'NO').upper()
    do_wafs = base.get('WAFSF', 'NO').upper()
    do_vrfy = base.get('DO_VRFY', 'YES').upper()
    do_metp = base.get('DO_METP', 'NO').upper()
    datm_src = base.get('datm_src', 'gefs').lower()
    n_tiles = 6

    tasks = []

    if app in ['S2S', 'S2SW']:
        # Copy prototype ICs
        deps = []
        base_cplic = dict_configs['coupled_ic']['BASE_CPLIC']

        # ATM ICs
        for file in ['gfs_ctrl.nc'] + [f'{datatype}_data.tile{tile_index}.nc' for datatype in ['gfs', 'sfc'] for tile_index in range(1, n_tiles + 1)]:
            data = f"{base_cplic}/{dict_configs['coupled_ic'][f'CPL_ATMIC']}/@Y@m@d@H/&CDUMP;/{base.get('CASE','C384')}/INPUT/{file}"
            dep_dict = {'type': 'data', 'data': data}
            deps.append(rocoto.add_dependency(dep_dict))

        # Ocean ICs
        if do_ocean in ["YES"]:
            ocn_res = base.get('OCNRES', '025')
            for res in ['res'] + [f'res_{res_index}' for res_index in range(1, 5)]:
                data = f"{base_cplic}/{dict_configs['coupled_ic'][f'CPL_OCNIC']}/@Y@m@d@H/ocn/{ocn_res:03d}/MOM.{res}.nc"
                dep_dict = {'type': 'data', 'data': data}
                deps.append(rocoto.add_dependency(dep_dict))

        # Ice ICs
        if do_ice in ["YES"]:
            ice_res = base.get('ICERES', '025')
            ice_res_dec = f'{float(ice_res)/100:.2f}'
            data = f"{base_cplic}/{dict_configs['coupled_ic'][f'CPL_ICEIC']}/@Y@m@d@H/ice/{ice_res:03d}/cice5_model_{ice_res_dec}.res_@Y@m@d@H.nc"
            dep_dict = {'type': 'data', 'data': data}
            deps.append(rocoto.add_dependency(dep_dict))

        # Wave ICs
        if do_wave in ["YES"]:
            for wave_grid in dict_configs['waveinit']['waveGRD'].split():
                data = f"{base_cplic}/{dict_configs['coupled_ic'][f'CPL_WAVIC']}/@Y@m@d@H/wav/{wave_grid}/@Y@m@d.@H0000.restart.{wave_grid}"
                dep_dict = {'type': 'data', 'data': data}
                deps.append(rocoto.add_dependency(dep_dict))

        dependencies = rocoto.create_dependency(dep_condition='and', dep=deps)
        task = wfu.create_wf_task('coupled_ic', cdump=cdump, envar=envars, dependency=dependencies)
        tasks.append(task)
        tasks.append('\n')

    elif app in ['NG-GODAS']:
        deps = []
        base_cplic = dict_configs['coupled_ic']['BASE_CPLIC']
        data = f"{base_cplic}/{dict_configs['coupled_ic'][f'CPL_DATM']}/{datm_src}/@Y@m@d@H/{datm_src}.@Y@m.nc"
        dep_dict = {'type': 'data', 'data': data}
        deps.append(rocoto.add_dependency(dep_dict))
        dependencies = rocoto.create_dependency(dep_condition='and', dep=deps)
        task = wfu.create_wf_task('coupled_ic', cdump=cdump, envar=envars, dependency=dependencies)
        tasks.append(task)
        tasks.append('\n')

        # Ocean ICs
        if do_ocean in ["YES"]:
            ocn_res = base.get('OCNRES', '025')
            for res in ['res'] + [f'res_{res_index}' for res_index in range(1, 5)]:
                data = f"{base_cplic}/{dict_configs['coupled_ic'][f'CPL_OCNIC']}/@Y@m@d@H/ocn/{ocn_res:03d}/MOM.{res}.nc"
                dep_dict = {'type': 'data', 'data': data}
                deps.append(rocoto.add_dependency(dep_dict))

        # Ice ICs
        if do_ice in ["YES"]:
            ice_res = base.get('ICERES', '025')
            ice_res_dec = f'{float(ice_res)/100:.2f}'
            data = f"{base_cplic}/{dict_configs['coupled_ic'][f'CPL_ICEIC']}/@Y@m@d@H/ice/{ice_res:03d}/cice5_model_{ice_res_dec}.res_@Y@m@d@H.nc"
            dep_dict = {'type': 'data', 'data': data}
            deps.append(rocoto.add_dependency(dep_dict))

    else:
        if hpssarch in ['YES']:
            deps = []
            data = '&ROTDIR;/&CDUMP;.@Y@m@d/@H/atmos/INPUT/sfc_data.tile6.nc'
            dep_dict = {'type': 'data', 'data': data}
            deps.append(rocoto.add_dependency(dep_dict))
            data = '&ROTDIR;/&CDUMP;.@Y@m@d/@H/atmos/RESTART/@Y@m@d.@H0000.sfcanl_data.tile6.nc'
            dep_dict = {'type': 'data', 'data': data}
            deps.append(rocoto.add_dependency(dep_dict))
            dependencies = rocoto.create_dependency(dep_condition='nor', dep=deps)

            task = wfu.create_wf_task('getic', cdump=cdump, envar=envars, dependency=dependencies)
            tasks.append(task)
            tasks.append('\n')

        # init
        deps = []
        data = '&ROTDIR;/&CDUMP;.@Y@m@d/@H/gfs.t@Hz.sanl'
        dep_dict = {'type': 'data', 'data': data}
        deps.append(rocoto.add_dependency(dep_dict))
        data = '&ROTDIR;/&CDUMP;.@Y@m@d/@H/gfs.t@Hz.atmanl.nemsio'
        dep_dict = {'type': 'data', 'data': data}
        deps.append(rocoto.add_dependency(dep_dict))
        data = '&ROTDIR;/&CDUMP;.@Y@m@d/@H/gfs.t@Hz.atmanl.nc'
        dep_dict = {'type': 'data', 'data': data}
        deps.append(rocoto.add_dependency(dep_dict))
        data = '&ROTDIR;/&CDUMP;.@Y@m@d/@H/atmos/gfs.t@Hz.atmanl.nc'
        dep_dict = {'type': 'data', 'data': data}
        deps.append(rocoto.add_dependency(dep_dict))
        data = '&ROTDIR;/&CDUMP;.@Y@m@d/@H/atmos/RESTART/@Y@m@d.@H0000.sfcanl_data.tile6.nc'
        dep_dict = {'type': 'data', 'data': data}
        deps.append(rocoto.add_dependency(dep_dict))
        dependencies = rocoto.create_dependency(dep_condition='or', dep=deps)

        if hpssarch in ['YES']:
            deps = []
            dep_dict = {'type': 'task', 'name': f'{cdump}getic'}
            deps.append(rocoto.add_dependency(dep_dict))
            dependencies2 = rocoto.create_dependency(dep=deps)

        deps = []
        deps.append(dependencies)
        if hpssarch in ['YES']:
            deps.append(dependencies2)
            dependencies = rocoto.create_dependency(dep_condition='and', dep=deps)

        task = wfu.create_wf_task('init', cdump=cdump, envar=envars, dependency=dependencies)
        tasks.append(task)
        tasks.append('\n')

    # waveinit
    if do_wave in ['Y', 'YES'] and do_wave_cdump in ['GFS', 'BOTH']:
        task = wfu.create_wf_task('waveinit', cdump=cdump, envar=envars)
        tasks.append(task)
        tasks.append('\n')

    # waveprep
    if do_wave in ['Y', 'YES'] and do_wave_cdump in ['GFS', 'BOTH'] and app in ['ATMW']:
        deps = []
        dep_dict = {'type': 'task', 'name': f'{cdump}waveinit'}
        deps.append(rocoto.add_dependency(dep_dict))
        if app not in ['S2S', 'S2SW']:
            dep_dict = {'type': 'task', 'name': f'{cdump}init'}
            deps.append(rocoto.add_dependency(dep_dict))
        dependencies = rocoto.create_dependency(dep_condition='and', dep=deps)
        task = wfu.create_wf_task('waveprep', cdump=cdump, envar=envars, dependency=dependencies)
        tasks.append(task)
        tasks.append('\n')

    # aerosol_init
    if do_aero in ['Y', 'YES']:
        deps = []
        if app in ['S2S', 'S2SW']:
            dep_dict = {'type': 'task', 'name': 'coupled_ic'}
        else:
            dep_dict = {'type': 'task', 'name': f'{cdump}init'}

        deps.append(rocoto.add_dependency(dep_dict))

        # Files from current cycle
        files = ['gfs_ctrl.nc'] + [f'gfs_data.tile{tile_index}.nc' for tile_index in range(1, n_tiles + 1)]
        for file in files:
            data = f'&ROTDIR;/&CDUMP;.@Y@m@d/@H/atmos/INPUT/{file}'
            dep_dict = {'type': 'data', 'data': data}
            deps.append(rocoto.add_dependency(dep_dict))

        # previous cycle
        dep_dict = {'type': 'cycleexist', 'offset': f'-{base["INTERVAL"]}'}
        deps.append(rocoto.add_dependency(dep_dict))

        # Files from previous cycle
        files = [f'@Y@m@d.@H0000.fv_core.res.nc'] + \
                [f'@Y@m@d.@H0000.fv_core.res.tile{tile_index}.nc' for tile_index in range(1, n_tiles + 1)] + \
                [f'@Y@m@d.@H0000.fv_tracer.res.tile{tile_index}.nc' for tile_index in range(1, n_tiles + 1)]

        for file in files:
            data = ['&ROTDIR;/&CDUMP;.@Y@m@d/@H/atmos/RERUN_RESTART/', file]
            offset = [f'-{base["INTERVAL"]}', None]
            dep_dict = {'type': 'data', 'data': data, 'offset': offset}
            deps.append(rocoto.add_dependency(dep_dict))

        dependencies = rocoto.create_dependency(dep_condition='and', dep=deps)
        task = wfu.create_wf_task('aerosol_init', cdump=cdump, envar=envars, dependency=dependencies)
        tasks.append(task)
        tasks.append('\n')

    # fcst
    deps = []
    if atmtype in ['MODEL']:
        data = '&ROTDIR;/&CDUMP;.@Y@m@d/@H/atmos/INPUT/sfc_data.tile6.nc'
        dep_dict = {'type':'data', 'data':data}
        deps.append(rocoto.add_dependency(dep_dict))
        data = '&ROTDIR;/&CDUMP;.@Y@m@d/@H/atmos/RESTART/@Y@m@d.@H0000.sfcanl_data.tile6.nc'
        dep_dict = {'type':'data', 'data':data}
        deps.append(rocoto.add_dependency(dep_dict))
        dependencies = rocoto.create_dependency(dep_condition='or', dep=deps)
    else:
        data = f'&ICSDIR;/@Y@m@d@H/datm/{datm_src}.@Y@m.nc'
        dep_dict = {'type':'data', 'data':data}
        deps.append(rocoto.add_dependency(dep_dict))
        data = '&ICSDIR;/@Y@m@d@H/ice/cice_model*.nc'
        dep_dict = {'type':'data', 'data':data}
        deps.append(rocoto.add_dependency(dep_dict))
        dependencies = rocoto.create_dependency(dep_condition='or', dep=deps)

    if do_wave in ['Y', 'YES'] and do_wave_cdump in ['GFS', 'BOTH']:
        deps = []
        if app in ['ATMW']:
            dep_dict = {'type': 'task', 'name': f'{cdump}waveprep'}
        else: 
            dep_dict = {'type': 'task', 'name': f'{cdump}waveinit'}
        deps.append(rocoto.add_dependency(dep_dict))
        dependencies2 = rocoto.create_dependency(dep_condition='and', dep=deps)

    if do_aero in ['Y', 'YES']:
        deps = []
        dep_dict = {'type': 'task', 'name': f'{cdump}aerosol_init'}
        deps.append(rocoto.add_dependency(dep_dict))
        deps2 = []
        dep_dict = {'type': 'cycleexist', 'offset': f'-{base["INTERVAL"]}'}
        deps2.append(rocoto.add_dependency(dep_dict))
        deps.append(rocoto.create_dependency(dep_condition='not', dep=deps2))
        dependencies3 = rocoto.create_dependency(dep_condition='or', dep=deps)

    deps = []
    deps.append(dependencies)
    if do_wave in ['Y', 'YES'] and do_wave_cdump in ['GFS', 'BOTH']:
        deps.append(dependencies2)
    if do_aero in ['Y', 'YES']:
        deps.append(dependencies3)
    dependencies = rocoto.create_dependency(dep_condition='and', dep=deps)

    task = wfu.create_wf_task('fcst', cdump=cdump, envar=envars, dependency=dependencies)
    tasks.append(task)
    tasks.append('\n')

    # post
    if do_post in ['Y', 'YES']:
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
        for wave_grid in dict_configs['wavepostsbs']['waveGRD'].split():
            data = f'&ROTDIR;/{cdump}.@Y@m@d/@H/wave/rundata/{cdump}wave.out_grd.{wave_grid}.@Y@m@d.@H0000'
            dep_dict = {'type': 'data', 'data': data}
            deps.append(rocoto.add_dependency(dep_dict))

        dependencies = rocoto.create_dependency(dep_condition='and', dep=deps)
        task = wfu.create_wf_task('wavepostsbs', cdump=cdump, envar=envars, dependency=dependencies)
        tasks.append(task)
        tasks.append('\n')

    # wavepostbndpnt
    if do_wave in ['Y', 'YES'] and do_wave_bnd in ['YES']:
        deps = []
        dep_dict = {'type': 'task', 'name': f'{cdump}fcst'}
        deps.append(rocoto.add_dependency(dep_dict))
        dependencies = rocoto.create_dependency(dep=deps)
        task = wfu.create_wf_task('wavepostbndpnt', cdump=cdump, envar=envars, dependency=dependencies)
        tasks.append(task)
        tasks.append('\n')

    # wavepostbndpntbll
    if do_wave in ['Y', 'YES'] and do_wave_bnd in ['YES']:
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
        dep_dict = {'type': 'task', 'name': f'{cdump}fcst'}
        deps.append(rocoto.add_dependency(dep_dict))
        if do_wave_bnd in ['YES']:
            dep_dict = {'type': 'task', 'name': f'{cdump}wavepostbndpntbll'}
            deps.append(rocoto.add_dependency(dep_dict))
        dependencies = rocoto.create_dependency(dep_condition='and', dep=deps)
        task = wfu.create_wf_task('wavepostpnt', cdump=cdump, envar=envars, dependency=dependencies)
        tasks.append(task)
        tasks.append('\n')

    # wavegempak
    if do_wave in ['Y', 'YES'] and do_gempak in ['Y', 'YES']:
        deps = []
        dep_dict = {'type': 'task', 'name': f'{cdump}wavepostsbs'}
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

    # ocnpost
    if do_ocean in ['YES']:
        deps = []
        if atmtype in ['MODEL']:
            data = f'&ROTDIR;/{cdump}.@Y@m@d/@H/atmos/{cdump}.t@Hz.log#dep#.txt'
            dep_dict = {'type': 'data', 'data': data}
            deps.append(rocoto.add_dependency(dep_dict))
            dependencies = rocoto.create_dependency(dep=deps)
        else:
            data = f'&ROTDIR;/{cdump}.@Y@m@d/@H/datm/datm.status'
            dep_dict = {'type': 'data', 'data': data}
            deps.append(rocoto.add_dependency(dep_dict))
            dependencies = rocoto.create_dependency(dep=deps)
        fhrgrp = rocoto.create_envar(name='FHRGRP', value='#grp#')
        fhrlst = rocoto.create_envar(name='FHRLST', value='#lst#')
        ROTDIR = rocoto.create_envar(name='ROTDIR', value='&ROTDIR;')
        postenvars = envars + [fhrgrp] + [fhrlst] + [ROTDIR]
        varname1, varname2, varname3 = 'grp', 'dep', 'lst'
        varval1, varval2, varval3 = get_postgroups(dict_configs['ocnpost'], cdump=cdump)
        vardict = {varname2: varval2, varname3: varval3}
        task = wfu.create_wf_task('ocnpost', cdump=cdump, envar=postenvars, dependency=dependencies,
                                  metatask='ocnpost', varname=varname1, varval=varval1, vardict=vardict)
        tasks.append(task)
        tasks.append('\n')

    # wafs
    if do_wafs in ['Y', 'YES']:
        deps = []
        data = f'&ROTDIR;/{cdump}.@Y@m@d/@H/atmos/{cdump}.t@Hz.wafs.grb2if006'
        dep_dict = {'type': 'data', 'data': data}
        deps.append(rocoto.add_dependency(dep_dict))
        data = f'&ROTDIR;/{cdump}.@Y@m@d/@H/atmos/{cdump}.t@Hz.wafs.grb2if012'
        dep_dict = {'type': 'data', 'data': data}
        deps.append(rocoto.add_dependency(dep_dict))
        data = f'&ROTDIR;/{cdump}.@Y@m@d/@H/atmos/{cdump}.t@Hz.wafs.grb2if015'
        dep_dict = {'type': 'data', 'data': data}
        deps.append(rocoto.add_dependency(dep_dict))
        data = f'&ROTDIR;/{cdump}.@Y@m@d/@H/atmos/{cdump}.t@Hz.wafs.grb2if018'
        dep_dict = {'type': 'data', 'data': data}
        deps.append(rocoto.add_dependency(dep_dict))
        data = f'&ROTDIR;/{cdump}.@Y@m@d/@H/atmos/{cdump}.t@Hz.wafs.grb2if021'
        dep_dict = {'type': 'data', 'data': data}
        deps.append(rocoto.add_dependency(dep_dict))
        data = f'&ROTDIR;/{cdump}.@Y@m@d/@H/atmos/{cdump}.t@Hz.wafs.grb2if024'
        dep_dict = {'type': 'data', 'data': data}
        deps.append(rocoto.add_dependency(dep_dict))
        data = f'&ROTDIR;/{cdump}.@Y@m@d/@H/atmos/{cdump}.t@Hz.wafs.grb2if027'
        dep_dict = {'type': 'data', 'data': data}
        deps.append(rocoto.add_dependency(dep_dict))
        data = f'&ROTDIR;/{cdump}.@Y@m@d/@H/atmos/{cdump}.t@Hz.wafs.grb2if030'
        dep_dict = {'type': 'data', 'data': data}
        deps.append(rocoto.add_dependency(dep_dict))
        data = f'&ROTDIR;/{cdump}.@Y@m@d/@H/atmos/{cdump}.t@Hz.wafs.grb2if033'
        dep_dict = {'type': 'data', 'data': data}
        deps.append(rocoto.add_dependency(dep_dict))
        data = f'&ROTDIR;/{cdump}.@Y@m@d/@H/atmos/{cdump}.t@Hz.wafs.grb2if036'
        dep_dict = {'type': 'data', 'data': data}
        deps.append(rocoto.add_dependency(dep_dict))
        dependencies = rocoto.create_dependency(dep_condition='and', dep=deps)
        task = wfu.create_wf_task('wafs', cdump=cdump, envar=envars, dependency=dependencies)
        tasks.append(task)
        tasks.append('\n')

    # wafsgcip
    if do_wafs in ['Y', 'YES']:
        deps = []
        data = f'&ROTDIR;/{cdump}.@Y@m@d/@H/atmos/{cdump}.t@Hz.wafs.grb2if006'
        dep_dict = {'type': 'data', 'data': data}
        deps.append(rocoto.add_dependency(dep_dict))
        data = f'&ROTDIR;/{cdump}.@Y@m@d/@H/atmos/{cdump}.t@Hz.wafs.grb2if012'
        dep_dict = {'type': 'data', 'data': data}
        deps.append(rocoto.add_dependency(dep_dict))
        data = f'&ROTDIR;/{cdump}.@Y@m@d/@H/atmos/{cdump}.t@Hz.wafs.grb2if015'
        dep_dict = {'type': 'data', 'data': data}
        deps.append(rocoto.add_dependency(dep_dict))
        data = f'&ROTDIR;/{cdump}.@Y@m@d/@H/atmos/{cdump}.t@Hz.wafs.grb2if018'
        dep_dict = {'type': 'data', 'data': data}
        deps.append(rocoto.add_dependency(dep_dict))
        data = f'&ROTDIR;/{cdump}.@Y@m@d/@H/atmos/{cdump}.t@Hz.wafs.grb2if021'
        dep_dict = {'type': 'data', 'data': data}
        deps.append(rocoto.add_dependency(dep_dict))
        data = f'&ROTDIR;/{cdump}.@Y@m@d/@H/atmos/{cdump}.t@Hz.wafs.grb2if024'
        dep_dict = {'type': 'data', 'data': data}
        deps.append(rocoto.add_dependency(dep_dict))
        data = f'&ROTDIR;/{cdump}.@Y@m@d/@H/atmos/{cdump}.t@Hz.wafs.grb2if027'
        dep_dict = {'type': 'data', 'data': data}
        deps.append(rocoto.add_dependency(dep_dict))
        data = f'&ROTDIR;/{cdump}.@Y@m@d/@H/atmos/{cdump}.t@Hz.wafs.grb2if030'
        dep_dict = {'type': 'data', 'data': data}
        deps.append(rocoto.add_dependency(dep_dict))
        data = f'&ROTDIR;/{cdump}.@Y@m@d/@H/atmos/{cdump}.t@Hz.wafs.grb2if033'
        dep_dict = {'type': 'data', 'data': data}
        deps.append(rocoto.add_dependency(dep_dict))
        data = f'&ROTDIR;/{cdump}.@Y@m@d/@H/atmos/{cdump}.t@Hz.wafs.grb2if036'
        dep_dict = {'type': 'data', 'data': data}
        deps.append(rocoto.add_dependency(dep_dict))
        dependencies = rocoto.create_dependency(dep_condition='and', dep=deps)
        task = wfu.create_wf_task('wafsgcip', cdump=cdump, envar=envars, dependency=dependencies)
        tasks.append(task)
        tasks.append('\n')

    # wafsgrib2
    if do_wafs in ['Y', 'YES']:
        deps = []
        data = f'&ROTDIR;/{cdump}.@Y@m@d/@H/atmos/{cdump}.t@Hz.wafs.grb2if006'
        dep_dict = {'type': 'data', 'data': data}
        deps.append(rocoto.add_dependency(dep_dict))
        data = f'&ROTDIR;/{cdump}.@Y@m@d/@H/atmos/{cdump}.t@Hz.wafs.grb2if012'
        dep_dict = {'type': 'data', 'data': data}
        deps.append(rocoto.add_dependency(dep_dict))
        data = f'&ROTDIR;/{cdump}.@Y@m@d/@H/atmos/{cdump}.t@Hz.wafs.grb2if015'
        dep_dict = {'type': 'data', 'data': data}
        deps.append(rocoto.add_dependency(dep_dict))
        data = f'&ROTDIR;/{cdump}.@Y@m@d/@H/atmos/{cdump}.t@Hz.wafs.grb2if018'
        dep_dict = {'type': 'data', 'data': data}
        deps.append(rocoto.add_dependency(dep_dict))
        data = f'&ROTDIR;/{cdump}.@Y@m@d/@H/atmos/{cdump}.t@Hz.wafs.grb2if021'
        dep_dict = {'type': 'data', 'data': data}
        deps.append(rocoto.add_dependency(dep_dict))
        data = f'&ROTDIR;/{cdump}.@Y@m@d/@H/atmos/{cdump}.t@Hz.wafs.grb2if024'
        dep_dict = {'type': 'data', 'data': data}
        deps.append(rocoto.add_dependency(dep_dict))
        data = f'&ROTDIR;/{cdump}.@Y@m@d/@H/atmos/{cdump}.t@Hz.wafs.grb2if027'
        dep_dict = {'type': 'data', 'data': data}
        deps.append(rocoto.add_dependency(dep_dict))
        data = f'&ROTDIR;/{cdump}.@Y@m@d/@H/atmos/{cdump}.t@Hz.wafs.grb2if030'
        dep_dict = {'type': 'data', 'data': data}
        deps.append(rocoto.add_dependency(dep_dict))
        data = f'&ROTDIR;/{cdump}.@Y@m@d/@H/atmos/{cdump}.t@Hz.wafs.grb2if033'
        dep_dict = {'type': 'data', 'data': data}
        deps.append(rocoto.add_dependency(dep_dict))
        data = f'&ROTDIR;/{cdump}.@Y@m@d/@H/atmos/{cdump}.t@Hz.wafs.grb2if036'
        dep_dict = {'type': 'data', 'data': data}
        deps.append(rocoto.add_dependency(dep_dict))
        dependencies = rocoto.create_dependency(dep_condition='and', dep=deps)
        task = wfu.create_wf_task('wafsgrib2', cdump=cdump, envar=envars, dependency=dependencies)
        tasks.append(task)
        tasks.append('\n')

    # wafsgrib20p25
    if do_wafs in ['Y', 'YES']:
        deps = []
        data = f'&ROTDIR;/{cdump}.@Y@m@d/@H/atmos/{cdump}.t@Hz.wafs.grb2if006'
        dep_dict = {'type': 'data', 'data': data}
        deps.append(rocoto.add_dependency(dep_dict))
        data = f'&ROTDIR;/{cdump}.@Y@m@d/@H/atmos/{cdump}.t@Hz.wafs.grb2if012'
        dep_dict = {'type': 'data', 'data': data}
        deps.append(rocoto.add_dependency(dep_dict))
        data = f'&ROTDIR;/{cdump}.@Y@m@d/@H/atmos/{cdump}.t@Hz.wafs.grb2if015'
        dep_dict = {'type': 'data', 'data': data}
        deps.append(rocoto.add_dependency(dep_dict))
        data = f'&ROTDIR;/{cdump}.@Y@m@d/@H/atmos/{cdump}.t@Hz.wafs.grb2if018'
        dep_dict = {'type': 'data', 'data': data}
        deps.append(rocoto.add_dependency(dep_dict))
        data = f'&ROTDIR;/{cdump}.@Y@m@d/@H/atmos/{cdump}.t@Hz.wafs.grb2if021'
        dep_dict = {'type': 'data', 'data': data}
        deps.append(rocoto.add_dependency(dep_dict))
        data = f'&ROTDIR;/{cdump}.@Y@m@d/@H/atmos/{cdump}.t@Hz.wafs.grb2if024'
        dep_dict = {'type': 'data', 'data': data}
        deps.append(rocoto.add_dependency(dep_dict))
        data = f'&ROTDIR;/{cdump}.@Y@m@d/@H/atmos/{cdump}.t@Hz.wafs.grb2if027'
        dep_dict = {'type': 'data', 'data': data}
        deps.append(rocoto.add_dependency(dep_dict))
        data = f'&ROTDIR;/{cdump}.@Y@m@d/@H/atmos/{cdump}.t@Hz.wafs.grb2if030'
        dep_dict = {'type': 'data', 'data': data}
        deps.append(rocoto.add_dependency(dep_dict))
        data = f'&ROTDIR;/{cdump}.@Y@m@d/@H/atmos/{cdump}.t@Hz.wafs.grb2if033'
        dep_dict = {'type': 'data', 'data': data}
        deps.append(rocoto.add_dependency(dep_dict))
        data = f'&ROTDIR;/{cdump}.@Y@m@d/@H/atmos/{cdump}.t@Hz.wafs.grb2if036'
        dep_dict = {'type': 'data', 'data': data}
        deps.append(rocoto.add_dependency(dep_dict))
        dependencies = rocoto.create_dependency(dep_condition='and', dep=deps)
        task = wfu.create_wf_task('wafsgrib20p25', cdump=cdump, envar=envars, dependency=dependencies)
        tasks.append(task)
        tasks.append('\n')

    # wafsblending
    if do_wafs in ['Y', 'YES']:
        deps = []
        dep_dict = {'type': 'task', 'name': f'{cdump}wafsgrib2'}
        deps.append(rocoto.add_dependency(dep_dict))
        dependencies = rocoto.create_dependency(dep=deps)
        task = wfu.create_wf_task('wafsblending', cdump=cdump, envar=envars, dependency=dependencies)
        tasks.append(task)
        tasks.append('\n')

    # wafsblending0p25
    if do_wafs in ['Y', 'YES']:
        deps = []
        dep_dict = {'type': 'task', 'name': f'{cdump}wafsgrib20p25'}
        deps.append(rocoto.add_dependency(dep_dict))
        dependencies = rocoto.create_dependency(dep=deps)
        task = wfu.create_wf_task('wafsblending0p25', cdump=cdump, envar=envars, dependency=dependencies)
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
        task = wfu.create_wf_task('gempak', cdump=cdump, envar=envars, dependency=dependencies)
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
    if do_post in ['Y', 'YES']:
        dep_dict = {'type':'metatask', 'name':f'{cdump}post'}
        deps.append(rocoto.add_dependency(dep_dict))
    if do_vrfy in ['Y', 'YES']:
        dep_dict = {'type':'task', 'name':f'{cdump}vrfy'}
        deps.append(rocoto.add_dependency(dep_dict))
    if cdump in ['gfs'] and do_metp in ['Y', 'YES']:
        dep_dict = {'type':'metatask', 'name':f'{cdump}metp'}
        deps.append(rocoto.add_dependency(dep_dict))
    dep_dict = {'type':'streq', 'left':'&ARCHIVE_TO_HPSS;', 'right':f'{hpssarch}'}
    deps.append(rocoto.add_dependency(dep_dict))
    if do_wave in ['Y', 'YES']:
      dep_dict = {'type': 'task', 'name': f'{cdump}wavepostsbs'}
      deps.append(rocoto.add_dependency(dep_dict))
      dep_dict = {'type': 'task', 'name': f'{cdump}wavepostpnt'}
      deps.append(rocoto.add_dependency(dep_dict))
      if do_wave_bnd in ['YES']:
          dep_dict = {'type': 'task', 'name': f'{cdump}wavepostbndpnt'}
          deps.append(rocoto.add_dependency(dep_dict))
    if do_ocean in ['Y', 'YES']:
      dep_dict = {'type': 'metatask', 'name': f'{cdump}ocnpost'}
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
