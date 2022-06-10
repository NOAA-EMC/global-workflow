#!/usr/bin/env python3

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
        print('MISMATCH in experiment directories!')
        print(f'config.base: EXPDIR = {repr(_base["EXPDIR"])}')
        print(f'input arg:     --expdir = {repr(args.expdir)}')
        sys.exit(1)

    gfs_steps = ['prep', 'anal', 'analdiag', 'analcalc', 'gldas', 'fcst', 'postsnd', 'post', 'vrfy', 'arch']
    gfs_steps_gempak = ['gempak']
    gfs_steps_awips = ['awips']
    gfs_steps_wafs = ['wafs', 'wafsgrib2', 'wafsblending', 'wafsgcip', 'wafsgrib20p25', 'wafsblending0p25']
    #hyb_steps = ['eobs', 'eomg', 'eupd', 'ecen', 'efcs', 'epos', 'earc']
    metp_steps = ['metp']
    wav_steps = ['waveinit', 'waveprep', 'wavepostsbs', 'wavepostbndpnt', 'wavepostbndpntbll', 'wavepostpnt']
    #Implement additional wave jobs at later date
    wav_steps_gempak = ['wavegempak']
    wav_steps_awips = ['waveawipsbulls', 'waveawipsgridded']
# From gfsv16b latest
#    gfs_steps = ['prep', 'anal', 'gldas', 'fcst', 'postsnd', 'post', 'awips', 'gempak', 'vrfy', 'metp', 'arch']
    hyb_steps = ['eobs', 'ediag', 'eomg', 'eupd', 'ecen', 'esfc', 'efcs', 'echgres', 'epos', 'earc']

    steps = gfs_steps + hyb_steps if _base.get('DOHYBVAR', 'NO') == 'YES' else gfs_steps
    steps = steps + metp_steps if _base.get('DO_METP', 'NO') == 'YES' else steps
    steps = steps + gfs_steps_gempak if _base.get('DO_GEMPAK', 'NO') == 'YES' else steps
    steps = steps + gfs_steps_awips if _base.get('DO_AWIPS', 'NO') == 'YES' else steps
    steps = steps + gfs_steps_wafs if _base.get('WAFSF', 'NO') == 'YES' else steps
    steps = steps + wav_steps if _base.get('DO_WAVE', 'NO') == 'YES' else steps
    steps = steps + wav_steps_gempak if _base.get('DO_GEMPAK', 'NO') == 'YES' else steps
    steps = steps + wav_steps_awips if _base.get('DO_AWIPS', 'NO') == 'YES' else steps

    dict_configs = wfu.source_configs(configs, steps)

    # Check and set gfs_cyc specific variables
    dict_configs['base'] = wfu.get_gfs_cyc_dates(dict_configs['base'])

    # First create workflow XML
    create_xml(dict_configs)

    # Next create the crontab
    wfu.create_crontab(dict_configs['base'])

    return


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
    do_wafs = base.get('WAFSF', 'NO').upper()
    do_metp = base.get('DO_METP', 'NO').upper()
    do_gldas = base.get('DO_GLDAS', 'NO').upper()
    do_wave = base.get('DO_WAVE', 'NO').upper()
    do_wave_cdump = base.get('WAVE_CDUMP', 'BOTH').upper()
    reservation = base.get('RESERVATION', 'NONE').upper()

    #tasks = ['prep', 'anal', 'fcst', 'post', 'vrfy', 'arch']
    tasks = ['prep', 'anal', 'analcalc']

    if cdump in ['gdas']:
        tasks += ['analdiag']
    if cdump in ['gdas'] and do_gldas in ['Y', 'YES']:
        tasks += ['gldas']
    if cdump in ['gdas'] and do_wave in ['Y', 'YES'] and do_wave_cdump in ['GDAS', 'BOTH']:
        #tasks += ['waveinit', 'waveprep', 'wavepostsbs', 'wavepostbndpnt', 'wavepostpnt', 'wavestat']
        tasks += ['waveinit', 'waveprep', 'wavepostsbs', 'wavepostbndpnt', 'wavepostbndpntbll', 'wavepostpnt']

    tasks += ['fcst', 'post', 'vrfy', 'arch']

    if cdump in ['gfs'] and do_wave in ['Y', 'YES'] and do_wave_cdump in ['GFS', 'BOTH']:
        #tasks += ['waveinit', 'waveprep', 'wavepostsbs', 'wavepostbndpnt', 'wavepostpnt', 'wavestat']
        tasks += ['waveinit', 'waveprep', 'wavepostsbs', 'wavepostbndpnt', 'wavepostbndpntbll', 'wavepostpnt']
    if cdump in ['gfs'] and do_bufrsnd in ['Y', 'YES']:
        tasks += ['postsnd']
    if cdump in ['gfs'] and do_gempak in ['Y', 'YES']:
        tasks += ['gempak']
    if cdump in ['gfs'] and do_wave in ['Y', 'YES'] and do_gempak in ['Y', 'YES']:
        tasks += ['wavegempak']
    if cdump in ['gfs'] and do_awips in ['Y', 'YES']:
        tasks += ['awips']
    if cdump in ['gfs'] and do_wafs in ['Y', 'YES']:
        tasks += ['wafs', 'wafsgrib2', 'wafsblending', 'wafsgcip', 'wafsgrib20p25', 'wafsblending0p25']
    if cdump in ['gfs'] and do_metp in ['Y', 'YES']:
        tasks += ['metp']
    if cdump in ['gfs'] and do_wave in ['Y', 'YES'] and do_awips in ['Y', 'YES']:
        tasks += ['waveawipsbulls', 'waveawipsgridded']

    dict_resources = OrderedDict()

    for task in tasks:

        cfg = dict_configs[task]

        wtimestr, resstr, queuestr, memstr, natstr = wfu.get_resources(machine, cfg, task, reservation, cdump=cdump)
        taskstr = f'{task.upper()}_{cdump.upper()}'

        strings = []
        strings.append(f'\t<!ENTITY QUEUE_{taskstr}     "{queuestr}">\n')
        if scheduler in ['slurm']:
            if task in ['arch']:
                strings.append(f'\t<!ENTITY PARTITION_{taskstr} "&PARTITION_SERVICE;">\n')
            else:
                strings.append(f'\t<!ENTITY PARTITION_{taskstr} "&PARTITION_BATCH;">\n')

        strings.append(f'\t<!ENTITY WALLTIME_{taskstr}  "{wtimestr}">\n')
        strings.append(f'\t<!ENTITY RESOURCES_{taskstr} "{resstr}">\n')
        if len(memstr) != 0:
            strings.append(f'\t<!ENTITY MEMORY_{taskstr}    "{memstr}">\n')
        strings.append(f'\t<!ENTITY NATIVE_{taskstr}    "{natstr}">\n')

        dict_resources[f'{cdump}{task}'] = ''.join(strings)

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
    reservation = base.get('RESERVATION', 'NONE').upper()

    dict_resources = OrderedDict()

    # These tasks can be run in either or both cycles
    if lobsdiag_forenkf in ['.T.', '.TRUE.']:
        tasks1 = ['eobs', 'ediag', 'eupd', 'echgres']
    else:
        tasks1 = ['eobs', 'eomg', 'eupd', 'echgres']

    if eupd_cyc in ['BOTH']:
        cdumps = ['gfs', 'gdas']
    elif eupd_cyc in ['GFS']:
        cdumps = ['gfs']
    elif eupd_cyc in ['GDAS']:
        cdumps = ['gdas']

    for cdump in cdumps:
        for task in tasks1:

            cfg = dict_configs['eobs'] if task in ['eomg'] else dict_configs[task]

            wtimestr, resstr, queuestr, memstr, natstr = wfu.get_resources(machine, cfg, task, reservation, cdump=cdump)

            taskstr = f'{task.upper()}_{cdump.upper()}'

            strings = []

            strings.append(f'\t<!ENTITY QUEUE_{taskstr}     "{queuestr}">\n')
            if scheduler in ['slurm']:
                strings.append(f'\t<!ENTITY PARTITION_{taskstr} "&PARTITION_BATCH;">\n')
            strings.append(f'\t<!ENTITY WALLTIME_{taskstr}  "{wtimestr}">\n')
            strings.append(f'\t<!ENTITY RESOURCES_{taskstr} "{resstr}">\n')
            if len(memstr) != 0:
                strings.appendf(f'\t<!ENTITY MEMORY_{taskstr}    "{memstr}">\n')
            strings.append(f'\t<!ENTITY NATIVE_{taskstr}    "{natstr}">\n')

            dict_resources[f'{cdump}{task}'] = ''.join(strings)


    # These tasks are always run as part of the GDAS cycle
    cdump = 'gdas'
    tasks2 = ['ecen', 'esfc', 'efcs', 'epos', 'earc']
    for task in tasks2:

        cfg = dict_configs[task]

        wtimestr, resstr, queuestr, memstr, natstr = wfu.get_resources(machine, cfg, task, reservation, cdump=cdump)

        taskstr = f'{task.upper()}_{cdump.upper()}'

        strings = []
        strings.append(f'\t<!ENTITY QUEUE_{taskstr}     "{queuestr}">\n')
        if scheduler in ['slurm']:
            if task in ['earc']:
                strings.append(f'\t<!ENTITY PARTITION_{taskstr} "&PARTITION_SERVICE;">\n')
            else:
                strings.append(f'\t<!ENTITY PARTITION_{taskstr} "&PARTITION_BATCH;">\n')

        strings.append(f'\t<!ENTITY WALLTIME_{taskstr}  "{wtimestr}">\n')
        strings.append(f'\t<!ENTITY RESOURCES_{taskstr} "{resstr}">\n')
        if len(memstr) != 0:
            strings.append(f'\t<!ENTITY MEMORY_{taskstr}    "{memstr}">\n')
        strings.append(f'\t<!ENTITY NATIVE_{taskstr}    "{natstr}">\n')

        dict_resources[f'{cdump}{task}'] = ''.join(strings)

    return dict_resources

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

    from builtins import any as b_any
    #from  __builtin__ import any as b_any

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
        hyp_tasks = {'gdaseobs':'gdaseobs',
                     'gdasediag':'gdasediag',
                     'gdaseomg':'gdaseomn',
                     'gdaseupd':'gdaseupd',
                     'gdasecen':'gdasecmn',
                     'gdasesfc':'gdasesfc',
                     'gdasefcs':'gdasefmn',
                     'gdasepos':'gdasepmn',
                     'gdasearc':'gdaseamn',
                     'gdasechgres':'gdasechgres'}
        for each_task, each_resource_string in dict_hyb_resources.items():
            #print(each_task,hyp_tasks[each_task])
            #print(dict_hyb_tasks[hyp_tasks[each_task]])
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
    for each_task, each_resource_string in dict_gdas_resources.items():
        if each_task not in dict_gdas_tasks:
            continue
        if 'MEMORY' not in each_resource_string:
            temp_task_string = []
            for each_line in re.split(r'(\s+)', dict_gdas_tasks[each_task]):
                if 'memory' not in each_line:
                     temp_task_string.append(each_line)
            dict_gdas_tasks[each_task] = ''.join(temp_task_string)

    # Removes <memory>&MEMORY_JOB_DUMP</memory> post mortem from gfs tasks
    for each_task, each_resource_string in dict_gfs_resources.items():
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

    xmlfile.append(workflow_footer)

    # Write the XML file
    fh = open(f'{base["EXPDIR"]}/{base["PSLOT"]}.xml', 'w')
    fh.write(''.join(xmlfile))
    fh.close()

    return


if __name__ == '__main__':
    main()
    sys.exit(0)
