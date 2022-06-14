#!/usr/bin/env python3

"""
    PROGRAM:
        Create the ROCOTO workflow for any experiment given the configuration of the GFS parallel

    FILE DEPENDENCIES:
        1. config files for the parallel; e.g. config.base, config.fcst[.gfs], etc.
        Without this dependency, the script will fail

    OUTPUT:
        1. PSLOT.xml: XML workflow
        2. PSLOT.crontab: crontab for ROCOTO run command

"""

import os
import sys
import re

from argparse import ArgumentParser, ArgumentDefaultsHelpFormatter
from hosts import Host
import workflow_utils as wfu
from configuration import Configuration


@property
def input_args():
    """
    Method to collect user arguments for `setup_workflow.py`
    """

    here = os.path.dirname(__file__)
    top = os.path.abspath(os.path.join(
        os.path.abspath(here), '../..'))

    description = """
        Sources configuration files based on application and\n
        creates "PSLOT.xml" for use with Rocoto.\n
        """

    parser = ArgumentParser(description=description,
                            formatter_class=ArgumentDefaultsHelpFormatter)

    parser.add_argument('--expdir', help='full path to experiment directory containing config files', type=str,
                        required=False, default=os.environ['PWD'])

    # Set up sub-parsers for various modes of experimentation
    subparser = parser.add_subparsers(dest='mode')
    cycled = subparser.add_parser(
        'cycled', help='arguments for cycled mode')
    forecasts = subparser.add_parser(
        'forecast-only', help='arguments for forecast-only mode')

    forecasts.add_argument('--cdump', help='cycle to run forecasts', type=str, choices=['gdas', 'gfs'], default='gfs',
                           required=False)

    args = parser.parse_args()

    return args


def main():

    user_inputs = input_args

    cfg = Configuration(user_inputs.expdir)


    sys.exit(0)

    _base = cfg.parse_config('config.base')

    if not os.path.samefile(args.expdir, _base['EXPDIR']):
        print('MISMATCH in experiment directories!')
        print(f'''config.base: EXPDIR = {repr(_base['EXPDIR'])}''')
        print(f'input arg:     --expdir = {repr(args.expdir)}')
        sys.exit(1)

    dict_configs = wfu.source_configs(cfg, taskplan)

    dict_configs['base']['CDUMP'] = args.cdump

    # First create workflow XML
    create_xml(dict_configs)

    # Next create the crontab
    wfu.create_crontab(dict_configs['base'])

    return


def get_resources(dict_configs, task_plan, cdump='gdas'):
    '''
        Create resource entities
    '''

    resources = wfu.get_taskplan_resources(dict_configs, task_plan, cdump=cdump)

    scheduler = Host.get_scheduler

    strings = []
    strings.append('\t<!-- BEGIN: Resource requirements for the workflow -->\n')
    strings.append('\n')

    for task in task_plan:

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


def get_workflow_body(dict_configs, cdump='gdas'):
    '''
        Create the workflow body
    '''

    strings = []

    strings.append('\n')
    strings.append(']>\n')
    strings.append('\n')
    strings.append(
        '<workflow realtime="F" scheduler="&SCHEDULER;" cyclethrottle="&CYCLETHROTTLE;" taskthrottle="&TASKTHROTTLE;">\n')
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

    # Removes <memory>&MEMORY_JOB_DUMP</memory> post mortem from gdas task_tasks
    temp_workflow = ''
    memory_dict = []
    for each_resource_string in re.split(r'(\s+)', resources):
        if 'MEMORY' in each_resource_string:
            memory_dict.append(each_resource_string)
    for each_line in re.split(r'(\s+)', workflow):
        if 'MEMORY' not in each_line:
            temp_workflow += each_line
        else:
            if any(substring in each_line for substring in memory_dict):
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
