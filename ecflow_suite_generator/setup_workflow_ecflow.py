#!/usr/bin/env python3

'''
    PROGRAM:
        Create the ecFlow workflow
    AUTHOR:
        Kyle Nevins
        kyle.nevins@noaa.gov
    FILE DEPENDENCIES:
        1. configuration file config.json
        2. config files for the parallel; e.g. config.base, config.fcst[.gfs], etc.
        Without this dependency, the script will fail
    OUTPUT:
        1. ecFlow definition file
        2. ecFlow scripts for post processing
'''

import os
import sys
from argparse import ArgumentParser, ArgumentDefaultsHelpFormatter
from datetime import datetime

import ecflow_config
from suites.ecflow_definitions import ecFlowDefinition

sys.path.append(os.path.join( os.path.dirname(__file__), "../ush/rocoto"))
import workflow_utils as wfu

def parse_command_line():
    parser = ArgumentParser(description='Create the definition file and ECF scripts to run an ecFlow workflow', formatter_class=ArgumentDefaultsHelpFormatter)
    parser.add_argument('--file',help='Path to the additional setup file to use.', type=str, required=False)
    parser.add_argument('--suite',help='Model suite to run: 00/06/12/18', type=str, choices=['00', '06', '12', '18'], default='00', required=True)
    parser.add_argument('--model',help='Model suite to run: gfs, gdas, enkf', type=str, default='all', choices=['gfs', 'gdas', 'enkf'], required=False)
    parser.add_argument('--forecast-only',help='Run the forecast only job', action='store_true', required=False)
    parser.add_argument('--postprocess', help='Include post processing suite', action='store_true', required=False)
    parser.add_argument('--generator-config', help='ecFlow Generator configuration file', type=str, default='ecflow_definitions.yml', required=False)
    parser.add_argument('--expdir',help='full path to experiment directory containing config files', type=str, required=False, default=os.environ['PWD'])

    arguments = parser.parse_args()

    return arguments

def main():
    args = parse_command_line()
    generator_config = args.generator_config

    # Set the suite to run
    os.environ['cyc'] = args.suite

    ecFlowconfig = ecflow_config.load_ecflow_config('%s' % generator_config )
    configs = wfu.get_configs(args.expdir)

    #if args.model == 'all':
    #    taskplan = ecFlowconfig['models'].keys()
    #else:
    #    taskplan = [args.model]
    #print(taskplan)
    #dict_configs = wfu.source_configs(configs, taskplan)

    envconfigs = {}
    envconfigs['base'] = wfu.config_parser([wfu.find_config('config.base', configs)])

    updated_ecFlowconfig = ecflow_config.update_ecflow_config(ecFlowconfig,envconfigs)

    ecFlowDef = ecFlowDefinition(updated_ecFlowconfig,envconfigs,args)

    if args.forecast_only:
        ecFlowDef.forecast_only()
    else:
        ecFlowDef.full_suite()

    ecFlowDef.display()

if __name__ == "__main__":
    main()
    sys.exit(0)
