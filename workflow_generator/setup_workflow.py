#!/usr/bin/env python3

'''
    PROGRAM:
        Create a workflow file for use by a supercomputer.
    AUTHOR:
        Kyle Nevins
        kyle.nevins@noaa.gov
    FILE DEPENDENCIES:
        1. The configuration file that defines what jobs to run.
        2. config files for the parallel; e.g. config.base, config.fcst[.gfs], etc.
        Without this dependency, the script will fail
        3. The workflow utils package from the existing Rocoto generator. That
        is used to read in the configuration files in the expdir.
    OUTPUT:
        1. Either an ecFlow definition file or a Rocoto XML file
'''

import os
import sys
from argparse import ArgumentParser, ArgumentDefaultsHelpFormatter
from datetime import datetime

sys.path.append(os.path.join( os.path.dirname(__file__), "../ush/rocoto"))
import workflow_utils as wfu

def parse_command_line():
    parser = ArgumentParser(description='Create the workflow files for either ecFlow or Rocoto', formatter_class=ArgumentDefaultsHelpFormatter)
    parser.add_argument('--nodeskip', help='Nodes that will be set to defstatus complete', type=str, nargs='*', required=False)
    parser.add_argument('--taskskip', help='Tasks that will be set to defstatus complete', type=str, nargs='*', required=False)
    parser.add_argument('--ecflow-config', help='ecFlow Generator configuration file', type=str, default='ecflow_build.yml', required=False)
    parser.add_argument('--expdir',help='full path to experiment directory containing config files', type=str, required=False, default=os.environ['PWD'])
    parser.add_argument('--savedir', help='Location to save the definition files', type=str, required=False, default=os.environ['PWD'])

    arguments = parser.parse_args()

    return arguments

def main():
    args = parse_command_line()

    environment_configs = wfu.get_configs(args.expdir)
    envconfigs = {}
    envconfigs['base'] = wfu.config_parser([wfu.find_config('config.base', environment_configs)])

    # Will need to remove the default from arg parser to allow rocoto
    if args.ecflow_config is not None:
        from ecflow_setup.ecflow_setup import Ecflowsetup
        workflow = Ecflowsetup(args,envconfigs)
    else:
        import rocoto_setup

    workflow.generate_workflow()
    workflow.save()
    workflow.print()

if __name__ == "__main__":
    main()
    sys.exit(0)
