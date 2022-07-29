#!/usr/bin/env python3

"""
    PROGRAM:
        Create a workflow file for use by a supercomputer.
    AUTHOR:
        Kyle Nevins
        kyle.nevins@noaa.gov
    FILE DEPENDENCIES:
        1. The configuration file that defines what jobs to run. It should be a
        YAML file following the syntax defined in the README.
        2. config files for the experiment; e.g. config.base, config.fcst[.gfs]
        etc.
        Without this dependency, the script will fail
        3. The workflow utils package from the existing Rocoto generator. That
        is used to read in the configuration files in the expdir.
        4. Any scripts defined in the YAML file must be present within the
        script repository.
    OUTPUT:
        1. Either an ecFlow definition file or a Rocoto XML file
        2. The folders and scripts needed to run either the ecflow suite or
        Rocoto suite.
"""

import os
from argparse import ArgumentParser, ArgumentDefaultsHelpFormatter
from configuration import Configuration
from ecFlow.ecflow_setup import Ecflowsetup


def input_args():
    """
    Method to collect user arguments for `setup_workflow.py`
    """
    parser = ArgumentParser(description=""" Create the workflow files for
                                ecFlow by deploying scripts and definition
                                files or Rocoto""",
                            formatter_class=ArgumentDefaultsHelpFormatter)
    parser.add_argument('--ecflow-config', type=str,
                        default='ecflow_build.yml', required=False,
                        help='ecFlow Generator configuration file')
    parser.add_argument('--expdir', type=str,
                        required=False, default=os.environ['PWD'],
                        help="""This is to be the full path to experiment'
                        'directory containing config files""")
    parser.add_argument('--savedir', type=str,
                        default=os.environ['PWD'], required=False,
                        help='Location to save the definition files')
    arguments = parser.parse_args()

    return arguments


if __name__ == "__main__":
    """
    This is the main function that will read in the command line arguments
    using the parse_command_line function and create an array for the
    environment configurations to be used throughout the application.

    For the ecFlow setup, it sets up a new workflow and then uses the generic
    functions which are available for the Rocoto setup as well of
    generate_workflow and save.
    """

    args = input_args()

    cfg = Configuration(args.expdir)
    envconfigs = dict()
    envconfigs['base'] = cfg.parse_config('config.base')

    workflow = Ecflowsetup(args, envconfigs)
    workflow.generate_workflow()
    workflow.save()
