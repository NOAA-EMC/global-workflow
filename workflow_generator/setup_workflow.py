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
import sys
from argparse import ArgumentParser, ArgumentDefaultsHelpFormatter
sys.path.append(os.path.join(os.path.dirname(__file__), "../ush/rocoto"))
import workflow_utils as wfu


def parse_command_line():
    """Parse the arguments from the command line

    This function pulls in the command line arguments and parses them using the
    argparse module.

    Parameters
    ----------
    None

    Returns
    -------
    arguments : array
        An array of the arguments that were passed in as well as any
        that were defaulted.
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


def main():
    """Main function to start the workflow generator

    This is the main function that will read in the command line arguments
    using the parse_command_line function and create an array for the
    environment configurations to be used throughout the application.

    For the ecFlow setup, it sets up a new workflow and then uses the generic
    functions which are available for the Rocoto setup as well of
    generate_workflow and save.

    ** Important note: This function does also pull from the ush/rocoto
    application to use the get_configs and config_parser functions to populate
    the environment variable array.

    Parameters
    ----------
    None

    Returns
    -------
    None
    """
    args = parse_command_line()

    environment_configs = wfu.get_configs(args.expdir)
    envconfigs = {}
    envconfigs['base'] = wfu.config_parser([wfu.find_config('config.base',
                                            environment_configs)])

    # The default setup in the parse_command_line() function assumes that if
    # the --ecflow-config file is set that it should be an ecflow setup. When
    # Rocoto is implemented, the default for --ecflow-config should be removed
    # and additional parameters added.
    if args.ecflow_config is not None:
        from ecflow_setup.ecflow_setup import Ecflowsetup
        workflow = Ecflowsetup(args, envconfigs)
    else:
        import rocoto_setup

    workflow.generate_workflow()
    workflow.save()


# Main Initializer
if __name__ == "__main__":
    main()
    sys.exit(0)
