#!/usr/bin/env python3
"""
Entry point for setting up workflow (Rocoto XML or EcFlow) for all applications in global-workflow
"""

import os
from logging import getLogger
from argparse import ArgumentParser, ArgumentDefaultsHelpFormatter

from applications.application_factory import app_config_factory
from rocoto.rocoto_xml_factory import rocoto_xml_factory
from wxflow import Configuration, Logger, logit


# Setup the logger
logger = getLogger(__name__)


# @logit(logger)
def input_args(*argv):
    """
    Method to collect user arguments for `setup_workflow.py`
    """

    description = """
        Sources configuration files based on application and
        creates workflow files for use with Rocoto or EcFlow.
        """

    parser = ArgumentParser(description=description,
                            formatter_class=ArgumentDefaultsHelpFormatter)

    # Common arguments across all modes
    parser.add_argument('expdir', help='full path to experiment directory containing config files',
                        type=str, default=os.environ['PWD'])
    parser.add_argument('--force', help='raise warnings instead of errors when possible',
                        action='store_true', dest="force")

    # Create subparsers for workflow engines
    subparsers = parser.add_subparsers(dest='workflow', required=True,
                                       help='workflow engine to use')

    # Rocoto subparser
    rocoto_parser = subparsers.add_parser('rocoto',
                                          help='Use Rocoto workflow engine',
                                          formatter_class=ArgumentDefaultsHelpFormatter)
    rocoto_parser.add_argument('--maxtries', help='maximum number of retries', type=int,
                               default=2, required=False)
    rocoto_parser.add_argument('--cyclethrottle', help='maximum number of concurrent cycles', type=int,
                               default=3, required=False)
    rocoto_parser.add_argument('--taskthrottle', help='maximum number of concurrent tasks', type=int,
                               default=25, required=False)
    rocoto_parser.add_argument('--verbosity', help='verbosity level of Rocoto', type=int,
                               default=10, required=False)

    # EcFlow subparser
    ecflow_parser = subparsers.add_parser('ecflow',
                                          help='Use EcFlow workflow engine',
                                          formatter_class=ArgumentDefaultsHelpFormatter)
    # EcFlow specific arguments can be added here in the future

    return parser.parse_args(argv[0][0] if len(argv[0]) else None)


# @logit(logger)
def check_expdir(cmd_expdir, cfg_expdir):

    if not os.path.samefile(cmd_expdir, cfg_expdir):
        logger.exception('MISMATCH in experiment directories!')
        logger.error(f'config.base:   EXPDIR = {cfg_expdir}')
        logger.error(f'  input arg: --expdir = {cmd_expdir}')
        raise ValueError('Abort!')


# @logit(logger)
def check_dir_writable(dir_path):
    if os.path.isdir(dir_path):
        if os.access(dir_path, os.W_OK):
            return True
        else:
            return False
    elif os.path.isfile(dir_path):
        return False
    else:  # Find the nearest parent directory that already exists
        test_parent = os.path.dirname(dir_path)
        if len(test_parent) == 0:
            return False
        while test_parent:
            if os.path.exists(test_parent):
                # Call check_dir_writable on the parent
                return check_dir_writable(test_parent)
            test_parent = os.path.dirname(test_parent)
            if len(test_parent) == 0:
                break
        if len(test_parent) == 0:
            return False


@logit(logger, name="setup_workflow.main")
def main(*argv):

    user_inputs = input_args(argv)

    # Handle workflow engine selection
    if user_inputs.workflow == 'ecflow':
        logger.info("EcFlow workflow engine selected")
        logger.error("EcFlow workflow is not yet implemented. Please use Rocoto for now.")
        raise NotImplementedError("EcFlow workflow is not yet implemented")

    logger.info("Rocoto workflow engine selected")

    # Build rocoto parameter dictionary - only available when rocoto is selected
    rocoto_param_dict = {}
    if user_inputs.workflow == 'rocoto':
        rocoto_param_dict = {'maxtries': user_inputs.maxtries,
                             'cyclethrottle': user_inputs.cyclethrottle,
                             'taskthrottle': user_inputs.taskthrottle,
                             'verbosity': user_inputs.verbosity}

    cfg = Configuration(user_inputs.expdir)

    base = cfg.parse_config('config.base')

    check_expdir(user_inputs.expdir, base['EXPDIR'])

    # Check if "HOMEDIR","STMP","PTMP" dirrctories are writable
    dir_keys = ["HOMEDIR", "STMP", "PTMP"]
    for dk in dir_keys:
        check_dir_writable(base[dk])
        if not check_dir_writable(base[dk]):
            msg = f'The {dk} path {base[dk]} cannot be written to!  Please correct this path and try again.'
            if user_inputs.force:
                print(f"WARNING {msg}")
            else:
                raise PermissionError(f'{msg}')

    net = base['NET']
    mode = base['MODE']

    # Configure the application
    app_config = app_config_factory.create(f'{net}_{mode}', cfg)

    # Create Rocoto Tasks and Assemble them into an XML
    xml = rocoto_xml_factory.create(f'{net}_{mode}', app_config, rocoto_param_dict)
    xml.write()


if __name__ == '__main__':

    # Setup the logger
    logger = Logger(logfile_path=os.environ.get("LOGFILE_PATH"),
                    level=os.environ.get("LOGGING_LEVEL", "INFO"),
                    colored_log=os.environ.get("COLORED_LOG", True))

    main()
