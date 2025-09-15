#!/usr/bin/env python3
"""
Entry point for setting up Rocoto XML for all applications in global-workflow
"""

import os
from logging import getLogger
from argparse import ArgumentParser, ArgumentDefaultsHelpFormatter

from applications.application_factory import app_config_factory
from rocoto.rocoto_xml_factory import rocoto_xml_factory
from ecflow.ecflow_suite_factory import ecflow_suite_factory
from wxflow import Configuration, Logger, logit


# Setup the logger
logger = getLogger(__name__)


# @logit(logger)
def input_args(*argv):
    """
    Method to collect user arguments for `setup_xml.py`
    """

    description = """
        Sources configuration files based on application and
        creates "$PSLOT.xml" for use with Rocoto.
        """

    parser = ArgumentParser(description=description,
                            formatter_class=ArgumentDefaultsHelpFormatter)

    # Common arguments across all modes
    parser.add_argument('expdir', help='full path to experiment directory containing config files',
                        type=str, default=os.environ['PWD'])

    parser.add_argument('--maxtries', help='maximum number of retries', type=int,
                        default=2, required=False)
    parser.add_argument('--cyclethrottle', help='maximum number of concurrent cycles', type=int,
                        default=3, required=False)
    parser.add_argument('--taskthrottle', help='maximum number of concurrent tasks', type=int,
                        default=25, required=False)
    parser.add_argument('--verbosity', help='verbosity level of Rocoto', type=int,
                        default=10, required=False)
    parser.add_argument('--force', help='raise warnings instead of errors when possible',
                        action='store_true', dest="force")

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


@logit(logger, name="setup_xml.main")
def main(*argv):

    user_inputs = input_args(argv)
    rocoto_param_dict = {'maxtries': user_inputs.maxtries,
                         'cyclethrottle': user_inputs.cyclethrottle,
                         'taskthrottle': user_inputs.taskthrottle,
                         'verbosity': user_inputs.verbosity}

    cfg = Configuration(user_inputs.expdir)

    base = cfg.parse_config('config.base')

    check_expdir(user_inputs.expdir, base['EXPDIR'])

    # Check if "HOMEDIR","STMP","PTMP" directories are writeable
    dir_keys = ["HOMEDIR", "STMP", "PTMP"]
    for dk in dir_keys:
        if not check_dir_writable(base[dk]):
            msg = f'The {dk} path {base[dk]} cannot be written to!  Please correct this path and try again.'
            if user_inputs.force:
                print(f"WARNING {msg}")
#            else:
#                raise PermissionError(f'{msg}')

    net = base['NET']
    mode = base['MODE']

    # Configure the application
    app_config = app_config_factory.create(f'{net}_{mode}', cfg)

    # Call the appropriate workflow engine factory
    ENGINE_MAP = {
        'rocoto': rocoto_xml_factory,
        'ecflow': ecflow_suite_factory,
    }

    # Create the XML (Rocoto) or Suite (ecFlow) object
    workflow = ENGINE_MAP[workflow_engine].create(f'{net}_{mode}', app_config, workflow_param_dict)
    workflow.write()


if __name__ == '__main__':

    # Setup the logger
    logger = Logger(logfile_path=os.environ.get("LOGFILE_PATH"),
                    level=os.environ.get("LOGGING_LEVEL", "INFO"),
                    colored_log=os.environ.get("COLORED_LOG", True))

    main()
