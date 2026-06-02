#!/usr/bin/env python3
"""
Entry point for setting up workflow (ecFlow only) for all applications in global-workflow.

The workflow is orchestrated exclusively by ecFlow. Rocoto has been decommissioned
per Requirement 1 (ecFlow-Only Orchestration).
"""

import os
from logging import getLogger
from argparse import ArgumentParser, ArgumentDefaultsHelpFormatter

from applications.application_factory import app_config_factory
from ecflow.ecflow_suite_factory import ecflow_suite_factory
from wxflow import AttrDict, Configuration, Logger, logit


# Setup the logger
logger = getLogger(__name__)


class RocotoDecommissionedError(RuntimeError):
    """Raised when a decommissioned Rocoto code path is invoked."""
    pass


def rocoto_deprecation_guard():
    """Emit a FATAL ERROR if a Rocoto code path is invoked.

    Per Requirement 1 (ecFlow-Only Orchestration) and Requirement 14.3,
    Rocoto has been decommissioned. This function should be called whenever
    any code path detects an attempt to use Rocoto.

    Raises
    ------
    RocotoDecommissionedError
        Always raised with a message referencing Requirement 1.
    """
    msg = (
        "FATAL ERROR: Rocoto is decommissioned per Requirement 1. "
        "Use ecFlow-only orchestration."
    )
    logger.critical(msg)
    raise RocotoDecommissionedError(msg)


def _check_for_rocoto_invocation(argv):
    """Check if the user attempted to invoke Rocoto and emit FATAL ERROR.

    Scans the raw argument list for 'rocoto' before argparse processes it,
    since the rocoto subparser has been removed and argparse would otherwise
    emit a generic 'invalid choice' error.
    """
    if argv is not None:
        for arg in argv:
            if arg.lower() == 'rocoto':
                rocoto_deprecation_guard()


# @logit(logger)
def input_args(*argv):
    """
    Method to collect user arguments for `setup_workflow.py`
    """

    description = """
        Sources configuration files based on application and
        creates workflow files for use with ecFlow.
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

    # ecFlow subparser (sole supported engine)
    ecflow_parser = subparsers.add_parser('ecflow',
                                          help='Use ecFlow workflow engine',
                                          formatter_class=ArgumentDefaultsHelpFormatter)
    ecflow_parser.add_argument('--verbosity', help='verbosity level of ecflow', type=int,
                               default=10, required=False)

    # Check for Rocoto invocation before argparse processes args
    # (provides a clear FATAL ERROR instead of a generic 'invalid choice' message)
    raw_args = argv[0][0] if len(argv[0]) else None
    _check_for_rocoto_invocation(raw_args)

    args = parser.parse_args(raw_args)

    return args


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
    workflow_engine = user_inputs.workflow

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
            else:
                raise PermissionError(f'{msg}')

    net = base['NET']
    mode = base['MODE']

    # Configure the application
    app_config = app_config_factory.create(f'{net}_{mode}', cfg)

    # Build workflow parameter dictionary
    workflow_config = AttrDict()
    workflow_config.workflow_engine = workflow_engine
    workflow_config.verbosity = user_inputs.verbosity

    # Create the ecFlow Suite object
    workflow = ecflow_suite_factory.create(f'{net}_{mode}', app_config, workflow_config)
    workflow.write()


if __name__ == '__main__':

    # Setup the logger
    logger = Logger(logfile_path=os.environ.get("LOGFILE_PATH"),
                    level=os.environ.get("LOGGING_LEVEL", "INFO"),
                    colored_log=os.environ.get("COLORED_LOG", True))

    main()
