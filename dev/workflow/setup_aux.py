#!/usr/bin/env python3

"""
Entry point for rendering the Jinja2-templated auxiliary workflow (aux.xml.j2)
into a Rocoto XML workflow file.

Workflow configuration is read from a YAML file (see dev/parm/aux/aux.yaml for
a sample). If no config path is provided, the script locates the repository root
via ``git rev-parse`` and uses ``<HOMEglobal>/dev/parm/aux/aux.yaml``.

NOTES:
    The dev/ush/gw_setup.sh script must be sourced before running this script
    to set up the Python environment with the wxflow library.
"""

import os
import yaml
from logging import getLogger
from argparse import ArgumentParser, ArgumentDefaultsHelpFormatter

from wxflow import Executable, Jinja, Logger, logit
from wxflow.executable import ProcessError

_here = os.path.dirname(os.path.abspath(__file__))

# Setup the logger
logger = getLogger(__name__)

# Required keys in the configuration YAML
_REQUIRED_CONFIG_KEYS = ['start_date', 'end_date', 'HOMEglobal', 'EXP_aux',
                         'ECF_OUT_gfs', 'COM_aux', 'DATAROOT_aux']


def _get_HOMEglobal():
    """
    Determine the repository root directory using ``git rev-parse``.

    Parameters
    ----------
    None

    Returns
    -------
    str
        Absolute path to the repository root directory

    Raises
    ------
    RuntimeError
        If the git command fails
    """
    try:
        git = Executable('git')
        result = git('-C', _here, 'rev-parse', '--show-toplevel', output=str)
        return result.strip()
    except ProcessError as e:
        raise RuntimeError(f"Failed to determine HOMEglobal via git: {e}") from e


def input_args():
    """
    Method to collect user arguments for ``setup_aux.py``

    Parameters
    ----------
    None

    Returns
    -------
    argparse.Namespace
        Parsed command-line arguments
    """

    description = """
        Renders the Jinja2-templated auxiliary workflow XML (aux.xml.j2)
        into a Rocoto XML workflow file for use with the Rocoto workflow manager.

        Workflow configuration is read from a YAML file. Copy
        dev/parm/aux/aux.yaml, fill in the values for your environment, and
        pass the path via --config.

        The dev/ush/gw_setup.sh script must be sourced before running this script
        to ensure the Python environment with wxflow is properly configured.
        """

    parser = ArgumentParser(description=description,
                            formatter_class=ArgumentDefaultsHelpFormatter)

    parser.add_argument('--config',
                        help='Full path to the aux configuration YAML file. '
                             'Defaults to <HOMEglobal>/dev/parm/aux/aux.yaml',
                        type=str, default=None)

    return parser.parse_args()


@logit(logger)
def main():

    user_inputs = input_args()

    if user_inputs.config is None:
        HOMEglobal = _get_HOMEglobal()
        config_path = os.path.join(HOMEglobal, 'dev', 'parm', 'aux', 'aux.yaml')
    else:
        config_path = user_inputs.config

    logger.info(f'Reading aux configuration: {config_path}')
    with open(config_path, 'r') as f:
        context = yaml.safe_load(f)

    missing_keys = [key for key in _REQUIRED_CONFIG_KEYS if key not in context]
    if missing_keys:
        raise KeyError(f"Required key(s) missing from config file {config_path}: "
                       f"{', '.join(missing_keys)}")

    template_path = os.path.join(context['HOMEglobal'], 'dev', 'parm', 'aux', 'aux.xml.j2')
    output_path = context.get('output') or os.path.join(context['EXP_aux'], 'aux.xml')

    # Create the output directory if it doesn't exist
    os.makedirs(os.path.dirname(output_path), exist_ok=True)

    # Render the Jinja2 template with the provided context and save to output_path
    logger.info(f'Rendering aux.xml template: {template_path}')
    Jinja(template_path, context).save(output_path)
    logger.info(f'Rendered aux.xml written to: {output_path}')


if __name__ == '__main__':

    # Setup the logger
    logger = Logger(logfile_path=os.environ.get("LOGFILE_PATH"),
                    level=os.environ.get("LOGGING_LEVEL", "INFO"),
                    colored_log=os.environ.get("COLORED_LOG", True))

    main()
