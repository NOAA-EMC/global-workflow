#!/usr/bin/env python3

"""
Entry point for rendering the Jinja2-templated auxiliary workflow (aux.xml.j2)
into a Rocoto XML workflow file.

Workflow configuration is read from a YAML file (see dev/parm/aux/aux.yaml for
a sample). If no config path is provided, the script locates the repository root
via ``git rev-parse`` and uses ``<HOMEglobal>/dev/parm/aux/aux.yaml``.

The aux.xml will be rendered into the directory ``EXP_aux`` specified in the configuration YAML.
The config files from parm/config/gfs (or the override directory specified by --gfs_config_dir)
will be linked into EXP_aux for use by the workflow.

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
_REQUIRED_CONFIG_KEYS = ['start_date', 'end_date', 'EXP_aux',
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


def calc_start_end_metp_dates(start_date, end_date):
    """
    Calculate the start and end dates for the METplus METP tool based on the
    provided workflow start and end dates. METplus runs on the 18z cycle only.

    Parameters
    ----------
    start_date : str
        Workflow start date in YYYYMMDDHH format
    end_date : str
        Workflow end date in YYYYMMDDHH format

    Returns
    -------
    tuple of str
        Tuple containing the calculated start and end dates for METP in YYYYMMDDHH format
    """

    from datetime import datetime, timedelta

    # Parse the input dates
    # Assume input dates are in UTC and in the format YYYYMMDDHHmm

    start_dt = datetime.strptime(start_date, '%Y%m%d%H%M')
    end_dt = datetime.strptime(end_date, '%Y%m%d%H%M')

    # Calculate the METP start date (the 18z cycle on the workflow start date)
    metp_start_dt = (start_dt - timedelta(days=1)).replace(hour=18, minute=0, second=0)

    # Calculate the METP end date (the 18z cycle on the workflow end date)
    metp_end_dt = end_dt.replace(hour=18, minute=0, second=0)

    # Convert to strings in the format YYYYMMDDHH
    metp_start_str = metp_start_dt.strftime('%Y%m%d%H%M')
    metp_end_str = metp_end_dt.strftime('%Y%m%d%H%M')

    return metp_start_str, metp_end_str


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

    parser.add_argument('--aux_config',
                        help='Full path to the aux configuration YAML file. '
                             'Defaults to <HOMEglobal>/dev/parm/aux/aux.yaml',
                        type=str, default=None)

    parser.add_argument('--gfs_config_dir',
                        help='Full path to the directory containing GFS configuration files. '
                             'Defaults to <HOMEglobal>/parm/config/gfs',
                        type=str, default=None)

    return parser.parse_args()


@logit(logger)
def main():

    user_inputs = input_args()

    if user_inputs.aux_config is None:
        HOMEglobal = _get_HOMEglobal()
        aux_config_path = os.path.join(HOMEglobal, 'dev', 'parm', 'aux', 'aux.yaml')
    else:
        aux_config_path = user_inputs.aux_config

    if user_inputs.gfs_config_dir is None:
        HOMEglobal = _get_HOMEglobal()
        gfs_config_dir = os.path.join(HOMEglobal, 'parm', 'config', 'gfs')
    else:
        gfs_config_dir = user_inputs.gfs_config_dir

    logger.info(f'Reading aux configuration: {aux_config_path}')
    with open(aux_config_path, 'r') as f:
        context = yaml.safe_load(f)

    missing_keys = [key for key in _REQUIRED_CONFIG_KEYS if key not in context]
    if missing_keys:
        raise KeyError(f"Required key(s) missing from config file {aux_config_path}: "
                       f"{', '.join(missing_keys)}")

    # Get the start and end METp dates
    metp_start_date, metp_end_date = calc_start_end_metp_dates(context['start_date'], context['end_date'])
    context['start_date_metp'] = metp_start_date
    context['end_date_metp'] = metp_end_date
    logger.info(f"Calculated METP start date: {metp_start_date}, METP end date: {metp_end_date}")

    # Check if HOMEglobal is set in the context, if not, set it using _get_HOMEglobal()
    if 'HOMEglobal' not in context:
        context['HOMEglobal'] = _get_HOMEglobal()
        logger.info(f"HOMEglobal not found in config; set to: {context['HOMEglobal']}")

    template_path = os.path.join(context['HOMEglobal'], 'dev', 'workflow', 'aux', 'aux.xml.j2')
    output_path = os.path.join(context['EXP_aux'], 'aux.xml')

    # Create the output directory if it doesn't exist
    os.makedirs(context['EXP_aux'], exist_ok=True)

    # Render the Jinja2 template with the provided context and save to output_path
    logger.info(f'Rendering aux.xml template: {template_path}')
    Jinja(template_path, context).save(output_path)
    logger.info(f'Rendered aux.xml written to: {output_path}')

    # Issue a warning if EXP_aux and gfs_config_dir are the same and skip linking
    if os.path.abspath(context['EXP_aux']) == os.path.abspath(gfs_config_dir):
        logger.warning(f"EXP_aux ({context['EXP_aux']}) and gfs_config_dir ({gfs_config_dir}) are the same. "
                       "Skipping linking of GFS config files to avoid overwriting.")
    else:
        logger.info(f"Linking GFS config files from {gfs_config_dir} to {context['EXP_aux']}")

        config_files = [f for f in os.listdir(gfs_config_dir) if os.path.isfile(os.path.join(gfs_config_dir, f))]
        for config in config_files:
            src = os.path.join(gfs_config_dir, config)
            dst = os.path.join(context['EXP_aux'], config)
            if not os.path.exists(dst):
                os.symlink(src, dst)
            else:
                logger.warning(f"Link {dst} already exists. Skipping.")

if __name__ == '__main__':

    # Setup the logger
    logger = Logger(logfile_path=os.environ.get("LOGFILE_PATH"),
                    level=os.environ.get("LOGGING_LEVEL", "INFO"),
                    colored_log=os.environ.get("COLORED_LOG", True))

    main()
