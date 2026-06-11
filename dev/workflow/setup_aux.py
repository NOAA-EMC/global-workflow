#!/usr/bin/env python3

"""
Entry point for rendering the Jinja2-templated auxiliary workflow (aux.xml.j2)
into a Rocoto XML workflow file.

Workflow configuration is read from a Jinja2-templated YAML file
(``dev/parm/aux/aux.yaml.j2``).  Most variables are derived automatically
when ``--expdir`` is supplied; they are read from ``config.base`` in the
target experiment directory.  Any variable that cannot be derived
(e.g. ``ECF_OUT_gfs``) must be set explicitly in a copy of the template
passed via ``--config``.

Optionally, a crontab entry that runs ``rocotorun`` every five minutes can
be written alongside the XML by passing ``--crontab``.

NOTES:
    The dev/ush/gw_setup.sh script must be sourced before running this script
    to set up the Python environment with the wxflow library.
"""

import os
from logging import getLogger
from argparse import ArgumentParser, ArgumentDefaultsHelpFormatter

from wxflow import AttrDict, Configuration, Executable, Jinja, Logger, logit, parse_j2yaml, which
from wxflow.executable import ProcessError

_here = os.path.dirname(os.path.abspath(__file__))

# Setup the logger
logger = getLogger(__name__)

# Keys that must be present (and non-empty) in the rendered configuration
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


def read_config_base(expdir):
    """
    Source ``config.base`` from the given experiment directory and return its
    exported variables as a dictionary.

    Parameters
    ----------
    expdir : str
        Full path to the experiment directory that contains ``config.base``.

    Returns
    -------
    AttrDict
        Dictionary of all variables exported by ``config.base``.

    Raises
    ------
    FileNotFoundError
        If ``config.base`` is not found inside *expdir*.
    RuntimeError
        If sourcing ``config.base`` fails.
    """
    config_base = os.path.join(expdir, 'config.base')
    if not os.path.isfile(config_base):
        raise FileNotFoundError(
            f"config.base not found in experiment directory: {expdir}\n"
            "Verify that --expdir points to a valid experiment directory "
            "that has been set up with setup_expt.py.")
    cfg = Configuration(expdir)
    logger.info(f'Reading config.base from: {config_base}')
    try:
        return cfg.parse_config('config.base')
    except Exception as exc:
        raise RuntimeError(f"Failed to source config.base in {expdir}: {exc}") from exc


def calc_start_end_metp_dates(start_date, end_date):
    """
    Calculate the start and end dates for the METplus METP tool based on the
    provided workflow start and end dates. METplus runs on the 18z cycle only.

    Parameters
    ----------
    start_date : str
        Workflow start date in YYYYMMDDHHMM format (12 digits).
        When derived from config.base, SDATE (YYYYMMDDHH) has '00' appended
        for the minutes field by the aux.yaml.j2 template.
    end_date : str
        Workflow end date in YYYYMMDDHHMM format (12 digits).

    Returns
    -------
    tuple of str
        Tuple containing the calculated start and end dates for METP in
        YYYYMMDDHHMM format
    """

    from datetime import datetime, timedelta

    # Parse the input dates (UTC, format YYYYMMDDHHmm)
    start_dt = datetime.strptime(start_date, '%Y%m%d%H%M')
    end_dt = datetime.strptime(end_date, '%Y%m%d%H%M')

    # Calculate the METP start date (18z cycle on the day before start)
    metp_start_dt = (start_dt - timedelta(days=1)).replace(hour=18, minute=0, second=0)

    # Calculate the METP end date (18z cycle on the workflow end date)
    metp_end_dt = end_dt.replace(hour=18, minute=0, second=0)

    metp_start_str = metp_start_dt.strftime('%Y%m%d%H%M')
    metp_end_str = metp_end_dt.strftime('%Y%m%d%H%M')

    return metp_start_str, metp_end_str


def write_crontab(output_xml, crontab_file=None, cronint=5):
    """
    Write a crontab entry that executes ``rocotorun`` every *cronint* minutes
    against the auxiliary workflow XML and database files.

    The database path is derived from *output_xml* by replacing the ``.xml``
    suffix with ``.db``.  If *rocotorun* cannot be found the function logs a
    warning and returns ``None`` without writing any file.

    Parameters
    ----------
    output_xml : str
        Full path to the rendered ``aux.xml`` file.
    crontab_file : str, optional
        Full path where the crontab fragment should be written.  Defaults to
        *output_xml* with the ``.xml`` extension replaced by ``.crontab``.
    cronint : int, optional
        Crontab interval in minutes (default: 5).

    Returns
    -------
    str or None
        Path to the written crontab file, or ``None`` if rocotorun was not
        found.
    """
    rocotorun = which('rocotorun')
    if rocotorun is None:
        logger.warning('rocotorun not found on PATH; crontab will not be created')
        return None

    rocotoruncmd = rocotorun.command
    output_db = os.path.splitext(output_xml)[0] + '.db'
    rocotorunstr = f'{rocotoruncmd} -d {output_db} -w {output_xml}'
    cronintstr = f'*/{cronint} * * * *'

    xml_basename = os.path.basename(os.path.splitext(output_xml)[0])
    replyto = os.environ.get('REPLYTO', '')

    crontab_strings = [
        '',
        f'#################### {xml_basename} ####################',
        'SHELL="/bin/bash"',
        f'MAILTO="{replyto}"',
        f'{cronintstr} {rocotorunstr}',
        '#################################################################',
        '',
    ]

    if crontab_file is None:
        crontab_file = os.path.splitext(output_xml)[0] + '.crontab'

    with open(crontab_file, 'w') as fh:
        fh.write('\n'.join(crontab_strings))

    logger.info(f'Crontab fragment written to: {crontab_file}')
    logger.info(f'To activate, run: crontab {crontab_file}')
    return crontab_file


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

        Most configuration variables are derived automatically from the target
        experiment's config.base when --expdir is provided.  Any variable not
        present in config.base (e.g. ECF_OUT_gfs) must be set explicitly in a
        copy of dev/parm/aux/aux.yaml.j2 passed via --config.

        The dev/ush/gw_setup.sh script must be sourced before running this
        script to ensure the Python environment with wxflow is properly
        configured.
        """

    parser = ArgumentParser(description=description,
                            formatter_class=ArgumentDefaultsHelpFormatter)

    parser.add_argument('--expdir',
                        help='Full path to the experiment directory whose '
                             'config.base supplies the template variables '
                             '(SDATE, EDATE, EXPDIR, ROTDIR, STMP, PSLOT, …). '
                             'When omitted all variables must be provided '
                             'explicitly in the --config file.',
                        type=str, default=None)

    parser.add_argument('--config',
                        help='Full path to the Jinja2-templated aux configuration '
                             'YAML file (aux.yaml.j2).  Defaults to '
                             '<HOMEglobal>/dev/parm/aux/aux.yaml.j2',
                        type=str, default=None)

    parser.add_argument('--crontab',
                        help='Write a crontab fragment for rocotorun alongside '
                             'the rendered XML file.',
                        action='store_true', default=False)

    return parser.parse_args()


@logit(logger)
def main():

    user_inputs = input_args()

    HOMEglobal = _get_HOMEglobal()

    # Build the Jinja2 template context, starting with an empty dict
    template_context = AttrDict()

    # If an experiment directory was provided, source config.base and merge its
    # variables into the template context so the aux.yaml.j2 template can
    # reference them (SDATE, EDATE, EXPDIR, ROTDIR, STMP, PSLOT, …)
    if user_inputs.expdir is not None:
        base_vars = read_config_base(user_inputs.expdir)
        template_context.update(base_vars)
        logger.info(f'Loaded {len(base_vars)} variables from config.base in {user_inputs.expdir}')

    # Always make HOMEglobal available (can be overridden by config.base)
    template_context.setdefault('HOMEglobal', HOMEglobal)

    # Resolve the path to the aux configuration template
    if user_inputs.config is None:
        config_path = os.path.join(HOMEglobal, 'dev', 'parm', 'aux', 'aux.yaml.j2')
    else:
        config_path = user_inputs.config

    logger.info(f'Reading aux configuration template: {config_path}')
    context = parse_j2yaml(config_path, template_context)

    missing_keys = [key for key in _REQUIRED_CONFIG_KEYS
                    if not context.get(key)]
    if missing_keys:
        # Separate keys that can come from config.base vs those that must be
        # set explicitly (ECF_OUT_gfs has no equivalent in config.base)
        explicit_only = {'ECF_OUT_gfs'}
        from_expdir = [k for k in missing_keys if k not in explicit_only]
        must_set = [k for k in missing_keys if k in explicit_only]
        hints = []
        if from_expdir:
            hints.append(f"Provide --expdir whose config.base exports: "
                         f"{', '.join(from_expdir)}")
        if must_set:
            hints.append(f"Set explicitly in the --config file: "
                         f"{', '.join(must_set)}")
        raise KeyError(
            f"Required key(s) missing or empty in rendered config "
            f"'{config_path}': {', '.join(missing_keys)}\n"
            + '\n'.join(hints))

    # Derive METP cycle dates (18z-only verification tool)
    metp_start_date, metp_end_date = calc_start_end_metp_dates(
        context['start_date'], context['end_date'])
    context['start_date_metp'] = metp_start_date
    context['end_date_metp'] = metp_end_date
    logger.info(f"Calculated METP start date: {metp_start_date}, "
                f"METP end date: {metp_end_date}")

    # HOMEglobal in the rendered context takes precedence over the repo root
    if 'HOMEglobal' not in context:
        context['HOMEglobal'] = HOMEglobal
    logger.info(f"Using HOMEglobal: {context['HOMEglobal']}")

    template_path = os.path.join(context['HOMEglobal'], 'dev', 'workflow', 'aux', 'aux.xml.j2')
    output_path = context.get('output') or os.path.join(context['EXP_aux'], 'aux.xml')

    # Create the output directory if it doesn't exist
    os.makedirs(os.path.dirname(output_path), exist_ok=True)

    # Render the aux.xml.j2 template and save the result
    logger.info(f'Rendering aux.xml template: {template_path}')
    Jinja(template_path, context).save(output_path)
    logger.info(f'Rendered aux.xml written to: {output_path}')

    # Optionally write a crontab fragment for rocotorun
    if user_inputs.crontab:
        write_crontab(output_path)


if __name__ == '__main__':

    # Setup the logger
    logger = Logger(logfile_path=os.environ.get("LOGFILE_PATH"),
                    level=os.environ.get("LOGGING_LEVEL", "INFO"),
                    colored_log=os.environ.get("COLORED_LOG", True))

    main()
