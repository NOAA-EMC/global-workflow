#!/usr/bin/env python3

"""
Entry point for setting up an experiment in the global-workflow.

This script handles the creation of experiment directories and configuration files
for various forecast systems (GFS, GEFS, SFS, GCAFS) in the Unified Forecast System.
It processes command-line arguments, creates the necessary directory structure,
and configures the experiment based on user inputs and host capabilities.
"""

import os
import shutil
from logging import getLogger
from argparse import ArgumentParser, ArgumentDefaultsHelpFormatter, SUPPRESS, ArgumentTypeError

from hosts import Host

from wxflow import parse_j2yaml, AttrDict, to_datetime, to_timedelta, to_YMDH, Jinja, Logger, logit


_here = os.path.dirname(__file__)
_top = os.path.abspath(os.path.join(os.path.abspath(_here), '../..'))

# Setup the logger
logger = getLogger(__name__)


# @logit(logger)
def makedirs_if_missing(dirname):
    """
    Creates a directory if not already present.

    Parameters
    ----------
    dirname : str
        Path to directory to create

    Returns
    -------
    None
    """
    if not os.path.exists(dirname):
        os.makedirs(dirname)


# @logit(logger)
def update_configs(host, inputs):
    """
<<<<<<< HEAD:workflow/setup_expt.py
    Method to copy config files from workflow to experiment directory.

    Parameters
    ----------
    inputs : argparse.Namespace
        User inputs to `setup_expt.py` containing configdir and expdir paths

    Returns
    -------
    None

    Raises
    ------
    IOError
        If no config files are found in the configdir
    """
    configdir = inputs.configdir
    expdir = os.path.join(inputs.expdir, inputs.pslot)

    configs = glob.glob(f'{configdir}/config.*')
    if len(configs) == 0:
        raise IOError(f'no config files found in {configdir}')
    for config in configs:
        shutil.copy(config, expdir)

    return


def update_configs(host, inputs):
    """
    Update configuration files with host-specific and user-provided settings.

    This function processes YAML templates, applies host-specific capabilities,
    and user inputs to create finalized configuration files.

    Parameters
    ----------
    host : Host
        Host object containing machine-specific information
    inputs : argparse.Namespace
        User inputs to `setup_expt.py`

    Returns
    -------
    None

    Raises
    ------
    FileNotFoundError
        If the YAML file specified in inputs does not exist
    """
=======
    Method to copy config files from workflow to experiment directory and render templates
    INPUTS:
        inputs: user inputs to `setup_expt.py`
    """
>>>>>>> upstream/develop:dev/workflow/setup_expt.py

    # @logit(logger)
    def _update_defaults(dict_in: dict) -> dict:
        """
        Process nested dictionaries by replacing defaults with overrides.

        Given an input dict_in of the form:
        {defaults: {config_name: {var1: value1, ...}}, config_name: {var1: value1, ...}}
        Replace values in ['defaults']['config_name']['var1'] with ['config_name']['var1']
        and return the ['defaults'] subdictionary as its own new dictionary.

        Parameters
        ----------
        dict_in : dict
            Input dictionary with defaults and overrides

        Returns
        -------
        dict
            Updated dictionary with defaults replaced by overrides
        """
        defaults = dict_in.pop('defaults', AttrDict())
        if 'defaults' in defaults:
            _update_defaults(defaults)
        defaults.update(dict_in)
        return defaults

    # map inputs_dict keys to keys used in configs
    inputs_dict_remapped = map_inputs_to_configs(inputs)

    # Combine host.info and inputs_dict into a single dict, add some additional keys
    host_plus_inputs_dict = AttrDict(host.info, **inputs_dict_remapped)
    host_plus_inputs_dict.HOMEgfs = _top
    host_plus_inputs_dict.MACHINE = host.machine.upper()

    # Read in the YAML file
    yaml_path = inputs.yaml
    if not os.path.exists(yaml_path):
        raise FileNotFoundError(f'YAML file does not exist, check path: {yaml_path}')
    yaml_dict = parse_j2yaml(yaml_path, host_plus_inputs_dict)

    # yaml_dict is in the form {defaults: {key1: val1, ...}, base: {key1: val1, ...}, ...}
    # _update_defaults replaces any keys/values in defaults with matching keys in base
    yaml_dict = _update_defaults(yaml_dict)

    # Copy the config files to the experiment directory
    files = [ff for ff in os.listdir(inputs.configdir) if os.path.isfile(os.path.join(inputs.configdir, ff))]
    for file in files:
        if file.endswith('.j2'):  # Jinja2 template; render it
            logger.info(f'Jinja2 template found: {file}')
            input_template = f'{inputs.configdir}/{file}'
            cfg_file = file[:-3]  # remove the .j2 extension
            output_config = f'{inputs.expdir}/{inputs.pslot}/{cfg_file}'  # output file in EXPDIR
            cfg_key = '.'.join(cfg_file.split('.')[1:])  # key to look for in yaml_dict
            _data = host_plus_inputs_dict.copy()
            if cfg_key in yaml_dict.keys():
                _data = AttrDict(_data, **yaml_dict[cfg_key])
            Jinja(input_template, _data).save(output_config)
        else:  # copy the file as is
            input_file = f'{inputs.configdir}/{file}'
            output_config = f'{inputs.expdir}/{inputs.pslot}/{file}'
            shutil.copy(input_file, output_config)

    return


<<<<<<< HEAD:workflow/setup_expt.py
def edit_baseconfig(host, inputs, yaml_dict):
    """
    Parse and populate the config.base file with host and user settings.

    Parameters
    ----------
    host : Host
        Host object containing machine-specific information
    inputs : argparse.Namespace
        User inputs to `setup_expt.py`
    yaml_dict : dict
        Dictionary containing YAML configuration values

    Returns
    -------
    None

    Raises
    ------
    ValueError
        If an invalid start type is provided
    """
=======
# @logit(logger)
def map_inputs_to_configs(inputs):
>>>>>>> upstream/develop:dev/workflow/setup_expt.py

    warm_start_map = {'warm': '.true.', 'cold': '.false.'}

    # Construct a dictionary from user inputs
<<<<<<< HEAD:workflow/setup_expt.py
    extend_dict = {
        "@PSLOT@": inputs.pslot,
        "@SDATE@": datetime_to_YMDH(inputs.idate),
        "@EDATE@": datetime_to_YMDH(inputs.edate),
        "@CASECTL@": f'C{inputs.resdetatmos}',
        "@OCNRES@": f"{int(100. * inputs.resdetocean):03d}",
        "@EXPDIR@": inputs.expdir,
        "@COMROOT@": inputs.comroot,
        "@EXP_WARM_START@": is_warm_start,
        "@MODE@": inputs.mode,
        "@INTERVAL_GFS@": inputs.interval,
        "@SDATE_GFS@": datetime_to_YMDH(inputs.sdate_gfs),
        "@APP@": inputs.app,
        "@NMEM_ENS@": getattr(inputs, 'nens', 0)
    }
=======
    try:
        dict_out = AttrDict({
            "PSLOT": inputs.pslot,
            "SDATE": to_YMDH(inputs.idate),
            "EDATE": to_YMDH(inputs.edate),
            "CASE_CTL": f'C{inputs.resdetatmos}',
            "OCNRES": f"{int(100.*inputs.resdetocean):03d}",
            "EXPDIR": inputs.expdir,
            "COMROOT": inputs.comroot,
            "EXP_WARM_START": warm_start_map[inputs.start],
            "MODE": inputs.mode,
            "INTERVAL_GFS": inputs.interval,
            "SDATE_GFS": to_YMDH(inputs.sdate_gfs),
            "APP": inputs.app,
            "NMEM_ENS": getattr(inputs, 'nens', 0),
            "ICSDIR": inputs.icsdir,
            "ACCOUNT": inputs.account,
        })
    except Exception as ee:
        raise Exception("Error in constructing dictionary from user inputs, check inputs: ") from ee
>>>>>>> upstream/develop:dev/workflow/setup_expt.py

    if dict_out.NMEM_ENS > 0:
        dict_out.CASE_ENS = f'C{inputs.resensatmos}'

    if inputs.mode in ['cycled']:
<<<<<<< HEAD:workflow/setup_expt.py
        extend_dict["@DOHYBVAR@"] = "YES" if inputs.nens > 0 else "NO"
        # Set EUPD_CYC based on ensemble members
        if inputs.nens > 0:
            extend_dict["@EUPD_CYC@"] = "gdas"  # or "both" or "gfs" as needed
        else:
            extend_dict["@EUPD_CYC@"] = ""  # Empty string when no ensemble
    else:
        # For forecast-only mode
        extend_dict["@EUPD_CYC@"] = ""  # No EnKF in forecast-only mode

    # Further extend/redefine base_dict with extend_dict
    base_dict = dict(base_dict, **extend_dict)

    # Add/override 'base'-specific declarations in base_dict
    if 'base' in yaml_dict:
        base_dict = dict(base_dict, **get_template_dict(yaml_dict['base']))

    base_input = f'{inputs.configdir}/config.base'
    base_output = f'{inputs.expdir}/{inputs.pslot}/config.base'
    edit_config(base_input, base_output, host.info, base_dict)

    return


def edit_config(input_config, output_config, host_info, config_dict):
    """
    Edit a configuration file by substituting template values.

    Given a templated input_config filename, parse it based on config_dict and
    host_info and write it out to the output_config filename.

    Parameters
    ----------
    input_config : str
        Path to the input template configuration file
    output_config : str
        Path to the output configuration file
    host_info : dict
        Dictionary containing host-specific information
    config_dict : dict
        Dictionary of template values to substitute

    Returns
    -------
    None
    """

    # Override defaults with machine-specific capabilties
    # e.g. some machines are not able to run metp jobs
    host_dict = get_template_dict(host_info)
    config_dict = dict(config_dict, **host_dict)

    # Read input config
    with open(input_config, 'rt') as fi:
        config_str = fi.read()

    # Substitute from config_dict
    for key, val in config_dict.items():
        config_str = config_str.replace(key, str(val))

    # Ensure no output_config file exists
    if os.path.exists(output_config):
        os.unlink(output_config)

    # Write output config
    with open(output_config, 'wt') as fo:
        fo.write(config_str)

    print(f'EDITED:  {output_config} as per user input.')

    return


def get_template_dict(input_dict):
    """
    Convert a dictionary into a template dictionary by adding @ symbols.

    Parameters
    ----------
    input_dict : dict
        Input dictionary with keys to be templated

    Returns
    -------
    dict
        Dictionary with keys wrapped in @ symbols for template substitution
    """
    output_dict = dict()

    for key, value in input_dict.items():
        # In some cases, the same config may be templated twice
        # Prevent adding additional "@"s
        if "@" in key:
            output_dict[f'{key}'] = value
        else:
            output_dict[f'@{key}@'] = value

    return output_dict
=======
        dict_out.DOHYBVAR = "YES" if dict_out.NMEM_ENS > 0 else "NO"

    return dict_out
>>>>>>> upstream/develop:dev/workflow/setup_expt.py


# @logit(logger)
def input_args(*argv):
    """
    Process command-line arguments for experiment setup.

    Parameters
    ----------
    *argv : list, optional
        Command line arguments

    Returns
    -------
    argparse.Namespace
        Parsed command-line arguments

    Raises
    ------
    ArgumentTypeError
        If interval is not a multiple of 6, or if dates are invalid
    ValueError
        If start type is invalid
    """

    ufs_apps = ['ATM', 'ATMA', 'ATMW', 'S2S', 'S2SA', 'S2SW', 'S2SWA']

    def _validate_interval(interval_str):
        """
        Validate that interval is a non-negative integer multiple of 6.

        Parameters
        ----------
        interval_str : str
            String representation of interval value

        Returns
        -------
        int
            Validated interval value

        Raises
        ------
        ArgumentTypeError
            If interval is not a valid non-negative integer multiple of 6
        """
        err_msg = f'must be a non-negative integer multiple of 6 ({interval_str} given)'
        try:
            interval = int(interval_str)
        except ValueError:
            raise ArgumentTypeError(err_msg)

        # This assumes the gdas frequency (assim_freq) is 6h
        # If this changes, the modulus needs to as well
        if interval < 0 or interval % 6 != 0:
            raise ArgumentTypeError(err_msg)
        return interval

    def _common_args(parser):
        """
        Add common arguments to all subparsers.

        Parameters
        ----------
        parser : argparse.ArgumentParser
            Parser to add arguments to

        Returns
        -------
        argparse.ArgumentParser
            Parser with added arguments
        """
        parser.add_argument('--pslot', help='parallel experiment name',
                            type=str, required=False, default='test')
        parser.add_argument('--resdetatmos', help='atmosphere resolution of the deterministic model forecast',
                            type=int, required=False, default=384)
        parser.add_argument('--resdetocean', help='ocean resolution of the deterministic model forecast',
                            type=float, required=False, default=0.0)  # 0.0 (or lower) means determine from resdetatmos (limited combinations will be available)
        parser.add_argument('--comroot', help='full path to COMROOT',
                            type=str, required=False, default=os.getenv('HOME'))
        parser.add_argument('--expdir', help='full path to EXPDIR',
                            type=str, required=False, default=os.getenv('HOME'))
        parser.add_argument('--idate', help='starting date of experiment, initial conditions must exist!',
                            required=True, type=lambda dd: to_datetime(dd))
        parser.add_argument('--edate', help='end date experiment', required=False, type=lambda dd: to_datetime(dd))
        parser.add_argument('--account', help='HPC account to use; default is host-dependent', required=False, default=os.getenv('HPC_ACCOUNT'))
        parser.add_argument('--interval', help='frequency of forecast (in hours); must be a multiple of 6', type=_validate_interval, required=False, default=6)
        parser.add_argument('--icsdir', help='full path to user initial condition directory', type=str, required=False, default='')
        parser.add_argument('--overwrite', help='overwrite previously created experiment (if it exists)',
                            action='store_true', required=False)
        return parser

    def _gfs_args(parser):
        """
        Add GFS-specific arguments to parser.

        Parameters
        ----------
        parser : argparse.ArgumentParser
            Parser to add arguments to

        Returns
        -------
        argparse.ArgumentParser
            Parser with added arguments
        """
        parser.add_argument('--start', help='restart mode: warm or cold', type=str,
                            choices=['warm', 'cold'], required=False, default='cold')
        parser.add_argument('--run', help='RUN to start the experiment',
                            type=str, required=False, default='gdas')
        # --configdir is hidden from help
        parser.add_argument('--configdir', help=SUPPRESS, type=str, required=False, default=os.path.join(_top, 'parm/config/gfs'))
        parser.add_argument('--yaml', help='Defaults to substitute from', type=str,
                            required=False, default=os.path.join(_top, 'parm/config/gfs/yaml/defaults.yaml'))
        return parser

    def _gfs_cycled_args(parser):
        """
        Add GFS cycled mode-specific arguments to parser.

        Parameters
        ----------
        parser : argparse.ArgumentParser
            Parser to add arguments to

        Returns
        -------
        argparse.ArgumentParser
            Parser with added arguments
        """
        parser.add_argument('--app', help='UFS application', type=str,
                            choices=ufs_apps, required=False, default='ATM')
        parser.add_argument('--sdate_gfs', help='date to start GFS', type=lambda dd: to_datetime(dd), required=False, default=None)
        return parser

    def _any_ensemble_args(parser):
        """
        Add ensemble-specific arguments to parser.

        Parameters
        ----------
        parser : argparse.ArgumentParser
            Parser to add arguments to

        Returns
        -------
        argparse.ArgumentParser
            Parser with added arguments
        """
        parser.add_argument('--resensatmos', help='atmosphere resolution of the ensemble model forecast',
                            type=int, required=False, default=192)
        parser.add_argument('--nens', help='number of ensemble members',
                            type=int, required=False, default=20)
        return parser

    def _any_forecast_args(parser):
        """
        Add forecast-specific arguments to parser.

        Parameters
        ----------
        parser : argparse.ArgumentParser
            Parser to add arguments to

        Returns
        -------
        argparse.ArgumentParser
            Parser with added arguments
        """
        parser.add_argument('--app', help='UFS application', type=str,
                            choices=ufs_apps, required=False, default='ATM')
        return parser

    def _gefs_args(parser):
        """
        Add GEFS-specific arguments to parser.

        Parameters
        ----------
        parser : argparse.ArgumentParser
            Parser to add arguments to

        Returns
        -------
        argparse.ArgumentParser
            Parser with added arguments
        """
        parser.add_argument('--start', help='restart mode: warm or cold', type=str,
                            choices=['warm', 'cold'], required=False, default='cold')
        parser.add_argument('--configdir', help=SUPPRESS, type=str, required=False,
                            default=os.path.join(_top, 'parm/config/gefs'))
        parser.add_argument('--yaml', help='Defaults to substitute from', type=str, required=False,
                            default=os.path.join(_top, 'parm/config/gefs/yaml/defaults.yaml'))
        return parser

    def _sfs_args(parser):
        """
        Add SFS-specific arguments to parser.

        Parameters
        ----------
        parser : argparse.ArgumentParser
            Parser to add arguments to

        Returns
        -------
        argparse.ArgumentParser
            Parser with added arguments
        """
        parser.add_argument('--start', help='restart mode: warm or cold', type=str,
                            choices=['warm', 'cold'], required=False, default='cold')
        parser.add_argument('--configdir', help=SUPPRESS, type=str, required=False,
                            default=os.path.join(_top, 'parm/config/sfs'))
        parser.add_argument('--yaml', help='Defaults to substitute from', type=str, required=False,
                            default=os.path.join(_top, 'parm/config/sfs/yaml/defaults.yaml'))
        return parser

    # GCAFS forecast-only arguments
    def _gcafs_args(parser):
        """
        Add GCAFS-specific arguments to parser.

        Parameters
        ----------
        parser : argparse.ArgumentParser
            Parser to add arguments to

        Returns
        -------
        argparse.ArgumentParser
            Parser with added arguments
        """
        parser.add_argument('--start', help='restart mode: warm or cold', type=str,
                            choices=['warm', 'cold'], required=False, default='cold')
        parser.add_argument('--configdir', help=SUPPRESS, type=str, required=False,
                            default=os.path.join(_top, 'parm/config/gcafs'))
        parser.add_argument('--yaml', help='Defaults to substitute from', type=str, required=False,
                            default=os.path.join(_top, 'parm/config/gcafs/yaml/defaults.yaml'))
        return parser

    description = """
        Setup files and directories to start a GFS parallel.\n
        Create EXPDIR, copy config files.\n
        Create ROTDIR experiment directory structure,
        """

    parser = ArgumentParser(description=description,
                            formatter_class=ArgumentDefaultsHelpFormatter)

    # Set up sub-parsers for various modes of experimentation
    sysparser = parser.add_subparsers(dest='system')
    gfs = sysparser.add_parser('gfs', help='arguments for GFS')
    gefs = sysparser.add_parser('gefs', help='arguments for GEFS')
    sfs = sysparser.add_parser('sfs', help='arguments for SFS')
    gcafs = sysparser.add_parser('gcafs', help='arguments for GCAFS')

    gfsmodeparser = gfs.add_subparsers(dest='mode')
    gfscycled = gfsmodeparser.add_parser('cycled', help='arguments for cycled mode')
    gfsforecasts = gfsmodeparser.add_parser('forecast-only', help='arguments for forecast-only mode')

    gefsmodeparser = gefs.add_subparsers(dest='mode')
    gefsforecasts = gefsmodeparser.add_parser('forecast-only', help='arguments for forecast-only mode')

    sfsmodeparser = sfs.add_subparsers(dest='mode')
    sfsforecasts = sfsmodeparser.add_parser('forecast-only', help='arguments for forecast-only mode')

    gcafsmodeparser = gcafs.add_subparsers(dest='mode')
    gcafsforecasts = gcafsmodeparser.add_parser('forecast-only', help='arguments for forecast-only mode')
    gcafscycled = gcafsmodeparser.add_parser('cycled', help='arguments for cycled mode')

    # Common arguments across all modes
    for subp in [gfscycled, gfsforecasts, gefsforecasts, sfsforecasts, gcafsforecasts, gcafscycled]:
        subp = _common_args(subp)

    # GFS-only arguments
    for subp in [gfscycled, gfsforecasts]:
        subp = _gfs_args(subp)

    # ensemble-only arguments
    for subp in [gfscycled, gefsforecasts, sfsforecasts, gcafscycled]:
        subp = _any_ensemble_args(subp)

    # GFS/GEFS forecast-only additional arguments
    for subp in [gfsforecasts, gefsforecasts, sfsforecasts, gcafsforecasts]:
        subp = _any_forecast_args(subp)

    # cycled mode additional arguments
    for subp in [gfscycled, gcafscycled]:
        subp = _gfs_cycled_args(subp)

    # GEFS forecast-only arguments
    for subp in [gefsforecasts]:
        subp = _gefs_args(subp)

    # SFS arguments
    for subp in [sfsforecasts]:
        subp = _sfs_args(subp)

    # GCAFS arguments
    for subp in [gcafsforecasts, gcafscycled]:
        subp = _gcafs_args(subp)

    inputs = parser.parse_args(list(*argv) if len(argv) else None)

    # Validate dates
    if inputs.edate is None:
        inputs.edate = inputs.idate

    if inputs.edate < inputs.idate:
        raise ArgumentTypeError(f'edate ({inputs.edate}) cannot be before idate ({inputs.idate})')

    # For forecast-only, GFS starts in the first cycle
    if not hasattr(inputs, 'sdate_gfs'):
        inputs.sdate_gfs = inputs.idate

    # For cycled, GFS starts after the half-cycle
    if inputs.sdate_gfs is None:
        inputs.sdate_gfs = inputs.idate + to_timedelta("6H")

    if inputs.interval > 0:
        if inputs.sdate_gfs < inputs.idate or inputs.sdate_gfs > inputs.edate:
            raise ArgumentTypeError(f'sdate_gfs ({inputs.sdate_gfs}) must be between idate ({inputs.idate}) and edate ({inputs.edate})')

    return inputs


# @logit(logger)
def query_and_clean(dirname, force_clean=False):
    """
    Query if a directory exists and gather user input for further action.

    Parameters
    ----------
    dirname : str
        Directory to check and potentially clean
    force_clean : bool, optional
        Whether to force cleaning without prompting, by default False

    Returns
    -------
    bool
        Whether the directory should be created
    """

    create_dir = True
    if os.path.exists(dirname):
        logger.warning(f'directory already exists in:')
        logger.warning(f'  {dirname}')
        if force_clean:
            overwrite = "YES"
            logger.warning(f'removing directory ...')
            logger.warning(f'  {dirname}')
        else:
            overwrite = input('Do you wish to over-write [y/N]: ')
        create_dir = True if overwrite in [
            'y', 'yes', 'Y', 'YES'] else False
        if create_dir:
            shutil.rmtree(dirname)

    return create_dir


# @logit(logger)
def validate_user_request(host, inputs):
    """
    Validate that the requested resolution is supported on the host machine.

    Parameters
    ----------
    host : Host
        Host object containing machine-specific information
    inputs : argparse.Namespace
        User inputs to `setup_expt.py`

    Returns
    -------
    None

    Raises
    ------
    NotImplementedError
        If the requested resolution is not supported on the host machine
    """
    supp_res = host.info['SUPPORTED_RESOLUTIONS']
    machine = host.machine
    for attr in ['resdetatmos', 'resensatmos']:
        try:
            expt_res = f'C{getattr(inputs, attr)}'
        except AttributeError:
            continue
        if expt_res not in supp_res:
            raise NotImplementedError(f"Supported resolutions on {machine} are:\n{', '.join(supp_res)}")


# @logit(logger)
def get_ocean_resolution(resdetatmos):
    """
    Determine the ocean resolution based on the atmosphere resolution.

    Parameters
    ----------
    resdetatmos : int
        Atmosphere resolution

    Returns
    -------
    float
        Corresponding ocean resolution

    Raises
    ------
    KeyError
        If ocean resolution for the given atmosphere resolution is not defined
    """
    atmos_to_ocean_map = {
        1152: 0.25, 768: 0.25, 384: 0.25,
        192: 1.0,
        96: 5.0, 48: 5.0}
    try:
        return atmos_to_ocean_map[resdetatmos]
    except KeyError:
        raise KeyError(f"Ocean resolution for {resdetatmos} is not implemented")


@logit(logger, name='setup_expt.main')
def main(*argv):
    """
    Main function to set up experiment directories and configuration.

    Parameters
    ----------
    *argv : list, optional
        Command line arguments

    Returns
    -------
    None
    """

    user_inputs = input_args(*argv)
    host = Host()

    validate_user_request(host, user_inputs)

    # Update the default host account if the user supplied one
    if user_inputs.account is not None:
        host.info.ACCOUNT = user_inputs.account

    # Determine ocean resolution if not provided
    if user_inputs.resdetocean <= 0:
        user_inputs.resdetocean = get_ocean_resolution(user_inputs.resdetatmos)

    rotdir = os.path.join(user_inputs.comroot, user_inputs.pslot)
    expdir = os.path.join(user_inputs.expdir, user_inputs.pslot)

    create_rotdir = query_and_clean(rotdir, force_clean=user_inputs.overwrite)
    create_expdir = query_and_clean(expdir, force_clean=user_inputs.overwrite)

    if create_rotdir:
        makedirs_if_missing(rotdir)

    if create_expdir:
        makedirs_if_missing(expdir)
        update_configs(host, user_inputs)

    max_len = max(len(expdir), len(rotdir)) + 8
    logger.info(f"*" * max_len)
    logger.info(f'EXPDIR: {expdir}')
    logger.info(f'ROTDIR: {rotdir}')
    logger.info(f"*" * max_len)


if __name__ == '__main__':

    # Setup the logger
    logger = Logger(logfile_path=os.environ.get("LOGFILE_PATH"),
                    level=os.environ.get("LOGGING_LEVEL", "INFO"),
                    colored_log=os.environ.get("COLORED_LOG", True))

    main()
