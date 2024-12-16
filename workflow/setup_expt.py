#!/usr/bin/env python3

"""
Entry point for setting up an experiment in the global-workflow
"""

import os
import glob
import shutil
from argparse import ArgumentParser, ArgumentDefaultsHelpFormatter, SUPPRESS, ArgumentTypeError

from hosts import Host

from wxflow import parse_j2yaml
from wxflow import AttrDict
from wxflow import to_datetime, to_timedelta, datetime_to_YMDH


_here = os.path.dirname(__file__)
_top = os.path.abspath(os.path.join(os.path.abspath(_here), '..'))


def makedirs_if_missing(dirname):
    """
    Creates a directory if not already present
    """
    if not os.path.exists(dirname):
        os.makedirs(dirname)


def fill_expdir(inputs):
    """
    Method to copy config files from workflow to experiment directory
    INPUTS:
        inputs: user inputs to `setup_expt.py`
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

    def _update_defaults(dict_in: dict) -> dict:
        # Given an input dict_in of the form
        # {defaults: {config_name: {var1: value1, ...}, }, config_name: {var1: value1, ...}}
        # Replace values in ['defaults']['config_name']['var1'] with ['config_name']['var1']
        # and return the ['defaults'] subdictionary as its own new dictionary.
        defaults = dict_in.pop('defaults', AttrDict())
        if 'defaults' in defaults:
            _update_defaults(defaults)
        defaults.update(dict_in)
        return defaults

    # Convert the inputs to an AttrDict
    data = AttrDict(host.info, **inputs.__dict__)

    # Read in the YAML file to fill out templates
    data.HOMEgfs = _top
    yaml_path = inputs.yaml
    if not os.path.exists(yaml_path):
        raise FileNotFoundError(f'YAML file does not exist, check path: {yaml_path}')
    yaml_dict = parse_j2yaml(yaml_path, data)

    # yaml_dict is in the form {defaults: {key1: val1, ...}, base: {key1: val1, ...}, ...}
    # _update_defaults replaces any keys/values in defaults with matching keys in base
    yaml_dict = _update_defaults(yaml_dict)

    # Override the YAML defaults with the host-specific capabilities
    # First update config.base
    edit_baseconfig(host, inputs, yaml_dict)

    # Update stage config
    stage_dict = {
        "@ICSDIR@": inputs.icsdir
    }
    host_dict = get_template_dict(host.info)
    stage_dict = dict(stage_dict, **host_dict)
    stage_input = f'{inputs.configdir}/config.stage_ic'
    stage_output = f'{inputs.expdir}/{inputs.pslot}/config.stage_ic'
    edit_config(stage_input, stage_output, host_dict, stage_dict)

    # Loop over other configs and update them with defaults
    for cfg in yaml_dict.keys():
        if cfg == 'base':
            continue
        cfg_file = f'{inputs.expdir}/{inputs.pslot}/config.{cfg}'
        cfg_dict = get_template_dict(yaml_dict[cfg])
        edit_config(cfg_file, cfg_file, host_dict, cfg_dict)

    return


def edit_baseconfig(host, inputs, yaml_dict):
    """
    Parses and populates the templated `HOMEgfs/parm/config/<gfs|gefs>/config.base`
    to `EXPDIR/pslot/config.base`
    """

    # Create base_dict which holds templated variables to be written to config.base
    base_dict = {
        "@HOMEgfs@": _top,
        "@MACHINE@": host.machine.upper()}

    if inputs.start in ["warm"]:
        is_warm_start = ".true."
    elif inputs.start in ["cold"]:
        is_warm_start = ".false."
    else:
        raise ValueError(f"Invalid start type: {inputs.start}")

    # Construct a dictionary from user inputs
    extend_dict = {
        "@PSLOT@": inputs.pslot,
        "@SDATE@": datetime_to_YMDH(inputs.idate),
        "@EDATE@": datetime_to_YMDH(inputs.edate),
        "@CASECTL@": f'C{inputs.resdetatmos}',
        "@OCNRES@": f"{int(100.*inputs.resdetocean):03d}",
        "@EXPDIR@": inputs.expdir,
        "@COMROOT@": inputs.comroot,
        "@EXP_WARM_START@": is_warm_start,
        "@MODE@": inputs.mode,
        "@INTERVAL_GFS@": inputs.interval,
        "@SDATE_GFS@": datetime_to_YMDH(inputs.sdate_gfs),
        "@APP@": inputs.app,
        "@NMEM_ENS@": getattr(inputs, 'nens', 0)
    }

    if getattr(inputs, 'nens', 0) > 0:
        extend_dict['@CASEENS@'] = f'C{inputs.resensatmos}'

    if inputs.mode in ['cycled']:
        extend_dict["@DOHYBVAR@"] = "YES" if inputs.nens > 0 else "NO"

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
    Given a templated input_config filename, parse it based on config_dict and
    host_info and write it out to the output_config filename.
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
    # Reads a templated input dictionary and updates the output

    output_dict = dict()

    for key, value in input_dict.items():
        # In some cases, the same config may be templated twice
        # Prevent adding additional "@"s
        if "@" in key:
            output_dict[f'{key}'] = value
        else:
            output_dict[f'@{key}@'] = value

    return output_dict


def input_args(*argv):
    """
    Method to collect user arguments for `setup_expt.py`
    """

    ufs_apps = ['ATM', 'ATMA', 'ATMW', 'S2S', 'S2SA', 'S2SW', 'S2SWA']

    def _validate_interval(interval_str):
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
        parser.add_argument('--app', help='UFS application', type=str,
                            choices=ufs_apps, required=False, default='ATM')
        parser.add_argument('--sdate_gfs', help='date to start GFS', type=lambda dd: to_datetime(dd), required=False, default=None)
        return parser

    def _gfs_or_gefs_ensemble_args(parser):
        parser.add_argument('--resensatmos', help='atmosphere resolution of the ensemble model forecast',
                            type=int, required=False, default=192)
        parser.add_argument('--nens', help='number of ensemble members',
                            type=int, required=False, default=20)
        return parser

    def _gfs_or_gefs_forecast_args(parser):
        parser.add_argument('--app', help='UFS application', type=str,
                            choices=ufs_apps, required=False, default='ATM')
        return parser

    def _gefs_args(parser):
        parser.add_argument('--start', help='restart mode: warm or cold', type=str,
                            choices=['warm', 'cold'], required=False, default='cold')
        parser.add_argument('--configdir', help=SUPPRESS, type=str, required=False,
                            default=os.path.join(_top, 'parm/config/gefs'))
        parser.add_argument('--yaml', help='Defaults to substitute from', type=str, required=False,
                            default=os.path.join(_top, 'parm/config/gefs/yaml/defaults.yaml'))
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

    gfsmodeparser = gfs.add_subparsers(dest='mode')
    gfscycled = gfsmodeparser.add_parser('cycled', help='arguments for cycled mode')
    gfsforecasts = gfsmodeparser.add_parser('forecast-only', help='arguments for forecast-only mode')

    gefsmodeparser = gefs.add_subparsers(dest='mode')
    gefsforecasts = gefsmodeparser.add_parser('forecast-only', help='arguments for forecast-only mode')

    # Common arguments across all modes
    for subp in [gfscycled, gfsforecasts, gefsforecasts]:
        subp = _common_args(subp)

    # GFS-only arguments
    for subp in [gfscycled, gfsforecasts]:
        subp = _gfs_args(subp)

    # ensemble-only arguments
    for subp in [gfscycled, gefsforecasts]:
        subp = _gfs_or_gefs_ensemble_args(subp)

    # GFS/GEFS forecast-only additional arguments
    for subp in [gfsforecasts, gefsforecasts]:
        subp = _gfs_or_gefs_forecast_args(subp)

    # cycled mode additional arguments
    for subp in [gfscycled]:
        subp = _gfs_cycled_args(subp)

    # GEFS forecast-only arguments
    for subp in [gefsforecasts]:
        subp = _gefs_args(subp)

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


def query_and_clean(dirname, force_clean=False):
    """
    Method to query if a directory exists and gather user input for further action
    """

    create_dir = True
    if os.path.exists(dirname):
        print(f'\ndirectory already exists in {dirname}')
        if force_clean:
            overwrite = "YES"
            print(f'removing directory ........ {dirname}\n')
        else:
            overwrite = input('Do you wish to over-write [y/N]: ')
        create_dir = True if overwrite in [
            'y', 'yes', 'Y', 'YES'] else False
        if create_dir:
            shutil.rmtree(dirname)

    return create_dir


def validate_user_request(host, inputs):
    supp_res = host.info['SUPPORTED_RESOLUTIONS']
    supp_waves = host.info.get('SUPPORT_WAVES', 'YES')
    machine = host.machine
    for attr in ['resdetatmos', 'resensatmos']:
        try:
            expt_res = f'C{getattr(inputs, attr)}'
        except AttributeError:
            continue
        if expt_res not in supp_res:
            raise NotImplementedError(f"Supported resolutions on {machine} are:\n{', '.join(supp_res)}")

    if "W" in inputs.app and supp_waves == "NO":
        raise NotImplementedError(f"Waves are not supported on {machine}")


def get_ocean_resolution(resdetatmos):
    """
    Method to determine the ocean resolution based on the atmosphere resolution
    Limited options are going to be available
    """
    atmos_to_ocean_map = {
        1152: 0.25, 768: 0.25, 384: 0.25,
        192: 1.0,
        96: 5.0, 48: 5.0}
    try:
        return atmos_to_ocean_map[resdetatmos]
    except KeyError:
        raise KeyError(f"Ocean resolution for {resdetatmos} is not implemented")


def main(*argv):

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
        fill_expdir(user_inputs)
        update_configs(host, user_inputs)

    print(f"*" * 100)
    print(f'EXPDIR: {expdir}')
    print(f'ROTDIR: {rotdir}')
    print(f"*" * 100)


if __name__ == '__main__':

    main()
