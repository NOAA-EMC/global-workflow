#!/usr/bin/env python3

"""
Entry point for setting up an experiment in the global-workflow
"""

import os
import glob
import shutil
from datetime import datetime
from argparse import ArgumentParser, ArgumentDefaultsHelpFormatter

from hosts import Host

from pygw.yaml_file import YAMLFile


_here = os.path.dirname(__file__)
_top = os.path.abspath(os.path.join(os.path.abspath(_here), '..'))

def makedirs_if_missing(dirname):
    """
    Creates a directory if not already present
    """
    if not os.path.exists(dirname):
        os.makedirs(dirname)


def fill_COMROT(host, inputs):
    """
    Method to populate the COMROT for supported modes.
    INPUTS:
        host: host object from class Host
        inputs: user inputs to setup_expt.py
    """

    fill_modes = {
        'cycled': fill_COMROT_cycled,
        'forecast-only': fill_COMROT_forecasts
    }

    try:
        fill_modes[inputs.mode](host, inputs)
    except KeyError:
        raise NotImplementedError(f'{inputs.mode} is not a supported mode.\n' +
                                  'Currently supported modes are:\n' +
                                  f'{" | ".join(fill_modes.keys())}')

    return


def fill_COMROT_cycled(host, inputs):
    """
    Implementation of 'fill_COMROT' for cycled mode
    """

    idatestr = inputs.idate.strftime('%Y%m%d%H')
    comrot = os.path.join(inputs.comrot, inputs.pslot)

    if inputs.icsdir is not None:
        # Link ensemble member initial conditions
        enkfdir = f'enkf{inputs.cdump}.{idatestr[:8]}/{idatestr[8:]}'
        makedirs_if_missing(os.path.join(comrot, enkfdir))
        for ii in range(1, inputs.nens + 1):
            makedirs_if_missing(os.path.join(comrot, enkfdir, f'mem{ii:03d}'))
            os.symlink(os.path.join(inputs.icsdir, idatestr, f'C{inputs.resens}', f'mem{ii:03d}', 'RESTART'),
                       os.path.join(comrot, enkfdir, f'mem{ii:03d}', 'RESTART'))

        # Link deterministic initial conditions
        detdir = f'{inputs.cdump}.{idatestr[:8]}/{idatestr[8:]}'
        makedirs_if_missing(os.path.join(comrot, detdir))
        os.symlink(os.path.join(inputs.icsdir, idatestr, f'C{inputs.resdet}', 'control', 'RESTART'),
                   os.path.join(comrot, detdir, 'RESTART'))

        # Link bias correction and radiance diagnostics files
        for fname in ['abias', 'abias_pc', 'abias_air', 'radstat']:
            os.symlink(os.path.join(inputs.icsdir, idatestr, f'{inputs.cdump}.t{idatestr[8:]}z.{fname}'),
                       os.path.join(comrot, detdir, f'{inputs.cdump}.t{idatestr[8:]}z.{fname}'))

    return


def fill_COMROT_forecasts(host, inputs):
    """
    Implementation of 'fill_COMROT' for forecast-only mode
    """
    return


def fill_EXPDIR(inputs):
    """
    Method to copy config files from workflow to experiment directory
    INPUTS:
        inputs: user inputs to `setup_expt.py`
    """
    configdir = inputs.configdir
    expdir = os.path.join(inputs.expdir, inputs.pslot)

    configs = glob.glob(f'{configdir}/config.*')
    exclude_configs = ['base', 'base.emc.dyn', 'base.nco.static', 'fv3.nco.static']
    for exclude in exclude_configs:
        try:
            configs.remove(f'{configdir}/config.{exclude}')
        except ValueError:
            pass
    if len(configs) == 0:
        raise IOError(f'no config files found in {configdir}')
    for config in configs:
        shutil.copy(config, expdir)

    return


def update_configs(host, inputs):

    # First update config.base
    edit_baseconfig(host, inputs)

    yaml_path = inputs.yaml
    yaml_dict = YAMLFile(path=yaml_path)

    # loop over other configs and update them
    for cfg in yaml_dict.keys():
        cfg_file = f'{inputs.expdir}/{inputs.pslot}/config.{cfg}'
        cfg_dict = get_template_dict(yaml_dict[cfg])
        edit_config(cfg_file, cfg_file, cfg_dict)

    return


def edit_baseconfig(host, inputs):
    """
    Parses and populates the templated `config.base.emc.dyn` to `config.base`
    """

    tmpl_dict = {
        "@HOMEgfs@": _top,
        "@MACHINE@": host.machine.upper()}

    # Replace host related items
    extend_dict = get_template_dict(host.info)
    tmpl_dict = dict(tmpl_dict, **extend_dict)

    extend_dict = dict()
    extend_dict = {
        "@PSLOT@": inputs.pslot,
        "@SDATE@": inputs.idate.strftime('%Y%m%d%H'),
        "@EDATE@": inputs.edate.strftime('%Y%m%d%H'),
        "@CASECTL@": f'C{inputs.resdet}',
        "@EXPDIR@": inputs.expdir,
        "@ROTDIR@": inputs.comrot,
        "@ICSDIR@": inputs.icsdir,
        "@EXP_WARM_START@": inputs.warm_start,
        "@MODE@": inputs.mode,
        "@gfs_cyc@": inputs.gfs_cyc,
        "@APP@": inputs.app
    }
    tmpl_dict = dict(tmpl_dict, **extend_dict)

    extend_dict = dict()
    if inputs.mode in ['cycled']:
        extend_dict = {
            "@CASEENS@": f'C{inputs.resens}',
            "@NMEM_ENKF@": inputs.nens,
        }
        tmpl_dict = dict(tmpl_dict, **extend_dict)

    if inputs.mode in ['cycled']:
        extend_dict = {
            "@CCPP_SUITE@": 'FV3_GFS_v16',
            "@IMP_PHYSICS@": 11
        }
    elif inputs.mode in ['forecast-only']:
        extend_dict = {
            "@CCPP_SUITE@": 'FV3_GFS_v17_p8',
            "@IMP_PHYSICS@": 8
        }
    tmpl_dict = dict(tmpl_dict, **extend_dict)


    base_input = f'{inputs.configdir}/config.base.emc.dyn'
    base_output = f'{inputs.expdir}/{inputs.pslot}/config.base'
    edit_config(base_input, base_output, tmpl_dict)

    return


def edit_config(input_config, output_config, config_dict):

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
    output_dict = dict()
    for key, value in input_dict.items():
        output_dict[f'@{key}@'] = value

    return output_dict

def input_args():
    """
    Method to collect user arguments for `setup_expt.py`
    """

    description = """
        Setup files and directories to start a GFS parallel.\n
        Create EXPDIR, copy config files.\n
        Create COMROT experiment directory structure,
        link initial condition files from $ICSDIR to $COMROT
        """

    parser = ArgumentParser(description=description,
                            formatter_class=ArgumentDefaultsHelpFormatter)

    # Set up sub-parsers for various modes of experimentation
    subparser = parser.add_subparsers(dest='mode')
    cycled = subparser.add_parser(
        'cycled', help='arguments for cycled mode')
    forecasts = subparser.add_parser(
        'forecast-only', help='arguments for forecast-only mode')

    # Common arguments across all modes
    for subp in [cycled, forecasts]:
        subp.add_argument('--pslot', help='parallel experiment name',
                          type=str, required=False, default='test')
        subp.add_argument('--resdet', help='resolution of the deterministic model forecast',
                          type=int, required=False, default=384)
        subp.add_argument('--comrot', help='full path to COMROT',
                          type=str, required=False, default=os.getenv('HOME'))
        subp.add_argument('--expdir', help='full path to EXPDIR',
                          type=str, required=False, default=os.getenv('HOME'))
        subp.add_argument('--idate', help='starting date of experiment, initial conditions must exist!', required=True, type=lambda dd: datetime.strptime(dd, '%Y%m%d%H'))
        subp.add_argument('--edate', help='end date experiment', required=True, type=lambda dd: datetime.strptime(dd, '%Y%m%d%H'))
        subp.add_argument('--icsdir', help='full path to initial condition directory', type=str, required=False, default=None)
        subp.add_argument('--configdir', help='full path to directory containing the config files',
                          type=str, required=False, default=os.path.join(_top, 'parm/config'))
        subp.add_argument('--cdump', help='CDUMP to start the experiment',
                          type=str, required=False, default='gdas')
        subp.add_argument('--gfs_cyc', help='GFS cycles to run', type=int,
                          choices=[0, 1, 2, 4], default=1, required=False)
        subp.add_argument('--start', help='restart mode: warm or cold', type=str,
                          choices=['warm', 'cold'], required=False, default='cold')

        subp.add_argument('--yaml', help='Defaults to substitute from', type=str,
                         required=False, default=os.path.join(_top, 'parm/config/yaml/defaults.yaml'))

    # cycled mode additional arguments
    cycled.add_argument('--resens', help='resolution of the ensemble model forecast',
                        type=int, required=False, default=192)
    cycled.add_argument('--nens', help='number of ensemble members',
                        type=int, required=False, default=20)
    cycled.add_argument('--app', help='UFS application', type=str,
                        choices=['ATM', 'ATMW', 'ATMA'], required=False, default='ATM')

    # forecast only mode additional arguments
    forecasts.add_argument('--app', help='UFS application', type=str, choices=[
        'ATM', 'ATMA', 'ATMW', 'S2S', 'S2SW', 'S2SWA', 'NG-GODAS'], required=False, default='ATM')

    args = parser.parse_args()

    if args.app in ['S2S', 'S2SW'] and args.icsdir is None:
        raise SyntaxError("An IC directory must be specified with --icsdir when running the S2S or S2SW app")

    # Add an entry for warm_start = .true. or .false.
    if args.start == "warm":
        args.warm_start = ".true."
    else:
        args.warm_start = ".false."
    return args


def query_and_clean(dirname):
    """
    Method to query if a directory exists and gather user input for further action
    """

    create_dir = True
    if os.path.exists(dirname):
        print()
        print(f'directory already exists in {dirname}')
        print()
        overwrite = input('Do you wish to over-write [y/N]: ')
        create_dir = True if overwrite in [
            'y', 'yes', 'Y', 'YES'] else False
        if create_dir:
            shutil.rmtree(dirname)

    return create_dir

def validate_user_request(host, inputs):
    expt_res = f'C{inputs.resdet}'
    supp_res = host.info['SUPPORTED_RESOLUTIONS']
    machine = host.machine
    if expt_res not in supp_res:
        raise NotImplementedError(f"Supported resolutions on {machine} are:\n{', '.join(supp_res)}")


if __name__ == '__main__':

    user_inputs = input_args()
    host = Host()

    validate_user_request(host, user_inputs)

    comrot = os.path.join(user_inputs.comrot, user_inputs.pslot)
    expdir = os.path.join(user_inputs.expdir, user_inputs.pslot)

    create_comrot = query_and_clean(comrot)
    create_expdir = query_and_clean(expdir)

    if create_comrot:
        makedirs_if_missing(comrot)
        fill_COMROT(host, user_inputs)

    if create_expdir:
        makedirs_if_missing(expdir)
        fill_EXPDIR(user_inputs)
        update_configs(host, user_inputs)
