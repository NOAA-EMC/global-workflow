#!/usr/bin/env python3

"""
Entry point for setting up an experiment in the global-workflow
"""

import os
import glob
import shutil
import warnings
from argparse import ArgumentParser, ArgumentDefaultsHelpFormatter, SUPPRESS

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

    comrot = os.path.join(inputs.comrot, inputs.pslot)

    do_ocean = do_ice = do_med = False

    if 'S2S' in inputs.app:
        do_ocean = do_ice = do_med = True

    if inputs.icsdir is None:
        warnings.warn("User did not provide '--icsdir' to stage initial conditions")
        return

    rdatestr = datetime_to_YMDH(inputs.idate - to_timedelta('T06H'))
    idatestr = datetime_to_YMDH(inputs.idate)

    # Test if we are using the new COM structure or the old flat one for ICs
    if inputs.start in ['warm']:
        pathstr = os.path.join(inputs.icsdir, f'{inputs.cdump}.{rdatestr[:8]}',
                               rdatestr[8:], 'model_data', 'atmos')
    else:
        pathstr = os.path.join(inputs.icsdir, f'{inputs.cdump}.{idatestr[:8]}',
                               idatestr[8:], 'model_data', 'atmos')

    if os.path.isdir(pathstr):
        flat_structure = False
    else:
        flat_structure = True

    # Destination always uses the new COM structure
    # These should match the templates defined in config.com
    if inputs.start in ['warm']:
        dst_atm_dir = os.path.join('model_data', 'atmos', 'restart')
        dst_med_dir = os.path.join('model_data', 'med', 'restart')
    else:
        dst_atm_dir = os.path.join('model_data', 'atmos', 'input')
        dst_med_dir = ''  # no mediator files for a "cold start"
        do_med = False
    dst_ocn_rst_dir = os.path.join('model_data', 'ocean', 'restart')
    dst_ocn_anl_dir = os.path.join('analysis', 'ocean')
    dst_ice_rst_dir = os.path.join('model_data', 'ice', 'restart')
    dst_atm_anl_dir = os.path.join('analysis', 'atmos')

    if flat_structure:
        # ICs are in the old flat COM structure
        if inputs.start in ['warm']:  # This is warm start experiment
            src_atm_dir = os.path.join('atmos', 'RESTART')
            src_med_dir = os.path.join('med', 'RESTART')
        elif inputs.start in ['cold']:  # This is a cold start experiment
            src_atm_dir = os.path.join('atmos', 'INPUT')
            src_med_dir = ''  # no mediator files for a "cold start"
            do_med = False
        # ocean and ice have the same filenames for warm and cold
        src_ocn_rst_dir = os.path.join('ocean', 'RESTART')
        src_ocn_anl_dir = 'ocean'
        src_ice_rst_dir = os.path.join('ice', 'RESTART')
        src_atm_anl_dir = 'atmos'
    else:
        src_atm_dir = dst_atm_dir
        src_med_dir = dst_med_dir
        src_ocn_rst_dir = dst_ocn_rst_dir
        src_ocn_anl_dir = dst_ocn_anl_dir
        src_ice_rst_dir = dst_ice_rst_dir
        src_atm_anl_dir = dst_atm_anl_dir

    def link_files_from_src_to_dst(src_dir, dst_dir):
        files = os.listdir(src_dir)
        for fname in files:
            os.symlink(os.path.join(src_dir, fname),
                       os.path.join(dst_dir, fname))
        return

    # Link ensemble member initial conditions
    if inputs.nens > 0:
        previous_cycle_dir = f'enkf{inputs.cdump}.{rdatestr[:8]}/{rdatestr[8:]}'
        current_cycle_dir = f'enkf{inputs.cdump}.{idatestr[:8]}/{idatestr[8:]}'

        for ii in range(1, inputs.nens + 1):
            memdir = f'mem{ii:03d}'
            # Link atmospheric files
            if inputs.start in ['warm']:
                dst_dir = os.path.join(comrot, previous_cycle_dir, memdir, dst_atm_dir)
                src_dir = os.path.join(inputs.icsdir, previous_cycle_dir, memdir, src_atm_dir)
            elif inputs.start in ['cold']:
                dst_dir = os.path.join(comrot, current_cycle_dir, memdir, dst_atm_dir)
                src_dir = os.path.join(inputs.icsdir, current_cycle_dir, memdir, src_atm_dir)
            makedirs_if_missing(dst_dir)
            link_files_from_src_to_dst(src_dir, dst_dir)

            # Link ocean files
            if do_ocean:
                dst_dir = os.path.join(comrot, previous_cycle_dir, memdir, dst_ocn_rst_dir)
                src_dir = os.path.join(inputs.icsdir, previous_cycle_dir, memdir, src_ocn_rst_dir)
                makedirs_if_missing(dst_dir)
                link_files_from_src_to_dst(src_dir, dst_dir)

                # First 1/2 cycle needs a MOM6 increment
                incfile = f'enkf{inputs.cdump}.t{idatestr[8:]}z.ocninc.nc'
                src_file = os.path.join(inputs.icsdir, current_cycle_dir, memdir, src_ocn_anl_dir, incfile)
                dst_file = os.path.join(comrot, current_cycle_dir, memdir, dst_ocn_anl_dir, incfile)
                makedirs_if_missing(os.path.join(comrot, current_cycle_dir, memdir, dst_ocn_anl_dir))
                os.symlink(src_file, dst_file)

            # Link ice files
            if do_ice:
                dst_dir = os.path.join(comrot, previous_cycle_dir, memdir, dst_ice_rst_dir)
                src_dir = os.path.join(inputs.icsdir, previous_cycle_dir, memdir, src_ice_rst_dir)
                makedirs_if_missing(dst_dir)
                link_files_from_src_to_dst(src_dir, dst_dir)

            # Link mediator files
            if do_med:
                dst_dir = os.path.join(comrot, previous_cycle_dir, memdir, dst_med_dir)
                src_dir = os.path.join(inputs.icsdir, previous_cycle_dir, memdir, src_med_dir)
                makedirs_if_missing(dst_dir)
                link_files_from_src_to_dst(src_dir, dst_dir)

    # Link deterministic initial conditions
    previous_cycle_dir = f'{inputs.cdump}.{rdatestr[:8]}/{rdatestr[8:]}'
    current_cycle_dir = f'{inputs.cdump}.{idatestr[:8]}/{idatestr[8:]}'

    # Link atmospheric files
    if inputs.start in ['warm']:
        dst_dir = os.path.join(comrot, previous_cycle_dir, dst_atm_dir)
        src_dir = os.path.join(inputs.icsdir, previous_cycle_dir, src_atm_dir)
    elif inputs.start in ['cold']:
        dst_dir = os.path.join(comrot, current_cycle_dir, dst_atm_dir)
        src_dir = os.path.join(inputs.icsdir, current_cycle_dir, src_atm_dir)

    makedirs_if_missing(dst_dir)
    link_files_from_src_to_dst(src_dir, dst_dir)

    # Link ocean files
    if do_ocean:
        dst_dir = os.path.join(comrot, previous_cycle_dir, dst_ocn_rst_dir)
        src_dir = os.path.join(inputs.icsdir, previous_cycle_dir, src_ocn_rst_dir)
        makedirs_if_missing(dst_dir)
        link_files_from_src_to_dst(src_dir, dst_dir)

        # First 1/2 cycle needs a MOM6 increment
        incfile = f'{inputs.cdump}.t{idatestr[8:]}z.ocninc.nc'
        src_file = os.path.join(inputs.icsdir, current_cycle_dir, src_ocn_anl_dir, incfile)
        dst_file = os.path.join(comrot, current_cycle_dir, dst_ocn_anl_dir, incfile)
        makedirs_if_missing(os.path.join(comrot, current_cycle_dir, dst_ocn_anl_dir))
        os.symlink(src_file, dst_file)

    # Link ice files
    if do_ice:
        dst_dir = os.path.join(comrot, previous_cycle_dir, dst_ice_rst_dir)
        src_dir = os.path.join(inputs.icsdir, previous_cycle_dir, src_ice_rst_dir)
        makedirs_if_missing(dst_dir)
        link_files_from_src_to_dst(src_dir, dst_dir)

    # Link mediator files
    if do_med:
        dst_dir = os.path.join(comrot, previous_cycle_dir, dst_med_dir)
        src_dir = os.path.join(inputs.icsdir, previous_cycle_dir, src_med_dir)
        makedirs_if_missing(dst_dir)
        link_files_from_src_to_dst(src_dir, dst_dir)

    # Link bias correction and radiance diagnostics files
    src_dir = os.path.join(inputs.icsdir, current_cycle_dir, src_atm_anl_dir)
    dst_dir = os.path.join(comrot, current_cycle_dir, dst_atm_anl_dir)
    makedirs_if_missing(dst_dir)
    for ftype in ['abias', 'abias_pc', 'abias_air', 'radstat']:
        fname = f'{inputs.cdump}.t{idatestr[8:]}z.{ftype}'
        src_file = os.path.join(src_dir, fname)
        if os.path.exists(src_file):
            os.symlink(src_file, os.path.join(dst_dir, fname))

    return


def fill_COMROT_forecasts(host, inputs):
    """
    Implementation of 'fill_COMROT' for forecast-only mode
    """
    print('forecast-only mode treats ICs differently and cannot be staged here')


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

    def _update_defaults(dict_in: dict) -> dict:
        defaults = dict_in.pop('defaults', AttrDict())
        defaults.update(dict_in)
        return defaults

    # Read in the YAML file to fill out templates and override host defaults
    data = AttrDict(host.info, **inputs.__dict__)
    data.HOMEgfs = _top
    yaml_path = inputs.yaml
    yaml_dict = _update_defaults(AttrDict(parse_j2yaml(yaml_path, data)))

    # First update config.base
    edit_baseconfig(host, inputs, yaml_dict)

    # loop over other configs and update them
    for cfg in yaml_dict.keys():
        if cfg == 'base':
            continue
        cfg_file = f'{inputs.expdir}/{inputs.pslot}/config.{cfg}'
        cfg_dict = get_template_dict(yaml_dict[cfg])
        edit_config(cfg_file, cfg_file, cfg_dict)

    return


def edit_baseconfig(host, inputs, yaml_dict):
    """
    Parses and populates the templated `config.base.emc.dyn` to `config.base`
    """

    tmpl_dict = {
        "@HOMEgfs@": _top,
        "@MACHINE@": host.machine.upper()}

    # Replace host related items
    extend_dict = get_template_dict(host.info)
    tmpl_dict = dict(tmpl_dict, **extend_dict)

    if inputs.start in ["warm"]:
        is_warm_start = ".true."
    elif inputs.start in ["cold"]:
        is_warm_start = ".false."

    extend_dict = dict()
    extend_dict = {
        "@PSLOT@": inputs.pslot,
        "@SDATE@": datetime_to_YMDH(inputs.idate),
        "@EDATE@": datetime_to_YMDH(inputs.edate),
        "@CASECTL@": f'C{inputs.resdet}',
        "@EXPDIR@": inputs.expdir,
        "@ROTDIR@": inputs.comrot,
        "@EXP_WARM_START@": is_warm_start,
        "@MODE@": inputs.mode,
        "@gfs_cyc@": inputs.gfs_cyc,
        "@APP@": inputs.app
    }
    tmpl_dict = dict(tmpl_dict, **extend_dict)

    extend_dict = dict()
    if getattr(inputs, 'nens', 0) > 0:
        extend_dict = {
            "@CASEENS@": f'C{inputs.resens}',
            "@NMEM_ENS@": inputs.nens,
        }
        tmpl_dict = dict(tmpl_dict, **extend_dict)

    extend_dict = dict()
    if inputs.mode in ['cycled']:
        extend_dict = {
            "@DOHYBVAR@": "YES" if inputs.nens > 0 else "NO",
        }
        tmpl_dict = dict(tmpl_dict, **extend_dict)

    try:
        tmpl_dict = dict(tmpl_dict, **get_template_dict(yaml_dict['base']))
    except KeyError:
        pass

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


def input_args(*argv):
    """
    Method to collect user arguments for `setup_expt.py`
    """

    ufs_apps = ['ATM', 'ATMA', 'ATMW', 'S2S', 'S2SA', 'S2SW']

    def _common_args(parser):
        parser.add_argument('--pslot', help='parallel experiment name',
                            type=str, required=False, default='test')
        parser.add_argument('--resdet', help='resolution of the deterministic model forecast',
                            type=int, required=False, default=384)
        parser.add_argument('--comrot', help='full path to COMROT',
                            type=str, required=False, default=os.getenv('HOME'))
        parser.add_argument('--expdir', help='full path to EXPDIR',
                            type=str, required=False, default=os.getenv('HOME'))
        parser.add_argument('--idate', help='starting date of experiment, initial conditions must exist!',
                            required=True, type=lambda dd: to_datetime(dd))
        parser.add_argument('--edate', help='end date experiment', required=True, type=lambda dd: to_datetime(dd))
        return parser

    def _gfs_args(parser):
        parser.add_argument('--start', help='restart mode: warm or cold', type=str,
                            choices=['warm', 'cold'], required=False, default='cold')
        parser.add_argument('--cdump', help='CDUMP to start the experiment',
                            type=str, required=False, default='gdas')
        # --configdir is hidden from help
        parser.add_argument('--configdir', help=SUPPRESS, type=str, required=False, default=os.path.join(_top, 'parm/config/gfs'))
        parser.add_argument('--yaml', help='Defaults to substitute from', type=str,
                            required=False, default=os.path.join(_top, 'parm/config/gfs/yaml/defaults.yaml'))
        return parser

    def _gfs_cycled_args(parser):
        parser.add_argument('--icsdir', help='full path to initial condition directory', type=str, required=False, default=None)
        parser.add_argument('--app', help='UFS application', type=str,
                            choices=ufs_apps, required=False, default='ATM')
        parser.add_argument('--gfs_cyc', help='cycles to run forecast', type=int,
                            choices=[0, 1, 2, 4], default=1, required=False)
        return parser

    def _gfs_or_gefs_ensemble_args(parser):
        parser.add_argument('--resens', help='resolution of the ensemble model forecast',
                            type=int, required=False, default=192)
        parser.add_argument('--nens', help='number of ensemble members',
                            type=int, required=False, default=20)
        return parser

    def _gfs_or_gefs_forecast_args(parser):
        parser.add_argument('--app', help='UFS application', type=str,
                            choices=ufs_apps + ['S2SWA'], required=False, default='ATM')
        parser.add_argument('--gfs_cyc', help='Number of forecasts per day', type=int,
                            choices=[1, 2, 4], default=1, required=False)
        return parser

    def _gefs_args(parser):
        parser.add_argument('--start', help='restart mode: warm or cold', type=str,
                            choices=['warm', 'cold'], required=False, default='cold')
        parser.add_argument('--configdir', help=SUPPRESS, type=str, required=False,
                            default=os.path.join(_top, 'parm/config/gefs'))
        parser.add_argument('--yaml', help='Defaults to substitute from', type=str, required=False,
                            default=os.path.join(_top, 'parm/config/gefs/yaml/defaults.yaml'))
        parser.add_argument('--icsdir', help='full path to initial condition directory [temporary hack in place for testing]',
                            type=str, required=False, default=None)
        return parser

    description = """
        Setup files and directories to start a GFS parallel.\n
        Create EXPDIR, copy config files.\n
        Create COMROT experiment directory structure,
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

    return parser.parse_args(list(*argv) if len(argv) else None)


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
    supp_res = host.info['SUPPORTED_RESOLUTIONS']
    machine = host.machine
    for attr in ['resdet', 'resens']:
        try:
            expt_res = f'C{getattr(inputs, attr)}'
        except AttributeError:
            continue
        if expt_res not in supp_res:
            raise NotImplementedError(f"Supported resolutions on {machine} are:\n{', '.join(supp_res)}")


def main(*argv):

    user_inputs = input_args(*argv)
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


if __name__ == '__main__':

    main()
