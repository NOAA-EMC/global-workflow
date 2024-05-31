#!/usr/bin/env python3
"""
Entry point for setting up Rocoto XML for all applications in global-workflow
"""

import os
from argparse import ArgumentParser, ArgumentDefaultsHelpFormatter

from applications.application_factory import app_config_factory
from rocoto.rocoto_xml_factory import rocoto_xml_factory
from wxflow import Configuration


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

    return parser.parse_args(argv[0][0] if len(argv[0]) else None)


def check_expdir(cmd_expdir, cfg_expdir):

    if not os.path.samefile(cmd_expdir, cfg_expdir):
        print('MISMATCH in experiment directories!')
        print(f'config.base:   EXPDIR = {cfg_expdir}')
        print(f'  input arg: --expdir = {cmd_expdir}')
        raise ValueError('Abort!')


def main(*argv):

    user_inputs = input_args(argv)
    rocoto_param_dict = {'maxtries': user_inputs.maxtries,
                         'cyclethrottle': user_inputs.cyclethrottle,
                         'taskthrottle': user_inputs.taskthrottle,
                         'verbosity': user_inputs.verbosity}

    print("expdir:",user_inputs.expdir)
    cfg = Configuration(user_inputs.expdir)

    print(f'config.base: {cfg.find_config("config.base")}')
    base = cfg.parse_config('config.base')
    print("base dictionary:", base)
    if 'EXPDIR' in base:
        print("base EXPDIR", base['EXPDIR'])
    else:
        print("base EXPDIR is not in base")

    check_expdir(user_inputs.expdir, base['EXPDIR'])

    net = base['NET']
    mode = base['MODE']

    # Configure the application
    app_config = app_config_factory.create(f'{net}_{mode}', cfg)

    # Create Rocoto Tasks and Assemble them into an XML
    xml = rocoto_xml_factory.create(f'{net}_{mode}', app_config, rocoto_param_dict)
    xml.write()


if __name__ == '__main__':

    main()
