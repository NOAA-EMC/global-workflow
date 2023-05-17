#!/usr/bin/env python3
"""
Entry point for setting up Rocoto XML for all applications in global-workflow
"""

import os
from argparse import ArgumentParser, ArgumentDefaultsHelpFormatter

from applications import AppConfig
from rocoto.workflow_xml import RocotoXML
from pygw.configuration import Configuration


def input_args():
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

    args = parser.parse_args()

    return args


def check_expdir(cmd_expdir, cfg_expdir):

    if not os.path.samefile(cmd_expdir, cfg_expdir):
        print('MISMATCH in experiment directories!')
        print(f'config.base:   EXPDIR = {cfg_expdir}')
        print(f'  input arg: --expdir = {cmd_expdir}')
        raise ValueError('Abort!')


if __name__ == '__main__':

    user_inputs = input_args()
    rocoto_param_dict = {'maxtries': user_inputs.maxtries,
                         'cyclethrottle': user_inputs.cyclethrottle,
                         'taskthrottle': user_inputs.taskthrottle,
                         'verbosity': user_inputs.verbosity}

    cfg = Configuration(user_inputs.expdir)

    base = cfg.parse_config('config.base')

    check_expdir(user_inputs.expdir, base['EXPDIR'])

    net = base['NET']
    mode = base['MODE']

    # Configure the application
    app_config = AppConfig.app_config_factory.create(f'{net}_{mode}', cfg)

    # Create Rocoto Tasks and Assemble them into an XML
    xml = RocotoXML.rocoto_xml_factory.create(f'{net}_{mode}', app_config, rocoto_param_dict)
    xml.write()
