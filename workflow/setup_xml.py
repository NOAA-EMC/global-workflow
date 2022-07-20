#!/usr/bin/env python3
"""
Entry point for setting up Rocoto XML for all applications in global-workflow
"""

import os
from argparse import ArgumentParser, ArgumentDefaultsHelpFormatter

from configuration import Configuration
from applications import AppConfig
from rocoto.workflow_xml import RocotoXML


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

    cfg = Configuration(user_inputs.expdir)

    check_expdir(user_inputs.expdir, cfg.parse_config('config.base')['EXPDIR'])

    # Configure the application
    app_config = AppConfig(cfg)

    # Create Rocoto Tasks and Assemble them into an XML
    xml = RocotoXML(app_config)
    xml.write()
