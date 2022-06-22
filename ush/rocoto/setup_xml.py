#!/usr/bin/env python3
"""
Entry point for setting up Rocoto XML for all applications in global-workflow
"""

import os
from argparse import ArgumentParser, ArgumentDefaultsHelpFormatter

from configuration import Configuration
from applications import AppConfig
from workflow_xml import RocotoXML


def input_args():
    """
    Method to collect user arguments for `setup_xml.py`
    """

    description = """
        Sources configuration files based on application and\n
        creates "PSLOT.xml" for use with Rocoto.\n
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
        subp.add_argument('--expdir', help='full path to experiment directory containing config files', type=str,
                          required=False, default=os.environ['PWD'])

    forecasts.add_argument('--cdump', help='cycle to run forecasts', type=str, choices=['gdas', 'gfs'], default='gfs',
                           required=False)

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
    app_config = AppConfig(user_inputs.mode, cfg)

    # Create Rocoto Tasks and Assemble them into an XML
    xml = RocotoXML(app_config)
    xml.write()
