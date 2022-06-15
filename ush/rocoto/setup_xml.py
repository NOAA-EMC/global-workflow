#!/usr/bin/env python3
"""
Entry point for setting up Rocoto XML for all applications in global-workflow
"""

import workflow_utils as wfu

from configuration import Configuration
from applications import AppConfig
from workflow_xml import RocotoXML


@property
def input_args():
    """
    Method to collect user arguments for `setup_xml.py`
    """

    here = os.path.dirname(__file__)
    top = os.path.abspath(os.path.join(
        os.path.abspath(here), '../..'))

    description = """
        Sources configuration files based on application and\n
        creates "PSLOT.xml" for use with Rocoto.\n
        """

    parser = ArgumentParser(description=description,
                            formatter_class=ArgumentDefaultsHelpFormatter)

    parser.add_argument('--expdir', help='full path to experiment directory containing config files', type=str,
                        required=False, default=os.environ['PWD'])

    # Set up sub-parsers for various modes of experimentation
    subparser = parser.add_subparsers(dest='mode')
    cycled = subparser.add_parser(
        'cycled', help='arguments for cycled mode')
    forecasts = subparser.add_parser(
        'forecast-only', help='arguments for forecast-only mode')

    forecasts.add_argument('--cdump', help='cycle to run forecasts', type=str, choices=['gdas', 'gfs'], default='gfs',
                           required=False)

    args = parser.parse_args()

    return args


def main():

    user_inputs = input_args

    cfg = Configuration(user_inputs.expdir)

    _base = cfg.parse_config('config.base')
    wfu.check_expdir(user_inputs.expdir, _base['EXPDIR'])

    # Configure the application
    app_config = AppConfig(user_inputs.mode, cfg)

    # Create Rocoto Tasks and Assemble them into an XML
    xml = RocotoXML(app_config)
    xml.write()

    # Create crontab for Rocoto #  TODO - move to RocotoXML since it is the beneficiary
    wfu.create_crontab(_base)
