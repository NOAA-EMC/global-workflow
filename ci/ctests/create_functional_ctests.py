#!/usr/bin/env python3

import os, sys
_here = os.path.dirname(__file__)
_top = os.path.abspath(os.path.join(os.path.abspath(_here), '../..'))
sys.path.insert(0, _top)

from argparse import ArgumentParser
from pathlib import Path
from wxflow import Configuration, AttrDict, parse_j2yaml, Logger, logit
from workflow.hosts import Host

def parse_args():
    """
    Parse command line arguments.

    Returns
    -------
    argparse.Namespace
        The parsed command line arguments.
    """

    description = """Arguments for creating and updating error log files
    """
    parser = ArgumentParser(description=description)

    parser.add_argument('--yaml', help='path to file for creating functional tests per case', required=True, type=Path)
    return parser.parse_args()

if __name__ == '__main__':

    # Put HOMEgfs into the test configuration
    args = parse_args()
    data = AttrDict(HOMEgfs=_top)
    data.update(os.environ)

    # Used for getting platform specific information
    host = Host()
    cfg = Configuration(f'{data.HOMEgfs}/ci/platforms')
    platform_config = cfg.parse_config(f'config.{host.machine.lower()}')

    # Get the case name from the yaml file basename
    # and get the configuration for the case
    case_name = os.path.basename(args.yaml).split('.')[0]
    platform_config['testcase'] = case_name
    data.update(platform_config)
    case_cfg = parse_j2yaml(path=args.yaml, data=data)
    case_cfg.update(platform_config)

    print(f'\nCase name: {case_cfg.testcase}\n')
    print(f"Input Data Path: {case_cfg.fcst_gfs.staged_datapath}")
    print(f"Input Data Path: {case_cfg.gfs_atmos_pro.staged_datapath}")