#!/usr/bin/env python3

import os, sys
import shutil
_here = os.path.dirname(__file__)
_top = os.path.abspath(os.path.join(os.path.abspath(_here), '../../..'))
sys.path.insert(0, _top)

from argparse import ArgumentParser
from pathlib import Path
from wxflow import Configuration, AttrDict, parse_j2yaml, Logger, logit, which, CommandNotFoundError, ProcessError
from workflow.hosts import Host

logger = Logger(level=os.environ.get("LOGGING_LEVEL", "DEBUG"), colored_log=False)

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

    parser.add_argument('--case', help='Case name', required=True, type=str)
    parser.add_argument('--job', help='Job name', required=True, type=str)
    parser.add_argument('--build_dir', help='CMake build directory', required=False, type=Path, default=None)
    return parser.parse_args()

if __name__ == '__main__':

    # Parse command line arguments
    args = parse_args()
    data = AttrDict(HOMEgfs=_top)
    data.update(os.environ)

    case_yaml_path = Path(f'{data.HOMEgfs}/ci/ctests/cases/{args.case}.yaml')
    pr_case_yaml_path = Path(f'{data.HOMEgfs}/ci/cases/pr/{args.case}.yaml')

    # Initialize host and platform configuration
    host = Host()
    cfg = Configuration(f'{data.HOMEgfs}/ci/platforms')
    platform_config = cfg.parse_config(f'config.{host.machine.lower()}')
    data.update(platform_config)

    print(f'{args.build_dir}/RUNTESTS/EXPDIR/{args.case}_{args.job}')
    cfg = Configuration(f'{args.build_dir}/RUNTESTS/EXPDIR/{args.case}_{args.job}')
    base_cfg = cfg.parse_config('config.base')

    pr_case_cfg = parse_j2yaml(path=pr_case_yaml_path, data=data)
    print (f'idate: {pr_case_cfg.idate}')

    print (base_cfg['PDY'])

    base_cfg.update(data)

    case_cfg = parse_j2yaml(path=case_yaml_path, data=data)
    print (case_cfg)
