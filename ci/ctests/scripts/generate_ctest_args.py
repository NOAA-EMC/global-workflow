#!/usr/bin/env python3

import os, sys
import shutil
_here = os.path.dirname(__file__)
_top = os.path.abspath(os.path.join(os.path.abspath(_here), '../..'))
sys.path.insert(0, _top)

from argparse import ArgumentParser
from pathlib import Path
from wxflow import Configuration, AttrDict, parse_j2yaml, Logger, logit, which, CommandNotFoundError, ProcessError
from workflow.hosts import Host
from wxflow.fsutils import mkdir_p, chdir, cp

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

    parser.add_argument('--yaml', help='paths to YAML files for creating functional tests per case', required=True, type=Path, nargs='+')
    return parser.parse_args()

if __name__ == '__main__':

    # Parse command line arguments
    args = parse_args()
    data = AttrDict(HOMEgfs=_top)
    data.update(os.environ)

    # Initialize host and configuration
    host = Host()
    cfg = Configuration(f'{data.HOMEgfs}/ci/platforms')

    # Initialize dictionaries to hold case names and their job names
    case_names = []
    case_jobs = {}

    for yaml in args.yaml:
        platform_config = cfg.parse_config(f'config.{host.machine.lower()}')
        case_name = os.path.basename(yaml).split('.')[0]
        platform_config['testcase'] = case_name
        data.update(platform_config)
        case_cfg = parse_j2yaml(path=yaml, data=data)
        case_cfg.update(platform_config)

        # Collect job names from the YAML file
        top_level_entries = [key for key in case_cfg.keys() if isinstance(case_cfg[key], dict)]
        job_names = [f"{case_name}_{entry}" for entry in top_level_entries]
        case_jobs[case_name.upper()] = ' '.join(job_names)
        case_names.append(case_name)

    # Prepare cmake command-line arguments
    cmake_args = [
        f"-DCASE_LIST='{ ' '.join(case_names) }'"
    ]
    for case_upper, jobs in case_jobs.items():
        cmake_args.append(f"-DJOB_NAMES_{case_upper}='{jobs}'")

    for each_arg in cmake_args:
        print(each_arg, end=' ')
    print()