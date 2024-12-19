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
    # Locate cmake executable
    try:
        cmake = which("cmake")
    except CommandNotFoundError:
        logger.exception("cmake not found in PATH")
        raise CommandNotFoundError("cmake not found in PATH")

    # Parse command line arguments
    args = parse_args()
    data = AttrDict(HOMEgfs=_top)
    data.update(os.environ)

    # Initialize host and configuration
    host = Host()
    cfg = Configuration(f'{data.HOMEgfs}/ci/platforms')

    # Create directory for tests
    test_dir = os.path.join(_here, 'TESTS')
    mkdir_p(test_dir)

    # Process each YAML file
    case_names = ""
    for yaml in args.yaml:
        platform_config = cfg.parse_config(f'config.{host.machine.lower()}')
        case_name = os.path.basename(yaml).split('.')[0]
        platform_config['testcase'] = case_name
        data.update(platform_config)
        case_cfg = parse_j2yaml(path=yaml, data=data)
        case_cfg.update(platform_config)

        # Create job list file for the case
        top_level_entries = [key for key in case_cfg.keys() if isinstance(case_cfg[key], dict)]
        job_list_file = f"TESTS/{case_name}_jobs.txt"
        with open(job_list_file, 'w') as f:
            for entry in top_level_entries:
                f.write(f"{case_name}_{entry}\n")

        case_names += f"{case_name} "

        platform_config.clear()
        data.clear()
        case_cfg.clear()

    # Run cmake to create the functional tests
    cmake.add_default_arg([f"-S {_here}", f"-B {test_dir}", f"-DCASE_LIST='{case_names[:-1]}'"])
    cmake()
