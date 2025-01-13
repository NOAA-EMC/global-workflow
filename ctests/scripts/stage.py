#!/usr/bin/env python3

import os, sys
import shutil
import datetime

_here = os.path.dirname(__file__)
_top = os.path.abspath(os.path.join(os.path.abspath(_here), '../../..'))
sys.path.insert(0, _top)

from argparse import ArgumentParser
from pathlib import Path
from wxflow import parse_j2yaml,  FileHandler, AttrDict, Logger

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

    parser.add_argument('-y', '--yaml', help='full path to yaml file describing the job test configuration', type=Path, required=True)
    parser.add_argument('-d', '--test_date', help='full path to yaml file describing the job test configuration', type=datetime, required=False)
    return parser.parse_args()

if __name__ == '__main__':

    # Parse command line arguments
    args = parse_args()
    data = {'TEST_DATE': args.test_date}
    case_cfg = parse_j2yaml(path=args.yaml, data=data)
    FileHandler(case_cfg.input_files).sync()
