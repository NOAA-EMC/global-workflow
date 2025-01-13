#!/usr/bin/env python3

import os
import datetime

from argparse import ArgumentParser
from pathlib import Path
from wxflow import parse_j2yaml, FileHandler, Logger

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
    parser.add_argument('-d', '--test_date', help='test date in YYYYMMDDHH format', type=str, required=False)
    return parser.parse_args()


if __name__ == '__main__':

    # Parse command line arguments
    args = parse_args()

    data = {}
    if args.test_date:
        data['TEST_DATE'] = datetime.datetime.strptime(args.test_date, '%Y%m%d%H')
    case_cfg = parse_j2yaml(path=args.yaml, data=data)
    FileHandler(case_cfg.input_files).sync()
