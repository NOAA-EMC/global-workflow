#!/usr/bin/env python3

"""
stage.py

This script is part of the ctest framework for testing Rocoto JJOBS that stages the
input files needed to run a JJOB independently from other jobs in the workflow.
The YAML file specified at the command line contains the paths to the staged input files
and their corresponding directories under the COMROOT of the experiment for the JJOB.

Usage:
    stage.py -y <yaml_file> [-d <test_date>]

Arguments:
    -y, --yaml      Path to the YAML file describing the job test configuration (required)
    -d, --test_date Test date in YYYYMMDDHH format (optional)

Example:
    ./stage.py -y /path/to/config.yaml -d 2021032312
"""

import os
import datetime

from argparse import ArgumentParser
from pathlib import Path
from wxflow import parse_j2yaml, FileHandler, Logger

# Initialize logger with environment variable for logging level
logger = Logger(level=os.environ.get("LOGGING_LEVEL", "DEBUG"), colored_log=False)


def parse_args():
    """
    Parse command line arguments.

    Returns
    -------
    argparse.Namespace
        The parsed command line arguments, including:
        - yaml: Path to the YAML file describing the job test configuration.
        - test_date: Optional test date in YYYYMMDDHH format.
    """
    description = """Arguments for creating and updating error log files
    """
    parser = ArgumentParser(description=description)

    # Add argument for YAML file path
    parser.add_argument('-y', '--yaml', help='full path to yaml file describing the job test configuration', type=Path, required=True)
    # Add optional argument for test date
    parser.add_argument('-d', '--test_date', help='test date in YYYYMMDDHH format', type=str, required=False)
    return parser.parse_args()


if __name__ == '__main__':

    # Parse command line arguments
    args = parse_args()

    data = {}
    if args.test_date:
        # Parse test date from string to datetime object
        data['TEST_DATE'] = datetime.datetime.strptime(args.test_date, '%Y%m%d%H')
    # Parse YAML configuration file with optional data
    case_cfg = parse_j2yaml(path=args.yaml, data=data)
    # Synchronize input files as per the parsed configuration
    FileHandler(case_cfg.input_files).sync()
