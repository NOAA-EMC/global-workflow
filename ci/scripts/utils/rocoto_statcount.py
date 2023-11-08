#!/usr/bin/env python3

import sys
import os

from wxflow import Executable, which, Logger
from argparse import ArgumentParser, ArgumentDefaultsHelpFormatter

logger = Logger(level=os.environ.get("LOGGING_LEVEL", "DEBUG"), colored_log=False)


def input_args():
    """
    Method to collect user arguments 
    """

    description = """
        """

    parser = ArgumentParser(description=description,
                            formatter_class=ArgumentDefaultsHelpFormatter)

    parser.add_argument('--xml',help='workflow_document', type=str)
    parser.add_argument('--db',help='database_file', type=str)

    args = parser.parse_args()

    return args

if __name__ == '__main__':

    args = input_args()

    rocotostat = which("rocotostat")
    if not rocotostat:
        logger.exception("rocotostat not found in PATH")
        sys.exit(-1)

    xml_file_path = os.path.abspath(args.xml)
    db_file_path = os.path.abspath(args.db)

    rocotostat.add_default_args(['-w',xml_file_path,'-d',db_file_path,'-s'])
    rocotostat_output = rocotostat(output=str)
    print(rocotostat_output)
