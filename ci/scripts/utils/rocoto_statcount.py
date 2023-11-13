#!/usr/bin/env python3

import sys
import os

from wxflow import Executable, which, Logger, CommandNotFoundError
from argparse import ArgumentParser, ArgumentDefaultsHelpFormatter

logger = Logger(level=os.environ.get("LOGGING_LEVEL", "DEBUG"), colored_log=False)


def input_args():
    """
    Parse command-line arguments.

    Returns
    -------
    args : Namespace
        The parsed command-line arguments.
    """

    description = """
        Using rocotostat to get the status of all jobs this scripts
        determines rocoto_state: if all cycles are done, then rocoto_state is Done.
        If all cycles are not done, then rocoto_state is Running.
        If the check_stalled is used then rocotorun is issued and
        rocotostat is run again and checks if all jobs have not advanced, then
        rocoto_state is Stalled and the script exits with -1.
        """

    parser = ArgumentParser(description=description,
                            formatter_class=ArgumentDefaultsHelpFormatter)

    parser.add_argument('-w', help='workflow_document', type=str)
    parser.add_argument('-d', help='database_file', type=str)

    args = parser.parse_args()

    return args


def rocoto_statcount():
    """
    Run rocotostat and process its output.
    """

    args = input_args()

    try:
        rocotostat = which("rocotostat")
    except CommandNotFoundError:
        logger.exception("rocotostat not found in PATH")
        raise CommandNotFoundError("rocotostat not found in PATH")

    xml_file_path = os.path.abspath(args.w)
    db_file_path = os.path.abspath(args.d)

    rocotostat_all = which("rocotostat")
    rocotostat.add_default_arg(['-w', xml_file_path, '-d', db_file_path, '-s'])
    rocotostat_all.add_default_arg(['-w', xml_file_path, '-d', db_file_path, '-a'])

    rocotostat_output = rocotostat(output=str)
    rocotostat_output = rocotostat_output.splitlines()[1:]
    rocotostat_output = [line.split()[0:2] for line in rocotostat_output]

    rocotostat_output_all = rocotostat_all(output=str)
    rocotostat_output_all = rocotostat_output_all.splitlines()[1:]
    rocotostat_output_all = [line.split()[0:4] for line in rocotostat_output_all]
    rocotostat_output_all = [line for line in rocotostat_output_all if len(line) != 1]

    rocoto_status = {
        'Cycles': len(rocotostat_output),
        'Done_Cycles':  sum([ sublist.count('Done') for sublist in rocotostat_output ])
    }

    status_cases = [ 'SUCCEEDED', 'FAIL', 'DEAD', 'RUNNING', 'SUBMITTING', 'QUEUED']
    for case in status_cases:
        rocoto_status[case] = sum([ sublist.count(case) for sublist in rocotostat_output_all ])

    return rocoto_status

if __name__ == '__main__':

    args = input_args()

    rocoto_status = rocoto_statcount()
    for status in rocoto_status:
        print(f'Number of {status} : {rocoto_status[status]}')
    if rocoto_status['Cycles'] == rocoto_status['Done_Cycles']:
        rocoto_state = 'Done'
    elif 'UNKNOWN' in rocoto_status:
        rocoto_state = 'Unknown'
        print(f'Rocoto State : {rocoto_state}')
    elif rocoto_status['RUNNING'] + rocoto_status['SUBMITTING'] + rocoto_status['QUEUED'] == 0:
        rocoto_state = 'Stalled'
        print(f'Rocoto State : {rocoto_state}')
        sys.exit(-1)
    else:
        rocoto_state = 'Running'
    print(f'Rocoto State : {rocoto_state}')
