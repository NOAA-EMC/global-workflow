#!/usr/bin/env python3

import sys
import os

from wxflow import Executable, which, Logger, CommandNotFoundError
from argparse import ArgumentParser, FileType

from collections import Counter

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
        Assuming rocotorun had just been run, and the rocoto_state is not Done, then
        rocoto_state is Stalled if there are no jobs that are RUNNING, SUBMITTING, or QUEUED.
        """

    parser = ArgumentParser(description=description)

    parser.add_argument('-w', help='workflow_document', type=FileType('r'), required=True)
    parser.add_argument('-d', help='database_file', metavar='Database File', type=FileType('r'), required=True)
    parser.add_argument('--verbose', action='store_true', help='List the states and the number of jobs that are in each', required=False)
    parser.add_argument('-v', action='store_true', help='List the states and the number of jobs that are in each', required=False)
    parser.add_argument('--export', action='store_true', help='create and export list of the status values for bash', required=False)

    args = parser.parse_args()

    return args


def rocotostat_summary(rocotostat):
    """
    ROCOTO_SUMMARY Run rocotostat and process its output.

    ROCOTO_SUMMARY(ROCOTOSTAT) adds a default argument '--summary' to the rocotostat
    command, runs it, and processes its output to return a dictionary with the total
    number of cycles and the number of cycles marked as 'Done'.

    Input:
    ROCOTOSTAT - The rocotostat command.

    Output:
    ROCOTO_STATUS - A dictionary with the total number of cycles and the number of cycles marked as 'Done'.
    """
    rocotostat.add_default_arg('--summary')
    rocotostat_output = rocotostat(output=str)
    rocotostat_output = rocotostat_output.splitlines()[1:]
    rocotostat_output = [line.split()[0:2] for line in rocotostat_output]

    rocoto_status = {
        'CYCLES_TOTAL': len(rocotostat_output),
        'CYCLES_DONE': sum([sublist.count('Done') for sublist in rocotostat_output])
    }
    return rocoto_status


def rocoto_statcount(rocotostat):
    """
    ROCOTO_STATCOUNT Run rocotostat and process its output.

    ROCOTO_STATCOUNT(ROCOTOSTAT) adds a default argument '--all' to the rocotostat
    command, runs it, and processes its output to return a dictionary with the count
    of each status case.

    Input:
    ROCOTOSTAT - The rocotostat command.

    Output:
    ROCOTO_STATUS - A dictionary with the count of each status case.
    """

    rocotostat.add_default_arg('--all')

    rocotostat_output = rocotostat(output=str)
    rocotostat_output = rocotostat_output.splitlines()[1:]
    rocotostat_output = [line.split()[0:4] for line in rocotostat_output]
    rocotostat_output = [line for line in rocotostat_output if len(line) != 1]

    status_cases = ['SUCCEEDED', 'FAIL', 'DEAD', 'RUNNING', 'SUBMITTING', 'QUEUED']

    rocoto_status = {}
    status_counts = Counter(case for sublist in rocotostat_output for case in sublist)
    for case in status_cases:
        rocoto_status[case] = status_counts[case]

    return rocoto_status

def is_done(rocoto_status):
    if rocoto_status['CYCLES_TOTAL'] == rocoto_status['CYCLES_DONE']:
        return True
    else:
        return False

def is_stalled(rocoto_status):
   if rocoto_status['RUNNING'] + rocoto_status['SUBMITTING'] + rocoto_status['QUEUED'] == 0:
       return True
   else:
       return False


if __name__ == '__main__':

    args = input_args()

    try:
        rocotostat = which("rocotostat")
    except CommandNotFoundError:
        logger.exception("rocotostat not found in PATH")
        raise CommandNotFoundError("rocotostat not found in PATH")

    rocotostat.add_default_arg(['-w', os.path.abspath(args.w.name), '-d', os.path.abspath(args.d.name)])

    error_return = 0
    rocoto_status = rocoto_statcount(rocotostat)
    rocoto_status.update(rocotostat_summary(rocotostat))

    if is_done(rocoto_status):
        rocoto_state = 'DONE'
    elif rocoto_status['DEAD'] > 0:
        error_return = rocoto_status['FAIL'] + rocoto_status['DEAD']
        rocoto_state = 'FAIL'
    elif 'UNKNOWN' in rocoto_status:
        error_return = rocoto_status['UNKNOWN']
        rocoto_state = 'UNKNOWN'
    elif is_stalled(rocoto_status):
        #
        #  TODO for now a STALLED state will be just a warning as it can
        #  produce a false negative if there is a timestamp on a file dependency.
        #
        #   error_return = -3
        rocoto_state = 'STALLED'
    else:
        rocoto_state = 'RUNNING'

    rocoto_status['ROCOTO_STATE'] = rocoto_state

    if args.verbose or args.v:
        for status in rocoto_status:
            if args.v:
                print(f'{status}:{rocoto_status[status]}')
            else:
                print(f'Number of {status} : {rocoto_status[status]}')

    if args.export:
        for status in rocoto_status:
            print(f'export {status}={rocoto_status[status]}')
    else:
        print(rocoto_state)

    sys.exit(error_return)
