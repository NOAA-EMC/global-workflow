#!/usr/bin/env python3

import sys
import os
import copy
from time import sleep

from wxflow import which, Logger, CommandNotFoundError, ProcessError
from argparse import ArgumentParser, FileType

from collections import Counter

logger = Logger(level=os.environ.get("LOGGING_LEVEL", "DEBUG"), colored_log=False)


def attempt_multiple_times(expression, max_attempts, sleep_duration=0, exception_class=Exception):
    """
    Retries a function multiple times.

    Try to execute the function expression up to max_attempts times ignoring any exceptions
    of the type exception_class, It waits for sleep_duration seconds between attempts.

    Parameters
    ----------
    expression : callable
        The function to be executed.
    max_attempts : int
        The maximum number of attempts to execute the function.
    sleep_duration : int, optional
        The number of seconds to wait between attempts. Default is 0.
    exception_class : Exception, optional
        The type of exception to catch. Default is the base Exception class, catching all exceptions.

    Returns
    -------
    The return value of the function expression.

    Raises
    ------
    exception_class
        If the function expression raises an exception of type exception_class
        in all max_attempts attempts.

    """

    attempt = 0
    last_exception = None
    while attempt < max_attempts:
        try:
            pass
            return expression()
        except exception_class as last_exception:
            attempt += 1
            sleep(sleep_duration)
    else:
        raise last_exception


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
    rocoto_summary Run rocotostat and process its output.

    rocoto_summary(rocotostat) adds a default argument '--summary' to the rocotostat
    command, runs it, and processes its output to return a dictionary with the total
    number of cycles and the number of cycles marked as 'Done'.

    Input:
    rocotostat - The rocotostat command.

    Output:
    rocoto_status - A dictionary with the total number of cycles and the number of cycles marked as 'Done'.
    """
    rocotostat = copy.deepcopy(rocotostat)
    rocotostat.add_default_arg('--summary')
    rocotostat_output = attempt_multiple_times(lambda: rocotostat(output=str), 3, 90, ProcessError)
    rocotostat_output = rocotostat_output.splitlines()[1:]
    rocotostat_output = [line.split()[0:2] for line in rocotostat_output]

    rocoto_status = {
        'CYCLES_TOTAL': len(rocotostat_output),
        'CYCLES_DONE': sum([sublist.count('Done') for sublist in rocotostat_output])
    }
    return rocoto_status


def rocoto_statcount(rocotostat):
    """
    rocoto_statcount Run rocotostat and process its output.

    rocoto_statcount(rocotostat) adds a default argument '--all' to the rocotostat
    command, runs it, and processes its output to return a dictionary with the count
    of each status case.

    Input:
    rocotostat - The rocotostat command.

    Output:
    rocoto_status - A dictionary with the count of each status case.
    """

    rocotostat = copy.deepcopy(rocotostat)
    rocotostat.add_default_arg('--all')

    rocotostat_output = attempt_multiple_times(lambda: rocotostat(output=str), 4, 120, ProcessError)
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
    """
    is_done Check if all cycles are done.

    is_done(rocoto_status) checks if the total number of cycles equals the number of
    done cycles in the rocoto_status dictionary.

    Input:
    rocoto_status - A dictionary with the count of each status case.

    Output:
    boolean - True if all cycles are done, False otherwise.
    """

    if rocoto_status['CYCLES_TOTAL'] == rocoto_status['CYCLES_DONE']:
        return True
    else:
        return False


def is_stalled(rocoto_status):
    """
    is_stalled Check if all cycles are stalled.

    is_stalled(rocoto_status) checks if all cycles are stalled by verifying if
    there are no jobs that are RUNNING, SUBMITTING, or QUEUED.

    Input:
    rocoto_status - A dictionary with the count of each status case.

    Output:
    boolean - True if all cycles are stalled, False otherwise.
    """

    if rocoto_status['RUNNING'] + rocoto_status['SUBMITTING'] + rocoto_status['QUEUED'] == 0:
        return True
    else:
        return False


if __name__ == '__main__':
    """
    main Execute the script.

    main() parses the input arguments, checks if the rocotostat command is available,
    adds default arguments to the rocotostat command, and runs it and reports
    out to stdout spcific information of rocoto workflow.
    """

    args = input_args()

    try:
        rocotostat = which("rocotostat")
    except CommandNotFoundError:
        logger.exception("rocotostat not found in PATH")
        raise CommandNotFoundError("rocotostat not found in PATH")

    rocotostat.add_default_arg(['-w', os.path.abspath(args.w.name), '-d', os.path.abspath(args.d.name)])

    rocoto_status = rocoto_statcount(rocotostat)
    rocoto_status.update(rocotostat_summary(rocotostat))

    error_return = 0
    if is_done(rocoto_status):
        rocoto_state = 'DONE'
    elif rocoto_status['DEAD'] > 0:
        error_return = rocoto_status['FAIL'] + rocoto_status['DEAD']
        rocoto_state = 'FAIL'
    elif 'UNKNOWN' in rocoto_status:
        error_return = rocoto_status['UNKNOWN']
        rocoto_state = 'UNKNOWN'
    elif is_stalled(rocoto_status):
        rocoto_status = attempt_multiple_times(lambda: rocoto_statcount(rocotostat), 2, 120, ProcessError)
        if is_stalled(rocoto_status):
            error_return = 3
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
