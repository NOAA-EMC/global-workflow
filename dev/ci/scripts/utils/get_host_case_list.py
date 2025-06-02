#!/usr/bin/env python3
import os
from os.path import basename, splitext
import sys
import glob
from wxflow import AttrDict, parse_j2yaml, find_upward


def get_host_cases(host, HOMEgfs=None):
    """
    Get list of test cases supported on a host

    Args:
        host (str): Host name to check
        HOMEgfs (str, optional): Path to the global-workflow repository root directory

    Returns:
        list: List of case names (without extension) supported on the host
    """
    HOMEgfs = HOMEgfs or find_upward('.github')
    case_list = []

    # Set up data for template rendering
    data = AttrDict(HOMEgfs=HOMEgfs)
    data.update(os.environ)

    # Get all case files
    case_files = glob.glob(f'{HOMEgfs}/dev/ci/cases/pr/*.yaml')

    for case_yaml in case_files:
        # Parse the case configuration
        case_conf = parse_j2yaml(path=case_yaml, data=data)

        # Skip cases that don't support this host
        if 'skip_ci_on_hosts' in case_conf:
            if host.lower() in [machine.lower() for machine in case_conf.skip_ci_on_hosts]:
                continue

        # Add the case name (without extension) to the list
        case_list.append(splitext(basename(case_yaml))[0])

    return case_list


if __name__ == '__main__':
    # When run as a script, maintain the original behavior
    if len(sys.argv) < 2 or sys.argv[1] in ('-h', '--help'):
        print('Usage: get_host_case_list.py <host_name>')
        sys.exit(1)

    host = sys.argv[1]
    cases = get_host_cases(host)
    print(' '.join(cases))
