#!/usr/bin/env python3
import os
from os.path import basename, splitext
import sys
import glob
from wxflow import parse_j2yaml
from wxflow import AttrDict
from find_homegfs import find_homegfs


def get_host_cases(host, homegfs=None):
    """
    Get list of test cases supported on a host

    Args:
        host (str): Host name to check
        homegfs (str, optional): Path to the global-workflow repository root directory

    Returns:
        list: List of case names (without extension) supported on the host
    """
    homegfs = homegfs or find_homegfs()
    case_list = []

    # Set up data for template rendering
    data = AttrDict(HOMEgfs=homegfs)
    data.update(os.environ)

    # Get all case files
    case_files = glob.glob(f'{homegfs}/dev/ci/cases/pr/*.yaml')

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
