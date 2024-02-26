#!/usr/bin/env python3
import os
from os.path import basename, splitext
import sys
import glob
from wxflow import parse_j2yaml
from wxflow import AttrDict

_here = os.path.dirname(__file__)
_top = os.path.abspath(os.path.join(os.path.abspath(_here), '../../..'))

if __name__ == '__main__':

    if len(sys.argv) < 2:
        print('Usage: get_host_case_list.py <host_name>')
        sys.exit(1)

    host = sys.argv[1]

    case_list = []
    HOMEgfs = _top
    data = AttrDict(HOMEgfs=_top)
    data.update(os.environ)

    case_files = glob.glob(f'{HOMEgfs}/ci/cases/pr/*.yaml')
    for case_yaml in case_files:
        case_conf = parse_j2yaml(path=case_yaml, data=data)
        if 'skip_ci_on_hosts' in case_conf:
            if host.lower() in [machine.lower() for machine in case_conf.skip_ci_on_hosts]:
                continue
        case_list.append(splitext(basename(case_yaml))[0])
    print(' '.join(case_list))
