#!/usr/bin/env python3

import os

from pygw.yaml_file import YAMLFile
from pygw.logger import logit

from argparse import ArgumentParser, ArgumentDefaultsHelpFormatter

#from hosts import Host

# @logit(logger)

_here = os.path.dirname(__file__)
_top = os.path.abspath(os.path.join(os.path.abspath(_here), '..'))

def input_args():
    """
    Method to collect user arguments for `create_experment.py`
    """
    description = """
        Single agument as a yaml file
        """
    parser = ArgumentParser(description=description,
                            formatter_class=ArgumentDefaultsHelpFormatter)

    parser.add_argument('--yaml', help='Defaults to substitute from', type=str,
                          required=True, default=os.path.join(_top, 'ci/experment1.yaml'))

    args = parser.parse_args()
    return args

if __name__ == '__main__':

    user_inputs = input_args()
#    host = Host()
#    print( "HOST:", host.machine )

    yaml_dict = YAMLFile(path=user_inputs.yaml)
    print(yaml_dict.CDATE)