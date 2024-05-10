#!/usr/bin/env python3

"""
This script parses a yaml file and returns the value of a specified key.
"""

import os
import sys
from wxflow import AttrDict, parse_j2yaml
from argparse import ArgumentParser
from pathlib import Path

_here = os.path.dirname(__file__)
_top = os.path.abspath(os.path.join(os.path.abspath(_here), '../../..'))

description = """parse yaml file and return value of key"""


def parse_args():
    """
    Parse command-line arguments.

    Returns:
        argparse.Namespace: The parsed command-line arguments.
    """

    parser = ArgumentParser(description=description)
    parser.add_argument('-y', '--yaml', help='full path to yaml file to parce', type=Path, required=True)
    parser.add_argument('-k', '--key', help='key to return value of', type=str, required=True)
    parser.add_argument('-s', '--string', help='output results as stings', action="store_true", required=False)
    return parser.parse_args()


def yq(yamlfile, key):
    """
    Parse a yaml file and return the value of a specified key.

    Args:
        yamlfile (Path): The path to the yaml file.
        key (str): The key to return the value of.

    Returns:
        The value of the specified key in the yaml file.
    """

    data = AttrDict(HOMEgfs=_top)
    data.update({'HOMEgfs': _top})
    ydict = parse_j2yaml(path=yamlfile, data=data)
    if key == 'all':
        return ydict
    list_keys = key.split('.')
    for k in list_keys:
        ydict = ydict.get(k, None)
        if ydict is None:
            break
    return ydict


if __name__ == '__main__':
    """
    Main function. Parses command-line arguments and prints the value of the specified key in the specified yaml file.
    """

    args = parse_args()
    values = yq(args.yaml, args.key)
    if args.string and isinstance(values, list):
        for value in values:
            print(value)
    else:
        print(values)
