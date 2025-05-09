#!/usr/bin/env python3

"""
This script parses a yaml file and returns the value of a specified key.
"""

import os
import sys
from wxflow import AttrDict, parse_j2yaml, find_upward
from argparse import ArgumentParser
from pathlib import Path

description = """parse yaml file and return value of key"""


def parse_args():
    """
    Parse command-line arguments.

    Returns:
        argparse.Namespace: The parsed command-line arguments.
    """

    parser = ArgumentParser(description=description)
    parser.add_argument('-y', '--yaml', help='full path to yaml file to parse', type=Path, required=True)
    parser.add_argument('-k', '--key', help='key to return value of', type=str, required=True)
    parser.add_argument('-s', '--string', help='output results as strings', action="store_true", required=False)
    parser.add_argument('-d', '--default', help='default value to return if key is not found', type=str, required=False)
    parser.add_argument('-f', '--fail-on-missing', help='exit with code 1 if key is not found', action="store_true", required=False)
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

    HOMEgfs = find_upward('.github')
    ydict = parse_j2yaml(path=yamlfile, data={'HOMEgfs': HOMEgfs})
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

    # Handle missing values
    if values is None:
        if hasattr(args, 'fail_on_missing') and args.fail_on_missing:
            print(f"Error: Key '{args.key}' not found in {args.yaml}", file=sys.stderr)
            sys.exit(1)
        elif hasattr(args, 'default') and args.default is not None:
            values = args.default
        else:
            # For shell script usage, an empty output is often more useful than "None"
            sys.exit(0)

    # Output formatting
    if args.string and isinstance(values, list):
        for value in values:
            print(value)
    else:
        print(values)
