#!/usr/bin/env python3
"""
convert_jobs.py
Example script showing how to use variable_name_converter to convert
all J-job scripts in dev/jobs between HOMEglobal-style and HOME${NET}-style.

Usage:
    # Convert HOMEglobal -> HOMEglobal in dev/jobs
    python3 convert_jobs.py to-net gfs

    # Convert HOMEglobal -> HOMEglobal in dev/jobs
    python3 convert_jobs.py to-global gfs

    # Revert all NET values at once
    python3 convert_jobs.py to-global all
"""

import argparse
import sys
from pathlib import Path

from variable_name_converter import (
    GlobalToNetConverter,
    NetToGlobalConverter,
    VALID_NET_VALUES,
)

REPO_ROOT = Path(__file__).resolve().parents[2]
JOBS_DIR = REPO_ROOT / 'dev' / 'jobs'


def main() -> None:
    parser = argparse.ArgumentParser(
        description='Convert variable names in all dev/jobs J-job scripts.'
    )
    subparsers = parser.add_subparsers(dest='direction', required=True)

    to_net = subparsers.add_parser(
        'to-net',
        help='Convert HOMEglobal etc. to HOME${NET} etc.'
    )
    to_net.add_argument('net', metavar='NET_value',
                        help=f'One of: {", ".join(VALID_NET_VALUES)}')

    to_global = subparsers.add_parser(
        'to-global',
        help='Convert HOME${NET} etc. back to HOMEglobal etc.'
    )
    to_global.add_argument('net', metavar='NET_value',
                           help=f'One of: {", ".join(VALID_NET_VALUES)}, or all')

    args = parser.parse_args()

    if args.direction == 'to-net':
        converter = GlobalToNetConverter(verbose=True)
    else:
        converter = NetToGlobalConverter(verbose=True)

    try:
        result = converter.convert(JOBS_DIR, args.net)
    except (ValueError, FileNotFoundError) as exc:
        print(f"ERROR: {exc}", file=sys.stderr)
        sys.exit(1)

    if not result.success:
        sys.exit(1)


if __name__ == '__main__':
    main()
