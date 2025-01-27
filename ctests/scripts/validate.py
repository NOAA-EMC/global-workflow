#!/usr/bin/env python3
"""
validate.py

Validation script that checks file checksums and verifies test outputs.

Usage
-----
validate.py --yaml <path_to_yaml> --test_date <YYYYMMDDHH>

Parameters
----------
--yaml : str
    Path to the YAML configuration file.
--test_date : str
    Test date in the format YYYYMMDDHH.

"""

import sys
import argparse
from pathlib import Path
import hashlib
from wxflow import parse_j2yaml, Logger, logit, to_datetime

logger = Logger(level="DEBUG", colored_log=True)


def parse_args():
    parser = argparse.ArgumentParser()
    parser.add_argument("--yaml", required=True)
    parser.add_argument("--test_date", required=True)
    return parser.parse_args()


def file_checksum(path):
    hasher = hashlib.md5()
    with open(path, "rb") as f:
        for chunk in iter(lambda: f.read(4096), b""):
            hasher.update(chunk)
    return hasher.hexdigest()


def validate_cmpfiles(config):
    cmpfiles = config.get("output_files", {}).get("cmpfiles", [])
    for pair in cmpfiles:
        file_a, file_b = pair
        print(f"checking file: {file_b} ... ", end="")
        if file_checksum(file_a) != file_checksum(file_b):
            raise ValueError(f"Checksum mismatch: {file_a} vs {file_b}")
        print("OK")


@logit(logger)
def main():
    args = parse_args()

    data = {}
    if args.test_date:
        # Parse test date from string to datetime object
        data['TEST_DATE'] = datetime.datetime.strptime(args.test_date, '%Y%m%d%H')

    files = parse_j2yaml(path=args.yaml, data=data)
    if 'output_files' not in files:
        print(f"No output files found for test: {args.yaml}")
        print("Nothing to validate (TODO - Stubbed).")
        sys.exit(0)

    validate_cmpfiles(files)
    print(f"All files exist and pass checksum for test: {args.yaml}")


if __name__ == "__main__":
    main()
