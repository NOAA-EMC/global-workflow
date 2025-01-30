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
    """
    parse_args
    Parses command line arguments.

    Returns
    -------
    argparse.Namespace
        Parsed command line arguments.
    """
    parser = argparse.ArgumentParser()
    parser.add_argument("--yaml", required=True)
    parser.add_argument("--test_date", required=True)
    return parser.parse_args()


def file_checksum(path):
    """
    file_checksum
    Computes the MD5 checksum of a file.

    Parameters
    ----------
    path : str
        Path to the file.

    Returns
    -------
    str
        MD5 checksum of the file.
    """
    hasher = hashlib.md5()
    with open(path, "rb") as f:
        for chunk in iter(lambda: f.read(4096), b""):
            hasher.update(chunk)
    return hasher.hexdigest()


def validate_cmpfiles(config):
    """
    validate_cmpfiles
    Validates that the checksums of paired files match.

    Parameters
    ----------
    config : dict
        Configuration dictionary containing file pairs to compare.

    Raises
    ------
    ValueError
        If the checksums of any paired files do not match.
    """
    cmpfiles = config.get("output_files", {}).get("cmpfiles", [])
    for pair in cmpfiles:
        file_a, file_b = pair
        if file_checksum(file_a) != file_checksum(file_b):
            logger.error(f"Checksum mismatch: {file_a} vs {file_b}")
            raise ValueError(f"Checksum mismatch: {file_a} vs {file_b}")
        logger.info(f"checksums match: {file_a} vs {file_b}")


@logit(logger)
def main():
    """
    main
    Main function that parses arguments, reads configuration, and validates file checksums.

    Raises
    ------
    SystemExit
        If no output files are found in the configuration.
    """
    args = parse_args()

    data = {}
    if args.test_date:
        # Parse test date from string to datetime object
        data['TEST_DATE'] = to_datetime(args.test_date)

    files = parse_j2yaml(path=args.yaml, data=data)
    if 'output_files' not in files:
        logger.info(f"No output files found for test: {args.yaml}")
        logger.info("Nothing to validate (TODO - Stubbed).")
        sys.exit(0)

    validate_cmpfiles(files)
    logger.info(f"All files exist and pass checksum for test: {args.yaml}")


if __name__ == "__main__":
    main()
