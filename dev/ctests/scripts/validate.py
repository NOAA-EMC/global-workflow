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

Environment Variables
--------------------
CTEST_VALIDATION_MODE : str, optional
    Controls validation behavior:
    - 'STRICT': All files must be present AND checksums must match
    - 'PRESENCE_ONLY' (default): All files must be present, but no checksum validation
    - 'CHECKSUM_ONLY': Only validate checksums for existing files, don't fail on missing files

"""

import sys
import os
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


def get_validation_mode():
    """
    get_validation_mode
    Gets the validation mode from the CTEST_VALIDATION_MODE environment variable.

    Returns
    -------
    str
        Validation mode: 'PRESENCE_ONLY' (default), 'STRICT', or 'CHECKSUM_ONLY'
    """
    mode = os.environ.get('CTEST_VALIDATION_MODE', 'PRESENCE_ONLY').upper()
    valid_modes = ['STRICT', 'PRESENCE_ONLY', 'CHECKSUM_ONLY']

    if mode not in valid_modes:
        logger.warning(f"Invalid CTEST_VALIDATION_MODE '{mode}', defaulting to 'PRESENCE_ONLY'")
        logger.warning(f"Valid modes are: {', '.join(valid_modes)}")
        mode = 'PRESENCE_ONLY'

    logger.info(f"Validation mode: {mode}")
    return mode


def validate_cmpfiles(config, validation_mode='PRESENCE_ONLY'):
    """
    validate_cmpfiles
    Validates that the checksums of paired files match based on validation mode.

    Parameters
    ----------
    config : dict
        Configuration dictionary containing file pairs to compare.
    validation_mode : str
        Validation mode: 'STRICT', 'PRESENCE_ONLY', or 'CHECKSUM_ONLY'

    Raises
    ------
    ValueError
        If validation fails according to the specified mode.
    FileNotFoundError
        If required files are missing in STRICT or PRESENCE_ONLY modes.
    """
    cmpfiles = config.get("output_files", {}).get("cmpfiles", [])

    if not cmpfiles:
        logger.info("No files to validate")
        return

    missing_files = []
    checksum_mismatches = []
    files_checked = 0
    files_present = 0

    logger.info(f"Validating {len(cmpfiles)} file pairs in {validation_mode} mode")

    for pair in cmpfiles:
        file_a, file_b = pair
        file_a_path = Path(file_a)
        file_b_path = Path(file_b)

        # Check file existence
        file_a_exists = file_a_path.exists()
        file_b_exists = file_b_path.exists()

        if not file_a_exists:
            missing_files.append(file_a)
        if not file_b_exists:
            missing_files.append(file_b)

        # Handle missing files based on validation mode
        if not (file_a_exists and file_b_exists):
            if validation_mode in ['STRICT', 'PRESENCE_ONLY']:
                logger.error(f"Missing files in pair: {file_a} (exists: {file_a_exists}), {file_b} (exists: {file_b_exists})")
                continue
            elif validation_mode == 'CHECKSUM_ONLY':
                logger.warning(f"Skipping missing files: {file_a} (exists: {file_a_exists}), {file_b} (exists: {file_b_exists})")
                continue

        files_present += 1

        # Skip checksum validation in PRESENCE_ONLY mode
        if validation_mode == 'PRESENCE_ONLY':
            logger.info(f"Files present (checksum skipped): {file_a} {file_b}")
            continue

        # Perform checksum validation
        try:
            checksum_a = file_checksum(file_a)
            checksum_b = file_checksum(file_b)

            if checksum_a != checksum_b:
                checksum_mismatches.append((file_a, file_b, checksum_a, checksum_b))
                logger.error(f"Checksum mismatch: {file_a} ({checksum_a}) vs {file_b} ({checksum_b})")
            else:
                logger.info(f"Checksum match: {file_a} {file_b} ({checksum_a})")
                files_checked += 1

        except Exception as e:
            logger.error(f"Error computing checksums for {file_a}, {file_b}: {e}")
            if validation_mode == 'STRICT':
                raise

    # Report results
    logger.info(f"Validation summary:")
    logger.info(f"  Total file pairs: {len(cmpfiles)}")
    logger.info(f"  File pairs present: {files_present}")
    if validation_mode != 'PRESENCE_ONLY':
        logger.info(f"  File pairs with matching checksums: {files_checked}")
    logger.info(f"  Missing files: {len(missing_files)}")
    logger.info(f"  Checksum mismatches: {len(checksum_mismatches)}")

    # Handle validation failures based on mode
    if validation_mode in ['STRICT', 'PRESENCE_ONLY'] and missing_files:
        error_msg = f"Missing {len(missing_files)} files in {validation_mode} mode: {missing_files[:5]}"
        if len(missing_files) > 5:
            error_msg += f" (and {len(missing_files) - 5} more)"
        logger.error(error_msg)
        raise FileNotFoundError(error_msg)

    if validation_mode in ['STRICT', 'CHECKSUM_ONLY'] and checksum_mismatches:
        error_msg = f"Found {len(checksum_mismatches)} checksum mismatches"
        logger.error(error_msg)
        for file_a, file_b, checksum_a, checksum_b in checksum_mismatches[:3]:
            logger.error(f"  {file_a} ({checksum_a}) != {file_b} ({checksum_b})")
        if len(checksum_mismatches) > 3:
            logger.error(f"  (and {len(checksum_mismatches) - 3} more mismatches)")
        raise ValueError(error_msg)


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

    data['STAGED_CTESTS'] = os.environ.get('STAGED_CTESTS')
    data['TEST_NAME'] = os.environ.get('TEST_NAME')
    data['RUNTESTS'] = os.environ.get('RUNTESTS')
    data['PSLOT'] = os.environ.get('PSLOT')

    files = parse_j2yaml(path=args.yaml, data=data)
    if 'output_files' not in files:
        logger.info(f"No output_files tag found for test: {args.yaml}")
        logger.info("Nothing to validate.")
        sys.exit(0)

    validation_mode = get_validation_mode()
    logger.info(f"Comparing files between these two location:\n  {data['STAGED_CTESTS']}\n  {data['RUNTESTS']}")
    validate_cmpfiles(files, validation_mode)

    # Success message based on validation mode
    if validation_mode == 'STRICT':
        logger.info(f"All files exist and pass checksum for test: {args.yaml}")
    elif validation_mode == 'PRESENCE_ONLY':
        logger.info(f"All files exist for test: {args.yaml}")
    elif validation_mode == 'CHECKSUM_ONLY':
        logger.info(f"All existing files pass checksum for test: {args.yaml}")


if __name__ == "__main__":
    main()
