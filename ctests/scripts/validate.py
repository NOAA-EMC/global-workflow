#!/usr/bin/env python3

import sys
import argparse
from pathlib import Path
import datetime
import hashlib
from wxflow import parse_j2yaml

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
        print(f"Comparing files: {pair} ...",end="")
        file_a, file_b = pair
        if file_checksum(file_a) != file_checksum(file_b):
            raise ValueError(f"Checksum mismatch: {file_a} vs {file_b}")
        print("OK")

if __name__ == "__main__":
    args = parse_args()

    data = {}
    if args.test_date:
        # Parse test date from string to datetime object
        data['TEST_DATE'] = datetime.datetime.strptime(args.test_date, '%Y%m%d%H')

    files = parse_j2yaml(path=args.yaml, data=data)
    validate_cmpfiles(files)

    print(f"All files exist for test: {args.test_name}")