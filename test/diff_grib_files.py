#! /bin/env python3

import re
import sys
import subprocess

def grib_corr(test_string: str):
    pattern = re.compile(r"(\d+:\d+:)(?P<var>.*):rpn_corr=(?P<corr>.*)")
    matches = [m.groupdict() for m in pattern.finditer(test_string)]

    count = 0
    for match in matches:
        if float(match['corr']) != 1.0:
            count = count + 1
            print(f"{match['var']}: corr={match['corr']}")

    if count == 0:
        print("All fields are identical!")
    else:
        print(f"{count} variables are different")

if __name__ == '__main__':
    fileA = sys.argv[0]
    fileB = sys.argv[1]

    string = subprocess.run(f"wgrib2 {fileA} -var -rpn 'sto_1' -import_grib {fileB} -rpn 'rcl_1:print_corr'", shell=True, stdout=subprocess.PIPE).stdout.decode("utf-8")
    grib_corr(string)
