#! /bin/env python3
'''
Compares two grib2 files and print any variables that have a
  non-identity correlation.

Syntax
------
diff_grib_files.py fileA fileB

Parameters
----------
fileA: string
    Path to the first grib2 file
fileB: string
    Path to the second grib2 file

'''
import re
import sys
import subprocess

# TODO - Update to also check the min just in case the grib files have a constant offset


def count_nonid_corr(test_string: str, quiet=False):
    '''
    Scan a wgrib2 print of the correlation between two values and count
      how many variables have a non-identity correlation. Any such variables
      are printed.

    wgrib2 is assumed to be invoked by the following command:
        wgrib2 {fileA} -var -rpn 'sto_1' -import_grib {fileB} -rpn 'rcl_1:print_corr'

    Parameters
    ----------
    test_string: str
        STDOUT from wgrib2 call.

    quiet: bool, optional
        Whether to suppress print messages of non-identy variables and summary.

    Returns
    -------
    int
        Number of non-identify correlations represented in the string.


    '''
    pattern = re.compile(r"(\d+:\d+:)(?P<var>.*):rpn_corr=(?P<corr>.*)")
    matches = [m.groupdict() for m in pattern.finditer(test_string)]

    count = 0
    for match in matches:
        if float(match['corr']) != 1.0:
            count = count + 1
            if not quiet:
                print(f"{match['var']}: corr={match['corr']}")

    if not quiet:
        if count == 0:
            print("All fields are identical!")
        else:
            print(f"{count} variables are different")

    return count


if __name__ == '__main__':
    fileA = sys.argv[0]
    fileB = sys.argv[1]

    wgrib2_cmd = f"wgrib2 {fileA} -var -rpn 'sto_1' -import_grib {fileB} -rpn 'rcl_1:print_corr'"

    string = subprocess.run(wgrib2_cmd, shell=True, stdout=subprocess.PIPE).stdout.decode("utf-8")

    count_nonid_corr(string)
