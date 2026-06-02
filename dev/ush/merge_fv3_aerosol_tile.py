#!/usr/bin/env python3
"""
Appends tracer data from one NetCDF file to another and updates the tracer
count.

usage: merge_fv3_chem_tile.py [-h] atm_file chem_file core_file ctrl_file rest_file variable_file [out_file]

Appends tracer data from one NetCDF file to another and updates the tracer
count.

positional arguments:
  atm_file       File containing the atmospheric initial conditions data
  chem_file      File containing the chemistry tracer data to be added
  core_file      File containing the dycore sigma level coefficients
  ctrl_file      File containing the sigma level coefficients for atmospheric IC data
  rest_file      File containing the pressure level thickness for the restart state
  variable_file  File containing list of tracer variable_names in the chem_file
                  to add to the atm_file, one tracer per line
  out_file       Name of file to create. If none is specified, the atm_file will be
                  edited in place. New file will be a copy of atm_file with the
                  specificed tracers listed in variable_file appended from chem_file
                  and ntracers updated.

optional arguments:
  -h, --help     show this help message and exit

"""
import os
import sys
import subprocess
from typing import List
from functools import partial
from shutil import copyfile
import argparse
import numpy as np
import netCDF4

# Make sure print statements are flushed immediately, otherwise
#   print statments may be out-of-order with subprocess output
print = partial(print, flush=True)


def merge_tile(base_file_name: str, ctrl_file_name: str, core_file_name: str, rest_file_name: str, append_file_name: str, tracers_to_append: List[str]) -> None:
    if not os.path.isfile(base_file_name):
        print("FATAL ERROR: Atmosphere file " + base_file_name + " does not exist!")
        sys.exit(102)

    if not os.path.isfile(ctrl_file_name):
        print("FATAL ERROR: Atmosphere control file " + ctrl_file_name + " does not exist!")
        sys.exit(103)

    if not os.path.isfile(core_file_name):
        print("FATAL ERROR: Dycore file " + core_file_name + " does not exist!")
        sys.exit(104)

    if not os.path.isfile(rest_file_name):
        print("FATAL ERROR: Atmosphere restart file " + rest_file_name + " does not exist!")
        sys.exit(105)

    if not os.path.isfile(append_file_name):
        print("FATAL ERROR: Chemistry file " + append_file_name + " does not exist!")
        sys.exit(106)

    append_file = netCDF4.Dataset(append_file_name, "r")
    base_file = netCDF4.Dataset(base_file_name, "r+")
    core_file = netCDF4.Dataset(core_file_name, "r")
    ctrl_file = netCDF4.Dataset(ctrl_file_name, "r")
    rest_file = netCDF4.Dataset(rest_file_name, "r")

    # read pressure layer thickness from restart file
    delp = rest_file["delp"][0, :]
    # read a, b coefficients to generate sigma levels
    ak = core_file["ak"][0, :]
    bk = core_file["bk"][0, :]

    # read surface pressure from initial conditions file
    psfc = base_file["ps"][:, :]
    # read sigma-level a, b coefficients from initial conditions control file
    ai = ctrl_file["vcoord"][0, 1:]
    bi = ctrl_file["vcoord"][1, 1:]

    # IC sigma levels must match model restart sigma levels
    if ak.size != ai.size:
        print("FATAL ERROR: Inconsistent size of A(k) arrays: src=", ak.size, ", dst=", ai.size)
        sys.exit(107)

    if bk.size != bi.size:
        print("FATAL ERROR: Inconsistent size of B(k) arrays: src=", bk.size, ", dst=", bi.size)
        sys.exit(108)

    dp = np.zeros(delp.shape)
    for k in range(0, dp.shape[0]):
        dp[k, :, :] = ak[k + 1] - ak[k] + psfc * (bk[k + 1] - bk[k])

    scale_factor = delp / dp

    # print(base_file)
    # print(base_file.dimensions["ntracer"])

    old_ntracer = base_file.dimensions["ntracer"].size
    new_ntracer = old_ntracer

    print("Adding the following variables to " + base_file_name + ":\n")

    print(" Name   | Total mass (restart) | Total mass (IC)      | Max column abs. diff.")
    print("-" * 8 + "+" + "-" * 22 + "+" + "-" * 22 + "+" + "-" * 24)
    for variable_name in tracers_to_append:
        variable = append_file[variable_name]
        if variable_name not in base_file.variables.keys():
            new_ntracer = new_ntracer + 1
            base_file.createVariable(variable_name, variable.datatype, base_file["sphum"].dimensions)
        base_file[variable_name][0, :, :] = 0.
        base_file[variable_name][1:, :, :] = scale_factor * variable[0, :, :, :]
        base_file[variable_name].setncatts(variable.__dict__)
        mass_src = variable * delp
        mass_dst = base_file[variable_name][1:, :, :] * dp
        mass_err_max = np.max(np.abs(mass_src - mass_dst))
        total_mass_src = np.sum(mass_src)
        total_mass_dst = np.sum(mass_dst)
        print(f' {variable_name:6}   {total_mass_src:20}   {total_mass_dst:20}    {mass_err_max:22}')
        # print("Done adding " + variable_name)

    print("-" * 79 + "\n")

    base_file.close()

    if new_ntracer != old_ntracer:
        print("Updating ntracer")

        # Use ncks to rewrite file without ntracer so we can define it anew
        subprocess.run(["ncks", "-x", "-v", "ntracer", "-O", base_file_name, base_file_name], check=True)

        base_file = netCDF4.Dataset(base_file_name, "r+")
        base_file.createDimension("ntracer", new_ntracer)
        # print(base_file.dimensions["ntracer"])
        base_file.close()

    # Remove checksum
    subprocess.run(["ncatted", "-a", "checksum,,d,,", base_file_name], check=True)


def main() -> None:
    parser = argparse.ArgumentParser(
        description="Appends tracer data from one NetCDF file to another and updates the tracer count.")
    parser.add_argument('atm_file', type=str, help="File containing the atmospheric initial conditions data")
    parser.add_argument('chem_file', type=str, help="File containing the chemistry tracer data to be added")
    parser.add_argument('core_file', type=str, help="File containing the dycore sigma level coefficients")
    parser.add_argument('ctrl_file', type=str, help="File containing the sigma level coefficients for atmospheric IC data")
    parser.add_argument('rest_file', type=str, help="File containing the pressure level thickness for the restart state")
    parser.add_argument('variable_file', type=str, help="File with list of tracer variable_names in the chem_file to add to the atm_file, one tracer per line")
    parser.add_argument('out_file', type=str, nargs="?", help="Name of file to create")

    args = parser.parse_args()

    atm_file_name = args.atm_file
    chem_file_name = args.chem_file
    core_file_name = args.core_file
    ctrl_file_name = args.ctrl_file
    rest_file_name = args.rest_file
    variable_file = args.variable_file
    out_file_name = args.out_file

    if out_file_name is None:
        print("INFO: No out_file specified, will edit atm_file in-place")
        out_file_name = atm_file_name
    else:
        if os.path.isfile(out_file_name):
            print("WARNING: Specified out file " + out_file_name + " exists and will be overwritten")
        copyfile(atm_file_name, out_file_name)

    variable_file = open(variable_file)
    variable_names = variable_file.read().splitlines()
    variable_file.close()

    merge_tile(out_file_name, ctrl_file_name, core_file_name, rest_file_name, chem_file_name, variable_names)

    # print(variable_names)


if __name__ == "__main__":
    main()
