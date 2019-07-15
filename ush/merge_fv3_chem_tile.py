#!/usr/bin/env python3
#
# Appends tracer data from one NetCDF file to another and updates the tracer
# count.
#
# usage: merge_fv3_chem_tile.py [-h] atm_file chem_file variable_file [out_file]
#
# Appends tracer data from one NetCDF file to another and updates the tracer
# count.
#
# positional arguments:
#   atm_file       File containing the atmospheric data
#   chem_file      File containing the chemistry tracer data to be added
#   variable_file  File containing list of tracer variable_names in the
#                  chem_file to add to the atm_file, one tracer per line
#   out_file       Name of file to create. If none is specified, the atm_file
#                  will be edited in place. New file will be a copy of atm_file
#                  with the specificed tracers listed in variable_file appended
#                  from chem_file and ntracers updated.

# optional arguments:
#   -h, --help     show this help message and exit
#

import os, sys, subprocess
from typing import List
from functools import partial
from shutil import copyfile
import argparse

try: 
	import netCDF4
except:
	print("FATAL ERROR: Failed to import netCDF4 in " + __file__ + "!")
	print("Make sure you are using a version of python 3 with netCDF4 installed")
	sys.exit(-100)

# Make sure print statements are flushed immediately, otherwise
#   print statments may be out-of-order with subprocess output
print = partial(print, flush=True)

def merge_tile(base_file_name: str, append_file_name: str, tracers_to_append: List[str]) -> None:
	if not os.path.isfile(base_file_name):
		print("FATAL ERROR: Atmosphere file " + base_file_name + " does not exist!")
		sys.exit(-102)

	if not os.path.isfile(append_file_name):
		print("FATAL ERROR: Chemistry file " + append_file_name + " does not exist!")
		sys.exit(-103)

	append_file = netCDF4.Dataset(append_file_name, "r")
	base_file = netCDF4.Dataset(base_file_name, "r+")

	# print(base_file)
	# print(base_file.dimensions["ntracer"])

	old_ntracer = base_file.dimensions["ntracer"].size
	new_ntracer = old_ntracer + len(tracers_to_append)

	# Copy over chemistry dimensions
	for dim_name in append_file.dimensions:
		base_file.createDimension(dim_name, append_file.dimensions[dim_name].size)

	for variable_name in tracers_to_append:
		print("Adding variable " + variable_name + " to file")
		variable = append_file[variable_name]
		base_file.createVariable(variable_name, variable.datatype, variable.dimensions)
		base_file[variable_name][:] = variable[:]
		base_file[variable_name].setncatts(variable.__dict__)
		# print("Done adding " + variable_name)

	print("Updating ntracer")

	# Use ncks to rewrite file without ntracer so we can define it anew
	subprocess.run(["ncks", "-x", "-v", "ntracer", "-O", base_file_name, base_file_name])

	base_file = netCDF4.Dataset(base_file_name, "r+")
	base_file.createDimension("ntracer", new_ntracer)
	# print(base_file.dimensions["ntracer"])

def main() -> None:
	parser = argparse.ArgumentParser(
		description="Appends tracer data from one NetCDF file to another and updates the tracer count.")
	parser.add_argument('atm_file', type=str, help="File containing the atmospheric data")
	parser.add_argument('chem_file', type=str, help="File containing the chemistry tracer data to be added")
	parser.add_argument('variable_file', type=str, help="File containing list of tracer variable_names in the chem_file to add to the atm_file, one tracer per line")
	parser.add_argument('out_file', type=str, nargs="?", help="Name of file to create. If none is specified, the atm_file will be edited in place. New file will be a copy of atm_file with the specificed tracers listed in variable_file appended from chem_file and ntracers updated.")

	args = parser.parse_args()

	atm_file_name = args.atm_file
	chem_file_name = args.chem_file
	variable_file = args.variable_file
	out_file_name = args.out_file

	if out_file_name == None:
		print("INFO: No out_file specified, will edit atm_file in-place")
		out_file_name = atm_file_name
	else:
		if os.path.isfile(out_file_name):
			print("WARNING: Specified out file " + out_file_name + " exists and will be overwritten")
		copyfile(atm_file_name, out_file_name)

	variable_file = open(variable_file)
	variable_names = variable_file.read().splitlines()
	variable_file.close()

	merge_tile(out_file_name, chem_file_name, variable_names)

	# print(variable_names)

if __name__ == "__main__":
	main()