#! /usr/bin/env python3

"""
"""

import os
import subprocess
import typing
from datetime import datetime, timedelta
from functools import partial

# Constants
atm_base_pattern = "{ics_dir}/%Y%m%d%H/atmos/{case}/INPUT"                    # Location of atmosphere ICs
atm_file_pattern = "{path}/gfs_data.{tile}.nc"                                # Atm IC file names
tracer_base_pattern = "{rot_dir}/{cdump}.%Y%m%d/%H/atmos/RERUN_RESTART"        # Time of previous run
tracer_file_pattern = "{tracer_base}/%Y%m%d.%H0000.fv_tracer.res.{tile}.nc"   # Time when restart is valid (current run)
tracer_list_file_pattern = "{parm_gfs}/chem/gocart_tracer.list"               # Text list of tracer names to copy
merge_script_pattern = "{ush_gfs}/merge_fv3_chem_tile.py"
n_tiles = 6
max_lookback = 4                                                              # Maximum number of past cycles to look for for tracer data
debug = True

# End configurable settings

# Make sure print statements are flushed immediately, otherwise
#   print statments may be out-of-order with subprocess output
print = partial(print, flush=True)

tiles = list(map(lambda t: "tile{t}".format(t=t), range(1, n_tiles + 1)))


def main() -> None:
	# Read in environment variables and make sure they exist
	cdate = get_env_var("CDATE")
	incr = int(get_env_var('FHCYC'))
	cdump = get_env_var("CDUMP")
	rot_dir = get_env_var("ROTDIR")
	ics_dir = get_env_var("ICSDIR")
	case = get_env_var("CASE")
	ush_gfs = get_env_var("USHgfs")
	parm_gfs = get_env_var("PARMgfs")
	# data = get_env_var('DATA')

	# os.chdir(data)

	merge_script = merge_script_pattern.format(ush_gfs=ush_gfs)
	tracer_list_file = tracer_list_file_pattern.format(parm_gfs=parm_gfs)

	time = datetime.strptime(cdate, "%Y%m%d%H")
	atm_source_path = time.strftime(atm_base_pattern.format(**locals()))

	if(debug):
		for var in ['merge_script', 'tracer_list_file', 'atm_source_path']:
			print(f'{var} = {f"{var}"}')

	atm_files = get_atm_files(atm_source_path)
	tracer_files = get_tracer_files(time, incr, max_lookback, rot_dir, cdump)

	if (tracer_files is not None):
		merge_tracers(merge_script, atm_files, tracer_files, tracer_list_file)

	return


# Retrieve environment variable and exit or print warning if not defined
def get_env_var(varname: str, fail_on_missing: bool = True) -> str:
	if(debug):
		print(f'Trying to read envvar {varname}')

	var = os.environ.get(varname)
	if(var is None):
		if(fail_on_missing is True):
			print("FATAL: Environment variable {varname} not set".format(varname=varname))
			exit(100)
		else:
			print("WARNING: Environment variable {varname} not set, continuing using None".format(varname=varname))
	if(debug):
		print(f'\tValue: {var}')
	return(var)


# Check if atm files exist
def get_atm_files(path: str) -> typing.List[str]:
	print(f'Checking for atm files in {path}')

	files = list(map(lambda tile: atm_file_pattern.format(tile=tile, path=path), tiles))
	for file_name in files:
		if(debug):
			print(f"\tChecking for {file_name}")
		if(not os.path.isfile(file_name)):
			print("FATAL: Atmosphere file {file_name} not found".format(file_name=file_name))
			exit(101)
		elif(debug):
			print(f"\t\tFound {file_name}")
	return files


# Find last cycle with tracer data available via restart files
def get_tracer_files(time: datetime, incr: int, max_lookback: int, rot_dir: str, cdump: str) -> typing.List[str]:
	print(f"Looking for tracer files in {rot_dir}")
	for lookback in map(lambda i: incr * (i + 1), range(max_lookback)):
		last_time = time - timedelta(hours=lookback)
		if(debug):
			print(f"\tChecking {last_time}")
		tracer_base = last_time.strftime(tracer_base_pattern.format(**locals()))
		files = list(map(lambda tile: time.strftime(tracer_file_pattern.format(tracer_base=tracer_base, tile=tile)), tiles))
		if(debug):
			print("\t\tLooking for files {files} in directory {tracer_base}")
		found = [file for file in files if os.path.isfile(file)]
		if(found):
			if(debug):
				print(f"\t\tAll files found!")
			return files
		else:
			print(last_time.strftime("Tracer files not found for %Y%m%d_%H"))

	if(not found):
		print("WARNING: Unable to find tracer files, will use zero fields")
		return None


# Merge tracer data into atmospheric data
def merge_tracers(merge_script: str, atm_files: typing.List[str], tracer_files: typing.List[str], tracer_list_file: str) -> None:
	print(f"Merging tracers")
	if(len(atm_files) != len(tracer_files)):
		print("FATAL: atmosphere file list and tracer file list are not the same length")
		exit(102)

	for atm_file, tracer_file in zip(atm_files, tracer_files):
		if(debug):
			print(f"\tMerging tracers from {tracer_file} into {atm_file}")
		subprocess.run([merge_script, atm_file, tracer_file, tracer_list_file], check=True)


if __name__ == "__main__":
	main()
	exit(0)
