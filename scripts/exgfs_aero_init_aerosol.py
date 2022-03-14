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
atm_ctrl_pattern = "{path}/gfs_ctrl.nc"                                       # Atm IC control file name
restrt_base_pattern = "{rot_dir}/{cdump}.%Y%m%d/%H/atmos/RERUN_RESTART"       # Location of restart files (time of previous run)
restrt_file_pattern = "{file_base}/{timestamp}fv_core.res.{tile}.nc"          # Name of restart data files (time when restart is valid)
tracer_file_pattern = "{file_base}/{timestamp}fv_tracer.res.{tile}.nc"        # Name of restart tracer files (time when restart is valid)
dycore_file_pattern = "{file_base}/{timestamp}fv_core.res.nc"                 # Name of restart dycore file (time when restart is valid)
tracer_list_file_pattern = "{parm_gfs}/chem/gocart_tracer.list"               # Text list of tracer names to copy
merge_script_pattern = "{ush_gfs}/merge_fv3_aerosol_tile.py"
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
	incr = int(get_env_var('STEP_GFS'))
	fcst_length = int(get_env_var('FHMAX_GFS'))
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

	atm_files, ctrl_files = get_atm_files(atm_source_path)
	tracer_files, rest_files, core_files = get_restart_files(time, incr, max_lookback, fcst_length, rot_dir, cdump)

	if (tracer_files is not None):
		merge_tracers(merge_script, atm_files, tracer_files, rest_files, core_files[0], ctrl_files[0], tracer_list_file)

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
def get_atm_files(path: str) -> typing.List[typing.List[str]]:
	print(f'Checking for atm files in {path}')

	file_list = []
	for file_pattern in atm_file_pattern, atm_ctrl_pattern:
		files = list(map(lambda tile: file_pattern.format(tile=tile, path=path), tiles))
		for file_name in files:
			if(debug):
				print(f"\tChecking for {file_name}")
			if(not os.path.isfile(file_name)):
				print("FATAL: Atmosphere file {file_name} not found".format(file_name=file_name))
				exit(101)
			elif(debug):
				print(f"\t\tFound {file_name}")
		file_list = file_list + [files]
	return file_list


# Find last cycle with restart files available
def get_restart_files(time: datetime, incr: int, max_lookback: int, fcst_length: int, rot_dir: str, cdump: str) -> typing.List[typing.List[str]]:
	print(f"Looking for restart tracer files in {rot_dir}")
	for lookback in map(lambda i: incr * (i + 1), range(max_lookback)):
		if(lookback > fcst_length):
			# Trying to look back farther than the length of a forecast
			break
		elif(lookback == fcst_length):
			# Restart files at the end of the cycle don't have a timestamp
			timestamp = ""
		else:
			timestamp = time.strftime("%Y%m%d.%H0000.")

		last_time = time - timedelta(hours=lookback)

		if(debug):
			print(f"\tChecking {last_time}")
		file_list = []
		file_base = last_time.strftime(restrt_base_pattern.format(**locals()))
		for file_pattern in tracer_file_pattern, restrt_file_pattern, dycore_file_pattern:
			files = list(map(lambda tile: file_pattern.format(timestamp=timestamp, file_base=file_base, tile=tile), tiles))
			if(debug):
				print(f"\t\tLooking for files {files} in directory {file_base}")
			found = [file for file in files if os.path.isfile(file)]
			if(found):
				file_list = file_list + [files]
			else:
				print(last_time.strftime("Restart files not found for %Y%m%d_%H"))
				break

		if(found):
			return file_list
		else:
			print("WARNING: Unable to find restart files, will use zero fields")
			return None


# Merge tracer data into atmospheric data
def merge_tracers(merge_script: str, atm_files: typing.List[str], tracer_files: typing.List[str], rest_files: typing.List[str], core_file: str, ctrl_file: str, tracer_list_file: str) -> None:
	print(f"Merging tracers")
	if(len(atm_files) != len(tracer_files)):
		print("FATAL: atmosphere file list and tracer file list are not the same length")
		exit(102)

	for atm_file, tracer_file, rest_file in zip(atm_files, tracer_files, rest_files):
		if(debug):
			print(f"\tMerging tracers from {tracer_file} into {atm_file}")
		subprocess.run([merge_script, atm_file, tracer_file, core_file, ctrl_file, rest_file, tracer_list_file], check=True)


if __name__ == "__main__":
	main()
	exit(0)
