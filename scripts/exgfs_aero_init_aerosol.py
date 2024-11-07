#! /usr/bin/env python3

'''
'script'-level control of the aerosol init job.

Reads environment variables, determines the atmospheric IC files and most recent available
restart files, then calls the script that merges the tracers from the restart files into
the IC files.

INPUTS
---------
This script requires the following environment variables be set beforehand:

CDATE:        Initial time in YYYYMMDDHH format
INTERVAL_GFS: Forecast cadence (frequency) in hours
FHMAX_GFS:    Forecast length in hours
RUN:          Forecast phase (gfs or gdas). Currently always expected to be gfs.
ROTDIR:       Rotating (COM) directory
USHgfs:       Path to global-workflow `ush` directory
PARMgfs:      Path to global-workflow `parm` directory

Additionally, the following data files are used:

- Tiled atmospheric initial conditions that follow the naming pattern determined by `atm_base_pattern` and `atm_file_pattern`
- Restart files from a previous cycle that fit the pattern determined by restart_base_pattern and restart_file_pattern,
  tracer_file_pattern, and dycore_file_pattern
- A static file containing a list of tracers from the restart files to be added to the IC files, determined by
  `tracer_list_file_pattern`

OUTPUTS
---------
The tiled atmospheric intial condition files will be updated with conservation-adjusted tracer fields from the restart files.


'''

import os
import subprocess
import typing
from datetime import datetime, timedelta
from functools import partial

# Constants
atm_base_pattern = "{rot_dir}/{run}.%Y%m%d/%H/model/atmos/input"         # Location of atmosphere ICs
atm_file_pattern = "{path}/gfs_data.{tile}.nc"                                # Atm IC file names
atm_ctrl_pattern = "{path}/gfs_ctrl.nc"                                       # Atm IC control file name
restart_base_pattern = "{rot_dir}/{run}.%Y%m%d/%H/model/atmos/restart"   # Location of restart files (time of previous run)
restart_file_pattern = "{file_base}/{timestamp}fv_core.res.{tile}.nc"         # Name of restart data files (time when restart is valid)
tracer_file_pattern = "{file_base}/{timestamp}fv_tracer.res.{tile}.nc"        # Name of restart tracer files (time when restart is valid)
dycore_file_pattern = "{file_base}/{timestamp}fv_core.res.nc"                 # Name of restart dycore file (time when restart is valid)
tracer_list_file_pattern = "{parm_gfs}/ufs/gocart/gocart_tracer.list"         # Text list of tracer names to copy
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
    incr = int(get_env_var('INTERVAL_GFS'))
    fcst_length = int(get_env_var('FHMAX_GFS'))
    run = get_env_var("RUN")
    rot_dir = get_env_var("ROTDIR")
    ush_gfs = get_env_var("USHgfs")
    parm_gfs = get_env_var("PARMgfs")

    # os.chdir(data)

    merge_script = merge_script_pattern.format(ush_gfs=ush_gfs)
    tracer_list_file = tracer_list_file_pattern.format(parm_gfs=parm_gfs)

    time = datetime.strptime(cdate, "%Y%m%d%H")
    atm_source_path = time.strftime(atm_base_pattern.format(**locals()))

    if (debug):
        for var in ['merge_script', 'tracer_list_file', 'atm_source_path']:
            print(f'{var} = {f"{var}"}')

    atm_files, ctrl_files = get_atm_files(atm_source_path)
    tracer_files, rest_files, core_files = get_restart_files(time, incr, max_lookback, fcst_length, rot_dir, run)

    if (tracer_files is not None):
        merge_tracers(merge_script, atm_files, tracer_files, rest_files, core_files[0], ctrl_files[0], tracer_list_file)

    return


def get_env_var(varname: str, fail_on_missing: bool = True) -> str:
    '''
    Retrieve environment variable and exit or print warning if not defined

    Parameters
    ----------
    varname : str
            Environment variable to read
    fail_on_missing : bool, optional
            Whether to fail (if True) or print warning (False) if environment variable is not defined (default: True)

    Returns
    ----------
    str
            Value of the named variable

    Raises
    ----------
    RuntimeError
            If fail_on_missing is True and environment variable is not defined

    '''
    if (debug):
        print(f'Trying to read envvar {varname}')

    var = os.environ.get(varname)
    if (var is None):
        if (fail_on_missing is True):
            raise RuntimeError(f'Environment variable {varname} not set')
        else:
            print(f"WARNING: Environment variable {varname} not set, continuing using None")
    if (debug):
        print(f'\tValue: {var}')
    return (var)


def get_atm_files(path: str) -> typing.List[typing.List[str]]:
    '''
    Checks whether all atmospheric IC files exist in the given location and returns a list
    of the filenames.

    Parameters
    ----------
    path : str
            Location where atmospheric IC files should exist

    Returns
    ----------
    list of str
            List of the full paths to each of the atmospheric files

    Raises
    ----------
    IOError
            If fail_on_missing is True and environment variable is not defined

    '''
    print(f'Checking for atm files in {path}')

    file_list = []
    for file_pattern in atm_file_pattern, atm_ctrl_pattern:
        files = list(map(lambda tile: file_pattern.format(tile=tile, path=path), tiles))
        for file_name in files:
            if (debug):
                print(f"\tChecking for {file_name}")
            if (not os.path.isfile(file_name)):
                raise IOError(f"Atmosphere file {file_name} not found")
            elif (debug):
                print(f"\t\tFound {file_name}")
        file_list = file_list + [files]
    return file_list


def get_restart_files(time: datetime, incr: int, max_lookback: int, fcst_length: int, rot_dir: str, run: str) -> typing.List[typing.List[str]]:
    '''
    Determines the last cycle where all the necessary restart files are available. Ideally the immediate previous cycle

    Parameters
    ----------
    time : datetime
            Initial time for the current forecast
    incr : int
            Forecast cadence in hours
    max_lookback : int
            Maximum number of cycles to look back before failing
    fcst_length : int
            Length of forecast in hours
    rot_dir : str
            Path to the ROTDIR (COM) directory
    run : str
            RUN of current forecast portion (currently should always be 'gfs')

    Returns
    ----------
    list of str
            Full pathnames of all restart files needed from previous cycle (fv_core and fv_tracer files)
            If all needed files aren't found within lookback period, An array of three None is returned instead.

    '''
    print(f"Looking for restart tracer files in {rot_dir}")
    for lookback in map(lambda i: incr * (i + 1), range(max_lookback)):
        if (lookback > fcst_length):
            # Trying to look back farther than the length of a forecast
            break
        elif (lookback == fcst_length):
            # Restart files at the end of the cycle don't have a timestamp
            timestamp = ""
        else:
            timestamp = time.strftime("%Y%m%d.%H0000.")

        last_time = time - timedelta(hours=lookback)

        if (debug):
            print(f"\tChecking {last_time}")
        file_list = []
        file_base = last_time.strftime(restart_base_pattern.format(**locals()))

        for file_pattern in tracer_file_pattern, restart_file_pattern, dycore_file_pattern:
            files = list(map(lambda tile: file_pattern.format(timestamp=timestamp, file_base=file_base, tile=tile), tiles))
            if (debug):
                print(f"\t\tLooking for files {files} in directory {file_base}")
            file_list = file_list + [files]

        found = all([os.path.isfile(file) for file in files for files in file_list])

        if (found):
            break
        else:
            print(last_time.strftime("Restart files not found for %Y%m%d_%H"))

    if (found):
        return file_list
    else:
        print("WARNING: Unable to find restart files, will use zero fields")
        return [None, None, None]


# Merge tracer data into atmospheric data
def merge_tracers(merge_script: str,
                  atm_files: typing.List[str],
                  tracer_files: typing.List[str],
                  rest_files: typing.List[str],
                  core_file: str,
                  ctrl_file: str,
                  tracer_list_file: str) -> None:
    '''
    Call the merger script to merge the tracers into the atmospheric IC files. Merged file is written to a temp file
    which then overwrites the original upon successful completion of the script.

    Parameters
    ----------
    merge_script : str
            Full path to the merge script
    atm_files : list of str
            List of paths to atmospheric IC files
    tracer_files : list of str
            List of paths to tracer restart files
    rest_files : list of str
            List of paths to dycore tile restart files
    core_file : str
            Path of dycore restart file
    ctrl_file : str
            Path of control file
    tracer_list_file : str
            Full path to the file listing the tracer variables to add

    Returns
    ----------
    None

    Raises
    ----------
    ValueError
            If `atm_files`, `tracer_files`, and `rest_files` are not all the same length
    CalledProcessError
            If merge script exits with a non-zero error

    '''
    print("Merging tracers")
    if (len(atm_files) != len(tracer_files)):
        raise ValueError("Atmosphere file list and tracer file list are not the same length")

    if (len(atm_files) != len(rest_files)):
        raise ValueError("Atmosphere file list and dycore file list are not the same length")

    for atm_file, tracer_file, rest_file in zip(atm_files, tracer_files, rest_files):
        if debug:
            print(f"\tMerging tracers from {tracer_file} into {atm_file}")
        temp_file = f'{atm_file}.tmp'
        subprocess.run([merge_script, atm_file, tracer_file, core_file, ctrl_file, rest_file, tracer_list_file, temp_file], check=True)
        os.replace(temp_file, atm_file)


if __name__ == "__main__":
    main()
    exit(0)
