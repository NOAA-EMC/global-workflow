#!/usr/bin/env bash
# process_atmos_6hrly.sh: A wrapper to handle the pipe for Slurm MPM to generate 6hourly files for each variable
set -e

# Arguments: $1=variable_string, $2=output_filename
var_string="$1"
output_file="$2"

# Note: COMIN_ATMOS_MASTER must be exported from the master script
# Safely stream the files into cat, then into wgrib2
find "${COMIN_ATMOS_MASTER}" -maxdepth 1 -type f -print0 | sort -zV | xargs -0 cat | ${WGRIB2} - -match "${var_string}" -grib "${output_file}"
