#!/usr/bin/env bash
# process_atmos_6hrly.sh: A wrapper to handle the pipe for Slurm MPM to generate 6hourly files for each variable
set -e

# Arguments: $1=variable_string, $2=output_filename
var_string="$1"
output_file="$2"

# Note: COMIN_ATMOS_MASTER must be exported from the master script
cat $(eval ls -v ${COMIN_ATMOS_MASTER}/*) | ${WGRIB2} - -match "${var_string}" -grib "${output_file}"

