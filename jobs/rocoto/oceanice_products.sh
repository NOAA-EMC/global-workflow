#! /usr/bin/env bash

source "${HOMEgfs}/ush/preamble.sh"

###############################################################
## ocean ice products driver script
## FHRLST : forecast hour list to post-process (e.g. f000, f000_f001_f002, ...)
###############################################################

# Source FV3GFS workflow modules
. "${HOMEgfs}/ush/load_fv3gfs_modules.sh"
status=$?
if (( status != 0 )); then exit "${status}"; fi

export job="oceanice_products"
export jobid="${job}.$$"

# shellcheck disable=SC2153,SC2001
IFS='_' read -ra fhrs <<< "${FHRLST//f}" # strip off the 'f's and convert to array
for fhr in "${fhrs[@]}"; do
    export FORECAST_HOUR=$(( 10#${fhr} ))
    "${HOMEgfs}/jobs/JGLOBAL_OCEANICE_PRODUCTS"
    status=$?
    if (( status != 0 )); then exit "${status}"; fi
done

exit $?
