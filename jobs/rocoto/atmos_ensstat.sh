#! /usr/bin/env bash

source "${HOMEgfs}/ush/preamble.sh"

###############################################################
## atmosphere products driver script
## FHRLST : forecast hour list to post-process (e.g. -f001, f000, f000_f001_f002, ...)
###############################################################

# Source FV3GFS workflow modules
. "${HOMEgfs}/ush/load_fv3gfs_modules.sh"
status=$?
if (( status != 0 )); then exit "${status}"; fi

export job="atmos_ensstat"
export jobid="${job}.$$"

# shellcheck disable=SC2153,SC2001
IFS='_' read -ra fhrs <<< "${FHRLST//f}" # strip off the 'f's and convert to array
for fhr in "${fhrs[@]}"; do
    # The analysis fhr is -001.  Performing math on negative, leading 0 integers is tricky.
    # The negative needs to be in front of "10#", so do some regex magic to make it happen.
    fhr="10#${fhr}"
    fhr=${fhr//10\#-/-10\#}
    export FORECAST_HOUR=$(( fhr ))
    "${HOMEgfs}/jobs/JGLOBAL_ATMOS_ENSSTAT"
    status=$?
    if (( status != 0 )); then exit "${status}"; fi
done

exit $?
