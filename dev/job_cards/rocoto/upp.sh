#! /usr/bin/env bash

set -x

###############################################################
## Offline UPP driver script
## UPP_RUN: analysis, forecast, goes.  See upp.yaml for valid options
## FHRLST : forecast hourlist to be post-process (e.g. f000, f000_f001_f002, ...)
###############################################################

# Source FV3GFS workflow modules
#source "${HOMEgfs}/dev/ush/load_modules.sh" run
#status=$?
#if (( status != 0 )); then exit "${status}"; fi
# Temporarily load modules from UPP on WCOSS2
source "${HOMEgfs}/ush/detect_machine.sh"
source "${HOMEgfs}/dev/ush/load_modules.sh" upp
status=$?
if [[ ${status} -ne 0 ]]; then
    exit "${status}"
fi

export job="upp"
export jobid="${job}.$$"

export FORECAST_HOUR=$((10#${FHR3}))

###############################################################
# Execute the JJOB
###############################################################
"${HOMEgfs}/dev/jobs/JGLOBAL_ATMOS_UPP"

exit $?
