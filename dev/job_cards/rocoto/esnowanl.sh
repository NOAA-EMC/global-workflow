#! /usr/bin/env bash

###############################################################
# Source UFSDA workflow modules
source "${HOMEglobal}/dev/ush/load_modules.sh" ufsda
status=$?
if [[ ${status} -ne 0 ]]; then
    exit "${status}"
fi

export job="esnowanl"
export jobid="${job}.$$"

###############################################################
# Execute the JJOB
"${HOMEglobal}/dev/jobs/JGLOBAL_SNOW_ANAL_ENS"
status=$?
exit "${status}"
