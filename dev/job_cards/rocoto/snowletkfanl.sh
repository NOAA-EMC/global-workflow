#! /usr/bin/env bash

###############################################################
# Source UFSDA workflow modules
source "${HOMEglobal}/dev/ush/load_modules.sh" ufsda
status=$?
if [[ ${status} -ne 0 ]]; then
    exit "${status}"
fi

export job="snowletkfanl"
export jobid="${job}.$$"

###############################################################
# Execute the JJOB
"${HOMEglobal}/dev/jobs/JGLOBAL_SNOW_LETKF_ANAL"
status=$?
exit "${status}"
