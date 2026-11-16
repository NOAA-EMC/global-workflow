#! /usr/bin/env bash

set -x

###############################################################
# Source UFSDA workflow modules
source "${HOMEglobal}/dev/ush/load_modules.sh" ufsda
status=$?
if [[ ${status} -ne 0 ]]; then
    exit "${status}"
fi

export job="soilanlens"
export jobid="${job}.$$"

###############################################################
# Execute the JJOB
"${HOMEglobal}/dev/jobs/JGLOBAL_SOIL_ANALYSIS_ENKF"
status=$?
exit "${status}"
