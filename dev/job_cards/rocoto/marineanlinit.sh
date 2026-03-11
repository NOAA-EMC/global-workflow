#! /usr/bin/env bash

export STRICT="NO"
set -x

###############################################################
# Source UFSDA workflow modules
source "${HOMEglobal}/dev/ush/load_modules.sh" ufsda
status=$?
if [[ ${status} -ne 0 ]]; then
    exit "${status}"
fi

export job="ocnanalprep"
export jobid="${job}.$$"

###############################################################
# Execute the JJOB
"${HOMEglobal}"/dev/jobs/JGLOBAL_MARINE_ANALYSIS_INITIALIZE
status=$?
exit "${status}"
