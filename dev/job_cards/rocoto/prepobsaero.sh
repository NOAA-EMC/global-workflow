#! /usr/bin/env bash

set -x

###############################################################
# Source UFSDA workflow modules
source "${HOMEgfs}/dev/ush/load_modules.sh" ufsda
status=$?
if [[ ${status} -ne 0 ]]; then
    exit "${status}"
fi

export job="prepobsaero"
export jobid="${job}.$$"

###############################################################
# Execute the JJOB
"${HOMEgfs}/jobs/JGLOBAL_PREP_OBS_AERO"
status=$?
exit "${status}"
