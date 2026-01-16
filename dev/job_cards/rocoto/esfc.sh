#! /usr/bin/env bash

set -x

###############################################################
# Source GSI workflow modules
source "${HOMEgfs}/dev/ush/load_modules.sh" gsi
status=$?
if [[ ${status} -ne 0 ]]; then
    exit "${status}"
fi

export job="esfc"
export jobid="${job}.$$"

###############################################################
# Execute the JJOB
"${HOMEgfs}/dev/jobs/JGLOBAL_ENKF_SFC"
status=$?

exit "${status}"
