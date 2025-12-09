#! /usr/bin/env bash

set -x

###############################################################
# Source FV3GFS workflow modules
source "${HOMEgfs}/dev/ush/load_modules.sh" gsi
status=$?
if [[ ${status} -ne 0 ]]; then
    exit "${status}"
fi

export job="ediag"
export jobid="${job}.$$"

###############################################################
# Execute the JJOB
"${HOMEgfs}/dev/jobs/JGLOBAL_ENKF_DIAG"
status=$?

exit "${status}"
