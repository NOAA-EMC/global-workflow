#! /usr/bin/env bash

set -x

###############################################################
# Source FV3GFS workflow modules
source "${HOMEglobal}/dev/ush/load_modules.sh" gsi
status=$?
if [[ ${status} -ne 0 ]]; then
    exit "${status}"
fi

export job="echgres"
export jobid="${job}.$$"

###############################################################
# Execute the JJOB
"${HOMEglobal}/dev/jobs/JENKFGDAS_ATMOS_ENS_CHANGE_RES"
status=$?

exit "${status}"
