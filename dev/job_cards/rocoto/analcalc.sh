#! /usr/bin/env bash

set -x

###############################################################
# Source GSI workflow modules
source "${HOMEglobal}/dev/ush/load_modules.sh" gsi
status=$?
if [[ ${status} -ne 0 ]]; then
    exit "${status}"
fi

export job="analcalc"
export jobid="${job}.$$"

###############################################################
# Execute the JJOB
"${HOMEglobal}/dev/jobs/JGLOBAL_ATMOS_ANALYSIS_CALC"
status=$?

exit "${status}"
