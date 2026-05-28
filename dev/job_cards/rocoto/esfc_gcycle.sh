#! /usr/bin/env bash

set -x

###############################################################
# Source GSI workflow modules
source "${HOMEglobal}/dev/ush/load_modules.sh" gsi
status=$?
if [[ ${status} -ne 0 ]]; then
    exit "${status}"
fi

export job="esfcg"
export jobid="${job}.$$"

###############################################################
# Execute the JJOB
"${HOMEglobal}/dev/jobs/JGLOBAL_ENKF_SFC_GCYCLE"
status=$?

exit "${status}"
