#! /usr/bin/env bash

###############################################################
# Source UFSDA workflow modules
source "${HOMEgfs}/dev/ush/load_modules.sh" ufsda
status=$?
if [[ ${status} -ne 0 ]]; then
    exit "${status}"
fi

export job="prepatmanlbias"
export jobid="${job}.$$"

###############################################################
# Execute the JJOB
"${HOMEgfs}/dev/jobs/JGLOBAL_ATM_PREP_ANL_BIAS"
status=$?
exit "${status}"
