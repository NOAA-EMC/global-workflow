#! /usr/bin/env bash

set -x

###############################################################
# Source UFSDA workflow modules
source "${HOMEgfs}/dev/ush/load_modules.sh" ufsda
status=$?
if [[ ${status} -ne 0 ]]; then
    exit "${status}"
fi

export job="atmensanlfv3inc"
export jobid="${job}.$$"

###############################################################
# Execute the JJOB
"${HOMEgfs}/jobs/JGLOBAL_ATMENS_ANALYSIS_FV3_INCREMENT"
status=$?
exit "${status}"
