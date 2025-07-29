#! /usr/bin/env bash

set -x

###############################################################
# Source UFSDA workflow modules
source "${HOMEgfs}/dev/ush/load_ufsda_modules.sh"
err=$?
if [[ ${err} -ne 0 ]]; then
    echo "FATAL ERROR Failed to load UFSDA modules!"
    exit "${err}"
fi

export job="anlstat"
export jobid="${job}.$$"

###############################################################
# Execute the JJOB
"${HOMEgfs}/jobs/JGLOBAL_ANALYSIS_STATS"
exit $?
