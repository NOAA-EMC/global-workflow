#! /usr/bin/env bash

set -x

###############################################################
# Source UFSDA workflow modules
source "${HOMEgfs}/dev/ush/load_ufsda_modules.sh"
status=$?
if [[ ${status} -ne 0 ]]; then
    exit "${status}"
fi

export job="aeroanlgenb"
export jobid="${job}.$$"

###############################################################

# Execute the JJOB
"${HOMEgfs}/jobs/JGDAS_AERO_ANALYSIS_GENERATE_BMATRIX"
status=$?
exit "${status}"
