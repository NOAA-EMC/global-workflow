#! /usr/bin/env bash

set -x

###############################################################
# Source GSI workflow modules
source "${HOMEgfs}/dev/ush/load_gw_gsi_modules.sh"
status=$?
if [[ ${status} -ne 0 ]]; then
  exit "${status}"
fi

export job="analcalc"
export jobid="${job}.$$"

###############################################################
# Execute the JJOB
"${HOMEgfs}/jobs/JGLOBAL_ATMOS_ANALYSIS_CALC"
status=$?


exit "${status}"
