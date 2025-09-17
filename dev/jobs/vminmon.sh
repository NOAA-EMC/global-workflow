#! /usr/bin/env bash

set -x

###############################################################
# Source FV3GFS workflow modules
source "${HOMEgfs}/dev/ush/load_gw_gsi_modules.sh"
status=$?
if [[ status -ne 0 ]]; then
  exit "${status}"
fi

export job="vminmon"
export jobid="${job}.$$"

###############################################################
# Execute the JJOB
echo
echo "=============== START TO RUN MINMON ==============="

"${HOMEgfs}/jobs/JGLOBAL_ATMOS_VMINMON"
status=$?

exit "${status}"
