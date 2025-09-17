#! /usr/bin/env bash

set -x

###############################################################
# Source GSI workflow modules
source "${HOMEgfs}/dev/ush/load_gw_gsi_modules.sh"
status=$?
if [[ status -ne 0 ]]; then
  exit "${status}"
fi

export job="verfrad"
export jobid="${job}.$$"

###############################################################
# Execute the JJOB
echo
echo "=============== START TO RUN RADMON DATA EXTRACTION ==============="

"${HOMEgfs}/jobs/JGDAS_ATMOS_VERFRAD"
status=$?

exit "${status}"
