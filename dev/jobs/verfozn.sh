#! /usr/bin/env bash

set -x

###############################################################
# Source GSI workflow modules
source "${HOMEgfs}/dev/ush/load_gw_gsi_modules.sh"
status=$?
if [[ status -ne 0 ]]; then
  exit "${status}"
fi

export job="verfozn"
export jobid="${job}.$$"

###############################################################
# Execute the JJOB
echo
echo "=============== START TO RUN OZMON DATA EXTRACTION ==============="

"${HOMEgfs}/jobs/JGDAS_ATMOS_VERFOZN"
status=$?

exit "${status}"
