#! /usr/bin/env bash

set -x

###############################################################
# Source GSI workflow modules
source "${HOMEglobal}/dev/ush/load_modules.sh" gsi
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

"${HOMEglobal}/dev/jobs/JGDAS_ATMOS_VERFOZN"
status=$?

exit "${status}"
