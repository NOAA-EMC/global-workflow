#! /usr/bin/env bash

set -x

###############################################################
# Source FV3GFS workflow modules
source "${HOMEgfs}/dev/ush/load_modules.sh" run
status=$?
if [[ "${status}" -ne 0 ]]; then
    exit "${status}"
fi

export job="prep_sfc"
export jobid="${job}.$$"

###############################################################
# Execute the JJOB
"${HOMEgfs}/dev/jobs/JGLOBAL_ATMOS_PREP_SFC"
status=$?

exit "${status}"
