#! /usr/bin/env bash

source "${HOMEgfs}/ush/preamble.sh"

###############################################################
echo
echo "=============== START TO SOURCE FV3GFS WORKFLOW MODULES ==============="
. "${HOMEgfs}/ush/load_fv3gfs_modules.sh"
status=$?
if [[ "${status}" -ne 0 ]]; then
    exit "${status}"
fi

export job="extractvars"
export jobid="${job}.$$"

###############################################################
echo
echo "=============== START TO RUN EXTRACTVARS ==============="
# Execute the JJOB
"${HOMEgfs}/jobs/JGLOBAL_EXTRACTVARS"
status=$?
if [[ "${status}" -ne 0 ]]; then
    exit "${status}"
fi

exit 0
