#! /usr/bin/env bash

source "${HOMEgfs}/ush/preamble.sh"

###############################################################
echo
echo "=============== START TO SOURCE FV3GFS WORKFLOW MODULES ==============="
. "${HOMEgfs}/ush/load_fv3gfs_modules.sh"
status=$?
if [[ ${status} -ne 0 ]]; then
    exit "${status}"
fi

export job="fit2obs"
export jobid="${job}.$$"

###############################################################
echo
echo "=============== START TO RUN FIT2OBS ==============="
# Execute the JJOB
"${HOMEgfs}/jobs/JGDAS_FIT2OBS"
status=$?
if [[ ${status} -ne 0 ]]; then
    exit "${status}"
fi

exit 0
