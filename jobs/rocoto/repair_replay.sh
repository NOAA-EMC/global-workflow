#! /usr/bin/env bash

source "${HOMEgfs}/ush/preamble.sh"

###############################################################
echo
echo "=============== START TO SOURCE FV3GFS WORKFLOW MODULES ==============="
. "${HOMEgfs}/ush/load_fv3gfs_modules.sh"
status=$?
[[ ${status} -ne 0 ]] && exit "${status}"

export job="repair_replay"
export jobid="${job}.$$"

###############################################################
# Execute the JJOB
"${HOMEgfs}/jobs/JGEFS_ATMOS_ACC"
status=$?
[[ ${status} -ne 0 ]] && exit "${status}"


exit 0
