#! /usr/bin/env bash

set -x

###############################################################
# Source FV3GFS workflow modules
source "${HOMEgfs}/dev/ush/load_fv3gfs_modules.sh"
status=$?
(( status != 0 )) && exit "${status}"

export job="vminmon"
export jobid="${job}.$$"

###############################################################
# Execute the JJOB
echo
echo "=============== START TO RUN MINMON ==============="

"${HOMEgfs}/jobs/JGLOBAL_ATMOS_VMINMON"
status=$?

exit "${status}"
