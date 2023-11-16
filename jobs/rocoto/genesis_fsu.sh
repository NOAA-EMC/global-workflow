#! /usr/bin/env bash

source "${HOMEgfs}/ush/preamble.sh"

###############################################################
# Source FV3GFS workflow modules
source "${HOMEgfs}/ush/load_fv3gfs_modules.sh"
status=$?
(( status != 0 )) && exit "${status}"

export job="genesis_fsu"
export jobid="${job}.$$"

###############################################################
# Execute the JJOB

"${HOMEgfs}/jobs/JGFS_ATMOS_FSU_GENESIS"
status=$?

exit "${status}"
