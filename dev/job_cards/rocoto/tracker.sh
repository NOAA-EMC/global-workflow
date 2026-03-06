#! /usr/bin/env bash

set -x

###############################################################
# Source FV3GFS workflow modules
source "${HOMEglobal}/dev/ush/load_modules.sh" run
status=$?
((status != 0)) && exit "${status}"

export job="tracker"
export jobid="${job}.$$"

###############################################################
# Execute the JJOB

"${HOMEglobal}/dev/jobs/JGFS_ATMOS_CYCLONE_TRACKER"
status=$?

exit "${status}"
