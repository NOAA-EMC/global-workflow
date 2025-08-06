#! /usr/bin/env bash

set -x

###############################################################
source "${HOMEgfs}/dev/ush/load_fv3gfs_modules.sh"
status=$?
if (( status != 0 )); then exit "${status}"; fi

export job="fbwind"
export jobid="${job}.$$"

# Execute the JJOB
"${HOMEgfs}/jobs/JGFS_ATMOS_FBWIND"

status=$?
exit "${status}"
