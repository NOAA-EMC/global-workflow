#! /usr/bin/env bash

set -x

###############################################################
source "${HOMEglobal}/dev/ush/load_modules.sh" run
status=$?
if ((status != 0)); then exit "${status}"; fi

export job="fbwind"
export jobid="${job}.$$"

# Execute the JJOB
"${HOMEglobal}/dev/jobs/JGFS_ATMOS_FBWIND"

status=$?
exit "${status}"
