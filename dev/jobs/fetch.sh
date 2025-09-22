#! /usr/bin/env bash

set -x

# Source FV3GFS workflow modules
source "${HOMEgfs}/dev/ush/load_fv3gfs_modules.sh"
status=$?
if [[ "${status}" -ne 0 ]]; then
    exit "${status}"
fi

export job="fetch"
export jobid="${job}.$$"

# Execute the JJOB
"${HOMEgfs}/jobs/JGLOBAL_FETCH"
status=$?


exit "${status}"
