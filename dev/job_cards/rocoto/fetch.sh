#! /usr/bin/env bash

set -x

# Source FV3GFS workflow modules
# Runs on the data transfer nodes; see Tasks.DTN_TASKS
source "${HOMEglobal}/dev/ush/load_modules.sh" dtn
status=$?
if [[ "${status}" -ne 0 ]]; then
    exit "${status}"
fi

export job="fetch"
export jobid="${job}.$$"

# Execute the JJOB
"${HOMEglobal}/dev/jobs/JGLOBAL_FETCH"
status=$?

exit "${status}"
