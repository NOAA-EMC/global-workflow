#! /usr/bin/env bash

set -x

# Source FV3GFS workflow modules
source "${HOMEglobal}/dev/ush/load_modules.sh" run
status=$?
if [[ "${status}" -ne 0 ]]; then
    exit "${status}"
fi

export job="stage_ic"
export jobid="${job}.$$"

# Execute the JJOB
"${HOMEglobal}/dev/jobs/JGLOBAL_STAGE_IC"
status=$?

exit "${status}"
