#! /usr/bin/env bash

set -x

# Source FV3GFS workflow modules
source "${HOMEgfs}/dev/ush/load_modules.sh" run
status=$?
if [[ "${status}" -ne 0 ]]; then
    exit "${status}"
fi

export job="stage_ic"
export jobid="${job}.$$"

# Execute the JJOB
"${HOMEgfs}/jobs/JGLOBAL_STAGE_IC"
status=$?

exit "${status}"
