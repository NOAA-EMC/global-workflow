#! /usr/bin/env bash

set -x

###############################################################
# Source FV3GFS workflow modules
source "${HOMEgfs}/ush/detect_machine.sh"
source "${HOMEgfs}/dev/ush/load_modules.sh" run

status=$?
if [[ ${status} -ne 0 ]]; then
    exit "${status}"
fi

export job="fcst_manager"
export jobid="${job}.$$"

# Execute the JJOB
"${HOMEgfs}/dev/jobs/JGLOBAL_FORECAST_MANAGER"
status=$?

exit "${status}"
