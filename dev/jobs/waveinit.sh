#! /usr/bin/env bash

set -x

###############################################################
source "${HOMEgfs}/dev/ush/load_modules.sh" ufswm
status=$?
if [[ ${status} -ne 0 ]]; then
    exit "${status}"
fi

export job="waveinit"
export jobid="${job}.$$"

###############################################################
# Execute the JJOB
if [[ "${RUN_WITH_CONTAINER}" == "YES" ]]; then
"${HOMEgfs}/exec/JGLOBAL_WAVE_INIT"
else
"${HOMEgfs}/jobs/JGLOBAL_WAVE_INIT"
fi
status=$?

exit "${status}"
