#! /usr/bin/env bash

set -x

###############################################################
#source "${HOMEgfs}/dev/ush/load_modules.sh" run
source "${HOMEgfs}/dev/ush/load_modules.sh" ufswm
status=$?
if [[ ${status} -ne 0 ]]; then
    exit "${status}"
fi

export job="waveinit"
export jobid="${job}.$$"

###############################################################
# Execute the JJOB
"${HOMEgfs}/dev/jobs/JGLOBAL_WAVE_INIT"
status=$?

exit "${status}"
