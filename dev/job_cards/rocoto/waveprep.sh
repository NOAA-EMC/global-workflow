#! /usr/bin/env bash

set -x

###############################################################
#source "${HOMEgfs}/dev/ush/load_modules.sh" run
source "${HOMEgfs}/dev/ush/load_modules.sh" ufswm
status=$?
if [[ ${status} -ne 0 ]]; then
    exit "${status}"
fi

export job="waveprep"
export jobid="${job}.$$"

###############################################################
# Execute the JJOB
"${HOMEgfs}/dev/jobs/JGLOBAL_WAVE_PREP"
status=$?
if [[ ${status} -ne 0 ]]; then
    exit "${status}"
fi

exit 0
