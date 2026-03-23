#! /usr/bin/env bash

set -x

###############################################################
#source "${HOMEglobal}/dev/ush/load_modules.sh" run
source "${HOMEglobal}/dev/ush/load_modules.sh" ufswm
status=$?
if [[ ${status} -ne 0 ]]; then
    exit "${status}"
fi

export job="waveprep"
export jobid="${job}.$$"

###############################################################
# Execute the JJOB
"${HOMEglobal}/dev/jobs/JGLOBAL_WAVE_PREP"
status=$?
if [[ ${status} -ne 0 ]]; then
    exit "${status}"
fi

exit 0
