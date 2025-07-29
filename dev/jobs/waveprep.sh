#! /usr/bin/env bash

set -x

###############################################################
#source "${HOMEgfs}/dev/ush/load_fv3gfs_modules.sh"
source "${HOMEgfs}/dev/ush/load_ufswm_modules.sh"
status=$?
if [[ ${status} -ne 0 ]]; then
    exit "${status}"
fi

export job="waveprep"
export jobid="${job}.$$"

###############################################################
# Execute the JJOB
"${HOMEgfs}/jobs/JGLOBAL_WAVE_PREP"
status=$?
if [[ ${status} -ne 0 ]]; then
    exit "${status}"
fi

exit 0
