#! /usr/bin/env bash

set -x

###############################################################
#source "${HOMEgfs}/ush/load_fv3gfs_modules.sh"
source "${HOMEgfs}/ush/load_ufswm_modules.sh"
status=$?
if [[ ${status} -ne 0 ]]; then
    exit "${status}"
fi

export job="waveinit"
export jobid="${job}.$$"

###############################################################
# Execute the JJOB
"${HOMEgfs}/jobs/JGLOBAL_WAVE_INIT"
status=$?

exit "${status}"
