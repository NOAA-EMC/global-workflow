#! /usr/bin/env bash

source "$HOMEgfs/ush/preamble.sh"

###############################################################
source $HOMEgfs/ush/load_fv3gfs_modules.sh
status=$?
if [[ ${status} -ne 0 ]]; then
    exit "${status}"
fi

export job="post"
export jobid="${job}.$$"

###############################################################
# Execute the JJOB
$HOMEgfs/jobs/JGLOBAL_WAVE_GEMPAK
status=$?

exit "${status}"
