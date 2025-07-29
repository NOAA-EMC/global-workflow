#! /usr/bin/env bash

set -x

###############################################################
echo
echo "=============== START TO SOURCE FV3GFS WORKFLOW MODULES ==============="
#source "${HOMEgfs}/dev/ush/load_fv3gfs_modules.sh
source "${HOMEgfs}/dev/ush/load_ufswm_modules.sh"
err=$?
if [[ "${err}" -ne 0 ]]; then
    exit "${err}"
fi

export job="wavepostbndpnt"
export jobid="${job}.$$"

###############################################################
echo
echo "=============== START TO RUN WAVE_POST_BNDPNT ==============="
# Execute the JJOB
"${HOMEgfs}/jobs/JGLOBAL_WAVE_POST_BNDPNT"
err=$?
if [[ "${err}" -ne 0 ]]; then
    exit "${err}"
fi

exit 0
