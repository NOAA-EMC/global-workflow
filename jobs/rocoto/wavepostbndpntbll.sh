#! /usr/bin/env bash

source "${HOMEgfs}/ush/preamble.sh"

###############################################################
echo
echo "=============== START TO SOURCE FV3GFS WORKFLOW MODULES ==============="
#. ${HOMEgfs}/ush/load_fv3gfs_modules.sh
source "${HOMEgfs}/ush/load_ufswm_modules.sh"
err=$?
if [[ "${err}" -ne 0 ]]; then
    exit "${err}"
fi

export job="wavepostbndpntbll"
export jobid="${job}.$$"

###############################################################
echo
echo "=============== START TO RUN WAVE_POST_BNDPNT ==============="
# Execute the JJOB
"${HOMEgfs}/jobs/JGLOBAL_WAVE_POST_BNDPNTBLL"
err=$?
if [[ "${err}" -ne 0 ]]; then
    exit "${err}"
fi

exit 0
