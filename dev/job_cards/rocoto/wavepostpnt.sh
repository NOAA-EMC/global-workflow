#! /usr/bin/env bash

set -x

###############################################################
echo
echo "=============== START TO SOURCE FV3GFS WORKFLOW MODULES ==============="
#source "${HOMEglobal}/dev/ush/load_modules.sh" run
source "${HOMEglobal}/dev/ush/load_modules.sh" ufswm
status=$?
if [[ "${status}" -ne 0 ]]; then exit "${status}"; fi

export job="wavepostpnt"
export jobid="${job}.$$"

###############################################################
echo
echo "=============== START TO RUN WAVE_POST_PNT ==============="
# Execute the JJOB
"${HOMEglobal}/dev/jobs/JGLOBAL_WAVE_POST_PNT"
err=$?
if [[ "${err}" -ne 0 ]]; then
    exit "${err}"
fi

exit 0
