#! /usr/bin/env bash

###############################################################
source "${HOMEglobal}/dev/ush/load_modules.sh" ufswm
err=$?
if [[ "${err}" -ne 0 ]]; then
    exit "${err}"
fi

export job="wave_stat_pnt"
export jobid="${job}.$$"

###############################################################
echo
echo "=============== START TO RUN WAVESTAT PNT ==============="
# Execute the JJOB
"${HOMEglobal}/dev/jobs/JGEFS_WAVE_STAT_PNT"
err=$?
if [[ "${err}" -ne 0 ]]; then
    exit "${err}"
fi

exit 0
