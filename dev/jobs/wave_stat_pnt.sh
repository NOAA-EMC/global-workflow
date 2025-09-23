#! /usr/bin/env bash

###############################################################
source "${HOMEgfs}/dev/ush/load_ufswm_modules.sh"
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
"${HOMEgfs}/jobs/JGEFS_WAVE_STAT_PNT"
err=$?
if [[ "${err}" -ne 0 ]]; then
  exit "${err}"
fi

exit 0

