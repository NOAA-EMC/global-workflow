#! /usr/bin/env bash

###############################################################
echo
echo "=============== START TO SOURCE FV3GFS WORKFLOW MODULES ==============="
#. ${HOMEgfs}/ush/load_fv3gfs_modules.sh
source "${HOMEgfs}/ush/load_ufswm_modules.sh"

err=$?
if [[ "${err}" -ne 0 ]]; then
	exit "${err}"
fi

export job="wavestat_pnt"
export jobid="${job}.$$"

###############################################################
echo
echo "=============== START TO RUN WAVE PREP ==============="
# Execute the JJOB
"${HOMEgfs}/jobs/JGEFS_WAVE_STAT_PNT"
err=$?
if [[ "${err}" -ne 0 ]]; then
	exit "${err}"
fi

exit 0

