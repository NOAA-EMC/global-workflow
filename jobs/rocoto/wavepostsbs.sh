#! /usr/bin/env bash

source "${HOMEgfs}/ush/preamble.sh"

###############################################################
# Source FV3GFS workflow modules
#. ${HOMEgfs}/ush/load_fv3gfs_modules.sh
. ${HOMEgfs}/ush/load_ufswm_modules.sh
status=$?
[[ ${status} -ne 0 ]] && exit ${status}

export job="wavepostsbs"
export jobid="${job}.$$"
export FORECAST_HOUR=$(( 10#${FHR3} ))
###############################################################
# Execute the JJOB
${HOMEgfs}/jobs/JGLOBAL_WAVE_POST_SBS
status=$?
[[ ${status} -ne 0 ]] && exit ${status}

exit 0
