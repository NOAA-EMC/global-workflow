#! /usr/bin/env bash

source "${HOMEgfs}/ush/preamble.sh"

###############################################################
echo
echo "=============== START TO SOURCE FV3GFS WORKFLOW MODULES ==============="
#. ${HOMEgfs}/ush/load_fv3gfs_modules.sh
. ${HOMEgfs}/ush/load_ufswm_modules.sh
status=$?
[[ ${status} -ne 0 ]] && exit ${status}

export job="waveinit"
export jobid="${job}.$$"

###############################################################
echo
echo "=============== START TO RUN WAVE INIT ==============="
# Execute the JJOB
${HOMEgfs}/jobs/JGLOBAL_WAVE_INIT
status=$?
[[ ${status} -ne 0 ]] && exit ${status}

exit 0
