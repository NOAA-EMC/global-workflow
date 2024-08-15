#! /usr/bin/env bash

source "${HOMEgfs}/ush/preamble.sh"

###############################################################
echo
echo "=============== START TO SOURCE FV3GFS WORKFLOW MODULES ==============="
. ${HOMEgfs}/ush/load_ufswm_modules.sh
status=$?
[[ ${status} -ne 0 ]] && exit ${status}

export job="repairf0306"
export jobid="${job}.$$"

###############################################################
echo
echo "=============== START TO RUN EXTRACTVARS ==============="
# Execute the JJOB
${HOMEgfs}/jobs/JGEFS_ATMOS_ACC
status=$?
[[ ${status} -ne 0 ]] && exit ${status}

exit 0
