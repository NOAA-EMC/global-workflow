#! /usr/bin/env bash

source "${HOMEgfs}/ush/preamble.sh"

###############################################################
# Source UFSDA workflow modules
. "${HOMEgfs}/ush/load_ufsda_modules.sh"
status=$?
[[ ${status} -ne 0 ]] && exit "${status}"

export job="esnowanl"
export jobid="${job}.$$"


###############################################################
# Execute the JJOB
"${HOMEgfs}/jobs/JGLOBAL_SNOW_ENSEMBLE_ANALYSIS"
status=$?
exit "${status}"
