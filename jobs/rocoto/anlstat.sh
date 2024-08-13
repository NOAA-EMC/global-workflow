#! /usr/bin/env bash

source "${HOMEgfs}/ush/preamble.sh"

###############################################################
# Source UFSDA workflow modules
. "${HOMEgfs}/ush/load_ufsda_modules.sh"
status=$?
[[ ${status} -ne 0 ]] && exit "${status}"

export job="anlstat"
export jobid="${job}.$$"

###############################################################
# Execute the JJOB
"${HOMEgfs}/jobs/JGLOBAL_ANALYSIS_STATS"
status=$?
exit "${status}"