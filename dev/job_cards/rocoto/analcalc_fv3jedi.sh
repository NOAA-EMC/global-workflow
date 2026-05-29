#! /usr/bin/env bash

set -x

###############################################################
# Source UFSDA workflow modules
source "${HOMEglobal}/dev/ush/load_modules.sh" ufsda
status=$?
[[ ${status} -ne 0 ]] && exit "${status}"

export job="analcalc_fv3jedi"
export jobid="${job}.$$"

###############################################################
# Execute the JJOB
"${HOMEglobal}"/dev/jobs/JGLOBAL_ATMOS_ANALYSIS_CALC_FV3JEDI
status=$?
exit "${status}"
