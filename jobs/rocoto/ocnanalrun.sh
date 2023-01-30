#! /usr/bin/env bash

source "${HOMEgfs}/ush/preamble.sh"

###############################################################
# Source UFSDA workflow modules
. "${HOMEgfs}/ush/load_ufsda_modules.sh"
status=$?
[[ ${status} -ne 0 ]] && exit "${status}"

export job="ocnanalrun"
export jobid="${job}.$$"

###############################################################
# Execute the JJOB
"${HOMEgfs}"/jobs/JGDAS_GLOBAL_OCEAN_ANALYSIS_RUN
status=$?
exit "${status}"
