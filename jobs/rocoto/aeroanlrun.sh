#! /usr/bin/env bash

source "${HOMEgfs}/ush/preamble.sh"

###############################################################
# Source UFSDA workflow modules
. "${HOMEgfs}/ush/load_ufsda_modules.sh"
status=$?
[[ ${status} -ne 0 ]] && exit "${status}"

###############################################################
# Execute the JJOB
"${HOMEgfs}"/jobs/JGDAS_GLOBAL_AERO_ANALYSIS_RUN
status=$?
exit "${status}"
