#! /usr/bin/env bash

export STRICT="NO"
source "${HOMEgfs}/ush/preamble.sh"

###############################################################
# Source UFSDA workflow modules
. "${HOMEgfs}/ush/load_ufsda_modules.sh" --eva
status=$?
[[ ${status} -ne 0 ]] && exit "${status}"

export job="ocnanalvrfy"
export jobid="${job}.$$"

###############################################################
# Execute the JJOB
"${HOMEgfs}/jobs/JGDAS_GLOBAL_OCEAN_ANALYSIS_VRFY"
status=$?
exit "${status}"
