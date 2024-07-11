#! /usr/bin/env bash

source "${HOMEgfs}/ush/preamble.sh"

###############################################################
# Source UFSDA workflow modules
. "${HOMEgfs}/ush/load_ufsda_modules.sh"
status=$?
[[ "${status}" -ne 0 ]] && exit "${status}"

export job="marinebmat"
export jobid="${job}.$$"

###############################################################
# Execute the JJOB
"${HOMEgfs}"/jobs/JGLOBAL_MARINE_BMAT
status=$?
exit "${status}"
