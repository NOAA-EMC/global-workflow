#! /usr/bin/env bash

source "${HOMEgfs}/ush/preamble.sh"

###############################################################
# Source UFSDA workflow modules
. "${HOMEgfs}/ush/load_ufsda_modules.sh"
status=$?
if [[ ${status} -ne 0 ]]; then
    exit "${status}"
fi

export job="marineanlfinal"
export jobid="${job}.$$"

###############################################################
# Execute the JJOB
"${HOMEgfs}"/jobs/JGLOBAL_MARINE_ANALYSIS_FINALIZE
status=$?
exit "${status}"
