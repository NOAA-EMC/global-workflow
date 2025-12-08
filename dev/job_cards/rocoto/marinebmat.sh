#! /usr/bin/env bash

set -x

###############################################################
# Source UFSDA workflow modules
source "${HOMEgfs}/dev/ush/load_modules.sh" ufsda
status=$?
if [[ "${status}" -ne 0 ]]; then
    exit "${status}"
fi

export job="marinebmat"
export jobid="${job}.$$"

###############################################################
# Execute the JJOB
"${HOMEgfs}"/dev/jobs/JGLOBAL_MARINE_BMAT
status=$?
exit "${status}"
