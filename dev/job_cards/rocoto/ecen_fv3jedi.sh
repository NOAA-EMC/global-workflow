#! /usr/bin/env bash

set -x

###############################################################
# Source UFSDA workflow modules
source "${HOMEgfs}/dev/ush/load_modules.sh" ufsda
status=$?
[[ ${status} -ne 0 ]] && exit "${status}"

export job="ecen_fv3jedi"
export jobid="${job}.$$"

###############################################################
# Execute the JJOB
"${HOMEgfs}"/dev/jobs/JGLOBAL_ENKF_ECEN_FV3JEDI
status=$?
exit "${status}"
