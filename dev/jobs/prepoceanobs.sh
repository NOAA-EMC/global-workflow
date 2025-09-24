#! /usr/bin/env bash

export STRICT="NO"

###############################################################
# Source UFSDA workflow modules
source "${HOMEgfs}/dev/ush/load_ufsda_modules.sh"
status=$?
if [[ ${status} -ne 0 ]]; then
    exit "${status}"
fi

export job="prepoceanobs"
export jobid="${job}.$$"

###############################################################
# Execute the JJOB
"${HOMEgfs}"/jobs/JGLOBAL_PREP_OCEAN_OBS
status=$?
exit "${status}"
