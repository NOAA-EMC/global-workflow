#! /usr/bin/env bash

set -x

###############################################################
# Source UFSDA workflow modules
source "${HOMEgfs}/dev/ush/load_modules.sh" run
status=$?
((status != 0)) && exit "${status}"

export job="prep_emissions"
export jobid="${job}.$$"

###############################################################
# Execute the JJOB
"${HOMEgfs}/dev/jobs/JGLOBAL_PREP_EMISSIONS"
status=$?
exit "${status}"
