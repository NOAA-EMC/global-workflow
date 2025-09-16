#! /usr/bin/env bash

set -x

###############################################################
# Source UFSDA workflow modules
source "${HOMEgfs}/dev/ush/load_fv3gfs_modules.sh"
status=$?
(( status !=  0 )) && exit "${status}"

export job="prep_emissions"
export jobid="${job}.$$"

###############################################################
# Execute the JJOB
"${HOMEgfs}/jobs/JGLOBAL_PREP_EMISSIONS"
status=$?
exit "${status}"
