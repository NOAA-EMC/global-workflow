#! /usr/bin/env bash

source "${HOMEgfs}/ush/preamble.sh"

# Hack to temporary skip this as the tracker has not been build
#   on Hercules Rocky 9 yet
# TODO: Remove this after tracker has been built for Rocky 9 #2639
if [[ "${machine}" == 'hercules' ]]; then exit 0; fi

###############################################################
# Source FV3GFS workflow modules
source "${HOMEgfs}/ush/load_fv3gfs_modules.sh"
status=$?
(( status != 0 )) && exit "${status}"

export job="genesis_fsu"
export jobid="${job}.$$"

###############################################################
# Execute the JJOB

"${HOMEgfs}/jobs/JGFS_ATMOS_FSU_GENESIS"
status=$?

exit "${status}"
