#! /usr/bin/env bash

source "${HOMEgfs}/ush/preamble.sh"

###############################################################
# Source FV3GFS workflow modules
source "${HOMEgfs}/ush/load_fv3gfs_modules.sh"
status=$?
(( status != 0 )) && exit "${status}"

export job="vminmon"
export jobid="${job}.$$"

###############################################################
# Execute the JJOB
echo
echo "=============== START TO RUN MINMON ==============="

"${HOMEgfs}/jobs/J${RUN^^}_ATMOS_VMINMON"
status=$?

exit "${status}"
