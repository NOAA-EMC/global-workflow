#! /usr/bin/env bash

source "${HOMEgfs}/ush/preamble.sh"

###############################################################
# Source FV3GFS workflow modules
source "${HOMEgfs}/ush/load_fv3gfs_modules.sh"
status=$?
(( status != 0 )) && exit "${status}"

export job="verfozn"
export jobid="${job}.$$"

###############################################################
# Execute the JJOB
echo
echo "=============== START TO RUN OZMON DATA EXTRACTION ==============="

"${HOMEgfs}/jobs/JGDAS_ATMOS_VERFOZN"
status=$?

exit "${status}"
