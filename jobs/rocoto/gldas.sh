#! /usr/bin/env bash

source "$HOMEgfs/ush/preamble.sh"

###############################################################
# Source FV3GFS workflow modules
. $HOMEgfs/ush/load_fv3gfs_modules.sh
status=$?
[[ $status -ne 0 ]] && exit $status

###############################################################
# Execute the JJOB. GLDAS only runs once per day.

$HOMEgfs/jobs/JGDAS_ATMOS_GLDAS
status=$?


exit $status
