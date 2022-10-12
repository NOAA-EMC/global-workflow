#! /usr/bin/env bash

source "$HOMEgfs/ush/preamble.sh"

###############################################################
# Source FV3GFS workflow modules
. $HOMEgfs/ush/load_fv3gfs_modules.sh
status=$?
[[ $status -ne 0 ]] && exit $status

export job="analdiag"
export jobid="${job}.$$"

###############################################################
# Execute the JJOB
$HOMEgfs/jobs/JGDAS_ATMOS_ANALYSIS_DIAG
status=$?


exit $status
