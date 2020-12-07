#!/bin/ksh -x

###############################################################
# Source FV3GFS workflow modules
. $HOMEgfs/ush/load_fv3gfs_modules.sh
status=$?
[[ $status -ne 0 ]] && exit $status

###############################################################
# Execute the JJOB
$HOMEgfs/jobs/JGDAS_ENKF_FCST
status=$?
exit $status
