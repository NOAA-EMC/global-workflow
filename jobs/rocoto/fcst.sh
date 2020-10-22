#!/bin/ksh -x

###############################################################
# Source FV3GFS workflow modules
#### . $HOMEgfs/ush/load_fv3gfs_modules.sh
. $HOMEgfs/ush/load_fv3gfs_modules_fcst.sh
status=$?
[[ $status -ne 0 ]] && exit $status

###############################################################
# Execute the JJOB
$HOMEgfs/jobs/JGLOBAL_FORECAST
status=$?
exit $status
