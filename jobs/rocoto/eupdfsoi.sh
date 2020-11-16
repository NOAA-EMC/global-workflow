#!/bin/ksh -x

###############################################################
# Source FV3GFS workflow modules
. $HOMEgfs/ush/load_fv3gfs_modules.sh
status=$?
[[ $status -ne 0 ]] && exit $status

###############################################################
# Execute the JJOB
# Updated by liaofan on 2020.05.07
#$HOMEgfs/jobs/JGLOBAL_ENKF_UPDATE
$HOMEgfs/jobs/JGLOBAL_EFSOI_UPDATE

status=$?
exit $status
