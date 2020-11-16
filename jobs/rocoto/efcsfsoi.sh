#!/bin/ksh -x

###############################################################
# Source FV3GFS workflow modules
. $HOMEgfs/ush/load_fv3gfs_modules.sh
status=$?
[[ $status -ne 0 ]] && exit $status

###############################################################
# Execute the JJOB
# Edited by liaofan on 2020.05.26
#$HOMEgfs/jobs/JGDAS_ENKF_FCST
$HOMEgfs/jobs/JGDAS_EFSOI_FCST

status=$?
exit $status
