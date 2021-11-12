#!/bin/sh -x

###############################################################
# Source FV3GFS workflow modules

##. $HOMEgfs/ush/load_fv3gfs_modules.sh
. $HOMEgfs/versions/run.ver

. $HOMEgfs/modulefiles/module_base.wcoss2
status=$?
[[ $status -ne 0 ]] && exit $status


###############################################################
# Execute the JJOB
$HOMEgfs/jobs/JGFS_ATMOS_POSTSND
status=$?
exit $status

