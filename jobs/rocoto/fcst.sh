#!/bin/bash

###############################################################
# Source FV3GFS workflow modules
. $HOMEgfs/ush/load_fv3gfs_modules.sh
status=$?
[[ $status -ne 0 ]] && exit $status

export DATAROOT="$RUNDIR/$CDATE/$CDUMP"
[[ ! -d $DATAROOT ]] && mkdir -p $DATAROOT

###############################################################
# Execute the JJOB
$HOMEgfs/jobs/JGLOBAL_FORECAST
status=$?
exit $status
