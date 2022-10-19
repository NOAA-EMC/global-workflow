#! /usr/bin/env bash

source "$HOMEgfs/ush/preamble.sh"

###############################################################
echo
echo "=============== BEGIN TO SOURCE FV3GFS WORKFLOW MODULES ==============="
. $HOMEgfs/ush/load_fv3gfs_modules.sh
status=$?
[[ $status -ne 0 ]] && exit $status

export SENDCOM="YES"
export COMPONENT=${COMPONENT:-atmos}

export COMIN="$ROTDIR/$CDUMP.$PDY/$cyc/$COMPONENT"
export COMOUT="$ROTDIR/$CDUMP.$PDY/$cyc/$COMPONENT/gempak"

# Execute the JJOB

$HOMEgfs/jobs/JGFS_ATMOS_GEMPAK

status=$?
exit $status
