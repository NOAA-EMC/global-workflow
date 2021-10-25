#!/bin/ksh

###############################################################
echo
echo "=============== START TO SOURCE FV3GFS WORKFLOW MODULES ==============="
. $HOMEgfs/ush/load_fv3gfs_modules.sh
status=$?
[[ $status -ne 0 ]] && exit $status

set -x

###############################################################
echo "=============== BEGIN TO SOURCE RELEVANT CONFIGS ==============="
configs="base wafsblending"
for config in $configs; do
    . $EXPDIR/config.${config}
    status=$?
    [[ $status -ne 0 ]] && exit $status
done

###############################################################

export DATAROOT="$RUNDIR/$CDATE/$CDUMP/wafsblending"
[[ -d $DATAROOT ]] && rm -rf $DATAROOT
mkdir -p $DATAROOT

export pid=${pid:-$$}
export jobid=${job}.${pid}
export DATA="${DATAROOT}/$job"

###############################################################
echo
echo "=============== START TO RUN WAFSBLENDING ==============="
# Execute the JJOB
$HOMEgfs/jobs/JGFS_ATMOS_WAFS_BLENDING
status=$?
exit $status

###############################################################
# Force Exit out cleanly
if [ ${KEEPDATA:-"NO"} = "NO" ] ; then rm -rf $DATAROOT ; fi
exit 0
