#!/bin/ksh -x

###############################################################
echo
echo "=============== START TO SOURCE FV3GFS WORKFLOW MODULES ==============="
. $HOMEgfs/ush/load_fv3gfs_modules.sh
status=$?
[[ $status -ne 0 ]] && exit $status

###############################################################
echo "=============== BEGIN TO SOURCE RELEVANT CONFIGS ==============="
configs="base wafs"
for config in $configs; do
    . $EXPDIR/config.${config}
    status=$?
    [[ $status -ne 0 ]] && exit $status
done

###############################################################

export pid=${pid:-$$}
export jobid=${job}.${pid}
export DATAROOT="$RUNDIR/$CDATE/$CDUMP/wafs.$jobid"
[[ -d $DATAROOT ]] && rm -rf $DATAROOT
mkdir -p $DATAROOT

export DATA="${DATAROOT}/$job"

###############################################################
echo
echo "=============== START TO RUN WAFS ==============="

# Execute the JJOB
$HOMEgfs/jobs/JGFS_WAFS
status=$?
exit $status

###############################################################
# Force Exit out cleanly
if [ ${KEEPDATA:-"NO"} = "NO" ] ; then rm -rf $DATAROOT ; fi
exit 0
