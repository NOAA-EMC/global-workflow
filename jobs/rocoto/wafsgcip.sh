#!/bin/ksh -x

###############################################################
echo
echo "=============== START TO SOURCE FV3GFS WORKFLOW MODULES ==============="
. $HOMEgfs/ush/load_fv3gfs_modules.sh
status=$?
[[ $status -ne 0 ]] && exit $status

###############################################################
echo "=============== BEGIN TO SOURCE RELEVANT CONFIGS ==============="
configs="base wafsgcip"
for config in $configs; do
    . $EXPDIR/config.${config}
    status=$?
    [[ $status -ne 0 ]] && exit $status
done

###############################################################

export DATAROOT="$RUNDIR/$CDATE/$CDUMP/wafsgcip"
[[ -d $DATAROOT ]] && rm -rf $DATAROOT
mkdir -p $DATAROOT

export pid=${pid:-$$}
export jobid=${job}.${pid}
export DATA="${DATAROOT}/$job"

###############################################################
echo
echo "=============== START TO RUN WAFSGCIP ==============="
# Execute the JJOB
$HOMEgfs/jobs/JGFS_WAFS_GCIP
status=$?
exit $status

###############################################################
# Force Exit out cleanly
if [ ${KEEPDATA:-"NO"} = "NO" ] ; then rm -rf $DATAROOT ; fi
exit 0
