#!/bin/ksh -x

###############################################################
echo
echo "=============== START TO SOURCE FV3GFS WORKFLOW MODULES ==============="
. $HOMEgfs/ush/load_fv3gfs_modules.sh
status=$?
[[ $status -ne 0 ]] && exit $status

###############################################################
echo "=============== BEGIN TO SOURCE RELEVANT CONFIGS ==============="
configs="base wafsgrib20p25"
for config in $configs; do
    . $EXPDIR/config.${config}
    status=$?
    [[ $status -ne 0 ]] && exit $status
done

###############################################################

export DATAROOT="$RUNDIR/$CDATE/$CDUMP/wafsgrib20p25"
[[ -d $DATAROOT ]] && rm -rf $DATAROOT
mkdir -p $DATAROOT

export pid=${pid:-$$}
export jobid=${job}.${pid}
export DATA="${DATAROOT}/$job"

###############################################################
echo
echo "=============== START TO RUN WAFSGRIB20p25 ==============="
# Execute the JJOB
$HOMEgfs/jobs/JGFS_WAFS_GRIB2_0P25
status=$?
exit $status

###############################################################
# Force Exit out cleanly
if [ ${KEEPDATA:-"NO"} = "NO" ] ; then rm -rf $DATAROOT ; fi
exit 0
