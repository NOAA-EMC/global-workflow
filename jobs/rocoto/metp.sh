#!/bin/ksh -x

###############################################################
## Abstract:
## Inline METplus verification and diagnostics driver script
## RUN_ENVIR : runtime environment (emc | nco)
## HOMEgfs   : /full/path/to/workflow
## EXPDIR : /full/path/to/config/files
## CDATE  : current analysis date (YYYYMMDDHH)
## CDUMP  : cycle name (gdas / gfs)
## PDY    : current date (YYYYMMDD)
## cyc    : current cycle (HH)
## METPCASE : METplus verification use case (g2g1 | g2o1 | pcp1)
###############################################################

###############################################################
echo
echo "=============== START TO SOURCE FV3GFS WORKFLOW MODULES ==============="
. $HOMEgfs/ush/load_fv3gfs_modules.sh
status=$?
[[ $status -ne 0 ]] && exit $status


###############################################################
echo
echo "=============== START TO SOURCE RELEVANT CONFIGS ==============="
configs="base vrfy metp"
for config in $configs; do
    . $EXPDIR/config.${config}
    status=$?
    [[ $status -ne 0 ]] && exit $status
done


###############################################################
echo
echo "=============== START TO SOURCE MACHINE RUNTIME ENVIRONMENT ==============="
. $BASE_ENV/${machine}.env metp
status=$?
[[ $status -ne 0 ]] && exit $status

###############################################################
export CDATEm1=$($NDATE -24 $CDATE)
export PDYm1=$(echo $CDATEm1 | cut -c1-8)

export COMIN="$ROTDIR/$CDUMP.$PDY/$cyc/atmos"
export DATAROOT="$RUNDIR/$CDATE/$CDUMP/vrfy"
[[ -d $DATAROOT ]] && rm -rf $DATAROOT
mkdir -p $DATAROOT


###############################################################
echo
echo "=============== START TO RUN METPLUS VERIFICATION ==============="
if [ $CDUMP = "gfs" ]; then

    if [ $RUN_GRID2GRID_STEP1 = "YES" -o $RUN_GRID2OBS_STEP1 = "YES" -o $RUN_PRECIP_STEP1 = "YES" ]; then

        $VERIF_GLOBALSH 
 
    fi
fi


if [ $CDUMP = "gdas" ]; then
    echo "METplus verification currently not supported for CDUMP=${CDUMP}"
fi
###############################################################
# Force Exit out cleanly
if [ ${KEEPDATA:-"NO"} = "NO" ] ; then rm -rf $DATAROOT ; fi
exit 0
