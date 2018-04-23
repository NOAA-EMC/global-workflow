#!/bin/ksh -x

###############################################################
## Abstract:
## Inline gempak driver script
## RUN_ENVIR : runtime environment (emc | nco)
## HOMEgfs   : /full/path/to/workflow
## EXPDIR : /full/path/to/config/files
## CDATE  : current analysis date (YYYYMMDDHH)
## CDUMP  : cycle name (gdas / gfs)
## PDY    : current date (YYYYMMDD)
## cyc    : current cycle (HH)
###############################################################

###############################################################
echo
echo "=============== BEGIN TO SOURCE FV3GFS WORKFLOW MODULES ==============="
. $HOMEgfs/ush/load_fv3gfs_modules.sh
status=$?
[[ $status -ne 0 ]] && exit $status


###############################################################
echo
echo "=============== BEGIN TO SOURCE RELEVANT CONFIGS ==============="
configs="base gempak"
for config in $configs; do
    . $EXPDIR/config.${config}
    status=$?
    [[ $status -ne 0 ]] && exit $status
done


###############################################################
echo
echo "=============== BEGIN TO SOURCE MACHINE RUNTIME ENVIRONMENT ==============="
. $BASE_ENV/${machine}.env gempak
status=$?
[[ $status -ne 0 ]] && exit $status

###############################################################
export CDATEm1=$($NDATE -24 $CDATE)
export PDYm1=$(echo $CDATEm1 | cut -c1-8)

export COMIN="$ROTDIR/$CDUMP.$PDY/$cyc"
export DATAROOT="$RUNDIR/$CDATE/$CDUMP/gempak"
[[ -d $DATAROOT ]] && rm -rf $DATAROOT
mkdir -p $DATAROOT


################################################################################
echo
echo "=============== BEGIN GEMPAK ==============="
export job="jgfs_gempak_${cyc}"
export jlogfile="$ROTDIR/logs/$CDATE/$job.log"    
export DATA="${DATAROOT}/$job"
export SENDCOM="YES"
export COMOUT="$ROTDIR/$CDUMP.$PDY/$cyc/nawips"
export FIXgfs=""  # set blank so that GEMPAKSH defaults FIXgfs to HOMEgfs/gempak/fix
export USHgfs=""  # set blank so that GEMPAKSH defaults FIXgfs to HOMEgfs/gempak/ush

$GEMPAKSH


###############################################################
# Force Exit out cleanly
if [ ${KEEPDATA:-"NO"} = "NO" ] ; then rm -rf $DATAROOT ; fi
exit 0
