#!/bin/bash

###############################################################
## Abstract:
## Inline awips driver script
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
configs="base awips"
for config in $configs; do
    . $EXPDIR/config.${config}
    status=$?
    [[ $status -ne 0 ]] && exit $status
done


###############################################################
echo
echo "=============== BEGIN TO SOURCE MACHINE RUNTIME ENVIRONMENT ==============="
. $BASE_ENV/${machine}.env awips
status=$?
[[ $status -ne 0 ]] && exit $status

###############################################################
export CDATEm1=$($NDATE -24 $CDATE)
export PDYm1=$(echo $CDATEm1 | cut -c1-8)

export COMIN="$ROTDIR/$CDUMP.$PDY/$cyc"
export DATAROOT="$RUNDIR/$CDATE/$CDUMP/awips"
[[ -d $DATAROOT ]] && rm -rf $DATAROOT
mkdir -p $DATAROOT


################################################################################
echo
echo "=============== BEGIN AWIPS ==============="
export SENDCOM="YES"
export COMOUT="$ROTDIR/$CDUMP.$PDY/$cyc"
export PCOM="$COMOUT/wmo"
export jlogfile="$ROTDIR/logs/$CDATE/jgfs_awips.log"    

fhmax=84
fhr=0
while [ $fhr -le $fhmax ]; do
    fhr3=$(printf %03i $fhr)
    export fcsthrs=$fhr3
    
    export job="jgfs_awips_f${fcsthrs}_20km_${cyc}"
    export DATA="${DATAROOT}/$job"
    $AWIPS20SH
    
    if [[ `expr $fhr % 6` -eq 0 ]]; then
	export job="jgfs_awips_f${fcsthrs}_${cyc}"
        export DATA="${DATAROOT}/$job"
        $AWIPSG2SH
    fi
    
    (( fhr = $fhr + 3 ))
done

fhmax=240
fhr=90
while [ $fhr -le $fhmax ]; do
    fhr3=$(printf %03i $fhr)
    export fcsthrs=$fhr3
    
    export job="jgfs_awips_f${fcsthrs}_20km_${cyc}"
    export DATA="${DATAROOT}/$job"
    $AWIPS20SH
    
    export job="jgfs_awips_f${fcsthrs}_${cyc}"
    export DATA="${DATAROOT}/$job"
    $AWIPSG2SH
    
    (( fhr = $fhr + 6 ))
done


###############################################################
# Force Exit out cleanly
if [ ${KEEPDATA:-"NO"} = "NO" ] ; then rm -rf $DATAROOT ; fi
exit 0
