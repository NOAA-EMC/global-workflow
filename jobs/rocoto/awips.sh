#!/bin/ksh -x

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

fhrlst=$(echo $FHRLST | sed -e 's/_/ /g; s/f/ /g; s/,/ /g')

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
export DATAROOT="$RUNDIR/$CDATE/$CDUMP/awips$FHRGRP"
[[ -d $DATAROOT ]] && rm -rf $DATAROOT
mkdir -p $DATAROOT


################################################################################
echo
echo "=============== BEGIN AWIPS ==============="
export SENDCOM="YES"
export COMOUT="$ROTDIR/$CDUMP.$PDY/$cyc"
export PCOM="$COMOUT/wmo"
export jlogfile="$ROTDIR/logs/$CDATE/jgfs_awips.log"    

for fhr in $fhrlst; do
    if [ -s $COMOUT/$CDUMP.t${cyc}z.pgrb2b.0p25.f${fhr}.idx ]; then

	fhmin=0
	fhmax=84
	if [ $fhr -ge $fhmin -a $fhr -le $fhmax ] ; then
	    if [[ `expr $fhr % 3` -eq 0 ]]; then
		fhr3=$(printf %03i $fhr)
		export fcsthrs=$fhr3
		export job="jgfs_awips_f${fcsthrs}_20km_${cyc}"
		export DATA="${DATAROOT}/$job"
		$AWIPS20SH
	    fi
	    
	    if [[ `expr $fhr % 6` -eq 0 ]]; then
		export job="jgfs_awips_f${fcsthrs}_${cyc}"
		export DATA="${DATAROOT}/$job"
		$AWIPSG2SH
	    fi
	fi

	fhmin=90
	fhmax=240
	if [ $fhr -ge $fhmin -a $fhr -le $fhmax ]; then
	    
	    if [[ `expr $fhr % 6` -eq 0 ]]; then
		fhr3=$(printf %03i $fhr)
		export fcsthrs=$fhr3
		
		export job="jgfs_awips_f${fcsthrs}_20km_${cyc}"
		export DATA="${DATAROOT}/$job"
		$AWIPS20SH
		
		export job="jgfs_awips_f${fcsthrs}_${cyc}"
		export DATA="${DATAROOT}/$job"
		$AWIPSG2SH
	    fi
	fi
	
    else
        echo "***WARNING*** $COMOUT/$CDUMP.t${cyc}z.pgrb2b.0p25.f${fhr}.idx NOT AVAILABLE"
    fi

done


###############################################################
# Force Exit out cleanly
if [ ${KEEPDATA:-"NO"} = "NO" ] ; then rm -rf $DATAROOT ; fi
exit 0
