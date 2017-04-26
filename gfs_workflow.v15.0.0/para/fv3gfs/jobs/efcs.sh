#!/bin/ksh -x
###############################################################
# < next few lines under version control, D O  N O T  E D I T >
# $Date$
# $Revision$
# $Author$
# $Id$
###############################################################

###############################################################
## Author: Rahul Mahajan  Org: NCEP/EMC  Date: April 2017

## Abstract:
## Ensemble forecast driver script
## EXPDIR : /full/path/to/config/files
## CDATE  : current analysis date (YYYYMMDDHH)
## CDUMP  : cycle name (gdas / gfs)
## ENSGRP : ensemble sub-group to make forecasts (1, 2, ...)
###############################################################

###############################################################
# Source relevant configs
configs="base fcst efcs"
for config in $configs; do
    . $EXPDIR/config.${config}
    status=$?
    [[ $status -ne 0 ]] && exit $status
done

###############################################################
# Source machine runtime environment
. $BASE_ENV/$machine.env efcs
status=$?
[[ $status -ne 0 ]] && exit $status

###############################################################
# Set script and dependency variables
export CASE=$CASE_ENKF
export COMROT=$ROTDIR
export DATA=$RUNDIR/$CDATE/$CDUMP/efcs.grp$ENSGRP
[[ -d $DATA ]] && rm -rf $DATA

export GDATE=`$NDATE -$assim_freq $CDATE`
chh=`echo $CDATE | cut -c9-10`
ghh=`echo $GDATE | cut -c9-10`

export APREFIX="${CDUMP}.t${chh}z."
export ASUFFIX=".nemsio"
export GPREFIX="${CDUMP}.t${ghh}z."
export GSUFFIX=".nemsio"

# If this is not the first cycle, we warm start the model
if [ $CDATE -gt $SDATE ]; then
    export warm_start=".true."
    export read_increment=".true."
    export ATMGES_FNAME="${GPREFIX}atmf006$GSUFFIX"
    if [ $RECENTER_ENKF = "YES" ]; then
        export ATMANL_FNAME="${APREFIX}ratmanl$ASUFFIX"
    else
        export ATMANL_FNAME="${APREFIX}atmanl$ASUFFIX"
    fi
    export ATMINC_FNAME="${APREFIX}atminc.nc"
else
    export warm_start=".false."
fi

# Since we do not update SST, SNOW or ICE via global_cycle;
# Pass these to the model; it calls surface cycle internally
if [ $warm_start = ".true." ]; then
    export FNTSFA="$DMPDIR/$CDATE/$CDUMP/${CDUMP}.t${chh}z.sstgrb"
    export FNACNA="$DMPDIR/$CDATE/$CDUMP/${CDUMP}.t${chh}z.engicegrb"
    export FNSNOA="$DMPDIR/$CDATE/$CDUMP/${CDUMP}.t${chh}z.snogrb"
fi

# Forecast length for EnKF forecast
export FHMIN=$FHMIN_ENKF
export FHOUT=$FHOUT_ENKF
export FHMAX=$FHMAX_ENKF

# Output frequency
if [ $FHMIN -eq 0 ]; then
    export fdiag_ENKF=$(echo $DELTIM 3600 | awk '{printf "%f", $1/$2}')
else
    export fdiag_ENKF=$FHMIN
fi
FHMIN_=$(($FHMIN + $FHOUT))
for fhr in `seq $FHMIN_ $FHOUT $FHMAX`; do
    export fdiag_ENKF="$fdiag,$fhr"
done

# Get ENSBEG/ENSEND from ENSGRP and NMEM_ENKF_GRP
ENSEND=`echo "$NMEM_ENKF_GRP * $ENSGRP" | bc`
ENSBEG=`echo "$ENSEND - $NMEM_ENKF_GRP + 1" | bc`
export ENSBEG=$ENSBEG
export ENSEND=$ENSEND

###############################################################
# Run relevant exglobal script
$ENKFFCSTSH
status=$?
[[ $status -ne 0 ]] && exit $status

###############################################################
# Exit out cleanly
exit 0
