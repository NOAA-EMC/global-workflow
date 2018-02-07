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
. $BASE_ENV/${machine}.env efcs
status=$?
[[ $status -ne 0 ]] && exit $status

###############################################################
# Set script and dependency variables
export CASE=$CASE_ENKF
export DATA=$RUNDIR/$CDATE/$CDUMP/efcs.grp$ENSGRP
[[ -d $DATA ]] && rm -rf $DATA

# Get ENSBEG/ENSEND from ENSGRP and NMEM_EFCSGRP
ENSEND=$((NMEM_EFCSGRP * ENSGRP))
ENSBEG=$((ENSEND - NMEM_EFCSGRP + 1))
export ENSBEG=$ENSBEG
export ENSEND=$ENSEND

cymd=$(echo $CDATE | cut -c1-8)
chh=$(echo  $CDATE | cut -c9-10)

export GDATE=$($NDATE -$assim_freq $CDATE)
gymd=$(echo $GDATE | cut -c1-8)
ghh=$(echo  $GDATE | cut -c9-10)

# Default warm_start is OFF
export warm_start=".false."

# If RESTART conditions exist; warm start the model
memchar="mem"$(printf %03i $ENSBEG)
if [ -f $ROTDIR/enkf.${CDUMP}.$gymd/$ghh/$memchar/RESTART/${cymd}.${chh}0000.coupler.res ]; then
    export warm_start=".true."
    if [ -f $ROTDIR/enkf.${CDUMP}.$cymd/$chh/$memchar/${CDUMP}.t${chh}z.atminc.nc ]; then
        export read_increment=".true."
    else
        echo "WARNING: WARM START $CDUMP $CDATE WITHOUT READING INCREMENT!"
    fi
fi

# Forecast length for EnKF forecast
export FHMIN=$FHMIN_ENKF
export FHOUT=$FHOUT_ENKF
export FHMAX=$FHMAX_ENKF

###############################################################
# Run relevant exglobal script
$ENKFFCSTSH
status=$?
[[ $status -ne 0 ]] && exit $status

###############################################################
# Double check the status of members in ENSGRP
EFCSGRP=$ROTDIR/enkf.${CDUMP}.$cymd/$chh/efcs.grp${ENSGRP}
if [ -f $EFCSGRP ]; then
    npass=$(grep "PASS" $EFCSGRP | wc -l)
else
    npass=0
fi
echo "$npass/$NMEM_EFCSGRP members successfull in efcs.grp$ENSGRP"
if [ $npass -ne $NMEM_EFCSGRP ]; then
    echo "ABORT!"
    cat $EFCSGRP
    exit 99
fi

###############################################################
# Exit out cleanly
exit 0
