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
## EnKF innovations for ensemble members driver script
## EXPDIR : /full/path/to/config/files
## CDATE  : current analysis date (YYYYMMDDHH)
## CDUMP  : cycle name (gdas / gfs)
## ENSGRP : ensemble sub-group to compute innovations (1, 2, ...)
###############################################################

###############################################################
# Source relevant configs
configs="base anal eobs"
for config in $configs; do
    . $EXPDIR/config.${config}
    status=$?
    [[ $status -ne 0 ]] && exit $status
done

###############################################################
# Source machine runtime environment
. $BASE_ENV/${machine}.env eobs
status=$?
[[ $status -ne 0 ]] && exit $status

###############################################################
# Set script and dependency variables
export GDATE=`$NDATE -$assim_freq $CDATE`

cymd=`echo $CDATE | cut -c1-8`
chh=`echo  $CDATE | cut -c9-10`
gymd=`echo $GDATE | cut -c1-8`
ghh=`echo  $GDATE | cut -c9-10`

export OPREFIX="${CDUMP}.t${chh}z."
export APREFIX="${CDUMP}.t${chh}z."
export ASUFFIX=".nemsio"
export GPREFIX="${CDUMP}.t${ghh}z."
export GSUFFIX=".nemsio"

export COMIN_OBS="$DMPDIR/$CDATE/$CDUMP"
export COMIN_GES="$ROTDIR/$CDUMP.$gymd/$ghh"
export COMIN_GES_ENS="$ROTDIR/enkf.$CDUMP.$gymd/$ghh"
export COMOUT="$ROTDIR/enkf.$CDUMP.$cymd/$chh"
export DATA="$RUNDIR/$CDATE/$CDUMP/eomg.grp$ENSGRP"
[[ -d $DATA ]] && rm -rf $DATA

export ATMGES_ENSMEAN="$COMIN_GES_ENS/${GPREFIX}atmf006.ensmean$GSUFFIX"
if [ ! -f $ATMGES_ENSMEAN ]; then
    echo "FILE MISSING: ATMGES_ENSMEAN = $ATMGES_ENSMEAN"
    exit 1
fi

export LEVS=`$NEMSIOGET $ATMGES_ENSMEAN dimz | awk '{print $2}'`
status=$?
[[ $status -ne 0 ]] && exit $status

# Guess Bias correction coefficients related to control
export GBIAS=${COMIN_GES}/${GPREFIX}abias
export GBIASPC=${COMIN_GES}/${GPREFIX}abias_pc
export GBIASAIR=${COMIN_GES}/${GPREFIX}abias_air
export GRADSTAT=${COMIN_GES}/${GPREFIX}radstat

# Use the selected observations from ensemble mean
export RUN_SELECT="NO"
export USE_SELECT="YES"
export SELECT_OBS="$COMOUT/${APREFIX}obsinput.ensmean"

###############################################################
# Get ENSBEG/ENSEND from ENSGRP and NMEM_EOMGGRP
ENSEND=`echo "$NMEM_EOMGGRP * $ENSGRP" | bc`
ENSBEG=`echo "$ENSEND - $NMEM_EOMGGRP + 1" | bc`
export ENSBEG=$ENSBEG
export ENSEND=$ENSEND

# Run relevant exglobal script
$ENKFINVOBSSH
status=$?
[[ $status -ne 0 ]] && exit $status

###############################################################
# Exit out cleanly
exit 0
