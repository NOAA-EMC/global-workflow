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
## EnKF update driver script
## EXPDIR : /full/path/to/config/files
## CDATE  : current analysis date (YYYYMMDDHH)
## CDUMP  : cycle name (gdas / gfs)
###############################################################

###############################################################
# Source relevant configs
configs="base anal eupd"
for config in $configs; do
    . $EXPDIR/config.${config}
    status=$?
    [[ $status -ne 0 ]] && exit $status
done

###############################################################
# Source machine runtime environment
. $BASE_ENV/${machine}.env eupd
status=$?
[[ $status -ne 0 ]] && exit $status

###############################################################
# Set script and dependency variables
export GDATE=`$NDATE -$assim_freq $CDATE`

cymd=`echo $CDATE | cut -c1-8`
chh=`echo  $CDATE | cut -c9-10`
gymd=`echo $GDATE | cut -c1-8`
ghh=`echo  $GDATE | cut -c9-10`

export GPREFIX="${CDUMP}.t${ghh}z."
export GSUFFIX=".nemsio"
export APREFIX="${CDUMP}.t${chh}z."
export ASUFFIX=".nemsio"

export COMIN_GES_ENS="$ROTDIR/enkf.$CDUMP.$gymd/$ghh"
export COMOUT_ANL_ENS="$ROTDIR/enkf.$CDUMP.$cymd/$chh"
export DATA="$RUNDIR/$CDATE/$CDUMP/eupd"
[[ -d $DATA ]] && rm -rf $DATA

export ATMGES_ENSMEAN="$COMIN_GES_ENS/${GPREFIX}atmf006.ensmean$GSUFFIX"
if [ ! -f $ATMGES_ENSMEAN ]; then
    echo "FILE MISSING: ATMGES_ENSMEAN = $ATMGES_ENSMEAN"
    exit 1
fi

export LEVS=`$NEMSIOGET $ATMGES_ENSMEAN dimz | awk '{print $2}'`
status=$?
[[ $status -ne 0 ]] && exit $status

###############################################################
# Run relevant exglobal script
$ENKFUPDSH
status=$?
[[ $status -ne 0 ]] && exit $status

###############################################################
# Exit out cleanly
exit 0
