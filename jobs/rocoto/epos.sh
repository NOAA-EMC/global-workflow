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
## Ensemble post-processing driver script
## EXPDIR : /full/path/to/config/files
## CDATE  : current analysis date (YYYYMMDDHH)
## CDUMP  : cycle name (gdas / gfs)
###############################################################

###############################################################
# Source relevant configs
configs="base epos"
for config in $configs; do
    . $EXPDIR/config.${config}
    status=$?
    [[ $status -ne 0 ]] && exit $status
done

###############################################################
# Source machine runtime environment
. $BASE_ENV/${machine}.env epos
status=$?
[[ $status -ne 0 ]] && exit $status

###############################################################
# Set script and dependency variables
cymd=$(echo $CDATE | cut -c1-8)
chh=$(echo  $CDATE | cut -c9-10)

export PREFIX="${CDUMP}.t${chh}z."
export SUFFIX=".nemsio"

export COMIN="$ROTDIR/enkf.$CDUMP.$cymd/$chh"
export COMOUT="$ROTDIR/enkf.$CDUMP.$cymd/$chh"
export DATA="$RUNDIR/$CDATE/$CDUMP/epos"
[[ -d $DATA ]] && rm -rf $DATA

export LEVS=$((LEVS-1))

###############################################################
# Run relevant exglobal script
$ENKFPOSTSH
status=$?
[[ $status -ne 0 ]] && exit $status

###############################################################
# Exit out cleanly
exit 0
