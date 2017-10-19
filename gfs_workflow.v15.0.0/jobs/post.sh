#!/bin/ksh -x
###############################################################
# < next few lines under version control, D O  N O T  E D I T >
# $Date$
# $Revision$
# $Author$
# $Id$
###############################################################

###############################################################
## Author: Fanglin Yang   Org: NCEP/EMC  Date: October 2016
##         Rahul Mahajan  Org: NCEP/EMC  Date: April 2017

## Abstract:
## NCEP post driver script
## EXPDIR : /full/path/to/config/files
## CDATE  : current analysis date (YYYYMMDDHH)
## CDUMP  : cycle name (gdas / gfs)
###############################################################

###############################################################
# Source relevant configs
configs="base post"
for config in $configs; do
    . $EXPDIR/config.${config}
    status=$?
    [[ $status -ne 0 ]] && exit $status
done

###############################################################
# Source machine runtime environment
. $BASE_ENV/${machine}.env post
status=$?
[[ $status -ne 0 ]] && exit $status

###############################################################
# Set script and dependency variables
PDY=$(echo $CDATE | cut -c1-8)
cyc=$(echo  $CDATE | cut -c9-10)

export COMROT=$ROTDIR/$CDUMP.$PDY/$cyc

res=$(echo $CASE | cut -c2-)
export JCAP=$((res*2-2))
export LONB=$((4*res))
export LATB=$((2*res))

export pgmout="/dev/null" # exgfs_nceppost.sh.ecf will hang otherwise
export PREFIX="$CDUMP.t${cyc}z."
export SUFFIX=".nemsio"

export DATA=$RUNDIR/$CDATE/$CDUMP/post
[[ -d $DATA ]] && rm -rf $DATA

# Run post job to create analysis grib files
export ATMANL=$ROTDIR/$CDUMP.$PDY/$cyc/${PREFIX}atmanl$SUFFIX
if [ -f $ATMANL ]; then
    export ANALYSIS_POST="YES"
    $POSTJJOBSH
    status=$?
    [[ $status -ne 0 ]] && exit $status
fi

# Run post job to create forecast grib files
if [ $CDUMP = "gfs" -o ${GPOST:-"NO"} = "YES" ]; then
    export ANALYSIS_POST="NO"
    $POSTJJOBSH
    status=$?
    [[ $status -ne 0 ]] && exit $status
fi

###############################################################
# Exit out cleanly
exit 0
