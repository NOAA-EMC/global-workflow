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
cyc=$(echo $CDATE | cut -c9-10)

export COMROT=$ROTDIR/$CDUMP.$PDY/$cyc

export pgmout="/dev/null" # exgfs_nceppost.sh.ecf will hang otherwise
export PREFIX="$CDUMP.t${cyc}z."
export SUFFIX=".nemsio"

export DATA=$RUNDIR/$CDATE/$CDUMP/post
[[ -d $DATA ]] && rm -rf $DATA

# Get metadata JCAP, LONB, LATB from ATMF00
ATMF00=$ROTDIR/$CDUMP.$PDY/$cyc/${PREFIX}atmf000$SUFFIX
if [ ! -f $ATMF00 ]; then
    echo "$ATMF00 does not exist and should, ABORT!"
    exit 99
fi

if [ $QUILTING = ".false." ]; then
    export JCAP=$($NEMSIOGET $ATMF00 jcap | awk '{print $2}')
    status=$?
    [[ $status -ne 0 ]] && exit $status
else
    # write component does not add JCAP anymore
    res=$(echo $CASE | cut -c2-)
    export JCAP=$((res*2-2))
fi

[[ $status -ne 0 ]] && exit $status
export LONB=$($NEMSIOGET $ATMF00 dimx | awk '{print $2}')
status=$?
[[ $status -ne 0 ]] && exit $status
export LATB=$($NEMSIOGET $ATMF00 dimy | awk '{print $2}')
status=$?
[[ $status -ne 0 ]] && exit $status

# Run post job to create analysis grib files
export ATMANL=$ROTDIR/$CDUMP.$PDY/$cyc/${PREFIX}atmanl$SUFFIX
if [ -f $ATMANL ]; then
    export ANALYSIS_POST="YES"
    $POSTJJOBSH
    status=$?
    [[ $status -ne 0 ]] && exit $status
fi

# Run post job to create forecast grib files
export ANALYSIS_POST="NO"
$POSTJJOBSH
status=$?
[[ $status -ne 0 ]] && exit $status

###############################################################
# Exit out cleanly
exit 0
