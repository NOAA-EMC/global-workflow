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
## FHRGRP : forecast hour group to post-process (e.g. 0, 1, 2 ...)
## FHRLST : forecast hourlist to be post-process (e.g. anl, f000, f000_f001_f002, ...)
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
export PDY=$(echo $CDATE | cut -c1-8)
export cyc=$(echo $CDATE | cut -c9-10)

export COMROT=$ROTDIR/$CDUMP.$PDY/$cyc

export pgmout="/dev/null" # exgfs_nceppost.sh.ecf will hang otherwise
export jlogfile=$ROTDIR/logs/jlogfile.${CDUMP}post
export PREFIX="$CDUMP.t${cyc}z."
export SUFFIX=".nemsio"

export DATA=$RUNDIR/$CDATE/$CDUMP/post$FHRGRP
[[ -d $DATA ]] && rm -rf $DATA

# Get the first hour in FHRLST to get dimensions etc.
fhr=$(echo $FHRLST | cut -d "_" -f1)
ATMFNAME=$ROTDIR/$CDUMP.$PDY/$cyc/${PREFIX}atm$fhr$SUFFIX

# Analysis post is always a separate job
if [ $FHRGRP -eq 0 ]; then

    export ANALYSIS_POST="YES"
    if [ ! -f $ATMFNAME ]; then
        echo "$ATMFNAME does not exist, EXITING!"
        exit 0
    fi
    flist_tmp=$ATMFNAME

else

    export ANALYSIS_POST="NO"
    if [ ! -f $ATMFNAME ]; then
        echo "$ATMFNAME does not exist and should, ABORT!"
        exit 99
    fi

    # Convert all "_" into " " in FHRLST, and create list of files to process
    fhrlst=$(echo $FHRLST | sed -e "s/_/ /g")
    flist_tmp=""
    for fhr in $fhrlst; do
        flist_tmp="$flist_tmp $ROTDIR/$CDUMP.$PDY/$cyc/${PREFIX}atm$fhr$SUFFIX"
    done

fi

export LONB=$($NEMSIOGET $ATMFNAME dimx | awk '{print $2}')
status=$?
[[ $status -ne 0 ]] && exit $status
export LATB=$($NEMSIOGET $ATMFNAME dimy | awk '{print $2}')
status=$?
[[ $status -ne 0 ]] && exit $status

if [ $QUILTING = ".false." ]; then
    export JCAP=$($NEMSIOGET $ATMFNAME jcap | awk '{print $2}')
    status=$?
    [[ $status -ne 0 ]] && exit $status
else
    # write component does not add JCAP anymore
    export JCAP=$((LATB-2))
fi

# List of files to post-process in this job are in flist:
export flist=$flist_tmp


#---------------------------------------------------------------
####################################
# Specify RUN Name and model
####################################
export NET=gfs
export RUN=$CDUMP

####################################
# SENDSMS  - Flag Events on SMS
# SENDCOM  - Copy Files From TMPDIR to $COMOUT
# SENDDBN  - Issue DBNet Client Calls
# RERUN    - Rerun posts from beginning (default no)
# VERBOSE  - Specify Verbose Output in global_postgp.sh
####################################
export SAVEGES=NO
export SENDSMS=NO
export SENDCOM=YES
export SENDDBN=NO
export RERUN=NO
export VERBOSE=YES

export HOMEglobal=${BASE_POST}
export HOMEgfs=${BASE_POST}
export HOMEgdas=${BASE_POST}

##############################################
# Define COM directories
##############################################
export COMIN=$COMROT
export COMOUT=$COMROT

export APRUN=${APRUN_NP}
export FIXglobal=$BASE_GSM/fix
export KEEPDATA=${KEEPDATA:-"NO"}
#---------------------------------------------------------------


for fname in $flist; do

    fname=`basename $fname`
    if [ $fname = "${PREFIX}atmanl$SUFFIX" ]; then
        export post_times="anl"
    else
        export post_times=`echo $fname | cut -d. -f3 | cut -c5-`
    fi
 
    $POSTJJOBSH
    status=$?
    [[ $status -ne 0 ]] && exit $status

done

###############################################################
# Exit out cleanly
exit 0
