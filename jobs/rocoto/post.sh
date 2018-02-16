#!/bin/ksh -x

###############################################################
## Abstract:
## NCEP post driver script
## RUN_ENVIR : runtime environment (emc | nco)
## EXPDIR : /full/path/to/config/files
## CDATE  : current analysis date (YYYYMMDDHH)
## CDUMP  : cycle name (gdas / gfs)
## PDY    : current date (YYYYMMDD)
## cyc    : current cycle (HH)
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
export RUN=$CDUMP

####################################
# SENDSMS  - Flag Events on SMS
# SENDCOM  - Copy Files From TMPDIR to $COMOUT
# SENDDBN  - Issue DBNet Client Calls
# RERUN    - Rerun posts from beginning (default no)
####################################
export SAVEGES=NO
export SENDSMS=NO
export SENDCOM=YES
export SENDDBN=NO
export RERUN=NO

export HOMEglobal=${HOMEpost}
export HOMEgfs=${HOMEpost}
export HOMEgdas=${HOMEpost}

##############################################
# Define COM directories
##############################################
export COMIN=$COMROT
export COMOUT=$COMROT

export APRUN=${APRUN_NP}
export FIXgfs=$HOMEgfs/fix
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
