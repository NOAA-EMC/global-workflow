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
## Inline verification and diagnostics driver script
## EXPDIR : /full/path/to/config/files
## CDATE  : current analysis date (YYYYMMDDHH)
## CDUMP  : cycle name (gdas / gfs)
###############################################################

###############################################################
# Source relevant configs
configs="base vrfy"
for config in $configs; do
    . $EXPDIR/config.${config}
    status=$?
    [[ $status -ne 0 ]] && exit $status
done

###############################################################
# Source machine runtime environment
. $BASE_ENV/${machine}.env vrfy
status=$?
[[ $status -ne 0 ]] && exit $status

###############################################################

export PDY=$(echo $CDATE | cut -c1-8)
export cyc=$(echo $CDATE | cut -c9-10)
export CDATEm1=$($NDATE -24 $CDATE)
export PDYm1=$(echo $CDATEm1 | cut -c1-8)

export COMIN="$ROTDIR/$CDUMP.$PDY/$cyc"
export DATAROOT="$RUNDIR/$CDATE/$CDUMP/vrfy"
[[ -d $DATAROOT ]] && rm -rf $DATAROOT

###############################################################
# Verify Fits
if [ $VRFYFITS = "YES" -a $CDUMP = $CDFNL ]; then

    export CDUMPFCST=$VDUMP
    export TMPDIR="$RUNDIR/$CDATE/$CDUMP"
    [[ ! -d $TMPDIR ]] && mkdir -p $TMPDIR

    $PREPQFITSH $PSLOT $CDATE $ROTDIR $ARCDIR $TMPDIR

fi

###############################################################
# Run VSDB Step1, Verify precipitation and Grid2Obs
# VSDB_STEP1 and VRFYPRCP works
if [ $CDUMP = "gfs" ]; then

    if [ $VSDB_STEP1 = "YES" -o $VRFYPRCP = "YES" -o $VRFYG2OBS = "YES" ]; then

        xdate=$(echo $($NDATE -${BACKDATEVSDB} $CDATE) | cut -c1-8)
        export ARCDIR1="$NOSCRUB/archive"
        export rundir="$RUNDIR/$CDUMP/$CDATE/vrfy/vsdb_exp"
        export COMROT="$ARCDIR1/dummy"

        $VSDBSH $xdate $xdate $vlength $cyc $PSLOT $CDATE $CDUMP $gfs_cyc

    fi
fi

###############################################################
# Run RadMon data extraction
if [ $VRFYRAD = "YES" -a $CDUMP = $CDFNL ]; then

    export EXP=$PSLOT
    export COMOUT="$ROTDIR/$CDUMP.$PDY/$cyc"
    export jlogfile="$ROTDIR/logs/$CDATE/${CDUMP}radmon.log"
    export TANKverf_rad="$TANKverf/stats/$PSLOT/$CDUMP.$PDY"
    export TANKverf_radM1="$TANKverf/stats/$PSLOT/$CDUMP.$PDYm1"
    export MY_MACHINE=$machine

    $VRFYRADSH

fi

###############################################################
# Run MinMon
if [ $VRFYMINMON = "YES" ]; then

    export COMOUT="$ROTDIR/$CDUMP.$PDY/$cyc"
    export jlogfile="$ROTDIR/logs/$CDATE/${CDUMP}minmon.log"
    export M_TANKverfM0="$M_TANKverf/stats/$PSLOT/$CDUMP.$PDY"
    export M_TANKverfM1="$M_TANKverf/stats/$PSLOT/$CDUMP.$PDYm1"
    export MY_MACHINE=$machine

    $VRFYMINSH

fi

################################################################################
# Verify tracks
if [ $VRFYTRAK = "YES" ]; then

   export DATA="${DATAROOT}/tracker"
   export COMOUT=$ARCDIR

   $TRACKERSH $CDATE $CDUMP $COMOUT $DATA

fi

################################################################################
# Verify genesis
if [ $VRFYGENESIS = "YES" -a $CDUMP = "gfs" ]; then

   export DATA="${DATAROOT}/genesis_tracker"
   export COMOUT=$ARCDIR
   export gfspara=$COMIN

   $GENESISSH $CDATE $CDUMP $COMOUT $DATA

fi

###############################################################
# Force Exit out cleanly
if [ ${KEEPDATA:-"NO"} = "NO" ] ; then rm -rf $DATAROOT ; fi
exit 0
