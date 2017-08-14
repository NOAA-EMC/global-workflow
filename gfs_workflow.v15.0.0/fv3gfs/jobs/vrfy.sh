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
cymd=`echo $CDATE | cut -c1-8`
chh=`echo  $CDATE | cut -c9-10`

###############################################################
# Verify Fits
if [ $VRFYFITS = "YES" -a $CDUMP = $CDFNL ]; then
    export CDUMPFCST=$VDUMP
    export TMPDIR="$RUNDIR/$CDUMP/$CDATE/vrfy/fit2obs/tmpdir"
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
        export COMROT="$ROTDIR/vrfyarch/dummy" # vrfyarch/dummy is required because of clumsiness in mkup_rain_stat.sh
        $VSDBSH $xdate $xdate $vlength $chh $PSLOT $CDATE $CDUMP $gfs_cyc
    fi
fi

###############################################################
# Exit out cleanly
exit 0
