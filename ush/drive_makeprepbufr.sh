#!/bin/sh -x
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
## Prepare for analysis driver script
## EXPDIR : /full/path/to/config/files
## CDATE  : current analysis date (YYYYMMDDHH)
## CDUMP  : cycle name (gdas / gfs)
###############################################################

###############################################################
# Source relevant configs
configs="base prep prepbufr"
for config in $configs; do
    . $EXPDIR/config.${config}
    status=$?
    [[ $status -ne 0 ]] && exit $status
done

###############################################################
# Source machine runtime environment
. $BASE_ENV/${machine}.env prepbufr
status=$?
[[ $status -ne 0 ]] && exit $status

###############################################################
KEEPDATA=${KEEPDATA:-"NO"}
DO_RELOCATE=${DO_RELOCATE:-"NO"}
DONST=${DONST:-"NO"}

###############################################################
# Set script and dependency variables
GDATE=$($NDATE -$assim_freq $CDATE)

cymd=$(echo $CDATE | cut -c1-8)
chh=$(echo  $CDATE | cut -c9-10)
gymd=$(echo $GDATE | cut -c1-8)
ghh=$(echo  $GDATE | cut -c9-10)

OPREFIX="${CDUMP}.t${chh}z."
OSUFFIX=".bufr_d"
GPREFIX="gdas.t${ghh}z."
GSUFFIX=".nemsio"
APREFIX="${CDUMP}.t${chh}z."
ASUFFIX=".nemsio"

COMIN_OBS=${COMIN_OBS:-"$DMPDIR/$CDATE/$CDUMP"}
COMIN_GES=${COMIN_GES:-"$ROTDIR/gdas.$gymd/$ghh"}
COMOUT=${COMOUT:-"$ROTDIR/$CDUMP.$cymd/$chh"}
[[ ! -d $COMOUT ]] && mkdir -p $COMOUT
export DATA="$RUNDIR/$CDATE/$CDUMP/prepbufr"
[[ -d $DATA ]] && rm -rf $DATA
mkdir -p $DATA
cd $DATA

###############################################################
# MAKEPREPBUFRSH environment specific variables
export NEMSIO_IN=".true."
export COMSP="$DATA/"
export NET=$CDUMP

###############################################################
# Link observation files in BUFRLIST
for bufrname in $BUFRLIST; do
    $NLN $COMIN_OBS/${OPREFIX}${bufrname}.tm00$OSUFFIX ${bufrname}.tm00$OSUFFIX
done

# Link first guess files
$NLN $COMIN_GES/${GPREFIX}atmf003${GSUFFIX} ./atmgm3$GSUFFIX
$NLN $COMIN_GES/${GPREFIX}atmf006${GSUFFIX} ./atmges$GSUFFIX
$NLN $COMIN_GES/${GPREFIX}atmf009${GSUFFIX} ./atmgp3$GSUFFIX

[[ -f $COMIN_GES/${GPREFIX}atmf004${GSUFFIX} ]] && $NLN $COMIN_GES/${GPREFIX}atmf004${GSUFFIX} ./atmgm2$GSUFFIX
[[ -f $COMIN_GES/${GPREFIX}atmf005${GSUFFIX} ]] && $NLN $COMIN_GES/${GPREFIX}atmf005${GSUFFIX} ./atmgm1$GSUFFIX
[[ -f $COMIN_GES/${GPREFIX}atmf007${GSUFFIX} ]] && $NLN $COMIN_GES/${GPREFIX}atmf007${GSUFFIX} ./atmgp1$GSUFFIX
[[ -f $COMIN_GES/${GPREFIX}atmf008${GSUFFIX} ]] && $NLN $COMIN_GES/${GPREFIX}atmf008${GSUFFIX} ./atmgp2$GSUFFIX

# If relocation is turned off: these files don't exist, touch them
if [ $DO_RELOCATE = "NO" ]; then
    touch $DATA/tcvitals.relocate.tm00
    touch $DATA/tropcy_relocation_status.tm00
    echo "RECORDS PROCESSED" >> $DATA/tropcy_relocation_status.tm00
fi

###############################################################
# if PREPDATA is YES and
# 1. the aircft bufr file is not found, set PREPACQC to NO
# 2. the ****** bufr file is not found, set ******** to NO
if [ $PREPDATA = "YES" ]; then
    [[ ! -s aircft.tm00$OSUFFIX ]] && export PREPACQC="NO"
fi

###############################################################
# Execute MAKEPREPBUFRSH

echo $(date) EXECUTING $MAKEPREPBUFRSH $CDATE >&2
$MAKEPREPBUFRSH $CDATE
status=$?
echo $(date) EXITING $MAKEPREPBUFRSH with return code $status >&2
[[ $status -ne 0 ]] && exit $status

###############################################################
# Create nsstbufr file
if [ $DONST = "YES" ]; then
    SFCSHPBF=${SFCSHPBF:-$COMIN_OBS/sfcshp.$CDUMP.$CDATE}
    TESACBF=${TESACBF:-$COMIN_OBS/tesac.$CDUMP.$CDATE}
    BATHYBF=${BATHYBF:-$COMIN_OBS/bathy.$CDUMP.$CDATE}
    TRKOBBF=${TRKOBBF:-$COMIN_OBS/trkob.$CDUMP.$CDATE}
    NSSTBF=${NSSTBF:-$COMOUT/${APREFIX}nsstbufr}

    cat $SFCSHPBF $TESACBF $BATHYBF $TRKOBBF > $NSSTBF
    status=$?
    echo $(date) CREATE $NSSTBF with return code $status >&2

    # NSST bufr file must be restricted since it contains unmasked ship ids
    chmod 640  $NSSTBF
    $CHGRP_CMD $NSSTBF
fi
###############################################################
# Copy prepbufr and prepbufr.acft_profiles to COMOUT
$NCP $DATA/prepda.t${chh}z        $COMOUT/${APREFIX}prepbufr
$NCP $DATA/prepbufr.acft_profiles $COMOUT/${APREFIX}prepbufr.acft_profiles

###############################################################
# Exit out cleanly
if [ $KEEPDATA = "NO" ] ; then rm -rf $DATA ; fi
exit 0
