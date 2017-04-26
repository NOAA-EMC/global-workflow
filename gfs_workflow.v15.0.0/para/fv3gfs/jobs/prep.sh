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
## Prepare for analysis driver script
## EXPDIR : /full/path/to/config/files
## CDATE  : current analysis date (YYYYMMDDHH)
## CDUMP  : cycle name (gdas / gfs)
###############################################################

###############################################################
# Source relevant configs
configs="base prep"
for config in $configs; do
    . $EXPDIR/config.${config}
    status=$?
    [[ $status -ne 0 ]] && exit $status
done

###############################################################
# Set script and dependency variables

# This is a dummy prep step
# It is simply copying prepbufr and prepbufr.acft_profiles
# from operations and placing it in COMROT
# In the near future, we will create them instead.

cymd=`echo $CDATE | cut -c1-8`
chh=`echo  $CDATE | cut -c9-10`

export OPREFIX="${CDUMP}.t${chh}z."

export COMIN_OBS="$DMPDIR/$CDATE/$CDUMP"
export COMOUT="$ROTDIR/$CDUMP.$cymd/$chh"
[[ ! -d $COMOUT ]] && mkdir -p $COMOUT

$NCP $COMIN_OBS/${OPREFIX}prepbufr               $COMOUT/${OPREFIX}prepbufr
$NCP $COMIN_OBS/${OPREFIX}prepbufr.acft_profiles $COMOUT/${OPREFIX}prepbufr.acft_profiles

###############################################################
# Exit out cleanly
exit 0
