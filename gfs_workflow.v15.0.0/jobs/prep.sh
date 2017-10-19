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
## Do prepatory tasks
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

export CHGRP_CMD=${CHGRP_CMD:-"chgrp ${group_name:-rstprod}"}
###############################################################
# Source machine runtime environment
. $BASE_ENV/${machine}.env prep
status=$?
[[ $status -ne 0 ]] && exit $status

###############################################################
# Set script and dependency variables

cymd=$(echo $CDATE | cut -c1-8)
chh=$(echo  $CDATE | cut -c9-10)

export OPREFIX="${CDUMP}.t${chh}z."

export COMIN_OBS="$DMPDIR/$CDATE/$CDUMP"
export COMOUT="$ROTDIR/$CDUMP.$cymd/$chh"
[[ ! -d $COMOUT ]] && mkdir -p $COMOUT

# Do relocation
if [ $DO_RELOCATE = "YES" ]; then
    $DRIVE_RELOCATESH
    echo "RELOCATION IS TURNED OFF in FV3, DRIVE_RELOCATESH does not exist, ABORT!"
    status=1
    [[ $status -ne 0 ]] && exit $status
fi

# Generate prepbufr files from dumps or copy from OPS
if [ $DO_MAKEPREPBUFR = "YES" ]; then
    $DRIVE_MAKEPREPBUFRSH
    [[ $status -ne 0 ]] && exit $status
else
    $NCP $COMIN_OBS/${OPREFIX}prepbufr               $COMOUT/${OPREFIX}prepbufr
    $NCP $COMIN_OBS/${OPREFIX}prepbufr.acft_profiles $COMOUT/${OPREFIX}prepbufr.acft_profiles
    [[ $DONST = "YES" ]] && $NCP $COMIN_OBS/${OPREFIX}nsstbufr $COMOUT/${OPREFIX}nsstbufr
fi

################################################################################
# Exit out cleanly
exit 0
