#!/bin/ksh -x

###############################################################
## Author: Rahul Mahajan  Org: NCEP/EMC  Date: April 2017

## Abstract:
## Do prepatory tasks
## RUN_ENVIR : runtime environment (emc | nco)
## HOMEgfs   : /full/path/to/workflow
## EXPDIR : /full/path/to/config/files
## CDATE  : current analysis date (YYYYMMDDHH)
## CDUMP  : cycle name (gdas / gfs)
## PDY    : current date (YYYYMMDD)
## cyc    : current cycle (HH)
###############################################################

###############################################################
# Source FV3GFS workflow modules
. $HOMEgfs/ush/load_fv3gfs_modules.sh
status=$?
[[ $status -ne 0 ]] && exit $status

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

export OPREFIX="${CDUMP}.t${cyc}z."

export COMOUT="$ROTDIR/$CDUMP.$PDY/$cyc"
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
    $NCP $DMPDIR/$CDATE/$CDUMP/${OPREFIX}prepbufr               $COMOUT/${OPREFIX}prepbufr
    $NCP $DMPDIR/$CDATE/$CDUMP/${OPREFIX}prepbufr.acft_profiles $COMOUT/${OPREFIX}prepbufr.acft_profiles
    [[ $DONST = "YES" ]] && $NCP $DMPDIR/$CDATE/$CDUMP/${OPREFIX}nsstbufr $COMOUT/${OPREFIX}nsstbufr
fi

################################################################################
# Exit out cleanly
exit 0
