#!/bin/bash

set -x

###############################################################
## Abstract:
## Create FV3 initial conditions from GFS intitial conditions
## RUN_ENVIR : runtime environment (emc | nco)
## HOMEgfs   : /full/path/to/workflow
## EXPDIR : /full/path/to/config/files
## CDATE  : current date (YYYYMMDDHH)
## CDUMP  : cycle name (gdas / gfs)
## PDY    : current date (YYYYMMDD)
## cyc    : current cycle (HH)
###############################################################

###############################################################
# Source FV3GFS workflow modules
. $USHgfs/load_fv3gfs_modules.sh
status=$?
[[ $status -ne 0 ]] && exit $status

###############################################################
# Source relevant configs
configs="base"
for config in $configs; do
    . $EXPDIR/config.${config}
    status=$?
    [[ $status -ne 0 ]] && exit $status
done

###############################################################
# Source machine runtime environment
. $BASE_ENV/${machine}.env init_aerosol
status=$?
[[ $status -ne 0 ]] && exit $status

# export DATAROOT="$RUNDIR/$CDATE/$CDUMP"
# [[ ! -d $DATAROOT ]] && mkdir -p $DATAROOT
# export DATA=${DATA:-${DATAROOT}/${jobid:?}}
# mkdir -p $DATA
# cd $DATA

$SCRgfs/exgfs_chem_init_aerosol.py

status=$?
if [[ $status -ne 0 ]]; then
    echo "FATAL ERROR: exgfs_chem_init_aerosol.py failed with error code $status"
    exit $status
fi

##############################################################
# Exit cleanly

set +x
exit 0
