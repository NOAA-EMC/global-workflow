#!/bin/ksh -x
###############################################################
# < next few lines under version control, D O  N O T  E D I T >
# $Date$
# $Revision$
# $Author$
# $Id$
###############################################################

###############################################################
## Author: Rahul Mahajan  Org: NCEP/EMC  Date: August 2017

## Abstract:
## Create FV3 initial conditions from GFS intitial conditions
## EXPDIR : /full/path/to/config/files
## CDATE  : current date (YYYYMMDDHH)
## CDUMP  : cycle name (gdas / gfs)
export EXPDIR=${1:-$EXPDIR}
export CDATE=${2:-$CDATE}
export CDUMP=${3:-$CDUMP}
###############################################################

###############################################################
# Source relevant configs
configs="base fv3ic"
for config in $configs; do
    . $EXPDIR/config.${config}
    status=$?
    [[ $status -ne 0 ]] && exit $status
done

###############################################################
# Source machine runtime environment
. $BASE_ENV/${machine}.env fv3ic
status=$?
[[ $status -ne 0 ]] && exit $status

# Temporary runtime directory
export DATA="$RUNDIR/$CDATE/$CDUMP/fv3ic$$"
[[ -d $DATA ]] && rm -rf $DATA

# Input GFS initial condition files
export INIDIR="$ICSDIR/$CDATE/$CDUMP"
export ATMANL="$ICSDIR/$CDATE/$CDUMP/siganl.${CDUMP}.$CDATE"
export SFCANL="$ICSDIR/$CDATE/$CDUMP/sfcanl.${CDUMP}.$CDATE"
if [ -f $ICSDIR/$CDATE/$CDUMP/nstanl.${CDUMP}.$CDATE ]; then
    export NSTANL="$ICSDIR/$CDATE/$CDUMP/nstanl.${CDUMP}.$CDATE"
fi

# Output FV3 initial condition files
export OUTDIR="$ICSDIR/$CDATE/$CDUMP/$CASE/INPUT"

export OMP_NUM_THREADS_CH=$CHGRESTHREAD
export APRUNC=$APRUN_CHGRES

# Call global_chgres_driver.sh
$BASE_GSM/ush/global_chgres_driver.sh
status=$?
if [ $status -ne 0 ]; then
    echo "global_chgres_driver.sh returned with a non-zero exit code, ABORT!"
    exit $status
fi

###############################################################
# Exit cleanly
exit 0
