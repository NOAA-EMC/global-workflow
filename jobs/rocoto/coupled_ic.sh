#!/bin/bash

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
. $HOMEgfs/ush/load_fv3gfs_modules.sh
status=$?
[[ $status -ne 0 ]] && exit $status

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


#TODO: make the following configurable variables instead of hardcoded 
CPL_ATMIC=CFSR
CPL_OCNIC=CPC3Dvar
CPL_ICEIC=CPC
OCNRES=025
ICERES=025

# Create ICSDIR if needed
[[ ! -d $ICSDIR/$CDATE ]] && mkdir -p $ICSDIR/$CDATE
[[ ! -d $ICSDIR/$CDATE/ocn ]] && mkdir -p $ICSDIR/$CDATE/ocn
[[ ! -d $ICSDIR/$CDATE/ice ]] && mkdir -p $ICSDIR/$CDATE/ice


# Setup ATM initial condition files
cp -r $ORIGIN_ROOT/$CPL_ATMIC/$CDATE/$CDUMP  $ICSDIR/$CDATE/

# Setup Ocean IC files 
cp -r $ORIGIN_ROOT/$CPL_OCNIC/$CDATE/ocn/$OCNRES/*  $ICSDIR/$CDATE/ocn/

#Setup Ice IC files 
cp $ORIGIN_ROOT/$CPL_ICEIC/$CDATE/ice/$ICERES/* $ICSDIR/$CDATE/ice/

#TODO for wave coupling
#if cplwav=true 
  #Setup Wave IC files
#fi 


export OUTDIR="$ICSDIR/$CDATE/$CDUMP/$CASE/INPUT"

# Stage the FV3 initial conditions to ROTDIR
COMOUT="$ROTDIR/$CDUMP.$PDY/$cyc"
[[ ! -d $COMOUT ]] && mkdir -p $COMOUT
cd $COMOUT || exit 99
rm -rf INPUT
$NLN $OUTDIR .

##############################################################
# Exit cleanly
exit 0
