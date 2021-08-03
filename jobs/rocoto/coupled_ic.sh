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
. $HOMEgfs/ush/load_fv3gfs_modules.sh
status=$?
[[ $status -ne 0 ]] && exit $status
err=0

export DATAROOT="$RUNDIR/$CDATE/$CDUMP"
[[ ! -d $DATAROOT ]] && mkdir -p $DATAROOT

###############################################################
# Source relevant configs
configs="base fv3ic wave"
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

# Create ICSDIR if needed
[[ ! -d $ICSDIR/$CDATE ]] && mkdir -p $ICSDIR/$CDATE
[[ ! -d $ICSDIR/$CDATE/atmos ]] && mkdir -p $ICSDIR/$CDATE/atmos
[[ ! -d $ICSDIR/$CDATE/ocn ]] && mkdir -p $ICSDIR/$CDATE/ocn
[[ ! -d $ICSDIR/$CDATE/ice ]] && mkdir -p $ICSDIR/$CDATE/ice

if [ $ICERES = '025' ]; then
  ICERESdec="0.25"
fi 
if [ $ICERES = '050' ]; then         
 ICERESdec="0.50"        
fi 

# Setup ATM initial condition files
cp -r $ORIGIN_ROOT/$CPL_ATMIC/$CDATE/$CDUMP/*  $ICSDIR/$CDATE/atmos/
rc=$?
if [[ $rc -ne 0 ]] ; then
  echo "FATAL: Unable to copy $ORIGIN_ROOT/$CPL_ATMIC/$CDATE/$CDUMP/* to $ICSDIR/$CDATE/atmos/ (Error code $rc)" 
fi
((err+=$rc))


# Setup Ocean IC files 
cp -r $ORIGIN_ROOT/$CPL_OCNIC/$CDATE/ocn/$OCNRES/MOM*.nc  $ICSDIR/$CDATE/ocn/
rc=$?
if [[ $rc -ne 0 ]] ; then
  echo "FATAL: Unable to copy $ORIGIN_ROOT/$CPL_OCNIC/$CDATE/ocn/$OCNRES/MOM*.nc to $ICSDIR/$CDATE/ocn/ (Error code $rc)"
fi
((err+=$rc))

#Setup Ice IC files 
cp $ORIGIN_ROOT/$CPL_ICEIC/$CDATE/ice/$ICERES/cice5_model_${ICERESdec}.res_$CDATE.nc $ICSDIR/$CDATE/ice/cice_model_${ICERESdec}.res_$CDATE.nc
rc=$?
if [[ $rc -ne 0 ]] ; then
  echo "FATAL: Unable to copy $ORIGIN_ROOT/$CPL_ICEIC/$CDATE/ice/$ICERES/cice5_model_${ICERESdec}.res_$CDATE.nc to $ICSDIR/$CDATE/ice/cice_model_${ICERESdec}.res_$CDATE.nc (Error code $rc)"
fi
((err+=$rc))

if [ $cplwav = ".true." ]; then
  [[ ! -d $ICSDIR/$CDATE/wav ]] && mkdir -p $ICSDIR/$CDATE/wav
  for grdID in $waveGRD
  do
    cp $ORIGIN_ROOT/$CPL_WAVIC/$CDATE/wav/$grdID/*restart.$grdID $ICSDIR/$CDATE/wav/
    rc=$?
    if [[ $rc -ne 0 ]] ; then
      echo "FATAL: Unable to copy $ORIGIN_ROOT/$CPL_WAVIC/$CDATE/wav/$grdID/*restart.$grdID to $ICSDIR/$CDATE/wav/ (Error code $rc)" 
      exit $rc
    fi
    ((err+=$rc))
  done
fi

# Stage the FV3 initial conditions to ROTDIR
export OUTDIR="$ICSDIR/$CDATE/atmos/$CASE/INPUT"
COMOUT="$ROTDIR/$CDUMP.$PDY/$cyc/atmos"
[[ ! -d $COMOUT ]] && mkdir -p $COMOUT
cd $COMOUT || exit 99
rm -rf INPUT
$NLN $OUTDIR .


if  [[ $err -ne 0 ]] ; then 
  echo "Fatal Error: ICs are not properly set-up" 
  exit $err 
fi 

##############################################################
# Exit cleanly

set +x
exit 0
