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

###############################################################
# Source relevant configs
configs="base coupled_ic wave"
for config in $configs; do
    . $EXPDIR/config.${config}
    status=$?
    [[ $status -ne 0 ]] && exit $status
done

###############################################################
# Source machine runtime environment
. $BASE_ENV/${machine}.env config.coupled_ic
status=$?
[[ $status -ne 0 ]] && exit $status

# Create ICSDIR if needed
[[ ! -d $ICSDIR/$CDATE ]] && mkdir -p $ICSDIR/$CDATE
if [ $ATMTYPE = "MODEL" ]; then
  [[ ! -d $ICSDIR/$CDATE/atmos ]] && mkdir -p $ICSDIR/$CDATE/atmos
fi
if [ $ATMTYPE = "DATA" ]; then
  [[ ! -d $ICSDIR/$CDATE/datm ]] && mkdir -p $ICSDIR/$CDATE/datm
fi
[[ ! -d $ICSDIR/$CDATE/ocn ]] && mkdir -p $ICSDIR/$CDATE/ocn
[[ ! -d $ICSDIR/$CDATE/ice ]] && mkdir -p $ICSDIR/$CDATE/ice

if [ $ICERES = '025' ]; then
  ICERESdec="0.25"
fi 
if [ $ICERES = '050' ]; then         
  ICERESdec="0.50"        
fi 

if [ $ATMTYPE = "MODEL" ]; then
  # Setup ATM initial condition files
  cp -r $BASE_CPLIC/$CPL_ATMIC/$CDATE/$CDUMP/*  $ICSDIR/$CDATE/atmos/
  rc=$?
  if [[ $rc -ne 0 ]] ; then
    echo "FATAL: Unable to copy $BASE_CPLIC/$CPL_ATMIC/$CDATE/$CDUMP/* to $ICSDIR/$CDATE/atmos/ (Error code $rc)" 
  fi
  ((err+=$rc))
fi

if [ $ATMTYPE = "DATA" ]; then
  # Setup DATM forcing files
  cp -r $BASE_CPLIC/${CPL_DATM}/${datm_src}/$CDATE/${datm_src}.*.nc $ICSDIR/$CDATE/datm/
fi

# Setup Ocean IC files 
cp -r $BASE_CPLIC/$CPL_OCNIC/$CDATE/ocn/$OCNRES/MOM*.nc  $ICSDIR/$CDATE/ocn/
rc=$?
if [[ $rc -ne 0 ]] ; then
  echo "FATAL: Unable to copy $BASE_CPLIC/$CPL_OCNIC/$CDATE/ocn/$OCNRES/MOM*.nc to $ICSDIR/$CDATE/ocn/ (Error code $rc)"
fi
((err+=$rc))

#Setup Ice IC files 
cp $BASE_CPLIC/$CPL_ICEIC/$CDATE/ice/$ICERES/cice5_model_${ICERESdec}.res_$CDATE.nc $ICSDIR/$CDATE/ice/cice_model_${ICERESdec}.res_$CDATE.nc
rc=$?
if [[ $rc -ne 0 ]] ; then
  echo "FATAL: Unable to copy $BASE_CPLIC/$CPL_ICEIC/$CDATE/ice/$ICERES/cice5_model_${ICERESdec}.res_$CDATE.nc to $ICSDIR/$CDATE/ice/cice_model_${ICERESdec}.res_$CDATE.nc (Error code $rc)"
fi
((err+=$rc))

if [ $DO_WAVE = "YES" ]; then
  [[ ! -d $ICSDIR/$CDATE/wav ]] && mkdir -p $ICSDIR/$CDATE/wav
  for grdID in $waveGRD
  do
    cp $BASE_CPLIC/$CPL_WAVIC/$CDATE/wav/$grdID/*restart.$grdID $ICSDIR/$CDATE/wav/
    rc=$?
    if [[ $rc -ne 0 ]] ; then
      echo "FATAL: Unable to copy $BASE_CPLIC/$CPL_WAVIC/$CDATE/wav/$grdID/*restart.$grdID to $ICSDIR/$CDATE/wav/ (Error code $rc)" 
    fi
    ((err+=$rc))
  done
fi

if [ $ATMTYPE = "MODEL" ]; then
  # Stage the FV3 initial conditions to ROTDIR
  export OUTDIR="$ICSDIR/$CDATE/atmos/$CASE/INPUT"
  COMOUT="$ROTDIR/$CDUMP.$PDY/$cyc/atmos"
  [[ ! -d $COMOUT ]] && mkdir -p $COMOUT
  cd $COMOUT || exit 99
  rm -rf INPUT
  $NLN $OUTDIR .
fi

#Stage the WW3 initial conditions to ROTDIR 
if [ $DO_WAVE = "YES" ]; then
  export OUTDIRw="$ICSDIR/$CDATE/wav"
  COMOUTw="$ROTDIR/$CDUMP.$PDY/$cyc/wave/restart"
  [[ ! -d $COMOUTw ]] && mkdir -p $COMOUTw
  cd $COMOUTw || exit 99
  $NLN $OUTDIRw/* .
fi

if  [[ $err -ne 0 ]] ; then 
  echo "Fatal Error: ICs are not properly set-up" 
  exit $err 
fi 

##############################################################
# Exit cleanly

set +x
exit 0
