#!/bin/sh

set -ax

#-------------------------------------------------------------------------------------------------
# Update surface fields in FV3 restart files on the cubed-sphere grid
# George Gayno,  09/XX/2017
# Rahul Mahajan, 10/11/2017
#-------------------------------------------------------------------------------------------------

export machine=${machine:-"WCOSS_C"}

export CASE=${CASE:-C768}                    # resolution of tile: 48, 96, 192, 384, 768, 1152, 3072
export CDATE=${CDATE:-${cdate:-2017031900}}  # format yyyymmddhh yyyymmddhh ...
export CDUMP=${CDUMP:-gfs}                   # gfs or gdas

pwd=$(pwd)
export NWPROD=${NWPROD:-$pwd}
export DMPDIR=${DMPDIR:-$NWPROD}
export HOMEgfs=${HOMEgfs:-$NWPROD/gfs.v15.0.0}
export FIXam=${FIXam:-$HOMEgfs/fix/fix_am}   
export FIXfv3=${FIXfv3:-$HOMEgfs/fix/fix_fv3_gmted2010}

ntiles=${ntiles:-6}
DONST=${DONST:-"NO"}
COMIN=${COMIN:-$pwd}
COMOUT=${COMOUT:-$pwd}

CYCLESH=${CYCLESH:-$HOMEgfs/ush/global_cycle.sh}
export CYCLEXEC=${CYCLEXEC:-$HOMEgfs/exec/global_cycle}
export OMP_NUM_THREADS_CY=${OMP_NUM_THREADS_CY:-24}
export APRUNCY=${APRUNCY:-"time"}
export VERBOSE=${VERBOSE:-"YES"}

export FHOUR=${FHOUR:-0}
export DELTSFC=${DELTSFC:-6}

PDY=$(echo $CDATE | cut -c1-8)
cyc=$(echo $CDATE | cut -c9-10)

export FNTSFA=${FNTSFA:-$DMPDIR/$CDATE/$CDUMP/${CDUMP}.t${cyc}z.rtgssthr.grb}
export FNSNOA=${FNSNOA:-$DMPDIR/$CDATE/$CDUMP/${CDUMP}.t${cyc}z.snogrb_t1534.3072.1536}
export FNACNA=${FNACNA:-$DMPDIR/$CDATE/$CDUMP/${CDUMP}.t${cyc}z.seaice.5min.blend.grb}

export CYCLVARS=${CYCLVARS:-"FSNOL=-2.,FSNOS=99999.,"}

if [ $DONST = "YES" ]; then
    export GSI_FILE=${GSI_FILE:-$COMOUT/dtfanl.nc}
    export ADJT_NST_ONLY=${ADJT_NST_ONLY:-".false."}
else
    export GSI_FILE="NULL"
    export ADJT_NST_ONLY=".false."
fi

CRES=$(echo $CASE | cut -c 2-)
JCAP_CASE=$((2*CRES-2))
LONB_CASE=$((4*CRES))
LATB_CASE=$((2*CRES))

export JCAP=${JCAP:-$JCAP_CASE}
export LONB=${LONB:-$LONB_CASE}
export LATB=${LATB:-$LATB_CASE}

export MAX_TASKS_CY=${MAX_TASKS_CY:-99999}

# Temporary rundirectory
export DATA=${DATA:-$pwd/rundir$$}
rm -fr $DATA
mkdir -p $DATA

for n in $(seq 1 $ntiles); do
  ln -fs $COMIN/$PDY.${cyc}0000.sfc_data.tile${n}.nc      $DATA/fnbgsi.00$n
  ln -fs $COMOUT/$PDY.${cyc}0000.sfcanl_data.tile${n}.nc  $DATA/fnbgso.00$n
  ln -fs $FIXfv3/C${CRES}/C${CRES}_grid.tile${n}.nc       $DATA/fngrid.00$n
  ln -fs $FIXfv3/C${CRES}/C${CRES}_oro_data.tile${n}.nc   $DATA/fnorog.00$n
done

$CYCLESH
rc=$?
if [[ $rc -ne 0 ]] ; then
    echo "***ERROR*** rc= $rc"
    exit $rc
fi

exit 0
