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
export BASE_GSM=${BASE_GSM:-$NWPROD/global_shared}
export FIXgsm=${FIXgsm:-$BASE_GSM/fix/fix_am}
export FIXfv3=${FIXfv3:-$BASE_GSM/fix/fix_fv3}

ntiles=${ntiles:-6}
DONST=${DONST:-"NO"}
COMIN=${COMIN:-$pwd}
COMOUT=${COMOUT:-$pwd}

CYCLESH=${CYCLESH:-$BASE_GSM/ush/global_cycle.sh}
export CYCLEXEC=${CYCLEXEC:-$BASE_GSM/exec/global_cycle}
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
    export NST_ANL=".true."
    export GSI_FILE=${GSI_FILE:-$COMOUT/dtfanl.nc}
else
    export NST_ANL=".false."
fi

CRES=$(echo $CASE | cut -c 2-)
JCAP_CASE=$((2*CRES-2))
LONB_CASE=$((4*CRES))
LATB_CASE=$((2*CRES))

export JCAP=${JCAP:-$JCAP_CASE}
export LONB=${LONB:-$LONB_CASE}
export LATB=${LATB:-$LATB_CASE}

# Temporary rundirectory
export DATA=${DATA:-$pwd/rundir$$}

for n in $(seq 1 $ntiles); do

    export TILE_NUM=$n

    SFCGES_TMP=$COMIN/RESTART/$PDY.${cyc}0000.sfc_data.tile${n}.nc
    SFCANL_TMP=$COMOUT/RESTART/$PDY.${cyc}0000.sfcanl_data.tile${n}.nc

    $CYCLESH $SFCGES_TMP $SFCANL_TMP
    rc=$?
    if [[ $rc -ne 0 ]] ; then
        echo "***ERROR*** rc= $rc"
        exit $rc
    fi

done

exit 0
