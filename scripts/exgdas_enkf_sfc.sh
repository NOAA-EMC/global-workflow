#!/bin/bash
################################################################################
####  UNIX Script Documentation Block
#                      .                                             .
# Script name:         exgdas_enkf_sfc.sh
# Script description:  generate ensemble surface analyses on tiles
#
# Author:        Rahul Mahajan      Org: NCEP/EMC     Date: 2017-03-02
#
# Abstract: This script generates ensemble surface analyses on tiles
#
# $Id$
#
# Attributes:
#   Language: POSIX shell
#   Machine: WCOSS-Cray/Theia
#
################################################################################

# Set environment.
VERBOSE=${VERBOSE:-"YES"}
if [ $VERBOSE = "YES" ]; then
   echo $(date) EXECUTING $0 $* >&2
   set -x
fi

# Directories.
pwd=$(pwd)

# Base variables
CDATE=${CDATE:-"2010010100"}
DONST=${DONST:-"NO"}
DOSFCANL_ENKF=${DOSFCANL_ENKF:-"YES"}
export CASE=${CASE:-384}
ntiles=${ntiles:-6}

# Utilities
NCP=${NCP:-"/bin/cp -p"}
NLN=${NLN:-"/bin/ln -sf"}
NEMSIOGET=${NEMSIOGET:-${NWPROD}/exec/nemsio_get}
NCLEN=${NCLEN:-$HOMEgfs/ush/getncdimlen}

# Scripts

# Executables.

# Files.
OPREFIX=${OPREFIX:-""}
OSUFFIX=${OSUFFIX:-""}
APREFIX=${APREFIX:-""}
APREFIX_ENKF=${APREFIX_ENKF:-$APREFIX}
ASUFFIX=${ASUFFIX:-$SUFFIX}
GPREFIX=${GPREFIX:-""}
GSUFFIX=${GSUFFIX:-$SUFFIX}

# Variables
NMEM_ENKF=${NMEM_ENKF:-80}
DOIAU=${DOIAU_ENKF:-"NO"}

# Global_cycle stuff
CYCLESH=${CYCLESH:-$HOMEgfs/ush/global_cycle.sh}
export CYCLEXEC=${CYCLEXEC:-$HOMEgfs/exec/global_cycle}
APRUN_CYCLE=${APRUN_CYCLE:-${APRUN:-""}}
NTHREADS_CYCLE=${NTHREADS_CYCLE:-${NTHREADS:-1}}
export FIXfv3=${FIXfv3:-$HOMEgfs/fix/fix_fv3_gmted2010}
export FIXgsm=${FIXgsm:-$HOMEgfs/fix/fix_am}
export CYCLVARS=${CYCLVARS:-"FSNOL=-2.,FSNOS=99999.,"}
export FHOUR=${FHOUR:-0}
export DELTSFC=${DELTSFC:-6}

APRUN_ESFC=${APRUN_ESFC:-${APRUN:-""}}
NTHREADS_ESFC=${NTHREADS_ESFC:-${NTHREADS:-1}}


################################################################################
# Preprocessing
mkdata=NO
if [ ! -d $DATA ]; then
   mkdata=YES
   mkdir -p $DATA
fi
cd $DATA || exit 99


################################################################################
# Update surface fields in the FV3 restart's using global_cycle.

PDY=$(echo $CDATE | cut -c1-8)
cyc=$(echo $CDATE | cut -c9-10)

GDATE=$($NDATE -$assim_freq $CDATE)
gPDY=$(echo $GDATE | cut -c1-8)
gcyc=$(echo $GDATE | cut -c9-10)
GDUMP=${GDUMP:-"gdas"}

BDATE=$($NDATE -3 $CDATE)
bPDY=$(echo $BDATE | cut -c1-8)
bcyc=$(echo $BDATE | cut -c9-10)

# Get dimension information based on CASE
res=$(echo $CASE | cut -c2-)
JCAP_CASE=$((res*2-2))
LATB_CASE=$((res*2))
LONB_CASE=$((res*4))

# Global cycle requires these files
export FNTSFA=${FNTSFA:-'                  '}
export FNACNA=${FNACNA:-$COMIN/${OPREFIX}seaice.5min.blend.grb}
export FNSNOA=${FNSNOA:-$COMIN/${OPREFIX}snogrb_t${JCAP_CASE}.${LONB_CASE}.${LATB_CASE}}
[[ ! -f $FNSNOA ]] && export FNSNOA="$COMIN/${OPREFIX}snogrb_t1534.3072.1536"
FNSNOG=${FNSNOG:-$COMIN_GES/${GPREFIX}snogrb_t${JCAP_CASE}.${LONB_CASE}.${LATB_CASE}}
[[ ! -f $FNSNOG ]] && FNSNOG="$COMIN_GES/${GPREFIX}snogrb_t1534.3072.1536"

# Set CYCLVARS by checking grib date of current snogrb vs that of prev cycle
if [ ${RUN_GETGES:-"NO"} = "YES" ]; then
    snoprv=$($GETGESSH -q -t snogrb_$JCAP_CASE -e $gesenvir -n $GDUMP -v $GDATE)
else
    snoprv=${snoprv:-$FNSNOG}
fi

if [ $($WGRIB -4yr $FNSNOA 2>/dev/null | grep -i snowc | awk -F: '{print $3}' | awk -F= '{print $2}') -le \
     $($WGRIB -4yr $snoprv 2>/dev/null | grep -i snowc | awk -F: '{print $3}' | awk -F= '{print $2}') ] ; then
    export FNSNOA=" "
    export CYCLVARS="FSNOL=99999.,FSNOS=99999.,"
else
    export SNOW_NUDGE_COEFF=${SNOW_NUDGE_COEFF:-0.}
    export CYCLVARS="FSNOL=${SNOW_NUDGE_COEFF},$CYCLVARS"
fi

if [ $DONST = "YES" ]; then
    export NST_FILE=${NST_FILE:-$COMIN/${APREFIX}dtfanl.nc}
else
    export NST_FILE="NULL"
fi

export APRUNCY=${APRUN_CYCLE:-$APRUN_ESFC}
export OMP_NUM_THREADS_CY=${NTHREADS_CYCLE:-$NTHREADS_ESFC}
export MAX_TASKS_CY=$NMEM_ENKF

if [ $DOIAU = "YES" ]; then
    # Update surface restarts at beginning of window when IAU is ON
    # For now assume/hold dtfanl.nc is valid at beginning of window.

    for n in $(seq 1 $ntiles); do

        export TILE_NUM=$n

        for imem in $(seq 1 $NMEM_ENKF); do

            cmem=$(printf %03i $imem)
            memchar="mem$cmem"

            [[ $TILE_NUM -eq 1 ]] && mkdir -p $COMOUT_ENS/$memchar/RESTART

            $NLN $COMIN_GES_ENS/$memchar/RESTART/$bPDY.${bcyc}0000.sfc_data.tile${n}.nc $DATA/fnbgsi.$cmem
            $NLN $COMOUT_ENS/$memchar/RESTART/$bPDY.${bcyc}0000.sfcanl_data.tile${n}.nc $DATA/fnbgso.$cmem
            $NLN $FIXfv3/$CASE/${CASE}_grid.tile${n}.nc                                $DATA/fngrid.$cmem
            $NLN $FIXfv3/$CASE/${CASE}_oro_data.tile${n}.nc                            $DATA/fnorog.$cmem

        done

        $CYCLESH
        export err=$?; err_chk

    done

fi

if [ $DOSFCANL_ENKF = "YES" ]; then
 for n in $(seq 1 $ntiles); do

    export TILE_NUM=$n

    for imem in $(seq 1 $NMEM_ENKF); do

        cmem=$(printf %03i $imem)
        memchar="mem$cmem"

        [[ $TILE_NUM -eq 1 ]] && mkdir -p $COMOUT_ENS/$memchar/RESTART

        $NLN $COMIN_GES_ENS/$memchar/RESTART/$PDY.${cyc}0000.sfc_data.tile${n}.nc $DATA/fnbgsi.$cmem
        $NLN $COMOUT_ENS/$memchar/RESTART/$PDY.${cyc}0000.sfcanl_data.tile${n}.nc $DATA/fnbgso.$cmem
        $NLN $FIXfv3/$CASE/${CASE}_grid.tile${n}.nc                               $DATA/fngrid.$cmem
        $NLN $FIXfv3/$CASE/${CASE}_oro_data.tile${n}.nc                           $DATA/fnorog.$cmem

    done

    $CYCLESH
    export err=$?; err_chk

 done
fi

################################################################################

################################################################################
# Postprocessing
cd $pwd
[[ $mkdata = "YES" ]] && rm -rf $DATA
set +x
if [ $VERBOSE = "YES" ]; then
   echo $(date) EXITING $0 with return code $err >&2
fi
exit $err
