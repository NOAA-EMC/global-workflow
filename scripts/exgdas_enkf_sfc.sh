#! /usr/bin/env bash

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
#
################################################################################

source "${USHgfs}/preamble.sh"

# Directories.
pwd=$(pwd)

# Base variables
DONST=${DONST:-"NO"}
GSI_SOILANAL=${GSI_SOILANAL:-"NO"}
DOSFCANL_ENKF=${DOSFCANL_ENKF:-"YES"}
export CASE=${CASE:-384}
ntiles=${ntiles:-6}

# Utilities
NCP=${NCP:-"/bin/cp -p"}
NLN=${NLN:-"/bin/ln -sf"}
NCLEN=${NCLEN:-${USHgfs}/getncdimlen}

# Scripts

# Executables.

# Files.
OPREFIX=${OPREFIX:-""}
OSUFFIX=${OSUFFIX:-""}
APREFIX=${APREFIX:-""}
APREFIX_ENS=${APREFIX_ENS:-$APREFIX}
GPREFIX=${GPREFIX:-""}
GPREFIX_ENS=${GPREFIX_ENS:-${GPREFIX}}

# Variables
NMEM_ENS_MAX=${NMEM_ENS:-80}
if [ "${RUN}" = "enkfgfs" ]; then
   NMEM_ENS=${NMEM_ENS_GFS:-30}
   ec_offset=${NMEM_ENS_GFS_OFFSET:-20}
   mem_offset=$((ec_offset * cyc/6))
else
   NMEM_ENS=${NMEM_ENS:-80}
   mem_offset=0
fi
DOIAU=${DOIAU_ENKF:-"NO"}

# Global_cycle stuff
CYCLESH=${CYCLESH:-${USHgfs}/global_cycle.sh}
export CYCLEXEC=${CYCLEXEC:-${EXECgfs}/global_cycle}
APRUN_CYCLE=${APRUN_CYCLE:-${APRUN:-""}}
NTHREADS_CYCLE=${NTHREADS_CYCLE:-${NTHREADS:-1}}
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

# Ignore possible spelling error (nothing is misspelled)
# shellcheck disable=SC2153
BDATE=$(${NDATE} -3 "${PDY}${cyc}")
bPDY=${BDATE:0:8}
bcyc=${BDATE:8:2}

# Get dimension information based on CASE
res=${CASE:2:}
JCAP_CASE=$((res*2-2))
LATB_CASE=$((res*2))
LONB_CASE=$((res*4))

# Global cycle requires these files
export FNTSFA=${FNTSFA:-'                  '}
export FNACNA=${FNACNA:-${COM_OBS}/${OPREFIX}seaice.5min.blend.grb}
export FNSNOA=${FNSNOA:-${COM_OBS}/${OPREFIX}snogrb_t${JCAP_CASE}.${LONB_CASE}.${LATB_CASE}}
[[ ! -f $FNSNOA ]] && export FNSNOA="${COM_OBS}/${OPREFIX}snogrb_t1534.3072.1536"
FNSNOG=${FNSNOG:-${COM_OBS_PREV}/${GPREFIX}snogrb_t${JCAP_CASE}.${LONB_CASE}.${LATB_CASE}}
[[ ! -f $FNSNOG ]] && FNSNOG="${COM_OBS_PREV}/${GPREFIX}snogrb_t1534.3072.1536"

# Set CYCLVARS by checking grib date of current snogrb vs that of prev cycle
if [ ${RUN_GETGES:-"NO"} = "YES" ]; then
    # Ignore possible spelling error (nothing is misspelled)
    # shellcheck disable=SC2153
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
    export NST_FILE=${NST_FILE:-${COM_ATMOS_ANALYSIS_DET}/${APREFIX}dtfanl.nc}
else
    export NST_FILE="NULL"
fi

export APRUNCY=${APRUN_CYCLE:-$APRUN_ESFC}
export OMP_NUM_THREADS_CY=${NTHREADS_CYCLE:-$NTHREADS_ESFC}
export MAX_TASKS_CY=$NMEM_ENS

if [ $DOIAU = "YES" ]; then
    # Update surface restarts at beginning of window when IAU is ON
    # For now assume/hold dtfanl.nc is valid at beginning of window.

    for n in $(seq 1 $ntiles); do

        export TILE_NUM=$n

        for imem in $(seq 1 $NMEM_ENS); do
            smem=$((imem + mem_offset))
            if (( smem > NMEM_ENS_MAX )); then
               smem=$((smem - NMEM_ENS_MAX))
            fi
            gmemchar="mem"$(printf %03i "$smem")
            cmem=$(printf %03i $imem)
            memchar="mem$cmem"

            MEMDIR=${memchar} YMD=${PDY} HH=${cyc} declare_from_tmpl \
                COM_ATMOS_RESTART_MEM:COM_ATMOS_RESTART_TMPL

            MEMDIR=${gmemchar} RUN=${GDUMP_ENS} YMD=${gPDY} HH=${gcyc} declare_from_tmpl \
                COM_ATMOS_RESTART_MEM_PREV:COM_ATMOS_RESTART_TMPL

            MEMDIR=${memchar} YMD=${PDY} HH=${cyc} declare_from_tmpl \
                COM_ATMOS_ANALYSIS_MEM:COM_ATMOS_ANALYSIS_TMPL

            [[ ${TILE_NUM} -eq 1 ]] && mkdir -p "${COM_ATMOS_RESTART_MEM}"
            ${NCP} "${COM_ATMOS_RESTART_MEM_PREV}/${bPDY}.${bcyc}0000.sfc_data.tile${n}.nc" \
                "${COM_ATMOS_RESTART_MEM}/${bPDY}.${bcyc}0000.sfcanl_data.tile${n}.nc"
            ${NLN} "${COM_ATMOS_RESTART_MEM_PREV}/${bPDY}.${bcyc}0000.sfc_data.tile${n}.nc" \
                "${DATA}/fnbgsi.${cmem}"
            ${NLN} "${COM_ATMOS_RESTART_MEM}/${bPDY}.${bcyc}0000.sfcanl_data.tile${n}.nc" \
                "${DATA}/fnbgso.${cmem}"
            ${NLN} "${FIXgfs}/orog/${CASE}/${CASE}_grid.tile${n}.nc"     "${DATA}/fngrid.${cmem}"
            ${NLN} "${FIXgfs}/orog/${CASE}/${CASE}.mx${OCNRES}_oro_data.tile${n}.nc" "${DATA}/fnorog.${cmem}"

            if [[ ${GSI_SOILANAL} = "YES" ]]; then
                FHR=6
                ${NLN} "${COM_ATMOS_ANALYSIS_MEM}/${APREFIX_ENS}sfci00${FHR}.nc" \
                   "${DATA}/lnd_incr.${cmem}"
            fi
        done # ensembles

        CDATE="${PDY}${cyc}" ${CYCLESH}
        export err=$?; err_chk

    done

fi

if [ $DOSFCANL_ENKF = "YES" ]; then
    for n in $(seq 1 $ntiles); do

        export TILE_NUM=$n

        for imem in $(seq 1 $NMEM_ENS); do
            smem=$((imem + mem_offset))
            if (( smem > NMEM_ENS_MAX )); then
               smem=$((smem - NMEM_ENS_MAX))
            fi
            gmemchar="mem"$(printf %03i "$smem")
            cmem=$(printf %03i $imem)
            memchar="mem$cmem"

            MEMDIR=${memchar} YMD=${PDY} HH=${cyc} declare_from_tmpl \
                COM_ATMOS_RESTART_MEM:COM_ATMOS_RESTART_TMPL

            RUN="${GDUMP_ENS}" MEMDIR=${gmemchar} YMD=${gPDY} HH=${gcyc} declare_from_tmpl \
                COM_ATMOS_RESTART_MEM_PREV:COM_ATMOS_RESTART_TMPL

            [[ ${TILE_NUM} -eq 1 ]] && mkdir -p "${COM_ATMOS_RESTART_MEM}"

            ${NCP} "${COM_ATMOS_RESTART_MEM_PREV}/${PDY}.${cyc}0000.sfc_data.tile${n}.nc" \
                "${COM_ATMOS_RESTART_MEM}/${PDY}.${cyc}0000.sfcanl_data.tile${n}.nc"
            ${NLN} "${COM_ATMOS_RESTART_MEM_PREV}/${PDY}.${cyc}0000.sfc_data.tile${n}.nc" \
                "${DATA}/fnbgsi.${cmem}"
            ${NLN} "${COM_ATMOS_RESTART_MEM}/${PDY}.${cyc}0000.sfcanl_data.tile${n}.nc" \
                "${DATA}/fnbgso.${cmem}"
            ${NLN} "${FIXgfs}/orog/${CASE}/${CASE}_grid.tile${n}.nc"      "${DATA}/fngrid.${cmem}"
            ${NLN} "${FIXgfs}/orog/${CASE}/${CASE}.mx${OCNRES}_oro_data.tile${n}.nc" "${DATA}/fnorog.${cmem}"

        done

        CDATE="${PDY}${cyc}" ${CYCLESH}
        export err=$?; err_chk

    done
fi

################################################################################

################################################################################
# Postprocessing
cd $pwd
[[ $mkdata = "YES" ]] && rm -rf $DATA


exit $err
