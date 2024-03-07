#! /usr/bin/env bash

################################################################################
####  UNIX Script Documentation Block
#                      .                                             .
# Script name:         exglobal_atmos_sfcanl.sh
# Script description:  Makes global model surface analysis files
#
# Author: Russ Treadon      Org: NCEP/EMC     Date: 2021-12-13
#
# Abstract: This script makes global model surface analysis files
#
# $Id$
#
# Attributes:
#   Language: POSIX shell
#
################################################################################

#  Set environment.

source "${USHgfs}/preamble.sh"

#  Directories.
pwd=$(pwd)

# Derived base variables
# Ignore possible spelling error (nothing is misspelled)
# shellcheck disable=SC2153
GDATE=$(${NDATE} -"${assim_freq}" "${PDY}${cyc}")
BDATE=$(${NDATE} -3 "${PDY}${cyc}")
bPDY=${BDATE:0:8}
bcyc=${BDATE:8:2}

# Utilities
export NCP=${NCP:-"/bin/cp"}
export NMV=${NMV:-"/bin/mv"}
export NLN=${NLN:-"/bin/ln -sf"}
export CHGRP_CMD=${CHGRP_CMD:-"chgrp ${group_name:-rstprod}"}
export NCLEN=${NCLEN:-${USHgfs}/getncdimlen}
COMPRESS=${COMPRESS:-gzip}
UNCOMPRESS=${UNCOMPRESS:-gunzip}
APRUNCFP=${APRUNCFP:-""}

# IAU
DOIAU=${DOIAU:-"NO"}
export IAUFHRS=${IAUFHRS:-"6"}

# Surface cycle related parameters
CYCLESH=${CYCLESH:-${USHgfs}/global_cycle.sh}
export CYCLEXEC=${CYCLEXEC:-${EXECgfs}/global_cycle}
NTHREADS_CYCLE=${NTHREADS_CYCLE:-24}
APRUN_CYCLE=${APRUN_CYCLE:-${APRUN:-""}}
export SNOW_NUDGE_COEFF=${SNOW_NUDGE_COEFF:-'-2.'}
export CYCLVARS=${CYCLVARS:-""}
export FHOUR=${FHOUR:-0}
export DELTSFC=${DELTSFC:-6}

# FV3 specific info (required for global_cycle)
export CASE=${CASE:-"C384"}
ntiles=${ntiles:-6}

# IAU
DOIAU=${DOIAU:-"NO"}
export IAUFHRS=${IAUFHRS:-"6"}

# Dependent Scripts and Executables
export NTHREADS_CALCINC=${NTHREADS_CALCINC:-1}
export APRUN_CALCINC=${APRUN_CALCINC:-${APRUN:-""}}
export APRUN_CALCANL=${APRUN_CALCANL:-${APRUN:-""}}
export APRUN_CHGRES=${APRUN_CALCANL:-${APRUN:-""}}

export CALCANLEXEC=${CALCANLEXEC:-${EXECgfs}/calc_analysis.x}
export CHGRESNCEXEC=${CHGRESNCEXEC:-${EXECgfs}/enkf_chgres_recenter_nc.x}
export CHGRESINCEXEC=${CHGRESINCEXEC:-${EXECgfs}/interp_inc.x}
export NTHREADS_CHGRES=${NTHREADS_CHGRES:-1}
CALCINCPY=${CALCINCPY:-${USHgfs}/calcinc_gfs.py}
CALCANLPY=${CALCANLPY:-${USHgfs}/calcanl_gfs.py}

export APRUN_CHGRES=${APRUN_CALCANL:-${APRUN:-""}}
CHGRESEXEC=${CHGRESEXEC:-${EXECgfs}/enkf_chgres_recenter.x}

# OPS flags
RUN=${RUN:-""}
SENDECF=${SENDECF:-"NO"}
SENDDBN=${SENDDBN:-"NO"}
RUN_GETGES=${RUN_GETGES:-"NO"}
GETGESSH=${GETGESSH:-"getges.sh"}
export gesenvir=${gesenvir:-${envir}}

# Observations
OPREFIX=${OPREFIX:-""}
OSUFFIX=${OSUFFIX:-""}

# Guess files
GPREFIX=${GPREFIX:-""}

# Analysis files
export APREFIX=${APREFIX:-""}
DTFANL=${DTFANL:-${COM_ATMOS_ANALYSIS}/${APREFIX}dtfanl.nc}

# Get dimension information based on CASE
res=$(echo ${CASE} | cut -c2-)
JCAP_CASE=$((res*2-2))
LATB_CASE=$((res*2))
LONB_CASE=$((res*4))

################################################################################
#  Preprocessing
mkdata=NO
if [[ ! -d ${DATA} ]]; then
   mkdata=YES
   mkdir -p ${DATA}
fi

cd ${DATA} || exit 99

if [[ ${DONST} = "YES" ]]; then
    export NSSTBF="${COM_OBS}/${OPREFIX}nsstbufr"
    ${NLN} ${NSSTBF} nsstbufr
fi


##############################################################
# Required model guess files


##############################################################
# Output files
if [[ ${DONST} = "YES" ]]; then
   ${NLN} ${DTFANL} dtfanl
fi


##############################################################
# Update surface fields in the FV3 restart's using global_cycle
mkdir -p "${COM_ATMOS_RESTART}"

# Global cycle requires these files
export FNTSFA=${FNTSFA:-${COM_OBS}/${OPREFIX}rtgssthr.grb}
export FNACNA=${FNACNA:-${COM_OBS}/${OPREFIX}seaice.5min.blend.grb}
export FNSNOA=${FNSNOA:-${COM_OBS}/${OPREFIX}snogrb_t${JCAP_CASE}.${LONB_CASE}.${LATB_CASE}}
[[ ! -f ${FNSNOA} ]] && export FNSNOA="${COM_OBS}/${OPREFIX}snogrb_t1534.3072.1536"
FNSNOG=${FNSNOG:-${COM_OBS_PREV}/${GPREFIX}snogrb_t${JCAP_CASE}.${LONB_CASE}.${LATB_CASE}}
[[ ! -f ${FNSNOG} ]] && FNSNOG="${COM_OBS_PREV}/${GPREFIX}snogrb_t1534.3072.1536"

# Set CYCLVARS by checking grib date of current snogrb vs that of prev cycle
if [[ ${RUN_GETGES} = "YES" ]]; then
    snoprv=$(${GETGESSH} -q -t snogrb_${JCAP_CASE} -e ${gesenvir} -n ${GDUMP} -v ${GDATE})
else
    snoprv=${snoprv:-${FNSNOG}}
fi

if [[ $(${WGRIB} -4yr ${FNSNOA} 2>/dev/null | grep -i snowc | awk -F: '{print $3}' | awk -F= '{print $2}') -le \
    $(${WGRIB} -4yr ${snoprv} 2>/dev/null | grep -i snowc | awk -F: '{print $3}' | awk -F= '{print $2}') ]] ; then
    export FNSNOA=" "
    export CYCLVARS="FSNOL=99999.,FSNOS=99999.,"
else
    export SNOW_NUDGE_COEFF=${SNOW_NUDGE_COEFF:-0.}
    export CYCLVARS="FSNOL=${SNOW_NUDGE_COEFF},${CYCLVARS}"
fi

if [[ ${DONST} = "YES" ]]; then
    export NST_FILE=${GSI_FILE:-${COM_ATMOS_ANALYSIS}/${APREFIX}dtfanl.nc}
else
    export NST_FILE="NULL"
fi

if [[ ${DOIAU} = "YES" ]]; then
    # update surface restarts at the beginning of the window, if IAU
    # For now assume/hold dtfanl.nc valid at beginning of window
    for n in $(seq 1 ${ntiles}); do
        ${NCP} "${COM_ATMOS_RESTART_PREV}/${bPDY}.${bcyc}0000.sfc_data.tile${n}.nc" \
                "${COM_ATMOS_RESTART}/${bPDY}.${bcyc}0000.sfcanl_data.tile${n}.nc"
        ${NLN} "${COM_ATMOS_RESTART_PREV}/${bPDY}.${bcyc}0000.sfc_data.tile${n}.nc" "${DATA}/fnbgsi.00${n}"
        ${NLN} "${COM_ATMOS_RESTART}/${bPDY}.${bcyc}0000.sfcanl_data.tile${n}.nc"   "${DATA}/fnbgso.00${n}"
        ${NLN} "${FIXgfs}/orog/${CASE}/${CASE}_grid.tile${n}.nc"                         "${DATA}/fngrid.00${n}"
        ${NLN} "${FIXgfs}/orog/${CASE}/${CASE}.mx${OCNRES}_oro_data.tile${n}.nc"                     "${DATA}/fnorog.00${n}"
    done

    export APRUNCY=${APRUN_CYCLE}
    export OMP_NUM_THREADS_CY=${NTHREADS_CYCLE}
    export MAX_TASKS_CY=${ntiles}

    CDATE="${PDY}${cyc}" ${CYCLESH}
    export err=$?; err_chk
fi

# Update surface restarts at middle of window
for n in $(seq 1 ${ntiles}); do
    if [[ ${DO_JEDISNOWDA:-"NO"} = "YES" ]]; then
        ${NCP} "${COM_SNOW_ANALYSIS}/${PDY}.${cyc}0000.sfc_data.tile${n}.nc" \
               "${COM_ATMOS_RESTART}/${PDY}.${cyc}0000.sfcanl_data.tile${n}.nc"
    else
        ${NCP} "${COM_ATMOS_RESTART_PREV}/${PDY}.${cyc}0000.sfc_data.tile${n}.nc" \
               "${COM_ATMOS_RESTART}/${PDY}.${cyc}0000.sfcanl_data.tile${n}.nc"
    fi
    ${NLN} "${COM_ATMOS_RESTART_PREV}/${PDY}.${cyc}0000.sfc_data.tile${n}.nc" "${DATA}/fnbgsi.00${n}"
    ${NLN} "${COM_ATMOS_RESTART}/${PDY}.${cyc}0000.sfcanl_data.tile${n}.nc"   "${DATA}/fnbgso.00${n}"
    ${NLN} "${FIXgfs}/orog/${CASE}/${CASE}_grid.tile${n}.nc"                       "${DATA}/fngrid.00${n}"
    ${NLN} "${FIXgfs}/orog/${CASE}/${CASE}.mx${OCNRES}_oro_data.tile${n}.nc"                   "${DATA}/fnorog.00${n}"
done

export APRUNCY=${APRUN_CYCLE}
export OMP_NUM_THREADS_CY=${NTHREADS_CYCLE}
export MAX_TASKS_CY=${ntiles}

CDATE="${PDY}${cyc}" ${CYCLESH}
export err=$?; err_chk


################################################################################
# Postprocessing
cd ${pwd}
[[ ${mkdata} = "YES" ]] && rm -rf ${DATA}


################################################################################

exit ${err}

################################################################################
