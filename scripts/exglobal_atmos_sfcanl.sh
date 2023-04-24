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

source "${HOMEgfs}/ush/preamble.sh"

#  Directories.
pwd=$(pwd)

# Base variables
CDATE=${CDATE:-"2001010100"}
CDUMP=${CDUMP:-"gdas"}
GDUMP=${GDUMP:-"gdas"}

# Derived base variables
GDATE=$(${NDATE} -${assim_freq} ${CDATE})
BDATE=$(${NDATE} -3 ${CDATE})
PDY=$(echo ${CDATE} | cut -c1-8)
cyc=$(echo ${CDATE} | cut -c9-10)
bPDY=$(echo ${BDATE} | cut -c1-8)
bcyc=$(echo ${BDATE} | cut -c9-10)

# Utilities
export NCP=${NCP:-"/bin/cp"}
export NMV=${NMV:-"/bin/mv"}
export NLN=${NLN:-"/bin/ln -sf"}
export CHGRP_CMD=${CHGRP_CMD:-"chgrp ${group_name:-rstprod}"}
export NCLEN=${NCLEN:-$HOMEgfs/ush/getncdimlen}
COMPRESS=${COMPRESS:-gzip}
UNCOMPRESS=${UNCOMPRESS:-gunzip}
APRUNCFP=${APRUNCFP:-""}

# IAU
DOIAU=${DOIAU:-"NO"}
export IAUFHRS=${IAUFHRS:-"6"}

# Surface cycle related parameters
CYCLESH=${CYCLESH:-${HOMEgfs}/ush/global_cycle.sh}
export CYCLEXEC=${CYCLEXEC:-${HOMEgfs}/exec/global_cycle}
NTHREADS_CYCLE=${NTHREADS_CYCLE:-24}
APRUN_CYCLE=${APRUN_CYCLE:-${APRUN:-""}}
export SNOW_NUDGE_COEFF=${SNOW_NUDGE_COEFF:-'-2.'}
export CYCLVARS=${CYCLVARS:-""}
export FHOUR=${FHOUR:-0}
export DELTSFC=${DELTSFC:-6}
export FIXgsm=${FIXgsm:-${HOMEgfs}/fix/am}
export FIXfv3=${FIXfv3:-${HOMEgfs}/fix/orog}

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

export CALCANLEXEC=${CALCANLEXEC:-${HOMEgfs}/exec/calc_analysis.x}
export CHGRESNCEXEC=${CHGRESNCEXEC:-${HOMEgfs}/exec/enkf_chgres_recenter_nc.x}
export CHGRESINCEXEC=${CHGRESINCEXEC:-${HOMEgfs}/exec/interp_inc.x}
export NTHREADS_CHGRES=${NTHREADS_CHGRES:-1}
CALCINCPY=${CALCINCPY:-${HOMEgfs}/ush/calcinc_gfs.py}
CALCANLPY=${CALCANLPY:-${HOMEgfs}/ush/calcanl_gfs.py}

export APRUN_CHGRES=${APRUN_CALCANL:-${APRUN:-""}}
CHGRESEXEC=${CHGRESEXEC:-${HOMEgfs}/exec/enkf_chgres_recenter.x}

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
DTFANL=${DTFANL:-${COMOUT}/${APREFIX}dtfanl.nc}

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
    export NSSTBF="${COMIN_OBS}/${OPREFIX}nsstbufr"
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
mkdir -p ${COMOUT}/RESTART

# Global cycle requires these files
export FNTSFA=${FNTSFA:-${COMIN_OBS}/${OPREFIX}rtgssthr.grb}
export FNACNA=${FNACNA:-${COMIN}/${OPREFIX}seaice.5min.blend.grb}
export FNSNOA=${FNSNOA:-${COMIN}/${OPREFIX}snogrb_t${JCAP_CASE}.${LONB_CASE}.${LATB_CASE}}
[[ ! -f ${FNSNOA} ]] && export FNSNOA="${COMIN}/${OPREFIX}snogrb_t1534.3072.1536"
FNSNOG=${FNSNOG:-${COMIN_GES}/${GPREFIX}snogrb_t${JCAP_CASE}.${LONB_CASE}.${LATB_CASE}}
[[ ! -f ${FNSNOG} ]] && FNSNOG="${COMIN_GES}/${GPREFIX}snogrb_t1534.3072.1536"

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
    export NST_FILE=${GSI_FILE:-${COMOUT}/${APREFIX}dtfanl.nc}
else
    export NST_FILE="NULL"
fi

if [[ ${DOIAU} = "YES" ]]; then
    # update surface restarts at the beginning of the window, if IAU
    # For now assume/hold dtfanl.nc valid at beginning of window
    for n in $(seq 1 ${ntiles}); do
        ${NCP} ${COMIN_GES}/RESTART/${bPDY}.${bcyc}0000.sfc_data.tile${n}.nc ${COMOUT}/RESTART/${bPDY}.${bcyc}0000.sfcanl_data.tile${n}.nc
        ${NLN} ${COMIN_GES}/RESTART/${bPDY}.${bcyc}0000.sfc_data.tile${n}.nc ${DATA}/fnbgsi.00${n}
        ${NLN} ${COMOUT}/RESTART/${bPDY}.${bcyc}0000.sfcanl_data.tile${n}.nc ${DATA}/fnbgso.00${n}
        ${NLN} ${FIXfv3}/${CASE}/${CASE}_grid.tile${n}.nc                    ${DATA}/fngrid.00${n}
        ${NLN} ${FIXfv3}/${CASE}/${CASE}_oro_data.tile${n}.nc                ${DATA}/fnorog.00${n}
    done

    export APRUNCY=${APRUN_CYCLE}
    export OMP_NUM_THREADS_CY=${NTHREADS_CYCLE}
    export MAX_TASKS_CY=${ntiles}

    ${CYCLESH}
    export err=$?; err_chk
fi

# Update surface restarts at middle of window
for n in $(seq 1 ${ntiles}); do
    ${NCP} ${COMOUT}/${PDY}.${cyc}0000.sfc_data.tile${n}.nc ${COMOUT}/RESTART/${PDY}.${cyc}0000.sfcanl_data.tile${n}.nc
    ${NLN} ${COMIN_GES}/RESTART/${PDY}.${cyc}0000.sfc_data.tile${n}.nc ${DATA}/fnbgsi.00${n}
    ${NLN} ${COMOUT}/RESTART/${PDY}.${cyc}0000.sfcanl_data.tile${n}.nc ${DATA}/fnbgso.00${n}
    ${NLN} ${FIXfv3}/${CASE}/${CASE}_grid.tile${n}.nc                  ${DATA}/fngrid.00${n}
    ${NLN} ${FIXfv3}/${CASE}/${CASE}_oro_data.tile${n}.nc              ${DATA}/fnorog.00${n}
done

export APRUNCY=${APRUN_CYCLE}
export OMP_NUM_THREADS_CY=${NTHREADS_CYCLE}
export MAX_TASKS_CY=${ntiles}

${CYCLESH}
export err=$?; err_chk


################################################################################
# Postprocessing
cd ${pwd}
[[ ${mkdata} = "YES" ]] && rm -rf ${DATA}


################################################################################

exit ${err}

################################################################################
