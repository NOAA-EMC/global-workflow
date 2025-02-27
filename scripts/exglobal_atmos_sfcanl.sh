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
cd "${DATA}" || exit 99

# Derived base variables

# Dependent Scripts and Executables
CYCLESH=${CYCLESH:-${USHgfs}/global_cycle.sh}
REGRIDSH=${REGRIDSH:-"${USHgfs}/regrid_gsiSfcIncr_to_tile.sh"}
export CYCLEXEC=${CYCLEXEC:-${EXECgfs}/global_cycle}
NTHREADS_CYCLE=${NTHREADS_CYCLE:-24}
APRUN_CYCLE=${APRUN_CYCLE:-${APRUN:-""}}

# Surface cycle related parameters
export SNOW_NUDGE_COEFF=${SNOW_NUDGE_COEFF:-'-2.'}
export CYCLVARS=${CYCLVARS:-""}
export FHOUR=${FHOUR:-0}
export DELTSFC=${DELTSFC:-6}

# Other info used in this script
RUN_GETGES=${RUN_GETGES:-"NO"}
GETGESSH=${GETGESSH:-"getges.sh"}
export gesenvir=${gesenvir:-${envir}}
# Ignore possible spelling error (nothing is misspelled)
# shellcheck disable=SC2153
GPREFIX="gdas.t${GDATE:8:2}z."
OPREFIX="${RUN/enkf}.t${cyc}z."
APREFIX="${RUN/enkf}.t${cyc}z."

ntiles=6


##############################################################
# Get dimension information based on CASE
res="${CASE:1}"
JCAP_CASE=$((res*2-2))
LATB_CASE=$((res*2))
LONB_CASE=$((res*4))

# Global cycle requires these files
export FNTSFA=${FNTSFA:-${COMIN_OBS}/${OPREFIX}rtgssthr.grb}
export FNACNA=${FNACNA:-${COMIN_OBS}/${OPREFIX}seaice.5min.blend.grb}
export FNSNOA=${FNSNOA:-${COMIN_OBS}/${OPREFIX}snogrb_t${JCAP_CASE}.${LONB_CASE}.${LATB_CASE}}
[[ ! -f ${FNSNOA} ]] && export FNSNOA="${COMIN_OBS}/${OPREFIX}snogrb_t1534.3072.1536"
FNSNOG=${FNSNOG:-${COMIN_OBS_PREV}/${GPREFIX}snogrb_t${JCAP_CASE}.${LONB_CASE}.${LATB_CASE}}
[[ ! -f ${FNSNOG} ]] && FNSNOG="${COMIN_OBS_PREV}/${GPREFIX}snogrb_t1534.3072.1536"

# Set CYCLVARS by checking grib date of current snogrb vs that of prev cycle
if [[ ${RUN_GETGES} = "YES" ]]; then
  snoprv=$(${GETGESSH} -q -t "snogrb_${JCAP_CASE}" -e "${gesenvir}" -n "${GDUMP}" -v "${GDATE}")
else
  snoprv=${snoprv:-${FNSNOG}}
fi

if [[ $(${WGRIB} -4yr "${FNSNOA}" 2>/dev/null | grep -i snowc | awk -F: '{print $3}' | awk -F= '{print $2}') -le \
  $(${WGRIB} -4yr "${snoprv}" 2>/dev/null | grep -i snowc | awk -F: '{print $3}' | awk -F= '{print $2}') ]] ; then
  export FNSNOA=" "
  export CYCLVARS="FSNOL=99999.,FSNOS=99999.,"
else
  export CYCLVARS="FSNOL=${SNOW_NUDGE_COEFF},${CYCLVARS}"
fi

# determine where the input snow restart files come from
if [[ "${DO_JEDISNOWDA:-}" == "YES" ]]; then
    sfcdata_dir="${COMIN_SNOW_ANALYSIS}"
else
    sfcdata_dir="${COMIN_ATMOS_RESTART_PREV}"
fi

# global_cycle executable specific variables
export APRUNCY=${APRUN_CYCLE}
export OMP_NUM_THREADS_CY=${NTHREADS_CYCLE}
export MAX_TASKS_CY=${ntiles}

# Copy fix files required by global_cycle to DATA just once
for (( nn=1; nn <= ntiles; nn++ )); do
  ${NCP} "${FIXgfs}/orog/${CASE}/${CASE}_grid.tile${nn}.nc"                 "${DATA}/fngrid.00${nn}"
  ${NCP} "${FIXgfs}/orog/${CASE}/${CASE}.mx${OCNRES}_oro_data.tile${nn}.nc" "${DATA}/fnorog.00${nn}"
done

# Copy the NSST analysis file for global_cycle
# There is only a single NSST analysis at the middle of the window
# For now use/assume it is the same at the beginning of the window if doing IAU
if [[ "${DONST}" == "YES" ]]; then
  ${NCP} "${COMIN_ATMOS_ANALYSIS}/${APREFIX}dtfanl.nc" "${DATA}/dtfanl"
  export NST_FILE="dtfanl"
else
  export NST_FILE="NULL"
fi

# Collect the dates in the window to update surface restarts
gcycle_dates=("${PDY}${cyc}")  # Always update surface restarts at middle of window
soilinc_fhrs=("${assim_freq}") # increment file at middle of window
LFHR=${assim_freq}
if [[ "${DOIAU:-}" == "YES" ]]; then  # Update surface restarts at beginning of window
  half_window=$(( assim_freq / 2 ))
  soilinc_fhrs+=("${half_window}")
  LFHR=-1
  BDATE=$(date --utc -d "${PDY} ${cyc} - ${half_window} hours" +%Y%m%d%H)
  gcycle_dates+=("${BDATE}")
fi

# if doing GSI soil anaysis, copy increment file and re-grid it to native model resolution
if [[ "${DO_GSISOILDA}" = "YES" ]]; then
 
    export COMIN_SOIL_ANALYSIS_MEM="${COMIN_ATMOS_ENKF_ANALYSIS_STAT}"
    export COMOUT_ATMOS_ANALYSIS_MEM="${COMIN_ATMOS_ANALYSIS}"
    export CASE_IN="${CASE_ENS}"
    export CASE_OUT="${CASE}"
    export OCNRES_OUT="${OCNRES}"
    export LFHR
 
    "${REGRIDSH}"

fi

# Loop over the dates in the window to update the surface restarts
for hr in "${!gcycle_dates[@]}"; do

  gcycle_date=${gcycle_dates[hr]}
  FHR=${soilinc_fhrs[hr]}
  echo "Updating surface restarts for ${gcycle_date} ..."

  datestr="${gcycle_date:0:8}.${gcycle_date:8:2}0000"

  if [[ "${DO_GSISOILDA}" = "YES" ]]; then
        for (( nn=1; nn <= ntiles; nn++ )); do
        ${NCP} "${COMIN_ATMOS_ANALYSIS}/sfci00${FHR}.tile${nn}.nc" \
           "${DATA}/soil_xainc.00${nn}" 
        done
  fi

  # Copy inputs from COMIN to DATA
  for (( nn=1; nn <= ntiles; nn++ )); do
    ${NCP} "${sfcdata_dir}/${datestr}.sfc_data.tile${nn}.nc" "${DATA}/fnbgsi.00${nn}"
    ${NCP} "${DATA}/fnbgsi.00${nn}"                       "${DATA}/fnbgso.00${nn}"
  done

  CDATE="${PDY}${cyc}" ${CYCLESH}
  export err=$?; err_chk

  # Copy outputs from DATA to COMOUT
  for (( nn=1; nn <= ntiles; nn++ )); do
    ${NCP} "${DATA}/fnbgso.00${nn}" "${COMOUT_ATMOS_RESTART}/${datestr}.sfcanl_data.tile${nn}.nc"
  done

done


################################################################################

exit "${err}"

################################################################################
