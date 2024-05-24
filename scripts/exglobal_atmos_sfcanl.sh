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
# Ignore possible spelling error (nothing is misspelled)
# shellcheck disable=SC2153
GDATE=$(date --utc -d "${PDY} ${cyc} - ${assim_freq} hours" +%Y%m%d%H)

# Dependent Scripts and Executables
CYCLESH=${CYCLESH:-${USHgfs}/global_cycle.sh}
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
    COMIN="${COMIN_SNOW_ANALYSIS}"
else
    COMIN="${COMIN_ATMOS_RESTART_PREV}"
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
if [[ "${DOIAU:-}" == "YES" ]]; then  # Update surface restarts at beginning of window
  half_window=$(( assim_freq / 2 ))
  BDATE=$(date --utc -d "${PDY} ${cyc} - ${half_window} hours" +%Y%m%d%H)
  gcycle_dates+=("${BDATE}")
fi

# Loop over the dates in the window to update the surface restarts
for gcycle_date in "${gcycle_dates[@]}"; do

  echo "Updating surface restarts for ${gcycle_date} ..."

  datestr="${gcycle_date:0:8}.${gcycle_date:8:2}0000"

  # Copy inputs from COMIN to DATA
  for (( nn=1; nn <= ntiles; nn++ )); do
    ${NCP} "${COMIN}/${datestr}.sfc_data.tile${nn}.nc" "${DATA}/fnbgsi.00${nn}"
    ${NCP} "${COMIN}/${datestr}.sfc_data.tile${nn}.nc" "${DATA}/fnbgso.00${nn}"
  done

  CDATE="${gcyle_date}" ${CYCLESH}
  export err=$?; err_chk

  # Copy outputs from DATA to COMOUT
  for (( nn=1; nn <= ntiles; nn++ )); do
    ${NCP} "${DATA}/fnbgso.00${nn}" "${COMOUT_ATMOS_RESTART}/${datestr}.sfcanl_data.tile${nn}.nc"
  done

done


################################################################################

exit "${err}"

################################################################################
