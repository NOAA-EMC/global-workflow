#!/usr/bin/env bash

source "${USHgfs}/preamble.sh"

# Locally scoped variables and functions
# shellcheck disable=SC2153
GDATE=$(date --utc -d "${PDY} ${cyc} - ${assim_freq} hours" +%Y%m%d%H)
gPDY="${GDATE:0:8}"
gcyc="${GDATE:8:2}"

MEMDIR_ARRAY=()
if [[ "${RUN:-}" = "gefs" ]]; then
  # Populate the member_dirs array based on the value of NMEM_ENS
  for ((ii = 0; ii <= "${NMEM_ENS:-0}"; ii++)); do
    MEMDIR_ARRAY+=("mem$(printf "%03d" "${ii}")")
  done
else
  MEMDIR_ARRAY+=("")
fi

# Initialize return code
err=0

error_message() {
  echo "FATAL ERROR: Unable to copy ${1} to ${2} (Error code ${3})"
}

###############################################################
for MEMDIR in "${MEMDIR_ARRAY[@]}"; do

  # Stage atmosphere initial conditions to ROTDIR
  if [[ ${EXP_WARM_START:-".false."} = ".true." ]]; then
    # Stage the FV3 restarts to ROTDIR (warm start)
    RUN=${rCDUMP} YMD=${gPDY} HH=${gcyc} declare_from_tmpl COM_ATMOS_RESTART_PREV:COM_ATMOS_RESTART_TMPL
    [[ ! -d "${COM_ATMOS_RESTART_PREV}" ]] && mkdir -p "${COM_ATMOS_RESTART_PREV}"
    for ftype in coupler.res fv_core.res.nc; do
      src="${BASE_CPLIC}/${CPL_ATMIC:-}/${PDY}${cyc}/${MEMDIR}/atmos/${PDY}.${cyc}0000.${ftype}"
      tgt="${COM_ATMOS_RESTART_PREV}/${PDY}.${cyc}0000.${ftype}"
      if [ ! -f ${tgt} ]; then
        ${NCP} "${src}" "${tgt}"
        rc=$?
      else
        rc=0
      fi
      ((rc != 0)) && error_message "${src}" "${tgt}" "${rc}"
      err=$((err + rc))
    done
    for ftype in ca_data fv_core.res fv_srf_wnd.res fv_tracer.res phy_data sfc_data; do
      for ((tt = 1; tt <= ntiles; tt++)); do
        src="${BASE_CPLIC}/${CPL_ATMIC:-}/${PDY}${cyc}/${MEMDIR}/atmos/${PDY}.${cyc}0000.${ftype}.tile${tt}.nc"
       #tgt="${COM_ATMOS_RESTART_PREV}/${PDY}.${cyc}0000.${ftype}.tile${tt}.nc"
        if (( tt > 6 )) ; then
            tgt="${COM_ATMOS_RESTART_PREV}/${PDY}.${cyc}0000.${ftype}.nest0$((tt-5)).tile${tt}.nc"
        else
            tgt="${COM_ATMOS_RESTART_PREV}/${PDY}.${cyc}0000.${ftype}.tile${tt}.nc"
        fi
        if [ ! -f ${tgt} ]; then
          ${NCP} "${src}" "${tgt}"
          rc=$?
        else
          rc=0
        fi
        ((rc != 0)) && error_message "${src}" "${tgt}" "${rc}"
        err=$((err + rc))
      done
    done
  else
    # Stage the FV3 cold-start initial conditions to ROTDIR
    YMD=${PDY} HH=${cyc} declare_from_tmpl COM_ATMOS_INPUT
    [[ ! -d "${COM_ATMOS_INPUT}" ]] && mkdir -p "${COM_ATMOS_INPUT}"
    if [[ "${RUN:-}" = "gefs" ]]; then
      src="${BASE_CPLIC}/${CPL_ATMIC:-}/${RUN}PDY}/${cyc}/${MEMDIR}/atmos/gfs_ctrl.nc"
    else
      src="${BASE_CPLIC}/${CPL_ATMIC:-}/${RUN}.${PDY}/${cyc}/model_data/atmos/input/gfs_ctrl.nc"
    fi
    tgt="${COM_ATMOS_INPUT}/gfs_ctrl.nc"

    echo "BASE_CPLIC=$BASE_CPLIC"
    echo "CPL_ATMIC=$CPL_ATMIC"
    echo "PDY=$PDY"
    echo "cyc=$cyc"
    echo "COM_ATMOS_INPUT=$COM_ATMOS_INPUT"
    echo "MEMDIR=$MEMDIR"
    echo "NCP=$NCP"
    echo "src=$src"
    echo "tgt=$tgt"
    if [ ! -f ${tgt} ]; then
      ${NCP} "${src}" "${tgt}"
      rc=$?
    else
      rc=0
    fi
    ((rc != 0)) && error_message "${src}" "${tgt}" "${rc}"
    err=$((err + rc))
    for ftype in gfs_data sfc_data; do
      for ((tt = 1; tt <= ntiles; tt++)); do
        src="${BASE_CPLIC}/${CPL_ATMIC:-}/${PDY}${cyc}/${MEMDIR}/atmos/${ftype}.tile${tt}.nc"
        if [[ "${RUN:-}" = "gefs" ]]; then
          src="${BASE_CPLIC}/${CPL_ATMIC:-}/${RUN}.${PDY}/${cyc}/${MEMDIR}/atmos/${ftype}.tile${tt}.nc"
        else
          src="${BASE_CPLIC}/${CPL_ATMIC:-}/${RUN}.${PDY}/${cyc}/model_data/atmos/input/${ftype}.tile${tt}.nc"
        fi
        tgt="${COM_ATMOS_INPUT}/${ftype}.tile${tt}.nc"
        if [ ! -f ${tgt} ]; then
          ${NCP} "${src}" "${tgt}"
          rc=$?
        else
          rc=0
        fi
        ((rc != 0)) && error_message "${src}" "${tgt}" "${rc}"
        err=$((err + rc))
      done
      if (( ntiles > 6 )); then
        ${NLN} "${COM_ATMOS_INPUT}/${ftype}.tile7.nc" "${COM_ATMOS_INPUT}/${ftype}.nest02.tile7.nc"
      fi
    done
  fi

  # Stage ocean initial conditions to ROTDIR (warm start)
  if [[ "${DO_OCN:-}" = "YES" ]]; then
    RUN=${rCDUMP} YMD=${gPDY} HH=${gcyc} declare_from_tmpl COM_OCEAN_RESTART_PREV:COM_OCEAN_RESTART_TMPL
    [[ ! -d "${COM_OCEAN_RESTART_PREV}" ]] && mkdir -p "${COM_OCEAN_RESTART_PREV}"
    src="${BASE_CPLIC}/${CPL_OCNIC:-}/${PDY}${cyc}/${MEMDIR}/ocean/${PDY}.${cyc}0000.MOM.res.nc"
    tgt="${COM_OCEAN_RESTART_PREV}/${PDY}.${cyc}0000.MOM.res.nc"
    if [ ! -f ${tgt} ]; then
      ${NCP} "${src}" "${tgt}"
      rc=$?
    else
      rc=0
    fi
    ((rc != 0)) && error_message "${src}" "${tgt}" "${rc}"
    err=$((err + rc))
    case "${OCNRES}" in
      "500" | "100")
        # Nothing more to do for these resolutions
        ;;
      "025" )
        for nn in $(seq 1 3); do
          src="${BASE_CPLIC}/${CPL_OCNIC:-}/${PDY}${cyc}/${MEMDIR}/ocean/${PDY}.${cyc}0000.MOM.res_${nn}.nc"
          tgt="${COM_OCEAN_RESTART_PREV}/${PDY}.${cyc}0000.MOM.res_${nn}.nc"
          if [ ! -f ${tgt} ]; then
            ${NCP} "${src}" "${tgt}"
            rc=$?
          else
            rc=0
          fi
          ((rc != 0)) && error_message "${src}" "${tgt}" "${rc}"
          err=$((err + rc))
        done
        ;;
      *)
        echo "FATAL ERROR: Unsupported ocean resolution ${OCNRES}"
        rc=1
        err=$((err + rc))
        ;;
    esac

    # Ocean Perturbation Files
    # Extra zero on MEMDIR ensure we have a number even if the string is empty
    if (( 0${MEMDIR:3} > 0 )) && [[ "${USE_OCN_PERTURB_FILES:-false}" == "true" ]]; then
        src="${BASE_CPLIC}/${CPL_OCNIC:-}/${PDY}${cyc}/${MEMDIR}/ocean/${PDY}.${cyc}0000.mom6_increment.nc"
        tgt="${COM_OCEAN_RESTART_PREV}/${PDY}.${cyc}0000.mom6_increment.nc"
        if [ ! -f ${tgt} ]; then
          ${NCP} "${src}" "${tgt}"
          rc=$?
        else
          rc=0
        fi
        ((rc != 0)) && error_message "${src}" "${tgt}" "${rc}"
        err=$((err + rc))
    fi

    # TODO: Do mediator restarts exists in a ATMW configuration?
    # TODO: No mediator is presumably involved in an ATMA configuration
    if [[ ${EXP_WARM_START:-".false."} = ".true." ]]; then
      # Stage the mediator restarts to ROTDIR (warm start/restart the coupled model)
      RUN=${rCDUMP} YMD=${gPDY} HH=${gcyc} declare_from_tmpl COM_MED_RESTART_PREV:COM_MED_RESTART_TMPL
      [[ ! -d "${COM_MED_RESTART_PREV}" ]] && mkdir -p "${COM_MED_RESTART_PREV}"
      src="${BASE_CPLIC}/${CPL_MEDIC:-}/${PDY}${cyc}/${MEMDIR}/med/${PDY}.${cyc}0000.ufs.cpld.cpl.r.nc"
      tgt="${COM_MED_RESTART_PREV}/${PDY}.${cyc}0000.ufs.cpld.cpl.r.nc"
      if [[ -f "${src}" ]]; then
        if [ ! -f ${tgt} ]; then
          ${NCP} "${src}" "${tgt}"
          rc=$?
        else
          rc=0
        fi
        ((rc != 0)) && error_message "${src}" "${tgt}" "${rc}"
        err=$((err + rc))
      else
        echo "WARNING: No mediator restarts available with warm_start=${EXP_WARM_START}"
      fi
    fi

  fi

  # Stage ice initial conditions to ROTDIR (warm start)
  if [[ "${DO_ICE:-}" = "YES" ]]; then
    RUN=${rCDUMP} YMD=${gPDY} HH=${gcyc} declare_from_tmpl COM_ICE_RESTART_PREV:COM_ICE_RESTART_TMPL
    [[ ! -d "${COM_ICE_RESTART_PREV}" ]] && mkdir -p "${COM_ICE_RESTART_PREV}"
    src="${BASE_CPLIC}/${CPL_ICEIC:-}/${PDY}${cyc}/${MEMDIR}/ice/${PDY}.${cyc}0000.cice_model.res.nc"
    tgt="${COM_ICE_RESTART_PREV}/${PDY}.${cyc}0000.cice_model.res.nc"
    if [ ! -f ${tgt} ]; then
      ${NCP} "${src}" "${tgt}"
      rc=$?
    else
      rc=0
    fi
    ((rc != 0)) && error_message "${src}" "${tgt}" "${rc}"
    err=$((err + rc))
  fi

  # Stage the WW3 initial conditions to ROTDIR (warm start; TODO: these should be placed in $RUN.$gPDY/$gcyc)
  if [[ "${DO_WAVE:-}" = "YES" ]]; then
    YMD=${PDY} HH=${cyc} declare_from_tmpl COM_WAVE_RESTART
    [[ ! -d "${COM_WAVE_RESTART}" ]] && mkdir -p "${COM_WAVE_RESTART}"
    for grdID in ${waveGRD}; do # TODO: check if this is a bash array; if so adjust
      src="${BASE_CPLIC}/${CPL_WAVIC:-}/${PDY}${cyc}/${MEMDIR}/wave/${PDY}.${cyc}0000.restart.${grdID}"
      tgt="${COM_WAVE_RESTART}/${PDY}.${cyc}0000.restart.${grdID}"
      if [ ! -f ${tgt} ]; then
        ${NCP} "${src}" "${tgt}"
        rc=$?
      else
        rc=0
      fi
      ((rc != 0)) && error_message "${src}" "${tgt}" "${rc}"
      err=$((err + rc))
    done
  fi

done # for MEMDIR in "${MEMDIR_ARRAY[@]}"; do

###############################################################
# Check for errors and exit if any of the above failed
if [[ "${err}" -ne 0 ]]; then
  echo "FATAL ERROR: Unable to copy ICs from ${BASE_CPLIC} to ${ROTDIR}; ABORT!"
  exit "${err}"
fi

##############################################################
# Exit cleanly
exit "${err}"
