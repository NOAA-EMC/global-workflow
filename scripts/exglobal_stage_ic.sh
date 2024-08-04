#!/usr/bin/env bash

source "${USHgfs}/preamble.sh"

# Locally scoped variables and functions
# shellcheck disable=SC2153
GDATE=$(date --utc -d "${PDY} ${cyc} - ${assim_freq} hours" +%Y%m%d%H)
gPDY="${GDATE:0:8}"
gcyc="${GDATE:8:2}"

RDATE=$(date --utc -d "${PDY} ${cyc} + ${OFFSET_START_HOUR} hours" +%Y%m%d%H)
DTG_PREFIX="${RDATE:0:8}.${RDATE:8:2}0000"

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
    prev_atmos_copy_list=(fv_core.res.nc coupler.res)
    for ftype in "${prev_atmos_copy_list[@]}"; do
      src="${BASE_CPLIC}/${CPL_ATMIC:-}/${PDY}${cyc}/${MEMDIR}/atmos/${DTG_PREFIX}.${ftype}"
      tgt="${COM_ATMOS_RESTART_PREV}/${DTG_PREFIX}.${ftype}"
      ${NCP} "${src}" "${tgt}"
      rc=$?
      ((rc != 0)) && error_message "${src}" "${tgt}" "${rc}"
      err=$((err + rc))
    done
    for ftype in ca_data fv_core.res fv_srf_wnd.res fv_tracer.res phy_data sfc_data; do
      for ((tt = 1; tt <= ntiles; tt++)); do
        src="${BASE_CPLIC}/${CPL_ATMIC:-}/${PDY}${cyc}/${MEMDIR}/atmos/${DTG_PREFIX}.${ftype}.tile${tt}.nc"
        if (( tt > 6 )) ; then
            tgt="${COM_ATMOS_RESTART_PREV}/${DTG_PREFIX}.${ftype}.nest0$((tt-5)).tile${tt}.nc"
        else
            tgt="${COM_ATMOS_RESTART_PREV}/${DTG_PREFIX}.${ftype}.tile${tt}.nc"
        fi
        ${NCP} "${src}" "${tgt}"
        rc=$?
        ((rc != 0)) && error_message "${src}" "${tgt}" "${rc}"
        err=$((err + rc))
      done
    done
  else
    # Stage the FV3 cold-start initial conditions to ROTDIR
    YMD=${PDY} HH=${cyc} declare_from_tmpl COM_ATMOS_INPUT
    [[ ! -d "${COM_ATMOS_INPUT}" ]] && mkdir -p "${COM_ATMOS_INPUT}"
    src="${BASE_CPLIC}/${CPL_ATMIC:-}/${PDY}${cyc}/${MEMDIR}/atmos/gfs_ctrl.nc"
    tgt="${COM_ATMOS_INPUT}/gfs_ctrl.nc"
    ${NCP} "${src}" "${tgt}"
    rc=$?
    ((rc != 0)) && error_message "${src}" "${tgt}" "${rc}"
    err=$((err + rc))
    for ftype in gfs_data sfc_data; do
      for ((tt = 1; tt <= ntiles; tt++)); do
        src="${BASE_CPLIC}/${CPL_ATMIC:-}/${PDY}${cyc}/${MEMDIR}/atmos/${ftype}.tile${tt}.nc"
        tgt="${COM_ATMOS_INPUT}/${ftype}.tile${tt}.nc"
        ${NCP} "${src}" "${tgt}"
        rc=$?
        ((rc != 0)) && error_message "${src}" "${tgt}" "${rc}"
        err=$((err + rc))
      done
      if (( ntiles > 6 )); then
        ${NLN} "${COM_ATMOS_INPUT}/${ftype}.tile7.nc" "${COM_ATMOS_INPUT}/${ftype}.nest02.tile7.nc"
      fi
    done
  fi
  
  # Atmosphere Perturbation Files (usually used with replay ICS)
  # Extra zero on MEMDIR ensure we have a number even if the string is empty
  if (( $((10#0${MEMDIR:3})) > 0 )) && [[ "${REPLAY_ICS:-NO}" == "YES" ]]; then
      YMD=${PDY} HH=${cyc} declare_from_tmpl COM_ATMOS_ANALYSIS:COM_ATMOS_ANALYSIS_TMPL
      [[ ! -d "${COM_ATMOS_ANALYSIS}" ]] && mkdir -p "${COM_ATMOS_ANALYSIS}"
      src="${BASE_CPLIC}/${CPL_ATMIC:-}/${PDY}${cyc}/${MEMDIR}/atmos/${DTG_PREFIX}.fv3_perturbation.nc"
      tgt="${COM_ATMOS_ANALYSIS}/${RUN}.t00z.atminc.nc"
      ${NCP} "${src}" "${tgt}"
      rc=${?}
      ((rc != 0)) && error_message "${src}" "${tgt}" "${rc}"
      err=$((err + rc))
  fi

  # Stage ocean initial conditions to ROTDIR (warm start)
  if [[ "${DO_OCN:-}" = "YES" ]]; then
    RUN=${rCDUMP} YMD=${gPDY} HH=${gcyc} declare_from_tmpl COM_OCEAN_RESTART_PREV:COM_OCEAN_RESTART_TMPL
    [[ ! -d "${COM_OCEAN_RESTART_PREV}" ]] && mkdir -p "${COM_OCEAN_RESTART_PREV}"
    src="${BASE_CPLIC}/${CPL_OCNIC:-}/${PDY}${cyc}/${MEMDIR}/ocean/${DTG_PREFIX}.MOM.res.nc"
    tgt="${COM_OCEAN_RESTART_PREV}/${DTG_PREFIX}.MOM.res.nc"
    ${NCP} "${src}" "${tgt}"
    rc=$?
    ((rc != 0)) && error_message "${src}" "${tgt}" "${rc}"
    err=$((err + rc))
    case "${OCNRES}" in
      "500" | "100")
        # Nothing more to do for these resolutions
        ;;
      "025" )
        for nn in $(seq 1 3); do
          src="${BASE_CPLIC}/${CPL_OCNIC:-}/${PDY}${cyc}/${MEMDIR}/ocean/${DTG_PREFIX}.MOM.res_${nn}.nc"
          tgt="${COM_OCEAN_RESTART_PREV}/${DTG_PREFIX}.MOM.res_${nn}.nc"
          ${NCP} "${src}" "${tgt}"
          rc=$?
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
    if (( $((10#0${MEMDIR:3})) > 0 )) && [[ "${REPLAY_ICS:-NO}" == "YES" ]]; then
        YMD=${PDY} HH=${cyc} declare_from_tmpl COM_OCEAN_ANALYSIS:COM_OCEAN_ANALYSIS_TMPL
        [[ ! -d "${COM_OCEAN_ANALYSIS}" ]] && mkdir -p "${COM_OCEAN_ANALYSIS}"
        src="${BASE_CPLIC}/${CPL_OCNIC:-}/${PDY}${cyc}/${MEMDIR}/ocean/${DTG_PREFIX}.mom6_perturbation.nc"
        tgt="${COM_OCEAN_ANALYSIS}/mom6_increment.nc"
        ${NCP} "${src}" "${tgt}"
        rc=${?}
        ((rc != 0)) && error_message "${src}" "${tgt}" "${rc}"
        err=$((err + rc))
    fi

    # TODO: Do mediator restarts exists in a ATMW configuration?
    # TODO: No mediator is presumably involved in an ATMA configuration
    if [[ ${EXP_WARM_START:-".false."} = ".true." ]]; then
      # Stage the mediator restarts to ROTDIR (warm start/restart the coupled model)
      RUN=${rCDUMP} YMD=${gPDY} HH=${gcyc} declare_from_tmpl COM_MED_RESTART_PREV:COM_MED_RESTART_TMPL
      [[ ! -d "${COM_MED_RESTART_PREV}" ]] && mkdir -p "${COM_MED_RESTART_PREV}"
      src="${BASE_CPLIC}/${CPL_MEDIC:-}/${PDY}${cyc}/${MEMDIR}/med/${DTG_PREFIX}.ufs.cpld.cpl.r.nc"
      tgt="${COM_MED_RESTART_PREV}/${DTG_PREFIX}.ufs.cpld.cpl.r.nc"
      if [[ -f "${src}" ]]; then
        ${NCP} "${src}" "${tgt}"
        rc=$?
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
    src="${BASE_CPLIC}/${CPL_ICEIC:-}/${PDY}${cyc}/${MEMDIR}/ice/${DTG_PREFIX}.cice_model.res.nc"
    tgt="${COM_ICE_RESTART_PREV}/${DTG_PREFIX}.cice_model.res.nc"
    ${NCP} "${src}" "${tgt}"
    rc=$?
    ((rc != 0)) && error_message "${src}" "${tgt}" "${rc}"
    err=$((err + rc))
  fi

  # Stage the WW3 initial conditions to ROTDIR (warm start; TODO: these should be placed in $RUN.$gPDY/$gcyc)
  if [[ "${DO_WAVE:-}" = "YES" ]]; then
    YMD=${gPDY} HH=${gcyc} declare_from_tmpl COM_WAVE_RESTART_PREV:COM_WAVE_RESTART_TMPL
    [[ ! -d "${COM_WAVE_RESTART_PREV}" ]] && mkdir -p "${COM_WAVE_RESTART_PREV}"
    for grdID in ${waveGRD}; do # TODO: check if this is a bash array; if so adjust
      src="${BASE_CPLIC}/${CPL_WAVIC:-}/${PDY}${cyc}/${MEMDIR}/wave/${DTG_PREFIX}.restart.${grdID}"
      tgt="${COM_WAVE_RESTART_PREV}/${DTG_PREFIX}.restart.${grdID}"
      ${NCP} "${src}" "${tgt}"
      rc=$?
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
