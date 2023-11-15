#!/usr/bin/env bash

source "${HOMEgfs}/ush/preamble.sh"

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
    RUN=${rCDUMP} YMD=${gPDY} HH=${gcyc} generate_com COM_ATMOS_RESTART_PREV:COM_ATMOS_RESTART_TMPL
    [[ ! -d "${COM_ATMOS_RESTART_PREV}" ]] && mkdir -p "${COM_ATMOS_RESTART_PREV}"
    for ftype in coupler.res fv_core.res.nc; do
      src="${BASE_CPLIC}/${CPL_ATMIC:-}/${PDY}${cyc}/${MEMDIR}/atmos/${PDY}.${cyc}0000.${ftype}"
      tgt="${COM_ATMOS_RESTART_PREV}/${PDY}.${cyc}0000.${ftype}"
      ${NCP} "${src}" "${tgt}"
      rc=$?
      ((rc != 0)) && error_message "${src}" "${tgt}" "${rc}"
      err=$((err + rc))
    done
    for ftype in ca_data fv_core.res fv_srf_wnd.res fv_tracer.res phy_data sfc_data; do
      for ((tt = 1; tt <= 6; tt++)); do
        src="${BASE_CPLIC}/${CPL_ATMIC:-}/${PDY}${cyc}/${MEMDIR}/atmos/${PDY}.${cyc}0000.${ftype}.tile${tt}.nc"
        tgt="${COM_ATMOS_RESTART_PREV}/${PDY}.${cyc}0000.${ftype}.tile${tt}.nc"
        ${NCP} "${src}" "${tgt}"
        rc=$?
        ((rc != 0)) && error_message "${src}" "${tgt}" "${rc}"
        err=$((err + rc))
      done
    done
  else
    # Stage the FV3 cold-start initial conditions to ROTDIR
    YMD=${PDY} HH=${cyc} generate_com COM_ATMOS_INPUT
    [[ ! -d "${COM_ATMOS_INPUT}" ]] && mkdir -p "${COM_ATMOS_INPUT}"
    src="${BASE_CPLIC}/${CPL_ATMIC:-}/${PDY}${cyc}/${MEMDIR}/atmos/gfs_ctrl.nc"
    tgt="${COM_ATMOS_INPUT}/gfs_ctrl.nc"
    ${NCP} "${src}" "${tgt}"
    rc=$?
    ((rc != 0)) && error_message "${src}" "${tgt}" "${rc}"
    err=$((err + rc))
    for ftype in gfs_data sfc_data; do
      for ((tt = 1; tt <= 6; tt++)); do
        src="${BASE_CPLIC}/${CPL_ATMIC:-}/${PDY}${cyc}/${MEMDIR}/atmos/${ftype}.tile${tt}.nc"
        tgt="${COM_ATMOS_INPUT}/${ftype}.tile${tt}.nc"
        ${NCP} "${src}" "${tgt}"
        rc=$?
        ((rc != 0)) && error_message "${src}" "${tgt}" "${rc}"
        err=$((err + rc))
      done
    done
  fi

  # Stage ocean initial conditions to ROTDIR (warm start)
  if [[ "${DO_OCN:-}" = "YES" ]]; then
    RUN=${rCDUMP} YMD=${gPDY} HH=${gcyc} generate_com COM_OCEAN_RESTART_PREV:COM_OCEAN_RESTART_TMPL
    [[ ! -d "${COM_OCEAN_RESTART_PREV}" ]] && mkdir -p "${COM_OCEAN_RESTART_PREV}"
    src="${BASE_CPLIC}/${CPL_OCNIC:-}/${PDY}${cyc}/${MEMDIR}/ocean/${PDY}.${cyc}0000.MOM.res.nc"
    tgt="${COM_OCEAN_RESTART_PREV}/${PDY}.${cyc}0000.MOM.res.nc"
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
          src="${BASE_CPLIC}/${CPL_OCNIC:-}/${PDY}${cyc}/${MEMDIR}/ocean/${PDY}.${cyc}0000.MOM.res_${nn}.nc"
          tgt="${COM_OCEAN_RESTART}/${PDY}.${cyc}0000.MOM.res_${nn}.nc"
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

    # TODO: Do mediator restarts exists in a ATMW configuration?
    # TODO: No mediator is presumably involved in an ATMA configuration
    if [[ ${EXP_WARM_START:-".false."} = ".true." ]]; then
      # Stage the mediator restarts to ROTDIR (warm start/restart the coupled model)
      RUN=${rCDUMP} YMD=${gPDY} HH=${gcyc} generate_com COM_MED_RESTART_PREV:COM_MED_RESTART_TMPL
      [[ ! -d "${COM_MED_RESTART_PREV}" ]] && mkdir -p "${COM_MED_RESTART_PREV}"
      src="${BASE_CPLIC}/${CPL_MEDIC:-}/${PDY}${cyc}/${MEMDIR}/med/${PDY}.${cyc}0000.ufs.cpld.cpl.r.nc"
      tgt="${COM_MED_RESTART_PREV}/${PDY}.${cyc}0000.ufs.cpld.cpl.r.nc"
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
    RUN=${rCDUMP} YMD=${gPDY} HH=${gcyc} generate_com COM_ICE_RESTART_PREV:COM_ICE_RESTART_TMPL
    [[ ! -d "${COM_ICE_RESTART_PREV}" ]] && mkdir -p "${COM_ICE_RESTART_PREV}"
    src="${BASE_CPLIC}/${CPL_ICEIC:-}/${PDY}${cyc}/${MEMDIR}/ice/${PDY}.${cyc}0000.cice_model.res.nc"
    tgt="${COM_ICE_RESTART_PREV}/${PDY}.${cyc}0000.cice_model.res.nc"
    ${NCP} "${src}" "${tgt}"
    rc=$?
    ((rc != 0)) && error_message "${src}" "${tgt}" "${rc}"
    err=$((err + rc))
  fi

  # Stage the WW3 initial conditions to ROTDIR (warm start; TODO: these should be placed in $RUN.$gPDY/$gcyc)
  if [[ "${DO_WAVE:-}" = "YES" ]]; then
    YMD=${PDY} HH=${cyc} generate_com COM_WAVE_RESTART
    [[ ! -d "${COM_WAVE_RESTART}" ]] && mkdir -p "${COM_WAVE_RESTART}"
    for grdID in ${waveGRD}; do # TODO: check if this is a bash array; if so adjust
      src="${BASE_CPLIC}/${CPL_WAVIC:-}/${PDY}${cyc}/${MEMDIR}/wave/${PDY}.${cyc}0000.restart.${grdID}"
      tgt="${COM_WAVE_RESTART}/${PDY}.${cyc}0000.restart.${grdID}"
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
