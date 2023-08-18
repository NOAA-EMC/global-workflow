#! /usr/bin/env bash

source "${HOMEgfs}/ush/preamble.sh"


# start staging

source "${HOMEgfs}/ush/jjob_header.sh" -e "stage_ic" -c "base stage_ic"


GDATE=$(date -d "${PDY} ${cyc} - ${assim_freq} hours" +%Y%m%d%H)
gPDY="${GDATE:0:8}"
gcyc="${GDATE:8:2}"

source "${HOMEgfs}/ush/file_utils.sh"
# Initialize return code
err=0

error_message(){
    echo "FATAL ERROR: Unable to copy ${1} to ${2} (Error code ${3})"
}

###############################################################
# Start staging

# Stage the initial conditions 
YMD=${PDY} HH=${cyc} generate_com -r COM_STAGE_IC
[[ ! -d "${COM_STAGE_IC}" ]] && mkdir -p "${COM_STAGE_IC}"

# copying IC files from /scratch1/NCEPDEV/global/glopara/data/ICSDIR/C48C48mx500/gefs.20210323 ${COMROT}/${PSLOT}/gefs.20210323

cp as  "" ""

source="${BASE_CPLIC}/${CPL_ATMIC}/${PDY}${cyc}/${CDUMP}/${CASE}/INPUT/gfs_ctrl.nc"


# Stage the FV3 initial conditions to ROTDIR (cold start)
YMD=${PDY} HH=${cyc} generate_com -r COM_ATMOS_INPUT
[[ ! -d "${COM_ATMOS_INPUT}" ]] && mkdir -p "${COM_ATMOS_INPUT}"
source="${BASE_CPLIC}/${CPL_ATMIC}/${PDY}${cyc}/${CDUMP}/${CASE}/INPUT/gfs_ctrl.nc"
target="${COM_ATMOS_INPUT}/gfs_ctrl.nc"
${NCP} "${source}" "${target}"
rc=$?
[[ ${rc} -ne 0 ]] && error_message "${source}" "${target}" "${rc}"
err=$((err + rc))
for ftype in gfs_data sfc_data; do
  for tt in $(seq 1 6); do
    source="${BASE_CPLIC}/${CPL_ATMIC}/${PDY}${cyc}/${CDUMP}/${CASE}/INPUT/${ftype}.tile${tt}.nc"
    target="${COM_ATMOS_INPUT}/${ftype}.tile${tt}.nc"
    ${NCP} "${source}" "${target}"
    rc=$?
    [[ ${rc} -ne 0 ]] && error_message "${source}" "${target}" "${rc}"
    err=$((err + rc))
  done
done

# Stage ocean initial conditions to ROTDIR (warm start)
if [[ "${DO_OCN:-}" = "YES" ]]; then
  YMD=${gPDY} HH=${gcyc} generate_com -r COM_OCEAN_RESTART
  [[ ! -d "${COM_OCEAN_RESTART}" ]] && mkdir -p "${COM_OCEAN_RESTART}"
  source="${BASE_CPLIC}/${CPL_OCNIC}/${PDY}${cyc}/ocn/${OCNRES}/MOM.res.nc"
  target="${COM_OCEAN_RESTART}/${PDY}.${cyc}0000.MOM.res.nc"
  ${NCP} "${source}" "${target}"
  rc=$?
  [[ ${rc} -ne 0 ]] && error_message "${source}" "${target}" "${rc}"
  err=$((err + rc))
  case "${OCNRES}" in
    "500" | "100")  # Only 5 degree or 1 degree ocean does not have MOM.res_[1-4].nc files
    ;;
    "025")  # Only 1/4 degree ocean has MOM.res_[1-4].nc files
      for nn in $(seq 1 4); do
        source="${BASE_CPLIC}/${CPL_OCNIC}/${PDY}${cyc}/ocn/${OCNRES}/MOM.res_${nn}.nc"
        if [[ -f "${source}" ]]; then
          target="${COM_OCEAN_RESTART}/${PDY}.${cyc}0000.MOM.res_${nn}.nc"
          ${NCP} "${source}" "${target}"
          rc=$?
          [[ ${rc} -ne 0 ]] && error_message "${source}" "${target}" "${rc}"
          err=$((err + rc))
        fi
      done
    ;;
    *)
      echo "FATAL ERROR: Unsupported ocean resolution ${OCNRES}"
      rc=1
      err=$((err + rc))
    ;;
  esac
fi

# Stage ice initial conditions to ROTDIR (warm start)
if [[ "${DO_ICE:-}" = "YES" ]]; then
  YMD=${gPDY} HH=${gcyc} generate_com -r COM_ICE_RESTART
  [[ ! -d "${COM_ICE_RESTART}" ]] && mkdir -p "${COM_ICE_RESTART}"
  ICERESdec=$(echo "${ICERES}" | awk '{printf "%0.2f", $1/100}')
  source="${BASE_CPLIC}/${CPL_ICEIC}/${PDY}${cyc}/ice/${ICERES}/cice5_model_${ICERESdec}.res_${PDY}${cyc}.nc"
  target="${COM_ICE_RESTART}/${PDY}.${cyc}0000.cice_model.res.nc"
  ${NCP} "${source}" "${target}"
  rc=$?
  [[ ${rc} -ne 0 ]] && error_message "${source}" "${target}" "${rc}"
  err=$((err + rc))
fi

# Stage the WW3 initial conditions to ROTDIR (warm start; TODO: these should be placed in $RUN.$gPDY/$gcyc)
if [[ "${DO_WAVE:-}" = "YES" ]]; then
  YMD=${PDY} HH=${cyc} generate_com -r COM_WAVE_RESTART
  [[ ! -d "${COM_WAVE_RESTART}" ]] && mkdir -p "${COM_WAVE_RESTART}"
  for grdID in ${waveGRD}; do  # TODO: check if this is a bash array; if so adjust
    source="${BASE_CPLIC}/${CPL_WAVIC}/${PDY}${cyc}/wav/${grdID}/${PDY}.${cyc}0000.restart.${grdID}"
    target="${COM_WAVE_RESTART}/${PDY}.${cyc}0000.restart.${grdID}"
    ${NCP} "${source}" "${target}"
    rc=$?
    [[ ${rc} -ne 0 ]] && error_message "${source}" "${target}" "${rc}"
    err=$((err + rc))
  done
fi

###############################################################
# Check for errors and exit if any of the above failed
if  [[ "${err}" -ne 0 ]] ; then
  echo "FATAL ERROR: Unable to copy ICs from ${BASE_CPLIC} to ${ROTDIR}; ABORT!"
  exit "${err}"
fi

##############################################################
# Exit cleanly
exit "${err}"
