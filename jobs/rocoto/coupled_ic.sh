#! /usr/bin/env bash

source "$HOMEgfs/ush/preamble.sh"

###############################################################
## Abstract:
## Copy initial conditions from BASE_CPLIC to ROTDIR for coupled forecast-only runs
## HOMEgfs   : /full/path/to/workflow
## EXPDIR : /full/path/to/config/files
## CDUMP  : cycle name (gdas / gfs)
## PDY    : current date (YYYYMMDD)
## cyc    : current cycle (HH)
###############################################################

###############################################################
# Source FV3GFS workflow modules
. ${HOMEgfs}/ush/load_fv3gfs_modules.sh
status=$?
[[ ${status} -ne 0 ]] && exit ${status}
err=0

###############################################################
# Source relevant configs
configs="base coupled_ic wave"
for config in ${configs}; do
    . ${EXPDIR}/config.${config}
    status=$?
    [[ ${status} -ne 0 ]] && exit ${status}
done

###############################################################
# Source machine runtime environment
. ${BASE_ENV}/${machine}.env config.coupled_ic
status=$?
[[ ${status} -ne 0 ]] && exit ${status}

###############################################################
# Locally scoped variables and functions
GDATE=$(date -d "${PDY} ${cyc} - ${assim_freq} hours" +%Y%m%d%H)
gPDY="${GDATE:0:8}"
gcyc="${GDATE:8:2}"

error_message(){
    echo "FATAL ERROR: Unable to copy ${1} to ${2} (Error code ${3})"
}

YMD=${PDY} HH=${cyc} generate_com -rx COM_ATMOS_INPUT COM_ICE_RESTART COM_WAVE_RESTART
YMD=${gPDY} HH=${gcyc} generate_com -rx COM_OCEAN_RESTART

###############################################################
# Start staging

# Stage the FV3 initial conditions to ROTDIR (cold start)
ATMdir="${COM_ATMOS_INPUT}"
[[ ! -d "${ATMdir}" ]] && mkdir -p "${ATMdir}"
source="${BASE_CPLIC}/${CPL_ATMIC}/${PDY}${cyc}/${CDUMP}/${CASE}/INPUT/gfs_ctrl.nc"
target="${ATMdir}/gfs_ctrl.nc"
${NCP} "${source}" "${target}"
rc=$?
[[ ${rc} -ne 0 ]] && error_message "${source}" "${target}" "${rc}"
err=$((err + rc))
for ftype in gfs_data sfc_data; do
  for tt in $(seq 1 6); do
    source="${BASE_CPLIC}/${CPL_ATMIC}/${PDY}${cyc}/${CDUMP}/${CASE}/INPUT/${ftype}.tile${tt}.nc"
    target="${ATMdir}/${ftype}.tile${tt}.nc"
    ${NCP} "${source}" "${target}"
    rc=$?
    [[ ${rc} -ne 0 ]] && error_message "${source}" "${target}" "${rc}"
    err=$((err + rc))
  done
done

# Stage ocean initial conditions to ROTDIR (warm start)
OCNdir="${COM_OCEAN_RESTART}"
[[ ! -d "${OCNdir}" ]] && mkdir -p "${OCNdir}"
source="${BASE_CPLIC}/${CPL_OCNIC}/${PDY}${cyc}/ocn/${OCNRES}/MOM.res.nc"
target="${OCNdir}/${PDY}.${cyc}0000.MOM.res.nc"
${NCP} "${source}" "${target}"
rc=$?
[[ ${rc} -ne 0 ]] && error_message "${source}" "${target}" "${rc}"
err=$((err + rc))
case $OCNRES in
  "025")
    for nn in $(seq 1 4); do
      source="${BASE_CPLIC}/${CPL_OCNIC}/${PDY}${cyc}/ocn/${OCNRES}/MOM.res_${nn}.nc"
      if [[ -f "${source}" ]]; then
        target="${OCNdir}/${PDY}.${cyc}0000.MOM.res_${nn}.nc"
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

# Stage ice initial conditions to ROTDIR (cold start as these are SIS2 generated)
ICEdir="${COM_ICE_RESTART}"
[[ ! -d "${ICEdir}" ]] && mkdir -p "${ICEdir}"
ICERESdec=$(echo "${ICERES}" | awk '{printf "%0.2f", $1/100}')
source="${BASE_CPLIC}/${CPL_ICEIC}/${PDY}${cyc}/ice/${ICERES}/cice5_model_${ICERESdec}.res_${PDY}${cyc}.nc"
target="${ICEdir}/${PDY}.${cyc}0000.cice_model.res.nc"
${NCP} "${source}" "${target}"
rc=$?
[[ ${rc} -ne 0 ]] && error_message "${source}" "${target}" "${rc}"
err=$((err + rc))

# Stage the WW3 initial conditions to ROTDIR (warm start; TODO: these should be placed in $RUN.$gPDY/$gcyc)
if [[ "${DO_WAVE}" = "YES" ]]; then
  WAVdir="${COM_WAVE_RESTART}"
  [[ ! -d "${WAVdir}" ]] && mkdir -p "${WAVdir}"
  for grdID in ${waveGRD}; do  # TODO: check if this is a bash array; if so adjust
    source="${BASE_CPLIC}/${CPL_WAVIC}/${PDY}${cyc}/wav/${grdID}/${PDY}.${cyc}0000.restart.${grdID}"
    target="${WAVdir}/${PDY}.${cyc}0000.restart.${grdID}"
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
exit 0
