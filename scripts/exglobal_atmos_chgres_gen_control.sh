#! /usr/bin/env bash
################################################################################
####  UNIX Script Documentation Block
#                      .                                             .
# Script name:         exglobal_atmos_chgres_gen_control.sh
# Script description:  Runs chgres on changing resolution of GEFS stage ic control member
################################################################################
#  Directories.
pwd=$(pwd)
# Dependent input scripts and Executables
export OMP_NUM_THREADS=${OMP_NUM_THREADS_CH:-1}
export CHGRESEXEC=${CHGRESEXEC:-${EXECufs}/chgres_cube}
export PGMOUT=${PGMOUT:-${pgmout:-'&1'}}
DATA=${DATA:-${pwd}}
################################################################################
# Dates
CDATE=${CDATE:?}
iy="$(echo "${CDATE}" | cut -c1-4)"
im="$(echo "${CDATE}" | cut -c5-6)"
id="$(echo "${CDATE}" | cut -c7-8)"
ih="$(echo "${CDATE}" | cut -c9-10)"
export iy im id ih
################################################################################
# Set up theinput and output directories
export DESTINATION_DIR="${DATA}"
export SOURCE_DIR="${HOMEgfs}/fix/orog/${CASE}"
export MOSAIC_DESTINATION_FILE="${DESTINATION_DIR}/${CASE}_mosaic.nc"
export HYBLEV_FILE="${DESTINATION_DIR}/global_hyblev.l${LEVS}.txt"
export SFC_FILE="gdas.t18z.sfcf003.nc"
export ATM_FILE="gdas.t18z.atma003.ensres.nc"
################################################################################
# Ensure the source directory exists
if [[ ! -d "${SOURCE_DIR}" ]]; then
    echo "Error: Source directory ${SOURCE_DIR} does not exist."
    exit 1
fi
################################################################################
# List of input files to copy
input_files=(
    "${HOMEgfs}/fix/am/global_hyblev.l${LEVS}.txt"
    "${SOURCE_DIR}/${CASE}_mosaic.nc"
    "${COMIN_ATMOS_HISTORY_MEM}/${SFC_FILE}"
    "${COMIN_ATMOS_RESTART_PREV_MEM}/${ATM_FILE}"
)
###############################################################################
for src in "${input_files[@]}"; do
    cpfs "${src}" "${DESTINATION_DIR}/"
    chmod -R u+w "${DESTINATION_DIR}/$(basename "${src}")"
done

oro_file_set=(
    "${CASE}_grid.tile"           "${SOURCE_DIR}"
    "${CASE}.mx${OCNRES}_oro_data.tile" "${SOURCE_DIR}"
)

sfc_file_set=(
    "${CASE}.mx${OCNRES}.slope_type.tile" "${SOURCE_DIR}/sfc"
    "${CASE}.mx${OCNRES}.maximum_snow_albedo.tile" "${SOURCE_DIR}/sfc"
    "${CASE}.mx${OCNRES}.snowfree_albedo.tile" "${SOURCE_DIR}/sfc"
    "${CASE}.mx${OCNRES}.soil_type.tile" "${SOURCE_DIR}/sfc"
    "${CASE}.mx${OCNRES}.vegetation_type.tile" "${SOURCE_DIR}/sfc"
    "${CASE}.mx${OCNRES}.substrate_temperature.tile" "${SOURCE_DIR}/sfc"
    "${CASE}.mx${OCNRES}.vegetation_greenness.tile" "${SOURCE_DIR}/sfc"
    "${CASE}.mx${OCNRES}.facsf.tile" "${SOURCE_DIR}/sfc"
)

# Process orography files
for file in "${oro_file_set[@]}"; do
  for i in {1..6}; do
    tile_file="${file}${i}.nc"
    cpfs "${SOURCE_DIR}/${tile_file}" "${DESTINATION_DIR}/"
    chmod -R u+w "${DESTINATION_DIR}/${tile_file}"
  done
done

# Process surface files
for file in "${sfc_file_set[@]}"; do
  for i in {1..6}; do
    tile_file="${file}${i}.nc"
    cpfs "${SOURCE_DIR}/sfc/${tile_file}" "${DESTINATION_DIR}/"
    chmod -R u+w "${DESTINATION_DIR}/${tile_file}"
  done
done
################################################################################
# Prepare the orography target files
OROG_TARGET_FILES=$(for i in {1..6}; do
    printf "\"${CASE}.mx${OCNRES}_oro_data.tile%d.nc\"" "${i}"
    if [[ "${i}" -lt 6 ]]; then
        printf ","
    fi
done)
export OROG_TARGET_FILES
################################################################################
# run the chgres script to change resolution of sfc file
export CONVERT_ATM=".false."
export CONVERT_SFC=".true."
export output_log="sfs_out.log" # namelist output log
"${HOMEgfs}/ush/gen_control_changres.sh"
err=$?
if [[ ${err} -ne 0 ]]; then
  echo "ERROR: sfc chgres run failed"
  exit "${err}"
fi
# run the chgres script to change resolution of atm file
export CONVERT_ATM=".true."
export CONVERT_SFC=".false."
export output_log="atm_out.log"
"${HOMEgfs}/ush/gen_control_changres.sh"
err=$?
if [[ ${err} -ne 0 ]]; then
  echo "ERROR: atm chgres run failed"
  exit "${err}"
fi
################################################################################
# Ensure COMOUT_ATMOS_INPUT_MEM directory exists, create if needed
if [[ ! -d "${COMOUT_ATMOS_INPUT_MEM}" ]]; then
  if ! mkdir -p "${COMOUT_ATMOS_INPUT_MEM}"; then
    echo "ERROR: Failed to create directory ${COMOUT_ATMOS_INPUT_MEM}"
    exit 1
  fi
fi
# Ensure COMIN_ATMOS_INPUT_MEM exists, create if needed, then copy out.atm.tile{1..6}.nc (force overwrite)
for i in {1..6}; do
  atm_file="out.atm.tile${i}.nc"
  sfc_file="out.sfc.tile${i}.nc"
  if [[ -f "${atm_file}" ]]; then
    cpfs "${atm_file}" "${COMOUT_ATMOS_INPUT_MEM}/"
  fi
  if [[ -f "${sfc_file}" ]]; then
    cpfs "${sfc_file}" "${COMOUT_ATMOS_INPUT_MEM}/"
  fi
done

if [[ -f "gfs_ctrl.nc" ]]; then
  cpfs "gfs_ctrl.nc" "${COMOUT_ATMOS_INPUT_MEM}/"
fi
exit "${err}"
################################################################################
