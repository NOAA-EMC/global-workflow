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
CHGRESEXEC=${CHGRESEXEC:-${EXECufs}/chgres_cube}
PGMOUT=${PGMOUT:-${pgmout:-'&1'}}
DATA=${DATA:-${pwd}}
################################################################################
#dates
CDATE=${CDATE:?}
iy=$(echo "${CDATE}" | cut -c1-4)
im=$(echo "${CDATE}" | cut -c5-6)
id=$(echo "${CDATE}" | cut -c7-8)
ih=$(echo "${CDATE}" | cut -c9-10)
################################################################################
# Set up theinput and output directories

DESTINATION_DIR="${DATA}"
SOURCE_DIR="${HOMEgfs}/fix/orog/${CASE}"
MOSAIC_DESTINATION_FILE="${DESTINATION_DIR}/${CASE}_mosaic.nc"
HYBLEV_FILE="${DESTINATION_DIR}/global_hyblev.l${LEVS}.txt"
SFC_FILE="gdas.t18z.sfcf003.nc"
ATM_FILE="gdas.t18z.atmf003.nc"
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
    "${COMIN_ATMOS_HISTORY_MEM}/${ATM_FILE}"
)
################################################################################
# Function to copy files and check success
copy_file() {
    local src=$1
    local dest=$2
    echo "Copying ${src} to ${dest}"
    if ! cp -rf "${src}" "${dest}"; then
        echo "Error: Failed to copy ${src} to ${dest}" >&2
        exit 1
    fi
}
###############################################################################
for src in "${input_files[@]}"; do
    copy_file "${src}" "${DESTINATION_DIR}/"
    chmod -R u+w "${DESTINATION_DIR}/$(basename "${src}")"
done

tile_file_set=(
    "${CASE}_grid.tile"           "${SOURCE_DIR}"
    "${CASE}.mx${OCNRES}_oro_data.tile" "${SOURCE_DIR}"
    "${CASE}.mx${OCNRES}.slope_type.tile" "${SOURCE_DIR}/sfc"
    "${CASE}.mx${OCNRES}.maximum_snow_albedo.tile" "${SOURCE_DIR}/sfc"
    "${CASE}.mx${OCNRES}.snowfree_albedo.tile" "${SOURCE_DIR}/sfc"
    "${CASE}.mx${OCNRES}.soil_type.tile" "${SOURCE_DIR}/sfc"
    "${CASE}.mx${OCNRES}.vegetation_type.tile" "${SOURCE_DIR}/sfc"
    "${CASE}.mx${OCNRES}.substrate_temperature.tile" "${SOURCE_DIR}/sfc"
    "${CASE}.mx${OCNRES}.vegetation_greenness.tile" "${SOURCE_DIR}/sfc"
    "${CASE}.mx${OCNRES}.facsf.tile" "${SOURCE_DIR}/sfc"
)

# Loop through patterns and tiles
for ((p=0; p<${#tile_file_set[@]}; p+=2)); do
  prefix="${tile_file_set[p]}"
  dir="${tile_file_set[p+1]}"
  for i in {1..6}; do
    tile_file="${prefix}${i}.nc"
    copy_file "${dir}/${tile_file}" "${DESTINATION_DIR}/"
    chmod -R u+w "${DESTINATION_DIR}/${tile_file}"
   done
done
echo "All files copied successfully."
################################################################################
# Prepare the orography target files
OROG_TARGET_FILES=$(for i in {1..6}; do
    printf "\"${CASE}.mx${OCNRES}_oro_data.tile%d.nc\"" "${i}"
    if [[ "${i}" -lt 6 ]]; then
        printf ","
    fi
done)
################################################################################
# run the chgres script to change resolution of sfc file
convert_atm=".false."
convert_sfc=".true."
output_log="./fort.41_1"
"${HOMEgfs}/ush/gen_control_changres.sh"
err=$?
if [[ ${err} -ne 0 ]]; then
  echo "ERROR: sfc chgres run failed"
  exit ${err}
fi
# run the chgres script to change resolution of atm file
convert_atm=".true."
convert_sfc=".false."
output_log="./fort.41_2"
"${HOMEgfs}/ush/gen_control_changres.sh"
err=$?
if [[ ${err} -ne 0 ]]; then
  echo "ERROR: atm chgres run failed"
  exit ${err}
fi
################################################################################
# Ensure COMIN_ATMOS_INPUT_MEM exists, create if needed, then copy out.atm.tile{1..6}.nc (force overwrite)
for i in {1..6}; do
  atm_file="out.atm.tile${i}.nc"
  sfc_file="out.sfc.tile${i}.nc"
  if [[ -f "${atm_file}" ]]; then
    copy_file "${atm_file}" "${COMOUT_ATMOS_INPUT_MEM}/"
  fi
  if [[ -f "${sfc_file}" ]]; then
    copy_file "${sfc_file}" "${COMOUT_ATMOS_INPUT_MEM}/"
  fi
done

if [[ -f "gfs_ctrl.nc" ]]; then
  copy_file "gfs_ctrl.nc" "${COMOUT_ATMOS_INPUT_MEM}/"
fi
exit "${err}"
################################################################################
