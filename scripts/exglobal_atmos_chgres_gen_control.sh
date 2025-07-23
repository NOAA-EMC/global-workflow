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
INPUT_DIR="/lfs/h2/emc/nems/noscrub/emc.nems/UFS_UTILS/reg_tests/chgres_cube/input_data/fv3.netcdf"
SOURCE_DIR="${HOMEgfs}/fix/orog/${CASE}"
MOSAIC_DESTINATION_FILE="${DESTINATION_DIR}/${CASE}_mosaic.nc"
HYBLEV_FILE="${DESTINATION_DIR}/global_hyblev.l${LEVS}.txt"
SFC_FILE="gfs.t00z.sfcf000.nc"
ATM_FILE="gfs.t00z.atmf000.nc"
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
    "${INPUT_DIR}/${SFC_FILE}"
    "${INPUT_DIR}/${ATM_FILE}"
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
# add the namelist and run chgres
cat << EOF > ./fort.41
&config
mosaic_file_target_grid="${MOSAIC_DESTINATION_FILE}"
fix_dir_target_grid="${DESTINATION_DIR}"
orog_dir_target_grid="${DESTINATION_DIR}"
orog_files_target_grid=${OROG_TARGET_FILES}
vcoord_file_target_grid="${HYBLEV_FILE}"
mosaic_file_input_grid="NULL"
orog_dir_input_grid="NULL"
orog_files_input_grid="NULL"
data_dir_input_grid="${DESTINATION_DIR}"
atm_files_input_grid="${ATM_FILE}"
atm_core_files_input_grid="NULL"
atm_tracer_files_input_grid="NULL"
sfc_files_input_grid="${SFC_FILE}"
nst_files_input_grid="NULL"
grib2_file_input_grid="NULL"
geogrid_file_input_grid="NULL"
varmap_file="NULL"
wam_parm_file="NULL"
cycle_year=${iy}
cycle_mon=${im}
cycle_day=${id}
cycle_hour=${ih}
convert_atm=.true.
convert_sfc=.true.
convert_nst=.true.
input_type="gaussian_netcdf"
tracers="sphum","liq_wat","o3mr","ice_wat","rainwat","snowwat","graupel"
tracers_input="spfh","clwmr","o3mr","icmr","rwmr","snmr","grle"
regional=0
halo_bndy=0
halo_blend=0
sotyp_from_climo=.true.
vgtyp_from_climo=.true.
vgfrc_from_climo=.true.
minmax_vgfrc_from_climo=.true.
tg3_from_soil=.false.
lai_from_climo=true.
external_model="GFS"
nsoill_out=4
thomp_mp_climo_file="NULL"
wam_cold_start=.false.
/
EOF

eval "${APRUN_CHGRES}" "${CHGRESEXEC}" "${PGMOUT}"
################################################################################
# Ensure COMIN_ATMOS_INPUT_MEM exists, create if needed, then copy out.atm.tile{1..6}.nc (force overwrite)
if [[ ! -d "${COMOUT_ATMOS_INPUT_MEM}" ]] && ! mkdir -p "${COMOUT_ATMOS_INPUT_MEM}"; then
    echo "Error: Failed to create directory ${COMOUT_ATMOS_INPUT_MEM}."
    exit 1
fi

for i in {1..6}; do
    src_file="out.atm.tile${i}.nc"
    if [[ -f "${src_file}" ]]; then
        echo "Copying ${src_file} to ${COMOUT_ATMOS_INPUT_MEM}/"
        copy_file "${src_file}" "${COMOUT_ATMOS_INPUT_MEM}/"
    else
        echo "Warning: ${src_file} does not exist and will not be copied."
    fi
done
exit "${err}"
################################################################################
