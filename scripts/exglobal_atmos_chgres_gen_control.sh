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
CHGRESEXEC=${CHGRESEXEC:-${EXECufs}/chgres_cube}
PGMOUT=${PGMOUT:-${pgmout:-'&1'}}
DATA=${DATA:-${pwd}}
################################################################################
# Dates
CDATE=${CDATE:?}
iy="$(echo "${CDATE}" | cut -c1-4)"
im="$(echo "${CDATE}" | cut -c5-6)"
id="$(echo "${CDATE}" | cut -c7-8)"
ih="$(echo "${CDATE}" | cut -c9-10)"
################################################################################
# Set up input and output directories
SOURCE_DIR="${HOMEgfs}/fix/orog/${CASE}"
MOSAIC_DESTINATION_FILE="${DATA}/${CASE}_mosaic.nc"
HYBLEV_FILE="${DATA}/global_hyblev.l${LEVS}.txt"
# uncomment when the correct files are available
# SFC_FILE="gdas.t18z.sfcf003.nc"
# ATM_FILE="gdas.t18z.atmf003.ensres.nc"
################################################################################
# Ensure the source directory exists
if [[ ! -d "${SOURCE_DIR}" ]]; then
    echo "Error: Source directory ${SOURCE_DIR} does not exist."
    exit 1
fi
################################################################################
# List of input files to copy
# uncomment when the correct files are available
input_files=(
    "${HOMEgfs}/fix/am/global_hyblev.l${LEVS}.txt"
    "${SOURCE_DIR}/${CASE}_mosaic.nc"
#    "${COMIN_ATMOS_HISTORY_MEM}/${SFC_FILE}"
#    "${COMIN_ATMOS_HISTORY_MEM}/${ATM_FILE}"
)
###############################################################################
for src in "${input_files[@]}"; do
    cpfs "${src}" "${DATA}/"
    chmod -R u+w "${DATA}/$(basename "${src}")"
done

# Define orography file patterns
oro_file_set=(
  "${CASE}_grid.tile"
  "${CASE}.mx${OCNRES}_oro_data.tile"
)

# Define surface file patterns
sfc_file_set=(
  "${CASE}.mx${OCNRES}.slope_type.tile"
  "${CASE}.mx${OCNRES}.maximum_snow_albedo.tile"
  "${CASE}.mx${OCNRES}.snowfree_albedo.tile"
  "${CASE}.mx${OCNRES}.soil_type.tile"
  "${CASE}.mx${OCNRES}.vegetation_type.tile"
  "${CASE}.mx${OCNRES}.substrate_temperature.tile"
  "${CASE}.mx${OCNRES}.vegetation_greenness.tile"
  "${CASE}.mx${OCNRES}.facsf.tile"
)

# Process orography files
for file in "${oro_file_set[@]}"; do
  for i in {1..6}; do
    tile_file="${file}${i}.nc"
    cpfs "${SOURCE_DIR}/${tile_file}" "${DATA}/"
    chmod -R u+w "${DATA}/${tile_file}"
  done
done

# Process surface files
for file in "${sfc_file_set[@]}"; do
  for i in {1..6}; do
    tile_file="${file}${i}.nc"
    cpfs "${SOURCE_DIR}/sfc/${tile_file}" "${DATA}/"
    chmod -R u+w "${DATA}/${tile_file}"
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
################################################################################
# add the namelist and run chgres
cat << EOF > ./fort.41
&config
mosaic_file_target_grid="${MOSAIC_DESTINATION_FILE}"
fix_dir_target_grid="${DATA}"
orog_dir_target_grid="${DATA}"
orog_files_target_grid=${OROG_TARGET_FILES}
vcoord_file_target_grid="${HYBLEV_FILE}"
mosaic_file_input_grid="NULL"
orog_dir_input_grid="NULL"
orog_files_input_grid="NULL"
data_dir_input_grid="${DATA}"
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
if [[ ! -d "${COMOUT_ATMOS_INPUT_MEM}" ]]; then
  if ! mkdir -p "${COMOUT_ATMOS_INPUT_MEM}"; then
    echo "ERROR: Failed to create directory ${COMOUT_ATMOS_INPUT_MEM}"
    exit 1
  fi
fi

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
