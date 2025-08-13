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
DATA=${DATA:-${pwd}}
################################################################################
# Dates
iy="${PDY:0:4}"
im="${PDY:4:6}"
id="${PDY:6:8}"
ih="${cyc}"
################################################################################
# Set up input and output directories
SOURCE_DIR="${HOMEgfs}/fix/orog/${CASE}"
################################################################################
# Ensure the source directory exists
if [[ ! -d "${SOURCE_DIR}" ]]; then
    echo "Error: Source directory ${SOURCE_DIR} does not exist."
    exit 1
fi
################################################################################
# copy input files to DATA from the source directory
cpfs "${HOMEgfs}/fix/am/global_hyblev.l${LEVS}.txt" "${DATA}/"
cpfs "${SOURCE_DIR}/${CASE}_mosaic.nc" "${DATA}/"
# uncomment and modify when the correct files are available
# cpfs "${COMIN_ATMOS_HISTORY_MEM}/${SFC_FILE}" "${DATA}/"
# cpfs "${COMIN_ATMOS_HISTORY_MEM}/${ATM_FILE}" "${DATA}/"
# SFC_FILE="gdas.t18z.sfcf003.nc"
# ATM_FILE="gdas.t18z.atmf003.ensres.nc"
###############################################################################
# copy orography,surface, and ancillary files to DATA from the source directory
for i in {1..6}; do
  cpfs "${SOURCE_DIR}/${CASE}_grid.tile${i}.nc" "${DATA}/"
  cpfs "${SOURCE_DIR}/${CASE}.mx${OCNRES}_oro_data.tile${i}.nc" "${DATA}/"
  cpfs "${SOURCE_DIR}/sfc/${CASE}.mx${OCNRES}.slope_type.tile${i}.nc" "${DATA}/"
  cpfs "${SOURCE_DIR}/sfc/${CASE}.mx${OCNRES}.maximum_snow_albedo.tile${i}.nc" "${DATA}/"
  cpfs "${SOURCE_DIR}/sfc/${CASE}.mx${OCNRES}.snowfree_albedo.tile${i}.nc" "${DATA}/"
  cpfs "${SOURCE_DIR}/sfc/${CASE}.mx${OCNRES}.soil_type.tile${i}.nc" "${DATA}/"
  cpfs "${SOURCE_DIR}/sfc/${CASE}.mx${OCNRES}.vegetation_type.tile${i}.nc" "${DATA}/"
  cpfs "${SOURCE_DIR}/sfc/${CASE}.mx${OCNRES}.substrate_temperature.tile${i}.nc" "${DATA}/"
  cpfs "${SOURCE_DIR}/sfc/${CASE}.mx${OCNRES}.vegetation_greenness.tile${i}.nc" "${DATA}/"
  cpfs "${SOURCE_DIR}/sfc/${CASE}.mx${OCNRES}.facsf.tile${i}.nc" "${DATA}/"
done
################################################################################
# add the namelist and run chgres
cat << EOF > ./fort.41
&config
  mosaic_file_target_grid="./${CASE}_mosaic.nc"
  fix_dir_target_grid="./"
  orog_dir_target_grid="./"
 
 orog_files_target_grid="${CASE}.mx${OCNRES}_oro_data.tile1.nc","${CASE}.mx${OCNRES}_oro_data.tile2.nc","${CASE}.mx${OCNRES}_oro_data.tile3.nc","${CASE}.mx${OCNRES}_oro_data.tile4.nc","${CASE}.mx${OCNRES}_oro_data.tile5.nc","${CASE}.mx${OCNRES}_oro_data.tile6.nc"
  vcoord_file_target_grid="./global_hyblev.l${LEVS}.txt"
  mosaic_file_input_grid="NULL"
  orog_dir_input_grid="NULL"
  orog_files_input_grid="NULL"
  data_dir_input_grid="./"
  atm_files_input_grid="${ATM_FILE}"
  atm_core_files_input_grid="NULL"
  atm_tracer_files_input_grid="NULL"
  sfc_files_input_grid="${SFC_FILE}"
  nst_files_input_grid="NULL"
  grib2_file_input_grid="NULL"
  geogrid_file_input_grid="NULL"
  varmap_file="NULL"
  wam_parm_file="NULL"
  cycle_year=${PDY:0:4}
  cycle_mon=${PDY:4:2}
  cycle_day=${PDY:6:2}
  cycle_hour=${cyc}
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

${APRUN_CHGRES} "${CHGRESEXEC}"
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
