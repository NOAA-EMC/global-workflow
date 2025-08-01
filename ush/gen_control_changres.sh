#!/usr/bin/env bash
################################################################################
####  UNIX Script Documentation Block
#                      .                                             .
# Script name:         gen_control_changres.sh
# Script description:  Generates chgres_cube namelist and runs the executable
#
# Author:              Global Workflow Team
# Date:                July 2025
#
# Script history log:
# 2025-07-28  Initial script creation
################################################################################

set -eu

# Generate the namelist for chgres
cat << EOF > ./fort.41
&config
mosaic_file_target_grid="${MOSAIC_DESTINATION_FILE:-"NULL"}"
fix_dir_target_grid="${DESTINATION_DIR:-"NULL"}"
orog_dir_target_grid="${DESTINATION_DIR:-"NULL"}"
orog_files_target_grid=${OROG_TARGET_FILES:-"NULL"}
vcoord_file_target_grid="${HYBLEV_FILE:-"NULL"}"
mosaic_file_input_grid="${MOSAIC_FILE_INPUT_GRID:-"NULL"}"
orog_dir_input_grid="${OROG_DIR_INPUT_GRID:-"NULL"}"
orog_files_input_grid="${OROG_FILES_INPUT_GRID:-"NULL"}"
data_dir_input_grid="${DESTINATION_DIR:-"NULL"}"
atm_files_input_grid="${ATM_FILE:-"NULL"}"
atm_core_files_input_grid="${ATM_CORE_FILES_INPUT_GRID:-"NULL"}"
atm_tracer_files_input_grid="${ATM_TRACER_FILES_INPUT_GRID:-"NULL"}"
sfc_files_input_grid="${SFC_FILE:-"NULL"}"
nst_files_input_grid="${NST_FILES_INPUT_GRID:-"NULL"}"
grib2_file_input_grid="${GRIB2_FILE_INPUT_GRID:-"NULL"}"
geogrid_file_input_grid="${GEOGRID_FILE_INPUT_GRID:-"NULL"}"
varmap_file="${VARMAP_FILE:-"NULL"}"
wam_parm_file="${WAM_PARM_FILE:-"NULL"}"
cycle_year=${iy:-"2020"}
cycle_mon=${im:-"01"}
cycle_day=${id:-"01"}
cycle_hour=${ih:-"00"}
convert_atm=${CONVERT_ATM:-".false."}
convert_sfc=${CONVERT_SFC:-".true."}
convert_nst=${CONVERT_NST:-".true."}
input_type="${INPUT_TYPE:-"gaussian_netcdf"}"
tracers="sphum","liq_wat","o3mr","ice_wat","rainwat","snowwat","graupel"
tracers_input="spfh","clwmr","o3mr","icmr","rwmr","snmr","grle"
regional=${REGIONAL:-"0"}
halo_bndy=${HALO_BNDY:-"0"}
halo_blend=${HALO_BLEND:-"0"}
sotyp_from_climo=${SOTYP_FROM_CLIMO:-".true."}
vgtyp_from_climo=${VGTYPE_FROM_CLIMO:-".true."}
vgfrc_from_climo=${VGFRC_FROM_CLIMO:-".true."}
minmax_vgfrc_from_climo=${MINMAX_VGFR_FROM_CLIMO:-".true."}
tg3_from_soil=${TG3_FROM_SOIL:-".false."}
lai_from_climo=${LAI_FROM_CLIMO:-".true."}
external_model=${EXTERNAL_MODEL:-"GFS"}
nsoill_out=${NSOILL_OUT:-"4"}
thomp_mp_climo_file=${THOMP_MP_CLIMO_FILE:-"NULL"}
wam_cold_start=${WAM_COLD_START:-".false."}
/
EOF

if [[ -n "${output_log:-}" ]]; then
  cpfs fort.41 "${output_log}"
fi

# Run the chgres executable
eval "${APRUN_CHGRES}" "${CHGRESEXEC}" "${PGMOUT}"
err=$?

exit "${err}"
