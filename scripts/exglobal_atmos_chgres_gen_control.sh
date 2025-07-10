#! /usr/bin/env bash
################################################################################
####  UNIX Script Documentation Block
#                      .                                             .
# Script name:         exglobal_atmos_chgres_gen_control.sh
# Script description:  Runs chgres on changing resolution of GEFS stage ic control member
################################################################################

#  Directories.
pwd=$(pwd)
# Dependent Scripts and Executables
CHGRESEXEC=${CHGRESEXEC:-${EXECufs}/chgres_cube}
export NTHREADS_CHGRES=${NTHREADS_CHGRES:-1}
PGMOUT=${PGMOUT:-${pgmout:-'&1'}}
PGMERR=${PGMERR:-${pgmerr:-'&2'}}
REDOUT=${REDOUT:-'1>'}
REDERR=${REDERR:-'2>'}
DATA=${DATA:-${pwd}}

export OMP_NUM_THREADS=${OMP_NUM_THREADS_CH:-1}
export APRUN_CHGRES="mpiexec -l -n 12 -ppn 12 --cpu-bind core"
##############################################################
DESTINATION_DIR="${DATA}/gen_control_ic"
SFC_DESTINATION_DIR="${DESTINATION_DIR}/sfc"
MOSAIC_DESTINATION_FILE="${DESTINATION_DIR}/C96_mosaic.nc"
ATM_FILE="gfs.t00z.atmf000.nc"
SFC_FILE="gfs.t00z.sfcf000.nc"
HYBLEV_FILE="${DESTINATION_DIR}/global_hyblev.l64.txt"
SOURCE_DIR="${HOMEgfs}/fix/orog/C96"

# Remove existing $DATA/gen_control_ic
rm -rf "${DESTINATION_DIR}"

# Create directory $DATA/gen_control_ic
echo "Creating directory: ${DESTINATION_DIR}"
if ! mkdir -p "${DESTINATION_DIR}"; then
    echo "Error: Failed to create directory ${DESTINATION_DIR}" >&2
    exit 1
fi

# Ensure the source directory exists
if [[ ! -d "${SOURCE_DIR}" ]]; then
    echo "Error: Source directory ${SOURCE_DIR} does not exist."
    exit 1
fi

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

# Copy all contents (including subdirectories) to $DESTINATION_DIR
echo "Copying all contents from ${SOURCE_DIR} to ${DESTINATION_DIR}..."
for item in "${SOURCE_DIR}"/*; do
    copy_file "${item}" "${DESTINATION_DIR}"
    chmod -R u+w "${DESTINATION_DIR}/$(basename "${item}")"
done

echo "All contents from ${SOURCE_DIR} copied successfully to ${DESTINATION_DIR}."

copy_file "/lfs/h2/emc/nems/noscrub/emc.nems/UFS_UTILS/reg_tests/chgres_cube/input_data/fv3.netcdf/gfs.t00z.atmf000.nc" "${DESTINATION_DIR}"
copy_file "/lfs/h2/emc/nems/noscrub/emc.nems/UFS_UTILS/reg_tests/chgres_cube/input_data/fv3.netcdf/gfs.t00z.sfcf000.nc" "${DESTINATION_DIR}"
copy_file "${HOMEgfs}/fix/am/global_hyblev.l64.txt" "${DESTINATION_DIR}"

echo "All files copied successfully."

# If analysis increment is written by GSI, regrid forecasts to increment resolution
cat << EOF > ./fort.41
&config
mosaic_file_target_grid="${MOSAIC_DESTINATION_FILE}"
fix_dir_target_grid="${SFC_DESTINATION_DIR}"
orog_dir_target_grid="${DESTINATION_DIR}"
orog_files_target_grid="C96.mx100_oro_data.tile1.nc","C96.mx100_oro_data.tile2.nc","C96.mx100_oro_data.tile3.nc","C96.mx100_oro_data.tile4.nc","C96.mx100_oro_data.tile5.nc","C96.mx100_oro_data.tile6.nc"
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
cycle_year=2020
cycle_mon=02
cycle_day=02
cycle_hour=00
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

"$APRUN_CHGRES" "$CHGRESEXEC" "$REDOUT$PGMOUT" "$REDERR$PGMERR"
exit "${err}"
