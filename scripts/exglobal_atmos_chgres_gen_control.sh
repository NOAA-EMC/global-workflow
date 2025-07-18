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
export APRUN_CHGRES=${APRUN_CHGRES:-"mpiexec -l -n ${ntasks} -ppn ${tasks_per_node} --cpu-bind core"}
################################################################################
#dates
CDATE=${CDATE:?}
iy=$(echo ${CDATE}|cut -c1-4)
im=$(echo ${CDATE}|cut -c5-6)
id=$(echo ${CDATE}|cut -c7-8)
ih=$(echo ${CDATE}|cut -c9-10)
cyc_hour=$(printf "%03d" "$(echo ${CDATE} | cut -c9-10)")
##############################################################
DESTINATION_DIR="${DATA}"
SFC_DESTINATION_DIR="${DESTINATION_DIR}/sfc"
MOSAIC_DESTINATION_FILE="${DESTINATION_DIR}/${CASE_CHANGE}_mosaic.nc"
ATM_FILE="gfs.t00z.atmf000.nc"
SFC_FILE="gfs.t00z.sfcf000.nc"
HYBLEV_FILE="${DESTINATION_DIR}/global_hyblev.l${LEVS}.txt"
SOURCE_DIR="${HOMEgfs}/fix/orog/${CASE_CHANGE}"

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
copy_file "${HOMEgfs}/fix/am/global_hyblev.l${LEVS}.txt" "${DESTINATION_DIR}"

echo "All files copied successfully."

OROG_TARGET_FILES=$(for i in {1..6}; do
    printf "\"${CASE_CHANGE}.mx100_oro_data.tile%d.nc\"" "$i"
    if [ "$i" -lt 6 ]; then
        printf ","
    fi
done)

# add the namelist
cat << EOF > ./fort.41
&config
mosaic_file_target_grid="${MOSAIC_DESTINATION_FILE}"
fix_dir_target_grid="${SFC_DESTINATION_DIR}"
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

eval "${APRUN_CHGRES}" "${CHGRESEXEC}" "${REDOUT}${PGMOUT}" "${REDERR}${PGMERR}"
# Ensure COMIN_ATMOS_INPUT_MEM directory exists, create if it does not
if [[ ! -d "${COMIN_ATMOS_INPUT_MEM}" ]]; then
    echo "Directory ${COMIN_ATMOS_INPUT_MEM} does not exist. Creating it."
    if ! mkdir -p "${COMIN_ATMOS_INPUT_MEM}"; then
        echo "Error: Failed to create directory ${COMIN_ATMOS_INPUT_MEM}."
        exit 1
    fi
fi

# Copy out.atm.tile{1..6}.nc to COMIN_ATMOS_INPUT_MEM, force overwrite
for i in {1..6}; do
    src_file="out.atm.tile${i}.nc"
    if [[ -f "${src_file}" ]]; then
        echo "Copying ${src_file} to ${COMIN_ATMOS_INPUT_MEM}/"
        cp -f "${src_file}" "${COMIN_ATMOS_INPUT_MEM}/"
    else
        echo "Warning: ${src_file} does not exist and will not be copied."
    fi
done
exit "${err}"
