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
DATA=${DATA:-$pwd}

# at ensemble resolution
export OMP_NUM_THREADS=${OMP_NUM_THREADS_CH:-1}
export APRUN_CHGRES="mpiexec -l -n 12 -ppn 12 --cpu-bind core"
##############################################################
#copy input data to DATA directory
# Ensure $DATA/gen_control_ic directory exists
# Forcefully create $DATA/gen_control_ic directory
if [ -d "$DATA/gen_control_ic" ]; then
    echo "Directory $DATA/gen_control_ic already exists. Removing it..."
    rm -rf "$DATA/gen_control_ic"
    if [ $? -ne 0 ]; then
        echo "Error: Failed to remove existing directory $DATA/gen_control_ic"
        exit 1
    fi
fi

echo "Creating directory: $DATA/gen_control_ic"
mkdir -p "$DATA/gen_control_ic"
if [ $? -ne 0 ]; then
    echo "Error: Failed to create directory $DATA/gen_control_ic"
    exit 1
fi

# Function to copy files and check success
copy_file() {
    local src=$1
    local dest=$2
    echo "Copying $src to $dest"
    cp -f "$src" "$dest"
    if [ $? -ne 0 ]; then
        echo "Error: Failed to copy $src to $dest"
        exit 1
    fi
}
export GEN_CONTROL_IC_DIR="$DATA/gen_control_ic"
export GEN_CONTROL_IC_MOSAIC="$GEN_CONTROL_IC_DIR/C96_mosaic.nc"
export ATM_FILE="gfs.t00z.atmf000.nc"
export SFC_FILE="gfs.t00z.sfcf000.nc"
# Copy required files to $DATA/gen_control_ic
copy_file "/lfs/h2/emc/nems/noscrub/emc.nems/UFS_UTILS/reg_tests/chgres_cube/fix/C96/C96_mosaic.nc" "$GEN_CONTROL_IC_DIR"
copy_file "/lfs/h2/emc/nems/noscrub/emc.nems/UFS_UTILS/reg_tests/chgres_cube/input_data/fv3.netcdf/gfs.t00z.sfcf000.nc" "$GEN_CONTROL_IC_DIR"
copy_file "/lfs/h2/emc/nems/noscrub/emc.nems/UFS_UTILS/reg_tests/chgres_cube/input_data/fv3.netcdf/gfs.t00z.atmf000.nc" "$GEN_CONTROL_IC_DIR"
copy_file "/lfs/h2/emc/nems/noscrub/emc.nems/UFS_UTILS/reg_tests/chgres_cube/fix/C96/C96.mx100_oro_data.tile1.nc" "$GEN_CONTROL_IC_DIR"
copy_file "/lfs/h2/emc/nems/noscrub/emc.nems/UFS_UTILS/reg_tests/chgres_cube/fix/C96/C96.mx100_oro_data.tile2.nc" "$GEN_CONTROL_IC_DIR"
copy_file "/lfs/h2/emc/nems/noscrub/emc.nems/UFS_UTILS/reg_tests/chgres_cube/fix/C96/C96.mx100_oro_data.tile3.nc" "$GEN_CONTROL_IC_DIR"
copy_file "/lfs/h2/emc/nems/noscrub/emc.nems/UFS_UTILS/reg_tests/chgres_cube/fix/C96/C96.mx100_oro_data.tile4.nc" "$GEN_CONTROL_IC_DIR"
copy_file "/lfs/h2/emc/nems/noscrub/emc.nems/UFS_UTILS/reg_tests/chgres_cube/fix/C96/C96.mx100_oro_data.tile5.nc" "$GEN_CONTROL_IC_DIR"
copy_file "/lfs/h2/emc/nems/noscrub/emc.nems/UFS_UTILS/reg_tests/chgres_cube/fix/C96/C96.mx100_oro_data.tile6.nc" "$GEN_CONTROL_IC_DIR"
copy_file "/lfs/h2/emc/nems/noscrub/emc.nems/UFS_UTILS/reg_tests/chgres_cube/fix/C96/C96_grid.tile1.nc" "$GEN_CONTROL_IC_DIR"
copy_file "/lfs/h2/emc/nems/noscrub/emc.nems/UFS_UTILS/reg_tests/chgres_cube/fix/C96/C96_grid.tile2.nc" "$GEN_CONTROL_IC_DIR"
copy_file "/lfs/h2/emc/nems/noscrub/emc.nems/UFS_UTILS/reg_tests/chgres_cube/fix/C96/C96_grid.tile3.nc" "$GEN_CONTROL_IC_DIR"
copy_file "/lfs/h2/emc/nems/noscrub/emc.nems/UFS_UTILS/reg_tests/chgres_cube/fix/C96/C96_grid.tile4.nc" "$GEN_CONTROL_IC_DIR"
copy_file "/lfs/h2/emc/nems/noscrub/emc.nems/UFS_UTILS/reg_tests/chgres_cube/fix/C96/C96_grid.tile5.nc" "$GEN_CONTROL_IC_DIR"
copy_file "/lfs/h2/emc/nems/noscrub/emc.nems/UFS_UTILS/reg_tests/chgres_cube/fix/C96/C96_grid.tile6.nc" "$GEN_CONTROL_IC_DIR"
copy_file "/lfs/h2/emc/global/noscrub/anton.fernando/UFS_UTILS/fix/am/global_hyblev.l64.txt" "$GEN_CONTROL_IC_DIR"

echo "All files copied successfully."

# If analysis increment is written by GSI, regrid forecasts to increment resolution
cat << EOF > ./fort.41
&config
mosaic_file_target_grid="$GEN_CONTROL_IC_MOSAIC"
fix_dir_target_grid="/lfs/h2/emc/nems/noscrub/emc.nems/UFS_UTILS/reg_tests/chgres_cube/fix/C96/sfc"
orog_dir_target_grid="/lfs/h2/emc/nems/noscrub/emc.nems/UFS_UTILS/reg_tests/chgres_cube/fix/C96"
orog_files_target_grid="C96.mx100_oro_data.tile1.nc","C96.mx100_oro_data.tile2.nc","C96.mx100_oro_data.tile3.nc","C96.mx100_oro_data.tile4.nc","C96.mx100_oro_data.tile5.nc","C96.mx100_oro_data.tile6.nc"
vcoord_file_target_grid="/lfs/h2/emc/global/noscrub/anton.fernando/UFS_UTILS/fix/am/global_hyblev.l64.txt"
mosaic_file_input_grid="NULL"
orog_dir_input_grid="NULL"
orog_files_input_grid="NULL"
data_dir_input_grid="/lfs/h2/emc/nems/noscrub/emc.nems/UFS_UTILS/reg_tests/chgres_cube/input_data/fv3.netcdf"
atm_files_input_grid=$ATM_FILE
atm_core_files_input_grid="NULL"
atm_tracer_files_input_grid="NULL"
sfc_files_input_grid=$SFC_FILE
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

$APRUN_CHGRES $CHGRESEXEC $REDOUT$PGMOUT $REDERR$PGMERR
exit $err
