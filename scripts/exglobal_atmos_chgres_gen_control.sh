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
# at full resolution
ATMF03="gfs.t00z.atmf000.nc"
SFC03="gfs.t00z.sfcf000.nc"
# at ensemble resolution
ATMF03ENS=${ATMF03ENS:-${COMOUT_ATMOS_HISTORY_MEM}/${APREFIX}atmf003.ensres.nc}
export OMP_NUM_THREADS=${OMP_NUM_THREADS_CH:-6}
export APRUN_CHGRES=${APRUN_CHGRES:-${APRUN:-""}}
##############################################################
# If analysis increment is written by GSI, regrid forecasts to increment resolution
cat << EOF > ./fort.41
&config
mosaic_file_target_grid="/lfs/h2/emc/nems/noscrub/emc.nems/UFS_UTILS/reg_tests/chgres_cube/fix/C96/C96_mosaic.nc"
fix_dir_target_grid="/lfs/h2/emc/nems/noscrub/emc.nems/UFS_UTILS/reg_tests/chgres_cube/fix/C96/sfc"
orog_dir_target_grid="/lfs/h2/emc/nems/noscrub/emc.nems/UFS_UTILS/reg_tests/chgres_cube/fix/C96"
orog_files_target_grid=""NULL"
vcoord_file_target_grid="/lfs/h2/emc/global/noscrub/anton.fernando/UFS_UTILS/fix/am/global_hyblev.l64.txt"
mosaic_file_input_grid="NULL"
orog_dir_input_grid="NULL"
orog_files_input_grid="NULL"
data_dir_input_grid="/lfs/h2/emc/nems/noscrub/emc.nems/UFS_UTILS/reg_tests/chgres_cube/input_data/fv3.netcdf"
atm_files_input_grid=$ATMF03
atm_core_files_input_grid="NULL"
atm_tracer_files_input_grid="NULL"
sfc_files_input_grid=$SFC03
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
