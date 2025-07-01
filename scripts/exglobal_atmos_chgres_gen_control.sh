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
cp "/lfs/h2/emc/global/noscrub/anton.fernando/global-workflow/sorc/ufs_utils.fd/exec/chgres_cube" .
cp "/lfs/h2/emc/nems/noscrub/emc.nems/UFS_UTILS/reg_tests/chgres_cube/input_data/fv3.netcdf/gfs.t00z.atmf000.nc" .
cp "/lfs/h2/emc/nems/noscrub/emc.nems/UFS_UTILS/reg_tests/chgres_cube/input_data/fv3.netcdf/gfs.t00z.sfcf000.nc" .



export CHGRESNCEXEC="./chgres_cube"
export NTHREADS_CHGRES=${NTHREADS_CHGRES:-1}

# at full resolution
ATMF03="/lfs/h2/emc/nems/noscrub/emc.nems/UFS_UTILS/reg_tests/chgres_cube/input_data/fv3.netcdf/gfs.t00z.atmf000.nc"
# at ensemble resolution
ATMF03ENS=${ATMF03ENS:-${COMOUT_ATMOS_HISTORY_MEM}/${APREFIX}atmf003.ensres.nc}
export APRUN=${APRUN:-${APRUN:-"mpiexec -n 12 -ppn 12 --cpu-bind core"}}
##############################################################
# If analysis increment is written by GSI, regrid forecasts to increment resolution
cat > chgres_nc_gauss03.nml << EOF
&config
mosaic_file_target_grid="/lfs/h2/emc/nems/noscrub/emc.nems/UFS_UTILS/reg_tests/chgres_cube/fix/C96/C96_mosaic.nc"
fix_dir_target_grid="/lfs/h2/emc/nems/noscrub/emc.nems/UFS_UTILS/reg_tests/chgres_cube/fix/C96/sfc"
orog_dir_target_grid="/lfs/h2/emc/nems/noscrub/emc.nems/UFS_UTILS/reg_tests/chgres_cube/fix/C96"
orog_files_target_grid="C96.mx100_oro_data.tile1.nc","C96.mx100_oro_data.tile2.nc","C96.mx100_oro_data.tile3.nc","C96.mx100_oro_data.tile4.nc","C96.mx100_oro_data.tile5.nc","C96.mx100_oro_data.tile6.nc"
vcoord_file_target_grid="/lfs/h2/emc/global/noscrub/anton.fernando/UFS_UTILS/reg_tests/chgres_cube/fix/am/global_hyblev.l64.txt"
mosaic_file_input_grid="NULL"
orog_dir_input_grid="NULL"
orog_files_input_grid="NULL"
data_dir_input_grid="/lfs/h2/emc/nems/noscrub/emc.nems/UFS_UTILS/reg_tests/chgres_cube/input_data/fv3.netcdf"
atm_files_input_grid="gfs.t00z.atmf000.nc"
atm_core_files_input_grid="NULL"
atm_tracer_files_input_grid="NULL"
sfc_files_input_grid="gfs.t00z.sfcf000.nc"
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

$APRUN $CHGRESNCEXEC "chgres_nc_gauss03.nml"
exit $err
