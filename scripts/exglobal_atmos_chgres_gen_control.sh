#! /usr/bin/env bash
################################################################################
####  UNIX Script Documentation Block
#                      .                                             .
# Script name:         exglobal_atmos_chgres_gen_control.sh
# Script description:  Runs chgres on changing resolution of GEFS stage ic control member
################################################################################
# copy input files to DATA from the source directory
cpreq "${FIXgfs}/am/global_hyblev.l${LEVS}.txt" "${DATA}/"
cpreq "${FIXgfs}/orog/${CASE}/${CASE}_mosaic.nc" "${DATA}/"
cpreq "${ATM_FILE}" "${DATA}/atm_input.nc"
cpreq "${SFC_FILE}" "${DATA}/sfc_input.nc"
###############################################################################
# copy orography,surface, and ancillary files to DATA from the source directory
for i in {1..6}; do
    cpreq "${FIXgfs}/orog/${CASE}/${CASE}_grid.tile${i}.nc" "${DATA}/"
    cpreq "${FIXgfs}/orog/${CASE}/${CASE}.mx${OCNRES}_oro_data.tile${i}.nc" "${DATA}/"
    cpreq "${FIXgfs}/orog/${CASE}/sfc/${CASE}.mx${OCNRES}.slope_type.tile${i}.nc" "${DATA}/"
    cpreq "${FIXgfs}/orog/${CASE}/sfc/${CASE}.mx${OCNRES}.maximum_snow_albedo.tile${i}.nc" "${DATA}/"
    cpreq "${FIXgfs}/orog/${CASE}/sfc/${CASE}.mx${OCNRES}.snowfree_albedo.tile${i}.nc" "${DATA}/"
    cpreq "${FIXgfs}/orog/${CASE}/sfc/${CASE}.mx${OCNRES}.soil_type.tile${i}.nc" "${DATA}/"
    cpreq "${FIXgfs}/orog/${CASE}/sfc/${CASE}.mx${OCNRES}.vegetation_type.tile${i}.nc" "${DATA}/"
    cpreq "${FIXgfs}/orog/${CASE}/sfc/${CASE}.mx${OCNRES}.substrate_temperature.tile${i}.nc" "${DATA}/"
    cpreq "${FIXgfs}/orog/${CASE}/sfc/${CASE}.mx${OCNRES}.vegetation_greenness.tile${i}.nc" "${DATA}/"
    cpreq "${FIXgfs}/orog/${CASE}/sfc/${CASE}.mx${OCNRES}.facsf.tile${i}.nc" "${DATA}/"
done
################################################################################
# add the namelist and run chgres
cat << EOF > ./fort.41
&config
  mosaic_file_target_grid="./${CASE}_mosaic.nc"
  fix_dir_target_grid="./"
  orog_dir_target_grid="./"
  orog_files_target_grid="${CASE}.mx${OCNRES}_oro_data.tile1.nc",\
                         "${CASE}.mx${OCNRES}_oro_data.tile2.nc",\
                         "${CASE}.mx${OCNRES}_oro_data.tile3.nc",\
                         "${CASE}.mx${OCNRES}_oro_data.tile4.nc",\
                         "${CASE}.mx${OCNRES}_oro_data.tile5.nc",\
                         "${CASE}.mx${OCNRES}_oro_data.tile6.nc"
  vcoord_file_target_grid="./global_hyblev.l${LEVS}.txt"
  mosaic_file_input_grid="NULL"
  orog_dir_input_grid="NULL"
  orog_files_input_grid="NULL"
  data_dir_input_grid="./"
  atm_files_input_grid="./atm_input.nc"
  atm_core_files_input_grid="NULL"
  atm_tracer_files_input_grid="NULL"
  sfc_files_input_grid="./sfc_input.nc"
  nst_files_input_grid="NULL"
  grib2_file_input_grid="NULL"
  geogrid_file_input_grid="NULL"
  varmap_file="NULL"
  wam_parm_file="NULL"
  cycle_year=${BDATE:0:4}
  cycle_mon=${BDATE:4:2}
  cycle_day=${BDATE:6:2}
  cycle_hour=${BDATE:8:2}
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

${APRUN_CHGRES} "${HOMEgfs}/sorc/ufs_utils.fd/exec/chgres_cube"
export err=$?
if [[ ${err} -ne 0 ]]; then
    err_exit "chgres_cube failed to create cold start ICs, ABORT!"
fi
################################################################################
# copy output files to com
for i in {1..6}; do
    cpreq "out.atm.tile${i}.nc" "${COMOUT_ATMOS_INPUT_MEM}/gfs_data.tile${i}.nc"
    cpreq "out.sfc.tile${i}.nc" "${COMOUT_ATMOS_INPUT_MEM}/sfc_data.tile${i}.nc"
done
cpreq "gfs_ctrl.nc" "${COMOUT_ATMOS_INPUT_MEM}/"
################################################################################
