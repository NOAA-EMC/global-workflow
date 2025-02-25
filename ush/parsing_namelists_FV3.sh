#! /usr/bin/env bash

# parsing namelist of FV3, diag_table, etc.

# Disable variable not used warnings
# shellcheck disable=SC2034
FV3_namelists(){

# setup the tables
DIAG_TABLE=${DIAG_TABLE:-${PARMgfs}/ufs/fv3/diag_table}
DIAG_TABLE_APPEND=${DIAG_TABLE_APPEND:-${PARMgfs}/ufs/fv3/diag_table_aod}
DATA_TABLE=${DATA_TABLE:-${PARMgfs}/ufs/MOM6_data_table.IN}
FIELD_TABLE=${FIELD_TABLE:-${PARMgfs}/ufs/fv3/field_table}

# set cdmbgwd
if (( gwd_opt == 2 )) && [[ ${do_gsl_drag_ls_bl} == ".true." ]]; then
  cdmbgwd=${cdmbgwd_gsl}
fi

# ensure non-prognostic tracers are set
dnats=${dnats:-0}

# build the diag_table
{
echo "UFS_Weather_Model_Forecast"
if [[ "${DOIAU}" = "YES" ]]; then
  echo "${previous_cycle:0:4} ${previous_cycle:4:2} ${previous_cycle:6:2} ${previous_cycle:8:2} 0 0"
else
  echo "${current_cycle:0:4} ${current_cycle:4:2} ${current_cycle:6:2} ${current_cycle:8:2} 0 0"
fi
cat "${DIAG_TABLE}"
if [[ -n "${AERO_DIAG_TABLE:-}" ]]; then
  cat "${AERO_DIAG_TABLE}"
fi
cat "${DIAG_TABLE_APPEND}"
} >> diag_table_template

local template=diag_table_template
local SYEAR=${current_cycle:0:4}
local SMONTH=${current_cycle:4:2}
local SDAY=${current_cycle:6:2}
local CHOUR=${current_cycle:8:2}
local MOM6_OUTPUT_DIR="./MOM6_OUTPUT"

if [[ "${REPLAY_ICS:-NO}" == "YES" ]]; then
  local current_cycle_p1 
  current_cycle_p1=$(date --utc -d "${current_cycle:0:8} ${current_cycle:8:2} + ${FHOUT_OCN} hours" +%Y%m%d%H)
  local current_cycle_offset
  current_cycle_offset=$(date --utc -d "${current_cycle:0:8} ${current_cycle:8:2} + ${OFFSET_START_HOUR} hours" +%Y%m%d%H)
  local SYEAR1=${current_cycle_p1:0:4}
  local SMONTH1=${current_cycle_p1:4:2}
  local SDAY1=${current_cycle_p1:6:2}
  local CHOUR1=${current_cycle_p1:8:2}
  local CHOUR_offset=${current_cycle_offset:8:2}
fi

atparse < "${template}" >> "diag_table"


# copy data table
${NCP} "${DATA_TABLE}" data_table

# build field_table
if [[ -n "${AERO_FIELD_TABLE:-}" ]]; then
  nrec=$(wc -l < "${FIELD_TABLE}")
  prec=${nrec}
  if (( dnats > 0 )); then
    prec=$( grep -F -n TRACER "${FIELD_TABLE}" 2> /dev/null | tail -n "${dnats}" | head -1 | cut -d: -f1 )
    prec=${prec:-0}
    prec=$(( prec > 0 ? prec - 1 : prec ))
  fi
  { \
    head -n "${prec}" "${FIELD_TABLE}" ; \
    cat "${AERO_FIELD_TABLE}" ; \
    tail -n $(( nrec - prec )) "${FIELD_TABLE}" ; \
  } > field_table
  # add non-prognostic tracers from additional table
  dnats=$(( dnats + dnats_aero ))
else
  ${NCP} "${FIELD_TABLE}" field_table
fi

cat > input.nml <<EOF
&atmos_model_nml
  blocksize = ${blocksize}
  chksum_debug = ${chksum_debug}
  dycore_only = ${dycore_only}
  ccpp_suite = ${CCPP_SUITE}
  ${atmos_model_nml:-}
/

&diag_manager_nml
  prepend_date = .false.
  max_output_fields = 300
  ${diag_manager_nml:-}
/

&fms_nml
  clock_grain = 'ROUTINE'
  domains_stack_size = ${domains_stack_size:-3000000}
  print_memory_usage = ${print_memory_usage:-".false."}
  ${fms_nml:-}
/

&fms2_io_nml
  netcdf_default_format = "netcdf4"
/

&fv_core_nml
  layout = ${layout_x},${layout_y}
  io_layout = ${io_layout}
  npx = ${npx}
  npy = ${npy}
  ntiles = ${ntiles}
  npz = ${npz}
  dz_min =  ${dz_min:-"6"}
  psm_bc = ${psm_bc:-"0"}
  grid_type = -1
  make_nh = ${make_nh}
  fv_debug = ${fv_debug:-".false."}
  range_warn = ${range_warn:-".true."}
  reset_eta = .false.
  n_sponge = ${n_sponge:-"10"}
  nudge_qv = ${nudge_qv:-".false."}
  nudge_dz = ${nudge_dz:-".false."}
  tau = ${tau:-10.}
  rf_cutoff = ${rf_cutoff:-"7.5e2"}
  d2_bg_k1 = ${d2_bg_k1:-"0.15"}
  d2_bg_k2 = ${d2_bg_k2:-"0.02"}
  kord_tm = ${kord_tm:-"-9"}
  kord_mt = ${kord_mt:-"9"}
  kord_wz = ${kord_wz:-"9"}
  kord_tr = ${kord_tr:-"9"}
  hydrostatic = ${hydrostatic}
  phys_hydrostatic = ${phys_hydrostatic}
  use_hydro_pressure = ${use_hydro_pressure}
  pass_full_omega_to_physics_in_non_hydrostatic_mode = ${pass_full_omega_to_physics_in_non_hydrostatic_mode:-".false."}
  beta = 0.
  a_imp = 1.
  p_fac = 0.1
  k_split = ${k_split}
  n_split = ${n_split}
  nwat = ${nwat:-2}
  na_init = ${na_init}
  d_ext = 0.
  dnats = ${dnats}
  fv_sg_adj = ${fv_sg_adj:-"450"}
  d2_bg = 0.
  nord = ${nord:-3}
  dddmp = ${dddmp:-0.1}
  d4_bg = ${d4_bg:-0.15}
  vtdm4 = ${vtdm4}
  delt_max = ${delt_max:-"0.002"}
  ke_bg = 0.
  do_vort_damp = ${do_vort_damp}
  external_ic = ${external_ic}
  external_eta = ${external_eta:-.true.}
  gfs_phil = ${gfs_phil:-".false."}
  nggps_ic = ${nggps_ic}
  mountain = ${mountain}
  ncep_ic = ${ncep_ic}
  d_con = ${d_con}
  hord_mt = ${hord_mt}
  hord_vt = ${hord_xx}
  hord_tm = ${hord_xx}
  hord_dp = ${hord_dp}
  hord_tr = ${hord_tr:-"8"}
  adjust_dry_mass = ${adjust_dry_mass:-".true."}
  dry_mass=${dry_mass:-98320.0}
  consv_te = ${consv_te}
  do_sat_adj = ${do_sat_adj:-".false."}
  fast_tau_w_sec = ${fast_tau_w_sec:-"0.2"}
  consv_am = .false.
  fill = .true.
  dwind_2d = .false.
  print_freq = ${print_freq}
  warm_start = ${warm_start}
  no_dycore = ${no_dycore}
  z_tracer = .true.
  agrid_vel_rst = ${agrid_vel_rst:-".true."}
  read_increment = ${read_increment}
  res_latlon_dynamics = ${res_latlon_dynamics}
  ${fv_core_nml-}
/

&external_ic_nml
  filtered_terrain = ${filtered_terrain}
  levp = ${LEVS}
  gfs_dwinds = ${gfs_dwinds}
  checker_tr = .false.
  nt_checker = 0
  ${external_ic_nml-}
/

&gfs_physics_nml
  fhzero       = ${FHZER}
  h2o_phys     = ${h2o_phys:-".true."}
  ldiag3d      = ${ldiag3d:-".false."}
  qdiag3d      = ${qdiag3d:-".false."}
  print_diff_pgr = ${print_diff_pgr:-".false."}
  fhcyc        = ${FHCYC}
  use_ufo      = ${use_ufo:-".true."}
  pre_rad      = ${pre_rad:-".false."}
  imp_physics  = ${imp_physics:-"99"}
EOF

case "${CCPP_SUITE:-}" in
  "FV3_GFS_v15p2_coupled")
  cat >> input.nml << EOF
  oz_phys      = .false.
  oz_phys_2015 = .true.
EOF
  ;;
  "FV3_GSD_v0")
  cat >> input.nml << EOF
  iovr         = ${iovr:-"3"}
  ltaerosol    = ${ltaerosol:-".false."}
  lradar       = ${lradar:-".false."}
  ttendlim     = ${ttendlim:-0.005}
  oz_phys      = ${oz_phys:-".false."}
  oz_phys_2015 = ${oz_phys_2015:-".true."}
  lsoil_lsm    = ${lsoil_lsm:-"4"}
  do_mynnedmf  = ${do_mynnedmf:-".false."}
  do_mynnsfclay = ${do_mynnsfclay:-".false."}
  icloud_bl    = ${icloud_bl:-"1"}
  bl_mynn_edmf = ${bl_mynn_edmf:-"1"}
  bl_mynn_tkeadvect=${bl_mynn_tkeadvect:-".true."}
  bl_mynn_edmf_mom=${bl_mynn_edmf_mom:-"1"}
  min_lakeice  = ${min_lakeice:-"0.15"}
  min_seaice   = ${min_seaice:-"0.15"}
  use_cice_alb = ${use_cice_alb:-".false."}
EOF
  ;;
  FV3_GFS_v16_coupled*)
  cat >> input.nml << EOF
  iovr         = ${iovr:-"3"}
  ltaerosol    = ${ltaerosol:-".false."}
  lradar       = ${lradar:-".false."}
  ttendlim     = ${ttendlim:-"0.005"}
  oz_phys      = ${oz_phys:-".false."}
  oz_phys_2015 = ${oz_phys_2015:-".true."}
  do_mynnedmf  = ${do_mynnedmf:-".false."}
  do_mynnsfclay = ${do_mynnsfclay:-".false."}
  icloud_bl    = ${icloud_bl:-"1"}
  bl_mynn_edmf = ${bl_mynn_edmf:-"1"}
  bl_mynn_tkeadvect = ${bl_mynn_tkeadvect:-".true."}
  bl_mynn_edmf_mom = ${bl_mynn_edmf_mom:-"1"}
  min_lakeice  = ${min_lakeice:-"0.15"}
  min_seaice   = ${min_seaice:-"0.15"}
EOF
  ;;
  FV3_GFS_v16*)
  cat >> input.nml << EOF
  iovr         = ${iovr:-"3"}
  ltaerosol    = ${ltaerosol:-".false."}
  lradar       = ${lradar:-".false."}
  ttendlim     = ${ttendlim:-"0.005"}
  oz_phys      = ${oz_phys:-".false."}
  oz_phys_2015 = ${oz_phys_2015:-".true."}
  lsoil_lsm    = ${lsoil_lsm:-"4"}
  do_mynnedmf  = ${do_mynnedmf:-".false."}
  do_mynnsfclay = ${do_mynnsfclay:-".false."}
  icloud_bl    = ${icloud_bl:-"1"}
  bl_mynn_edmf = ${bl_mynn_edmf:-"1"}
  bl_mynn_tkeadvect = ${bl_mynn_tkeadvect:-".true."}
  bl_mynn_edmf_mom = ${bl_mynn_edmf_mom:-"1"}
  min_lakeice  = ${min_lakeice:-"0.15"}
  min_seaice   = ${min_seaice:-"0.15"}
EOF
  ;;
  FV3_GFS_v17*)
  local default_dt_inner=$(( DELTIM/2 ))
  cat >> input.nml << EOF
  iovr         = ${iovr:-"3"}
  ltaerosol    = ${ltaerosol:-".false."}
  lradar       = ${lradar:-".true."}
  ttendlim     = ${ttendlim:-"-999"}
  dt_inner     = ${dt_inner:-"${default_dt_inner}"}
  sedi_semi    = ${sedi_semi:-".true."}
  decfl        = ${decfl:-"10"}
  oz_phys      = ${oz_phys:-".false."}
  oz_phys_2015 = ${oz_phys_2015:-".true."}
  lsoil_lsm    = ${lsoil_lsm:-"4"}
  do_mynnedmf  = ${do_mynnedmf:-".false."}
  do_mynnsfclay = ${do_mynnsfclay:-".false."}
  icloud_bl    = ${icloud_bl:-"1"}
  bl_mynn_edmf = ${bl_mynn_edmf:-"1"}
  bl_mynn_tkeadvect = ${bl_mynn_tkeadvect:-".true."}
  bl_mynn_edmf_mom = ${bl_mynn_edmf_mom:-"1"}
  do_ugwp      = ${do_ugwp:-".false."}
  do_tofd      = ${do_tofd:-".false."}
  gwd_opt      = ${gwd_opt:-"2"}
  do_ugwp_v0   = ${do_ugwp_v0:-".false."}
  do_ugwp_v1   = ${do_ugwp_v1:-".true."}
  do_ugwp_v0_orog_only = ${do_ugwp_v0_orog_only:-".false."}
  do_ugwp_v0_nst_only  = ${do_ugwp_v0_nst_only:-".false."}
  do_gsl_drag_ls_bl    = ${do_gsl_drag_ls_bl:-".true."}
  do_gsl_drag_ss       = ${do_gsl_drag_ss:-".true."}
  do_gsl_drag_tofd     = ${do_gsl_drag_tofd:-".true."}
  do_gwd_opt_psl       = ${do_gwd_opt_psl:-".false."}
  do_ugwp_v1_orog_only = ${do_ugwp_v1_orog_only:-".false."}
  min_lakeice  = ${min_lakeice:-"0.15"}
  min_seaice   = ${min_seaice:-"0.15"}
  use_cice_alb = ${use_cice_alb:-".false."}
EOF
  ;;
  FV3_global_nest*)
  local default_dt_inner=$(( DELTIM/2 ))
  cat >> input.nml << EOF
  iovr         = ${iovr:-"3"}
  lcnorm       = ${lcnorm:-".false."}
  ltaerosol    = ${ltaerosol:-".false."}
  lradar       = ${lradar:-".true."}
  ttendlim     = ${ttendlim:-"-999"}
  dt_inner     = ${dt_inner:-"${default_dt_inner}"}
  sedi_semi    = ${sedi_semi:-".true."}
  decfl        = ${decfl:-"10"}
  oz_phys      = ${oz_phys:-".false."}
  oz_phys_2015 = ${oz_phys_2015:-".true."}
  lsoil_lsm    = ${lsoil_lsm:-"4"}
  do_mynnedmf  = ${do_mynnedmf:-".false."}
  do_mynnsfclay = ${do_mynnsfclay:-".false."}
  icloud_bl    = ${icloud_bl:-"1"}
  bl_mynn_edmf = ${bl_mynn_edmf:-"1"}
  bl_mynn_tkeadvect = ${bl_mynn_tkeadvect:-".true."}
  bl_mynn_edmf_mom = ${bl_mynn_edmf_mom:-"1"}
  do_ugwp      = ${do_ugwp:-".false."}
  do_tofd      = ${do_tofd:-".false."}
  gwd_opt      = ${gwd_opt:-"2"}
  do_ugwp_v0   = ${do_ugwp_v0:-".false."}
  do_ugwp_v1   = ${do_ugwp_v1:-".true."}
  do_ugwp_v0_orog_only = ${do_ugwp_v0_orog_only:-".false."}
  do_ugwp_v0_nst_only  = ${do_ugwp_v0_nst_only:-".false."}
  do_gsl_drag_ls_bl    = ${do_gsl_drag_ls_bl:-".true."}
  do_gsl_drag_ss       = ${do_gsl_drag_ss:-".true."}
  do_gsl_drag_tofd     = ${do_gsl_drag_tofd:-".true."}
  do_ugwp_v1_orog_only = ${do_ugwp_v1_orog_only:-".false."}
  min_lakeice  = ${min_lakeice:-"0.15"}
  min_seaice   = ${min_seaice:-"0.15"}
  use_cice_alb = ${use_cice_alb:-".false."}
EOF
  ;;
  *)
  cat >> input.nml << EOF
  iovr         = ${iovr:-"3"}
EOF
  ;;
esac

cat >> input.nml <<EOF
  pdfcld       = ${pdfcld:-".false."}
  fhswr        = ${FHSWR:-"3600."}
  fhlwr        = ${FHLWR:-"3600."}
  ialb         = ${IALB:-"1"}
  iems         = ${IEMS:-"1"}
  iaer         = ${IAER}
  icliq_sw     = ${icliq_sw:-"2"}
  ico2         = ${ICO2}
  isubc_sw     = ${isubc_sw:-"2"}
  isubc_lw     = ${isubc_lw:-"2"}
  isol         = ${ISOL:-"2"}
  lwhtr        = ${lwhtr:-".true."}
  swhtr        = ${swhtr:-".true."}
  cnvgwd       = ${cnvgwd:-".true."}
  shal_cnv     = ${shal_cnv:-".true."}
  cal_pre      = ${cal_pre:-".true."}
  redrag       = ${redrag:-".true."}
  dspheat      = ${dspheat:-".true."}
  hybedmf      = ${hybedmf:-".false."}
  satmedmf     = ${satmedmf-".true."}
  isatmedmf    = ${isatmedmf-"1"}
  lheatstrg    = ${lheatstrg-".false."}
  lseaspray    = ${lseaspray:-".true."}
  random_clds  = ${random_clds:-".true."}
  trans_trac   = ${trans_trac:-".true."}
  cnvcld       = ${cnvcld:-".true."}
  xr_cnvcld    = ${xr_cnvcld:-".true."}
  imfshalcnv   = ${imfshalcnv:-"2"}
  imfdeepcnv   = ${imfdeepcnv:-"2"}
  progsigma    = ${progsigma:-".true."}
  betascu      = ${betascu:-"8.0"}
  betamcu      = ${betamcu:-"1.0"}
  betadcu      = ${betadcu:-"2.0"}
  ras          = ${ras:-".false."}
  cdmbgwd      = ${cdmbgwd:-"3.5,0.25"}
  psl_gwd_dx_factor  = ${psl_gwd_dx_factor:-"6.0"}
  prslrd0      = ${prslrd0:-"0."}
  ivegsrc      = ${ivegsrc:-"1"}
  isot         = ${isot:-"1"}
  lsoil        = ${lsoil:-"4"}
  lsm          = ${lsm:-"2"}
  iopt_dveg    = ${iopt_dveg:-"1"}
  iopt_crs     = ${iopt_crs:-"1"}
  iopt_btr     = ${iopt_btr:-"1"}
  iopt_run     = ${iopt_run:-"1"}
  iopt_sfc     = ${iopt_sfc:-"1"}
  iopt_frz     = ${iopt_frz:-"1"}
  iopt_inf     = ${iopt_inf:-"1"}
  iopt_rad     = ${iopt_rad:-"1"}
  iopt_alb     = ${iopt_alb:-"2"}
  iopt_snf     = ${iopt_snf:-"4"}
  iopt_tbot    = ${iopt_tbot:-"2"}
  iopt_stc     = ${iopt_stc:-"1"}
  iopt_trs     = ${iopt_trs:-"2"}
  iopt_diag    = ${iopt_diag:-"2"}
  debug        = ${gfs_phys_debug:-".false."}
  nstf_name    = ${nstf_name}
  nst_anl      = ${nst_anl}
  psautco      = ${psautco:-"0.0008,0.0005"}
  prautco      = ${prautco:-"0.00015,0.00015"}
  lgfdlmprad   = ${lgfdlmprad:-".false."}
  effr_in      = ${effr_in:-".false."}
  ldiag_ugwp   = ${ldiag_ugwp:-".false."}
  do_RRTMGP          = ${do_RRTMGP:-".false."}
  active_gases       = ${active_gases:-'h2o_co2_o3_n2o_ch4_o2'}
  ngases             = ${ngases:-"6"}
  lw_file_gas        = ${lw_file_gas:-'rrtmgp-data-lw-g128-210809.nc'}
  lw_file_clouds     = ${lw_file_clouds:-'rrtmgp-cloud-optics-coeffs-lw.nc'}
  sw_file_gas        = ${sw_file_gas:-'rrtmgp-data-sw-g112-210809.nc'}
  sw_file_clouds     = ${sw_file_clouds:-'rrtmgp-cloud-optics-coeffs-sw.nc'}
  rrtmgp_nGptsSW     = ${rrtmgp_nGptsSW:-"112"}
  rrtmgp_nGptsLW     = ${rrtmgp_nGptsLW:-"128"}
  rrtmgp_nBandsLW    = ${rrtmgp_nBandsLW:-"16"}
  rrtmgp_nBandsSW    = ${rrtmgp_nBandsSW:-"14"}
  doGP_cldoptics_LUT = ${doGP_cldoptics_LUT:-".false."}
  doGP_lwscat        = ${doGP_lwscat:-".false."}
EOF

if [[ ${cplchm} = ".true." ]]; then
  cat >> input.nml << EOF
  fscav_aero = ${fscav_aero:-'*:0.0'}
EOF
fi

cat >> input.nml <<EOF
  do_sppt      = ${do_sppt:-".false."}
  do_shum      = ${do_shum:-".false."}
  do_skeb      = ${do_skeb:-".false."}
  frac_grid    = ${FRAC_GRID:-".true."}
  cplchm       = ${cplchm:-".false."}
  cplflx       = ${cplflx:-".false."}
  cplice       = ${cplice:-".false."}
  cplwav       = ${cplwav:-".false."}
  cplwav2atm   = ${cplwav2atm:-".false."}
EOF

if [[ ${DO_SPPT} = "YES" ]]; then
cat >> input.nml <<EOF
  pert_mp = .false.
  pert_radtend = .false.
  pert_clds = .true.
EOF
fi

# Add namelist for IAU
if [[ ${DOIAU} = "YES" ]]; then
  cat >> input.nml << EOF
  iaufhrs      = ${IAUFHRS}
  iau_delthrs  = ${IAU_DELTHRS}
  iau_inc_files= ${IAU_INC_FILES}
  iau_drymassfixer = .false.
  iau_filter_increments = ${IAU_FILTER_INCREMENTS:-".false."}
EOF
fi

if [[ ${DO_CA:-"NO"} = "YES" ]]; then
  cat >> input.nml << EOF
  do_ca      = .true.
  ca_global  = ${ca_global:-".false."}
  ca_sgs     = ${ca_sgs:-".true."}
  nca        = ${nca:-"1"}
  ncells     = ${ncells:-"5"}
  nlives     = ${nlives:-"12"}
  nseed      = ${nseed:-"1"}
  nfracseed  = ${nfracseed:-"0.5"}
  nthresh    = ${nthresh:-"18"}
  ca_trigger = ${ca_trigger:-".true."}
  nspinup    = ${nspinup:-"1"}
  iseed_ca   = ${ISEED_CA:-"12345"}
EOF
fi

if [[ "${DO_LAND_PERT:-NO}" == "YES" ]]; then
  cat >> input.nml << EOF
  lndp_type = ${lndp_type:-2}
  n_var_lndp = ${n_var_lndp:-0}
EOF
fi

# Close &gfs_physics_nml section
cat >> input.nml << EOF
/
EOF

if [[ ${knob_ugwp_version} -eq 0 ]]; then
  cat >> input.nml << EOF
&cires_ugwp_nml
  knob_ugwp_solver  = ${knob_ugwp_solver:-2}
  knob_ugwp_source  = ${knob_ugwp_source:-1,1,0,0}
  knob_ugwp_wvspec  = ${knob_ugwp_wvspec:-1,25,25,25}
  knob_ugwp_azdir   = ${knob_ugwp_azdir:-2,4,4,4}
  knob_ugwp_stoch   = ${knob_ugwp_stoch:-0,0,0,0}
  knob_ugwp_effac   = ${knob_ugwp_effac:-1,1,1,1}
  knob_ugwp_doaxyz  = ${knob_ugwp_doaxyz:-1}
  knob_ugwp_doheat  = ${knob_ugwp_doheat:-1}
  knob_ugwp_dokdis  = ${knob_ugwp_dokdis:-1}
  knob_ugwp_ndx4lh  = ${knob_ugwp_ndx4lh:-1}
  knob_ugwp_version = ${knob_ugwp_version:-0}
  launch_level      = ${launch_level:-54}
/
EOF
fi

if [[ ${knob_ugwp_version} -eq 1 ]]; then
  cat >> input.nml << EOF
&cires_ugwp_nml
  knob_ugwp_solver  = ${knob_ugwp_solver:-2}
  knob_ugwp_source  = ${knob_ugwp_source:-1,1,0,0}
  knob_ugwp_wvspec  = ${knob_ugwp_wvspec:-1,25,25,25}
  knob_ugwp_azdir   = ${knob_ugwp_azdir:-2,4,4,4}
  knob_ugwp_stoch   = ${knob_ugwp_stoch:-0,0,0,0}
  knob_ugwp_effac   = ${knob_ugwp_effac:-1,1,1,1}
  knob_ugwp_doaxyz  = ${knob_ugwp_doaxyz:-1}
  knob_ugwp_doheat  = ${knob_ugwp_doheat:-1}
  knob_ugwp_dokdis  = ${knob_ugwp_dokdis:-2}
  knob_ugwp_ndx4lh  = ${knob_ugwp_ndx4lh:-4}
  knob_ugwp_version = ${knob_ugwp_version:-1}
  knob_ugwp_palaunch = ${knob_ugwp_palaunch:-275.0e2}
  knob_ugwp_nslope   = ${knob_ugwp_nslope:-1}
  knob_ugwp_lzmax    = ${knob_ugwp_lzmax:-15.750e3}
  knob_ugwp_lzmin    = ${knob_ugwp_lzmin:-0.75e3}
  knob_ugwp_lzstar   = ${knob_ugwp_lzstar:-2.0e3}
  knob_ugwp_taumin   = ${knob_ugwp_taumin:-0.25e-3}
  knob_ugwp_tauamp   = ${knob_ugwp_tauamp:-3.0e-3}
  knob_ugwp_lhmet    = ${knob_ugwp_lhmet:-200.0e3}
  knob_ugwp_orosolv  = ${knob_ugwp_orosolv:-'pss-1986'}
/
EOF
fi

echo "" >> input.nml

cat >> input.nml <<EOF
&gfdl_cloud_microphysics_nml
  sedi_transport = .true.
  do_sedi_heat = .false.
  rad_snow = .true.
  rad_graupel = .true.
  rad_rain = .true.
  const_vi = .false.
  const_vs = .false.
  const_vg = .false.
  const_vr = .false.
  vi_max = 1.
  vs_max = 2.
  vg_max = 12.
  vr_max = 12.
  qi_lim = 1.
  prog_ccn = .false.
  do_qa = .true.
  fast_sat_adj = .true.
  tau_l2v = 225.
  tau_v2l = 150.
  tau_g2v = 900.
  rthresh = 10.e-6  ! This is a key parameter for cloud water
  dw_land  = 0.16
  dw_ocean = 0.10
  ql_gen = 1.0e-3
  ql_mlt = 1.0e-3
  qi0_crt = 8.0E-5
  qs0_crt = 1.0e-3
  tau_i2s = 1000.
  c_psaci = 0.05
  c_pgacs = 0.01
  rh_inc = 0.30
  rh_inr = 0.30
  rh_ins = 0.30
  ccn_l = 300.
  ccn_o = 100.
  c_paut = 0.5
  c_cracw = 0.8
  use_ppm = .false.
  use_ccn = .true.
  mono_prof = .true.
  z_slope_liq  = .true.
  z_slope_ice  = .true.
  de_ice = .false.
  fix_negative = .true.
  icloud_f = 1
  mp_time = 150.
  reiflag = ${reiflag:-"2"}

  ${gfdl_cloud_microphysics_nml:-}
/

&interpolator_nml
  interp_method = 'conserve_great_circle'
  ${interpolator_nml:-}
/

&namsfc
  FNGLAC   = '${FNGLAC}'
  FNMXIC   = '${FNMXIC}'
  FNTSFC   = '${FNTSFC}'
  FNSNOC   = '${FNSNOC}'
  FNZORC   = '${FNZORC}'
  FNALBC   = '${FNALBC}'
  FNALBC2  = '${FNALBC2}'
  FNAISC   = '${FNAISC}'
  FNTG3C   = '${FNTG3C}'
  FNVEGC   = '${FNVEGC}'
  FNVETC   = '${FNVETC}'
  FNSOTC   = '${FNSOTC}'
  FNSOCC   = '${FNSOCC}'
  FNSMCC   = '${FNSMCC}'
  FNMSKH   = '${FNMSKH}'
  FNTSFA   = '${FNTSFA}'
  FNACNA   = '${FNACNA:-}'
  FNSNOA   = '${FNSNOA:-}'
  FNVMNC   = '${FNVMNC:-}'
  FNVMXC   = '${FNVMXC:-}'
  FNSLPC   = '${FNSLPC:-}'
  FNABSC   = '${FNABSC:-}'
  LDEBUG = ${LDEBUG:-".false."}
  FSMCL(2) = ${FSMCL2:-99999}
  FSMCL(3) = ${FSMCL3:-99999}
  FSMCL(4) = ${FSMCL4:-99999}
  LANDICE  = ${landice:-".true."}
  FTSFS = ${FTSFS:-90}
  FAISL = ${FAISL:-99999}
  FAISS = ${FAISS:-99999}
  FSNOL = ${FSNOL:-99999}
  FSNOS = ${FSNOS:-99999}
  FSICL = ${FSICL:-99999}
  FSICS = ${FSICS:-99999}
  FTSFL = ${FTSFL:-99999}
  FVETL = ${FVETL:-99999}
  FSOTL = ${FSOTL:-99999}
  FvmnL = ${FvmnL:-99999}
  FvmxL = ${FvmxL:-99999}
  FSLPL = ${FSLPL:-99999}
  FABSL = ${FABSL:-99999}
  ${namsfc_nml:-}
/

&fv_grid_nml
  grid_file = 'INPUT/grid_spec.nc'
  ${fv_grid_nml:-}
/
EOF

# Add namelist for stochastic physics options
echo "" >> input.nml
#if [ $MEMBER -gt 0 ]; then
if [[ "${DO_SPPT}" = "YES" || "${DO_SHUM}" = "YES" || "${DO_SKEB}" = "YES" || "${DO_LAND_PERT}" = "YES" ]]; then

    cat >> input.nml << EOF
&nam_stochy
  stochini=${stochini:-".false."}
EOF

  if [[ ${DO_SKEB} = "YES" ]]; then
    cat >> input.nml << EOF
  skeb = ${SKEB}
  iseed_skeb = ${ISEED_SKEB:-${ISEED}}
  skeb_tau = ${SKEB_TAU:-"-999."}
  skeb_lscale = ${SKEB_LSCALE:-"-999."}
  skebnorm = ${SKEBNORM:-"1"}
  skeb_npass = ${SKEB_NPASS:-"30"}
  skeb_vdof = ${SKEB_VDOF:-"5"}
EOF
  fi

  if [[ ${DO_SHUM} = "YES" ]]; then
    cat >> input.nml << EOF
  shum = ${SHUM}
  iseed_shum = ${ISEED_SHUM:-${ISEED}}
  shum_tau = ${SHUM_TAU:-"-999."}
  shum_lscale = ${SHUM_LSCALE:-"-999."}
EOF
  fi

  if [[ ${DO_SPPT} = "YES" ]]; then
    cat >> input.nml << EOF
  sppt = ${SPPT}
  iseed_sppt = ${ISEED_SPPT:-${ISEED}}
  sppt_tau = ${SPPT_TAU:-"-999."}
  sppt_lscale = ${SPPT_LSCALE:-"-999."}
  sppt_logit = ${SPPT_LOGIT:-".true."}
  sppt_sfclimit = ${SPPT_SFCLIMIT:-".true."}
  use_zmtnblck = ${use_zmtnblck:-".true."}
  pbl_taper = ${pbl_taper:-"0,0,0,0.125,0.25,0.5,0.75"}
EOF
  fi

  if [[ "${DO_OCN_SPPT:-NO}" == "YES" ]]; then
    cat >> input.nml <<EOF
  OCNSPPT=${OCNSPPT}
  OCNSPPT_LSCALE=${OCNSPPT_LSCALE}
  OCNSPPT_TAU=${OCNSPPT_TAU}
  ISEED_OCNSPPT=${ISEED_OCNSPPT:-${ISEED}}
EOF
  fi

  if [[ "${DO_OCN_PERT_EPBL:-NO}" == "YES" ]]; then
    cat >> input.nml <<EOF
  EPBL=${EPBL}
  EPBL_LSCALE=${EPBL_LSCALE}
  EPBL_TAU=${EPBL_TAU}
  ISEED_EPBL=${ISEED_EPBL:-${ISEED}}
EOF
  fi

  cat >> input.nml << EOF
/
EOF

  if [[ ${DO_LAND_PERT} = "YES" ]]; then
    cat >> input.nml << EOF
&nam_sfcperts
  lndp_type = ${lndp_type}
  LNDP_TAU = ${LNDP_TAU}
  LNDP_SCALE = ${LNDP_SCALE}
  ISEED_LNDP = ${ISEED_LNDP:-${ISEED}}
  lndp_var_list = ${lndp_var_list}
  lndp_prt_list = ${lndp_prt_list}
/
EOF
  else
    cat >> input.nml << EOF
&nam_sfcperts
/
EOF
  fi

else

  cat >> input.nml << EOF
&nam_stochy
/
&nam_sfcperts
/
EOF

fi

# Echo out formatted "input.nml"
echo "===================================="
echo "FV3_namelists(): 'input.nml'"
cat input.nml
echo "===================================="
}
