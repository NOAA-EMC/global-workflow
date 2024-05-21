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

# Generate input.nml from global template
local BLOCKSIZE=${blocksize}
local CHKSUM_DEBUG=${chksum_debug}
local DYCORE_ONLY=${dycore_only}
local CCPP_SUITE=${CCPP_SUITE}

local MAX_OUTPUT_FIELDS=300

local DOMAINS_STACK_SIZE=${domains_stack_size:-3000000}
local PRINT_MEMORY_USAGE=${print_memory_usage:-".false."}

local INPES=${layout_x}
local JNPES=${layout_y}
local IO_LAYOUT=${io_layout} #Change global
local NPX=${npx}
local NPY=${npy}
local NTILES=${ntiles} #Change global
local NPZ=${npz}
local DZ_MIN=${dz_min:-"6"}
local PSM_BC=${psm_bc:-"0"}
local MAKE_NH=${make_nh}
local FV_DEBUG=${fv_debug:-".false."} #Change global
local RANGE_WARN=${range_warn:-".true."} #Change global
local N_SPONGE=${n_sponge:-"10"} #Change global
local NUDGE_QV=${nudge_qv:-".false."} #Change global
local NUDGE_DZ=${nudge_dz:-".false."} #Change global
local TAU=${tau:-10.} #Change global
local RF_CUTOFF=${rf_cutoff:-"7.5e2"} #Change global
local D2_BG_K1=${d2_bg_k1:-"0.15"} 
local D2_BG_K2=${d2_bg_k2:-"0.02"}
local KORD_TM=${kord_tm:-"-9"} #Change global
local KORD_MT=${kord_mt:-"9"} #Change global
local KORD_WZ=${kord_wz:-"9"} #Change global
local KORD_TR=${kord_tr:-"9"} #Change global
local HYDROSTATIC=${hydrostatic} #Change global
local PHYS_HYDROSTATIC=${phys_hydrostatic} #Change global
local USE_HYDRO_PRESSURE=${use_hydro_pressure} #Change global
local K_SPLIT=${k_split} #Change global
local N_SPLIT=${n_split} #Change global
local NWAT=${nwat:-2} #Change global
local NA_INIT=${na_init}
local DNATS=${dnats}
local FV_SG_ADJ=${fv_sg_adj:-"450"} #Change global
local NORD=${nord:-3} #Change global
local DDDMP=${dddmp:-0.1}
local D4_BG=${d4_bg:-0.15} #Change global
local VTDM4=${vtdm4} #Change global
local DELT_MAX=${delt_max:-"0.002"} #Change global
local DO_VORT_DAMP=${do_vort_damp} #Change global
local EXTERNAL_IC=${external_ic}
local EXTERNAL_ETA=${external_eta:-.true.} #Change global
local GFS_PHIL=${gfs_phil:-".false."} #Change global
local NGGPS_IC=${nggps_ic}
local MOUNTAIN=${mountain}
local NCEP_IC=${ncep_ic} #Change global
local D_CON=${d_con} #Change global
local HORD_MT=${hord_mt} #Change global 
local HORD_VT=${hord_xx} #Change global
local HORD_TM=${hord_xx} #Change global
local HORD_DP=-${hord_xx} #Change global
local HORD_TR=${hord_tr:-"8"} #Change global
local ADJUST_DRY_MASS=${adjust_dry_mass:-".true."} #Change global
local DRY_MASS=${dry_mass:-98320.0} #Change global
local CONSV_TE=${consv_te} #Change global
local DO_SAT_ADJ=${do_sat_adj:-".false."}
local FAST_TAU_W_SEC=${fast_tau_w_sec:-"0.2"}
local PRINT_FREQ=${print_freq} #Change global
local WARM_START=${warm_start} 
local NO_DYCORE=${no_dycore} #Change global
local AGRID_VEL_RST=${agrid_vel_rst:-".true."} #Change global
local READ_INCREMENT=${read_increment}
local RES_LATLON_DYNAMICS=${res_latlon_dynamics}

local FILTERED_TERRAIN=${filtered_terrain} #Change global
local NPZP=${LEVS}
local GFS_DWINDS=${gfs_dwinds} #Change global

local FHZERO=${FHZER}
local H2O_PHYS=${h2o_phys:-".true."} #Change global
local LDIAG3D=${ldiag3d:-".false."}
local QDIAG3D=${qdiag3d:-".false."}
local PRINT_DIFF_PGR=${print_diff_pgr:-".false."}
local FHCYC=${FHCYC}
local USE_UFO=${use_ufo:-".true."} #Change global
local PRE_RAD=${pre_rad:-".false."} #Change global
local IMP_PHYSICS=${imp_physics:-"99"}

local HIDE_NEST='!'
local LCNORM=' '
local DOGP_SGS_CNV=' '
local IAU_INC_FILES=' '
local USE_MED_FLUX=' '
local CPLLND=.false.
local CPLLND2ATM=' '
local LNDP_TYPE=' '
local N_VAR_LNDP=' '
local PERT_MP=' '
local PERT_RADTEND=' '
local PERT_CLDS=' '
local IAU_FILTER_INCREMENTS=' '

case "${CCPP_SUITE:-}" in
  "FV3_GFS_v15p2_coupled")
  local OZ_PHYS=.false. #Change global
  local OZ_PHYS_2015=.true. #Change global
  ;;
  "FV3_GSD_v0")
  local IOVR=${iovr:-"3"}
  local LTAEROSOL=${ltaerosol:-".false."} #Change global
  local LRADAR=${lradar:-".false."} #Change global
  local TTENDLIM=${ttendlim:-0.005} #Change global
  local OZ_PHYS=${oz_phys:-".false."} #Change global
  local OZ_PHYS_2015=${oz_phys_2015:-".true."} #Change global
  local LSOIL_LSM=${lsoil_lsm:-"4"} #Change global
  local DO_MYNNEDMF=${do_mynnedmf:-".false."}
  local DO_MYNNSFCLAY=${do_mynnsfclay:-".false."} #Change global
  local ICLOUD_BL=${icloud_bl:-"1"} #Change global
  local BL_MYNN_EDMF=${bl_mynn_edmf:-"1"} #Change global
  local BL_MYNN_TKEADVECT=${bl_mynn_tkeadvect:-".true."} #Change global
  local BL_MYNN_EDMF_MOM=${bl_mynn_edmf_mom:-"1"} #Change global
  local MIN_LAKEICE=${min_lakeice:-"0.15"}
  local MIN_SEAICE=${min_seaice:-"0.15"}
  local USE_CICE_ALB=${use_cice_alb:-".false."}
  ;;
  FV3_GFS_v16_coupled*)
  local IOVR=${iovr:-"3"}
  local LTAEROSOL=${ltaerosol:-".false."} #Change global
  local LRADAR=${lradar:-".false."} #Change global
  local TTENDLIM=${ttendlim:-"0.005"} #Change global
  local OZ_PHYS=${oz_phys:-".false."} #Change global
  local OZ_PHYS_2015=${oz_phys_2015:-".true."} #Change global
  local DO_MYNNEDMF=${do_mynnedmf:-".false."}
  local DO_MYNNSFCLAY=${do_mynnsfclay:-".false."} #Change global
  local ICLOUD_BL=${icloud_bl:-"1"} #Change global
  local BL_MYNN_EDMF=${bl_mynn_edmf:-"1"} #Change global
  local BL_MYNN_TKEADVECT=${bl_mynn_tkeadvect:-".true."} #Change global
  local BL_MYNN_EDMF_MOM=${bl_mynn_edmf_mom:-"1"} #Change global
  local MIN_LAKEICE=${min_lakeice:-"0.15"}
  local MIN_SEAICE=${min_seaice:-"0.15"}
  ;;
  FV3_GFS_v16*)
  local IOVR=${iovr:-"3"}
  local LTAEROSOL=${ltaerosol:-".false."} #Change global
  local LRADAR=${lradar:-".false."} #Change global
  local TTENDLIM=${ttendlim:-"0.005"} #Change global
  local OZ_PHYS=${oz_phys:-".false."} #Change global
  local OZ_PHYS_2015=${oz_phys_2015:-".true."} #Change global
  local LSOIL_LSM=${lsoil_lsm:-"4"} #Change global
  local DO_MYNNEDMF=${do_mynnedmf:-".false."}
  local DO_MYNNSFCLAY=${do_mynnsfclay:-".false."}  #Change global
  local ICLOUD_BL=${icloud_bl:-"1"}  #Change global
  local BL_MYNN_EDMF=${bl_mynn_edmf:-"1"}  #Change global
  local BL_MYNN_TKEADVECT=${bl_mynn_tkeadvect:-".true."}  #Change global
  local BL_MYNN_EDMF_MOM=${bl_mynn_edmf_mom:-"1"}  #Change global
  local MIN_LAKEICE=${min_lakeice:-"0.15"}
  local MIN_SEAICE=${min_seaice:-"0.15"}
  ;;
  FV3_GFS_v17*)
  local default_dt_inner=$(( DELTIM/2 ))
  local IOVR=${iovr:-"3"}
  local LTAEROSOL=${ltaerosol:-".false."}  #Change global
  local LRADAR=${lradar:-".true."}  #Change global
  local TTENDLIM=${ttendlim:-"-999"}  #Change global
  local DT_INNER=${dt_inner:-"${default_dt_inner}"}
  local SEDI_SEMI=${sedi_semi:-".true."}
  local DECFL=${decfl:-"10"}
  local OZ_PHYS=${oz_phys:-".false."}   #Change global
  local OZ_PHYS_2015=${oz_phys_2015:-".true."}  #Change global
  local LSOIL_LSM=${lsoil_lsm:-"4"}  #Change global
  local DO_MYNNEDMF=${do_mynnedmf:-".false."}
  local DO_MYNNSFCLAY=${do_mynnsfclay:-".false."}   #Change global
  local ICLOUD_BL=${icloud_bl:-"1"}   #Change global
  local BL_MYNN_EDMF=${bl_mynn_edmf:-"1"}   #Change global
  local BL_MYNN_TKEADVECT=${bl_mynn_tkeadvect:-".true."}  #Change global
  local BL_MYNN_EDMF_MOM=${bl_mynn_edmf_mom:-"1"}  #Change global
  local DO_UGWP=${do_ugwp:-".false."}  #Change global
  local DO_TOFD=${do_tofd:-".false."}  #Change global
  local GWD_OPT=${gwd_opt:-"2"}
  local DO_UGWP_V0=${do_ugwp_v0:-".false."}
  local DO_UGWP_V1=${do_ugwp_v1:-".true."}
  local DO_UGWP_V0_OROG_ONLY=${do_ugwp_v0_orog_only:-".false."}   #Change global
  local DO_UGWP_V0_NST_ONLY=${do_ugwp_v0_nst_only:-".false."}
  local DO_GSL_DRAG_LS_BL=${do_gsl_drag_ls_bl:-".true."}
  local DO_GSL_DRAG_SS=${do_gsl_drag_ss:-".true."}
  local DO_GSL_DRAG_TOFD=${do_gsl_drag_tofd:-".true."}
  local DO_UGWP_V1_OROG_ONLY=${do_ugwp_v1_orog_only:-".false."}
  local MIN_LAKEICE=${min_lakeice:-"0.15"}
  local MIN_SEAICE=${min_seaice:-"0.15"}
  local USE_CICE_ALB=${use_cice_alb:-".false."}
  ;;
  FV3_global_nest*)
  local default_dt_inner=$(( DELTIM/2 ))
  local HIDE_NEST=' '
  local IOVR=${iovr:-"3"}
  local LCNORM=${lcnorm:-".false."} #### Not in nml
  local LTAEROSOL=${ltaerosol:-".false."}  #Change global
  local LRADAR=${lradar:-".true."}  #Change global
  local TTENDLIM=${ttendlim:-"-999"}  #Change global
  local DT_INNER=${dt_inner:-"${default_dt_inner}"}
  local SEDI_SEMI=${sedi_semi:-".true."}
  local DECFL=${decfl:-"10"}
  local OZ_PHYS=${oz_phys:-".false."}  #Change global
  local OZ_PHYS_2015=${oz_phys_2015:-".true."}  #Change global
  local LSOIL_LSM=${lsoil_lsm:-"4"}  #Change global
  local DO_MYNNEDMF=${do_mynnedmf:-".false."}
  local DO_MYNNSFCLAY=${do_mynnsfclay:-".false."}  #Change global
  local ICLOUD_BL=${icloud_bl:-"1"}  #Change global
  local BL_MYNN_EDMF=${bl_mynn_edmf:-"1"}  #Change global
  local BL_MYNN_TKEADVECT=${bl_mynn_tkeadvect:-".true."}  #Change global
  local BL_MYNN_EDMF_MOM=${bl_mynn_edmf_mom:-"1"}  #Change global
  local DO_UGWP=${do_ugwp:-".false."}  #Change global
  local DO_TOFD=${do_tofd:-".false."}  #Change global
  local GWD_OPT=${gwd_opt:-"2"}
  local DO_UGWP_V0=${do_ugwp_v0:-".false."}
  local DO_UGWP_V1=${do_ugwp_v1:-".true."}
  local DO_UGWP_V0_OROG_ONLY=${do_ugwp_v0_orog_only:-".false."}  #Change global
  local DO_UGWP_V0_NST_ONLY=${do_ugwp_v0_nst_only:-".false."}
  local DO_GSL_DRAG_LS_BL=${do_gsl_drag_ls_bl:-".true."}
  local DO_GSL_DRAG_SS=${do_gsl_drag_ss:-".true."}
  local DO_GSL_DRAG_TOFD=${do_gsl_drag_tofd:-".true."}
  local DO_UGWP_V1_OROG_ONLY=${do_ugwp_v1_orog_only:-".false."}
  local MIN_LAKEICE=${min_lakeice:-"0.15"}
  local MIN_SEAICE=${min_seaice:-"0.15"}
  local USE_CICE_ALB=${use_cice_alb:-".false."}
  ;;
  *)
  local IOVR=${iovr:-"3"}
  ;;
esac
local PDFCLD=${pdfcld:-".false."}  #Change global
local FHSWR=${FHSWR:-"3600."}  #Change global
local FHLWR=${FHLWR:-"3600."}  #Change global
local IALB=${IALB:-"1"}
local IEMS=${IEMS:-"1"}
local IAER=${IAER}
local ICLIQ_SW=${icliq_sw:-"2"}
local ICO2=${ICO2}   #Change global
local ISUBC_SW=${isubc_sw:-"2"}  #Change global
local ISUBC_LW=${isubc_lw:-"2"}  #Change global
local ISOL=${ISOL:-"2"}  #Change global
local LWHTR=${lwhtr:-".true."}  #Change global
local SWHTR=${swhtr:-".true."}  #Change global
local CNVGWD=${cnvgwd:-".true."}  #Change global
local SHAL_CNV=${shal_cnv:-".true."}
local CAL_PRE=${cal_pre:-".true."}  #Change global
local REDRAG=${redrag:-".true."}  #Change global
local DSPHEAT=${dspheat:-".true."}  #Change global
local HYBEDMF=${hybedmf:-".false."}  #Change global
local SATMEDMF=${satmedmf-".true."}
local ISATMEDMF=${isatmedmf-"1"}  #Change global
local LHEATSTRG=${lheatstrg-".false."}
local LSEASPRAY=${lseaspray:-".true."}
local RANDOM_CLDS=${random_clds:-".true."}
local TRANS_TRAC=${trans_trac:-".true."}  #Change global
local CNVCLD=${cnvcld:-".true."}
local IMFSHALCNV=${imfshalcnv:-"2"}
local IMFDEEPCNV=${imfdeepcnv:-"2"}
local PROGSIGMA=${progsigma:-".true."}
local BETASCU=${betascu:-"8.0"}
local BETAMCU=${betamcu:-"1.0"}
local BETADCU=${betadcu:-"2.0"}
local RAS=${ras:-".false."}
local CDMBWD=${cdmbgwd:-"3.5,0.25"}
local PRSLRD0=${prslrd0:-"0."}  #Change global
local IVEGSRC=${ivegsrc:-"1"}  #Change global
local ISOT=${isot:-"1"}  #Change global
local LSOIL=${lsoil:-"4"}  #Change global
local LSM=${lsm:-"2"}
local IOPT_DVEG=${iopt_dveg:-"1"}
local IOPT_CRS=${iopt_crs:-"1"}
local IOPT_BTR=${iopt_btr:-"1"}  #Change global
local IOPT_RUN=${iopt_run:-"1"}  #Change global
local IOPT_SFC=${iopt_sfc:-"1"}
local IOPT_TRS=${iopt_trs:-"2"}
local IOPT_DIAG=${iopt_diag:-"1"}  
local IOPT_FRZ=${iopt_frz:-"1"}  #Change global
local IOPT_INF=${iopt_inf:-"1"}  #Change global
local IOPT_RAD=${iopt_rad:-"1"}
local IOPT_ALB=${iopt_alb:-"2"}
local IOPT_SNF=${iopt_snf:-"4"}  #Change global
local IOPT_TBOT=${iopt_tbot:-"2"}  #Change global
local IOPT_STC=${iopt_stc:-"1"}
local DEBUG=${gfs_phys_debug:-".false."}  #Change global
local NSTF_NAME=${nstf_name}
local NST_ANL=${nst_anl}  #Change global
local PSAUTCO=${psautco:-"0.0008,0.0005"}  #Change global
local PRAUTCO=${prautco:-"0.00015,0.00015"}  #Change global
local LGFDLMPRAD=${lgfdlmprad:-".false."}
local EFFR_IN=${effr_in:-".false."}  #Change global
local LDIAG_UGWP=${ldiag_ugwp:-".false."}
local DO_RRTMGP=${do_RRTMGP:-".false."}
local ACTIVE_GASES=${active_gases:-'h2o_co2_o3_n2o_ch4_o2'}  #Change global
local NGASES=${ngases:-"6"}  #Change global
local LW_FILE_GAS=${lw_file_gas:-'rrtmgp-data-lw-g128-210809.nc'}  #Change global
local LW_FILE_CLOUDS=${lw_file_clouds:-'rrtmgp-cloud-optics-coeffs-lw.nc'}  #Change global
local SW_FILE_GAS=${sw_file_gas:-'rrtmgp-data-sw-g112-210809.nc'}  #Change global
local SW_FILE_CLOUDS=${sw_file_clouds:-'rrtmgp-cloud-optics-coeffs-sw.nc'}  #Change global
local RRTMGP_NGPTSSW=${rrtmgp_nGptsSW:-"112"}  #Change global
local RRTMGP_NGPTSLW=${rrtmgp_nGptsLW:-"128"}  #Change global
local RRTMGP_NBANDSLW=${rrtmgp_nBandsLW:-"16"}  #Change global
local RRTMGP_NBANDSSW=${rrtmgp_nBandsSW:-"14"}  #Change global
local DOGP_CLDOPTICS_LUT=${doGP_cldoptics_LUT:-".false."}
local DOGP_LWSCAT=${doGP_lwscat:-".false."}

if [[ ${cplchm} = ".true." ]]; then
  local FSCAV_AERO=${fscav_aero:-'*:0.0'}  #Find default if not set
else
  local FSCAV_AERO='"*:0.3","so2:0.0","msa:0.0","dms:0.0","nh3:0.4","nh4:0.6","bc1:0.6","bc2:0.6","oc1:0.4","oc2:0.4","dust1:0.6","dust2:0.6","dust3:0.6","dust4:0.6","dust5:0.6","seas1:0.5","seas2:0.5","seas3:0.5","seas4:0.5","seas5:0.5"'
fi

local DO_SPPT=${do_sppt:-".false."}
local DO_SHUM=${do_shum:-".false."}
local DO_SKEB=${do_skeb:-".false."}
local FRAC_GRID=${FRAC_GRID:-".true."} #Setting self-variable
local CPLCHM=${cplchm:-".false."}
local CPLFLX=${cplflx:-".false."}
local CPLICE=${cplice:-".false."}
local CPLWAV=${cplwav:-".false."}
local CPLWAV2ATM=${cplwav2atm:-".false."}

if [[ ${DO_SPPT} = "YES" ]]; then
  local HIDE_SPPT=' '
  local PERT_MP=.false.  #Doesn't exist
  local PERT_RADTEND=.false. #Doesn't exist
  local PERT_CLDS=.true. #Doesn't exist
else
  local HIDE_SPPT='!'
fi

# Add namelist for IAU
if [[ ${DOIAU} = "YES" ]]; then
  local HIDE_IAU=' '
  local IAUFHRS=${IAUFHRS}
  local IAU_DELTHRS=${IAU_DELTHRS}
  local IAU_INC_FILES=${IAU_INC_FILES}
  local IAU_FILTER_INCREMENTS=${IAU_FILTER_INCREMENTS:-".false."}  #Doesn't exist
else
  local HIDE_IAU='!'
fi

if [[ ${DO_CA:-"NO"} = "YES" ]]; then
  local DO_CA=.true.
  local CA_GLOBAL=${ca_global:-".false."}
  local CA_SGS=${ca_sgs:-".true."}
  local NCA=${nca:-"1"}
  local NCELLS=${ncells:-"5"}
  local NLIVES=${nlives:-"12"}
  local NSEED=${nseed:-"1"}
  local NFRACSEED=${nfracseed:-"0.5"}
  local NTHRESH=${nthresh:-"18"}
  local CA_TRIGGER=${ca_trigger:-".true."}
  local NSPINUP=${nspinup:-"1"}
  local ISEED_CA=${ISEED_CA:-"12345"}
fi

if [[ "${DO_LAND_PERT:-NO}" == "YES" ]]; then
  local LNDP_TYPE=${lndp_type:-2}
  local N_VAR_LNDP=${n_var_lndp:-0}
fi

local KNOB_UGWP_SOLVER=${knob_ugwp_solver:-2}  #Change global
local KNOB_UGWP_SOURCE=${knob_ugwp_source:-1,1,0,0}  #Change global
local KNOB_UGWP_WVSPEC=${knob_ugwp_wvspec:-1,25,25,25}  #Change global
local KNOB_UGWP_AZDIR=${knob_ugwp_azdir:-2,4,4,4}  #Change global
local KNOB_UGWP_STOCH=${knob_ugwp_stoch:-0,0,0,0}  #Change global
local KNOB_UGWP_EFFAC=${knob_ugwp_effac:-1,1,1,1}  #Change global
local KNOB_UGWP_DOAXYZ=${knob_ugwp_doaxyz:-1}  #Change global
local KNOB_UGWP_DOHEAT=${knob_ugwp_doheat:-1}  #Change global
local KNOB_UGWP_DOKDIS=${knob_ugwp_dokdis:-2}
local KNOB_UGWP_NDX4LH=${knob_ugwp_ndx4lh:-4}
local LAUNCH_LEVEL=${launch_level:-54} #Check bools
local KNOB_UGWP_PALAUNCH=${knob_ugwp_palaunch:-275.0e2}  #Change global
local KNOB_UGWP_NSLOPE=${knob_ugwp_nslope:-1}
local KNOB_UGWP_LZMAX=${knob_ugwp_lzmax:-15.750e3}  #Change global
local KNOB_UGWP_LZMIN=${knob_ugwp_lzmin:-0.75e3}  #Change global
local KNOB_UGWP_LZSTAR=${knob_ugwp_lzstar:-2.0e3}  #Change global
local KNOB_UGWP_TAUMIN=${knob_ugwp_taumin:-0.25e-3}  #Change global
local KNOB_UGWP_TAUAMP=${knob_ugwp_tauamp:-3.0e-3}  #Change global
local KNOB_UGWP_LHMET=${knob_ugwp_lhmet:-200.0e3}  #Change global
local KNOB_UGWP_OROSOLV=${knob_ugwp_orosolv:-'pss-1986'}  #Change global

if [[ ${knob_ugwp_version} -eq 0 ]]; then
  local HIDE_UGWPV0=' '
  local HIDE_UGWPV1='!'
  local KNOB_UGWP_VERSION=${knob_ugwp_version:-0} 
fi

if [[ ${knob_ugwp_version} -eq 1 ]]; then
  local HIDE_UGWPV0='!'
  local HIDE_UGWPV1=' '
  local KNOB_UGWP_VERSION=${knob_ugwp_version:-1}
fi

local REIFLAG=${reiflag:-"2"}  #Change global

local FNACNA='${FNACNA:-}'
local FNSNOA='${FNSNOA:-}'
local FNVMNC='${FNVMNC:-}'
local FNVMXC='${FNVMXC:-}'
local FNSLPC='${FNSLPC:-}'
local FNABSC='${FNABSC:-}'
local LDEBUG=${LDEBUG:-".false."}   #Change global
local FSMCL2=${FSMCL2:-99999}    #Change global
local FSMCL3=${FSMCL3:-99999}    #Change global
local FSMCL4=${FSMCL4:-99999}    #Change global
local LANDICE=${landice:-".true."}
local FTSFS=${FTSFS:-90}
local FAISL=${FAISL:-99999}    #Change global
local FAISS=${FAISS:-99999}    #Change global
local FSNOL=${FSNOL:-99999}    #Change global
local FSNOS=${FSNOS:-99999}    #Change global
local FSICL=${FSICL:-99999}
local FSICS=${FSICS:-99999}
local FTSFL=${FTSFL:-99999}    #Change global
local FVETL=${FVETL:-99999}    #Change global
local FSOTL=${FSOTL:-99999}    #Change global
local FVMNL=${FvmnL:-99999}    #Change global
local FVMXL=${FvmxL:-99999}    #Change global
local FSLPL=${FSLPL:-99999}    #Change global
local FABSL=${FABSL:-99999}    #Change global

#HIDE_SPPT already defined above
local HIDE_SKEB='!'
local HIDE_SHUM='!'
local HIDE_OCNSPPT='!'
local HIDE_EPBL='!'
local HIDE_LAND_PERT='!'
local SKEB=${SKEB:-".false."}
local ISEED_SKEB=${ISEED_SKEB:-${ISEED}}
local SKEB_TAU=${SKEB_TAU:-"-999."}
local SKEB_LSCALE=${SKEB_LSCALE:-"-999."}
local SKEBNORM=${SKEBNORM:-"1"}
local SKEB_NPASS=${SKEB_NPASS:-"30"}
local SKEB_VDOF=${SKEB_VDOF:-"5"}
local SHUM=${SHUM:-".false."}
local ISEED_SHUM=${ISEED_SHUM:-${ISEED}}
local SHUM_TAU=${SHUM_TAU:-"-999."}
local SHUM_LSCALE=${SHUM_LSCALE:-"-999."}
local SPPT=${SPPT:-".false."}
local ISEED_SPPT=${ISEED_SPPT:-${ISEED}}
local SPPT_TAU=${SPPT_TAU:-"-999."}
local SPPT_LSCALE=${SPPT_LSCALE:-"-999."}
local SPPT_LOGIT=${SPPT_LOGIT:-".true."}
local SPPT_SFCLIMIT=${SPPT_SFCLIMIT:-".true."}
local USE_ZMTNBLCK=${use_zmtnblck:-".true."}
local PBL_TAPER=${pbl_taper:-"0,0,0,0.125,0.25,0.5,0.75"}
local OCNSPPT=${OCNSPPT:-".false."}
local OCNSPPT_LSCALE=${OCNSPPT_LSCALE:-"-999."}
local OCNSPPT_TAU=${OCNSPPT_TAU:-"-999."}
local ISEED_OCNSPPT=${ISEED_OCNSPPT:-${ISEED}}
local EPBL=${EPBL:-".false."}
local EPBL_LSCALE=${EPBL_LSCALE:-"-999."}
local EPBL_TAU=${EPBL_TAU:-"-999."}
local ISEED_EPBL=${ISEED_EPBL:-${ISEED}}
local LNDP_TYPE=${lndp_type:-"-999."}
local LNDP_TAU=${LNDP_TAU:-"-999."}
local LNDP_LSCALE=${LNDP_SCALE:-"-999."}
local ISEED_LNDP=${ISEED_LNDP:-${ISEED}}
local LNDP_VAR_LIST=${lndp_var_list:-"-999."}
local LNDP_PRT_LIST=${lndp_prt_list:-"-999."}
# Add namelist for stochastic physics options
if [[ "${DO_SPPT}" = "YES" || "${DO_SHUM}" = "YES" || "${DO_SKEB}" = "YES" || "${DO_LAND_PERT}" = "YES" ]]; then
  if [[ ${DO_SKEB} = "YES" ]]; then
	local HIDE_SKEB=' '
  fi

  if [[ ${DO_SHUM} = "YES" ]]; then
	local HIDE_SHUM=' '
  fi

  if [[ ${DO_SPPT} = "YES" ]]; then
	local HIDE_SPPT=' '
  fi

  if [[ "${DO_OCN_SPPT:-NO}" == "YES" ]]; then
	local HIDE_OCNSPPT=' '
  fi
  if [[ "${DO_OCN_PERT_EPBL:-NO}" == "YES" ]]; then
    local HIDE_EPBL=' '
  fi
  if [[ ${DO_LAND_PERT} = "YES" ]]; then
	local HIDE_LAND_PERT=' '
  fi
fi

local LNDP_MODEL_TYPE=' '
local MOM6_RESTART_SETTING=' '
local MOM6_RESTART_DIR=' '
local template='/scratch1/NCEPDEV/stmp2/Daniel.Sarmiento/global-workflow/global_control.nml.IN'
atparse < "${template}" >> "input.nml"
}
