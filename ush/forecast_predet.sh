#! /usr/bin/env bash

to_seconds() {
  # Function to convert HHMMSS to seconds since 00Z
  local hhmmss hh mm ss seconds padded_seconds
  hhmmss=${1:?}
  hh=${hhmmss:0:2}
  mm=${hhmmss:2:2}
  ss=${hhmmss:4:2}
  seconds=$((10#${hh}*3600+10#${mm}*60+10#${ss}))
  padded_seconds=$(printf "%05d" "${seconds}")
  echo "${padded_seconds}"
}

middle_date(){
  # Function to calculate mid-point date in YYYYMMDDHH between two dates also in YYYYMMDDHH
  local date1 date2 date1s date2s dtsecsby2 mid_date
  date1=${1:?}
  date2=${2:?}
  date1s=$(date --utc -d "${date1:0:8} ${date1:8:2}:00:00" +%s)
  date2s=$(date --utc -d "${date2:0:8} ${date2:8:2}:00:00" +%s)
  dtsecsby2=$(( $((date2s - date1s)) / 2 ))
  mid_date=$(date --utc -d "${date1:0:8} ${date1:8:2} + ${dtsecsby2} seconds" +%Y%m%d%H%M%S)
  echo "${mid_date:0:10}"
}

nhour(){
  # Function to calculate hours between two dates (This replicates prod-util NHOUR)
  local date1 date2 seconds1 seconds2 hours
  date1=${1:?}
  date2=${2:?}
  # Convert dates to UNIX timestamps
  seconds1=$(date --utc -d "${date1:0:8} ${date1:8:2}:00:00" +%s)
  seconds2=$(date --utc -d "${date2:0:8} ${date2:8:2}:00:00" +%s)
  hours=$(( $((seconds1 - seconds2)) / 3600 ))  # Calculate the difference in seconds and convert to hours
  echo "${hours}"
}

FV3_coldstarts(){
  # Function to return an comma-separated string of cold-start input files for FV3
  # Create an array of chgres-ed FV3 files
  local fv3_input_files tile_files
  fv3_input_files=(gfs_ctrl.nc)
  tile_files=(gfs_data sfc_data)
  local nn tt
  for (( nn = 1; nn <= ntiles; nn++ )); do
    for tt in "${tile_files[@]}"; do
      fv3_input_files+=("${tt}.tile${nn}.nc")
    done
  done
  # Create a comma separated string from array using IFS
  IFS=, echo "${fv3_input_files[*]}"
}

FV3_restarts(){
  # Function to return an comma-separated string of warm-start input files for FV3
  # Create an array of FV3 restart files
  local fv3_restart_files tile_files
  fv3_restart_files=(coupler.res fv_core.res.nc)
  tile_files=(fv_core.res fv_srf_wnd.res fv_tracer.res phy_data sfc_data ca_data)
  local nn tt
  for (( nn = 1; nn <= ntiles; nn++ )); do
    for tt in "${tile_files[@]}"; do
      fv3_restart_files+=("${tt}.tile${nn}.nc")
    done
  done
  # Create a comma separated string from array using IFS
  IFS=, echo "${fv3_restart_files[*]}"
}

# shellcheck disable=SC2034
common_predet(){
  echo "SUB ${FUNCNAME[0]}: Defining variables for shared through model components"

  RUN=${RUN:-gdas}
  rCDUMP=${rCDUMP:-${RUN}}

  CDATE=${CDATE:-"${PDY}${cyc}"}
  ENSMEM=${ENSMEM:-000}
  MEMBER=$(( 10#${ENSMEM:-"-1"} )) # -1: control, 0: ensemble mean, >0: ensemble member $MEMBER

  # Define significant cycles
  half_window=$(( assim_freq / 2 ))
  current_cycle="${PDY}${cyc}"
  previous_cycle=$(date --utc -d "${current_cycle:0:8} ${current_cycle:8:2} - ${assim_freq} hours" +%Y%m%d%H)
  next_cycle=$(date --utc -d "${current_cycle:0:8} ${current_cycle:8:2} + ${assim_freq} hours" +%Y%m%d%H)
  current_cycle_begin=$(date --utc -d "${current_cycle:0:8} ${current_cycle:8:2} - ${half_window} hours" +%Y%m%d%H)
  current_cycle_end=$(date --utc -d "${current_cycle:0:8} ${current_cycle:8:2} + ${half_window} hours" +%Y%m%d%H)
  next_cycle_begin=$(date --utc -d "${next_cycle:0:8} ${next_cycle:8:2} - ${half_window} hours" +%Y%m%d%H)
  forecast_end_cycle=$(date --utc -d "${current_cycle:0:8} ${current_cycle:8:2} + ${FHMAX} hours" +%Y%m%d%H)

  # Define model start date for current_cycle and next_cycle as the time the forecast will start
  if [[ "${DOIAU:-NO}" == "YES" ]]; then
    model_start_date_current_cycle="${current_cycle_begin}"
    model_start_date_next_cycle="${next_cycle_begin}"
  else
    if [[ "${REPLAY_ICS:-NO}" == "YES" ]]; then
      model_start_date_current_cycle=${current_cycle_end}
    else
      model_start_date_current_cycle=${current_cycle}
    fi
    model_start_date_next_cycle=${next_cycle}
  fi

  FHMIN=${FHMIN:-0}
  FHMAX=${FHMAX:-9}
  FHOUT=${FHOUT:-3}
  FHMAX_HF=${FHMAX_HF:-0}
  FHOUT_HF=${FHOUT_HF:-1}

  if [[ ! -d "${COMOUT_CONF}" ]]; then mkdir -p "${COMOUT_CONF}"; fi

  cd "${DATA}" || ( echo "FATAL ERROR: Unable to 'cd ${DATA}', ABORT!"; exit 8 )

  # Several model components share DATA/INPUT for input data
  if [[ ! -d "${DATA}/INPUT" ]]; then mkdir -p "${DATA}/INPUT"; fi

}

# shellcheck disable=SC2034
FV3_predet(){
  echo "SUB ${FUNCNAME[0]}: Defining variables for FV3"

  if [[ ! -d "${COMOUT_ATMOS_HISTORY}" ]]; then mkdir -p "${COMOUT_ATMOS_HISTORY}"; fi
  if [[ ! -d "${COMOUT_ATMOS_MASTER}" ]]; then mkdir -p "${COMOUT_ATMOS_MASTER}"; fi
  if [[ ! -d "${COMOUT_ATMOS_RESTART}" ]]; then mkdir -p "${COMOUT_ATMOS_RESTART}"; fi
  if [[ ! -d "${DATArestart}/FV3_RESTART" ]]; then mkdir -p "${DATArestart}/FV3_RESTART"; fi
  ${NLN} "${DATArestart}/FV3_RESTART" "${DATA}/RESTART"

  FHZER=${FHZER:-6}
  FHCYC=${FHCYC:-24}
  restart_interval=${restart_interval:-${FHMAX}}
  # restart_interval = 0 implies write restart at the END of the forecast i.e. at FHMAX
  # Convert restart interval into an explicit list for FV3
  if (( restart_interval == 0 )); then
    restart_interval=${FHMAX}
    FV3_RESTART_FH=("${restart_interval}")
  else
    # shellcheck disable=SC2312
    mapfile -t FV3_RESTART_FH < <(seq "${restart_interval}" "${restart_interval}" "${FHMAX}")
    # If the last forecast hour is not in the array, add it
    local nrestarts=${#FV3_RESTART_FH[@]}
    if (( FV3_RESTART_FH[nrestarts-1] != FHMAX )); then
      FV3_RESTART_FH+=("${FHMAX}")
    fi
  fi

  # Convert output settings into an explicit list for FV3
  # Create an FV3 fhr list to be used in the filenames
  FV3_OUTPUT_FH=""
  local fhr=${FHMIN}
  if (( FHOUT_HF > 0 && FHMAX_HF > 0 )); then
    FV3_OUTPUT_FH="${FV3_OUTPUT_FH} $(seq -s ' ' "${FHMIN}" "${FHOUT_HF}" "${FHMAX_HF}")"
    fhr=${FHMAX_HF}
  fi
  FV3_OUTPUT_FH="${FV3_OUTPUT_FH} $(seq -s ' ' "${fhr}" "${FHOUT}" "${FHMAX}")"

  # Create an FV3 fhr list to be used in the namelist
  # The FV3 fhr list for the namelist and the FV3 fhr list for the filenames
  # are only different when REPLAY_ICS is set to YES
  if [[ "${REPLAY_ICS:-NO}" == "YES"  ]]; then
    local FV3_OUTPUT_FH_s
    FV3_OUTPUT_FH_NML="$(echo "scale=5; ${OFFSET_START_HOUR}+(${DELTIM}/3600)" | bc -l)"
    FV3_OUTPUT_FH_s=$(( OFFSET_START_HOUR * 3600 + DELTIM ))
    local fhr=${FHOUT}
    if (( FHOUT_HF > 0 && FHMAX_HF > 0 )); then
      FV3_OUTPUT_FH_NML="${FV3_OUTPUT_FH_NML} $(seq -s ' ' "$(( OFFSET_START_HOUR + FHOUT_HF ))" "${FHOUT_HF}" "${FHMAX_HF}")"
      FV3_OUTPUT_FH_s="${FV3_OUTPUT_FH_s} $(seq -s ' ' "$(( OFFSET_START_HOUR * 3600 + FHOUT_HF * 3600 ))" "$(( FHOUT_HF * 3600 ))" "$(( FHMAX_HF * 3600 ))")"
      fhr=${FHMAX_HF}
    fi
    FV3_OUTPUT_FH_NML="${FV3_OUTPUT_FH_NML} $(seq -s ' ' "${fhr}" "${FHOUT}" "${FHMAX}")"
    FV3_OUTPUT_FH_s="${FV3_OUTPUT_FH_s} $(seq -s ' ' "$(( fhr * 3600 ))" "$(( FHOUT * 3600 ))" "$(( FHMAX * 3600 ))")"
    local hh mm ss s_total
    FV3_OUTPUT_FH_hhmmss=""
    for s_total in ${FV3_OUTPUT_FH_s}; do
      # Convert seconds to HHH:MM:SS
      (( ss = s_total, mm = ss / 60, ss %= 60, hh = mm / 60, mm %= 60 )) || true
      FV3_OUTPUT_FH_hhmmss="${FV3_OUTPUT_FH_hhmmss} $(printf "%03d-%02d-%02d" "${hh}" "${mm}" "${ss}")"
    done
    # Create a string from an array
  else # If non-replay ICs are being used
    # The FV3 fhr list for the namelist and the FV3 fhr list for the filenames
    # are identical when REPLAY_ICS is set to NO
    FV3_OUTPUT_FH_NML="${FV3_OUTPUT_FH}"
  fi

  # Other options
  PREFIX_ATMINC=${PREFIX_ATMINC:-""} # allow ensemble to use recentered increment

  # IAU options
  IAUFHRS=${IAUFHRS:-0}
  IAU_DELTHRS=${IAU_DELTHRS:-0}

  #------------------------------------------------------------------
  # changeable parameters
  # dycore definitions
  res="${CASE:1}"
  resp=$((res+1))
  npx=${resp}
  npy=${resp}
  npz=$((LEVS-1))
  io_layout="1,1"
  #ncols=$(( (${npx}-1)*(${npy}-1)*3/2 ))

  # spectral truncation and regular grid resolution based on FV3 resolution
  JCAP_CASE=$((2*res-2))
  LONB_CASE=$((4*res))
  LATB_CASE=$((2*res))

  JCAP=${JCAP:-${JCAP_CASE}}
  LONB=${LONB:-${LONB_CASE}}
  LATB=${LATB:-${LATB_CASE}}

  LONB_IMO=${LONB_IMO:-${LONB_CASE}}
  LATB_JMO=${LATB_JMO:-${LATB_CASE}}

  # NSST Options
  # nstf_name contains the NSST related parameters
  # nstf_name(1) : NST_MODEL (NSST Model) : 0 = OFF, 1 = ON but uncoupled, 2 = ON and coupled
  # nstf_name(2) : NST_SPINUP : 0 = OFF, 1 = ON,
  # nstf_name(3) : NST_RESV (Reserved, NSST Analysis) : 0 = OFF, 1 = ON
  # nstf_name(4) : ZSEA1 (in mm) : 0
  # nstf_name(5) : ZSEA2 (in mm) : 0
  # nst_anl      : .true. or .false., NSST analysis over lake
  NST_MODEL=${NST_MODEL:-0}
  NST_SPINUP=${NST_SPINUP:-0}
  NST_RESV=${NST_RESV-0}
  ZSEA1=${ZSEA1:-0}
  ZSEA2=${ZSEA2:-0}
  nstf_name=${nstf_name:-"${NST_MODEL},${NST_SPINUP},${NST_RESV},${ZSEA1},${ZSEA2}"}
  nst_anl=${nst_anl:-".false."}

  # blocking factor used for threading and general physics performance
  #nyblocks=$(expr \( $npy - 1 \) \/ $layout_y )
  #nxblocks=$(expr \( $npx - 1 \) \/ $layout_x \/ 32)
  #if [ $nxblocks -le 0 ]; then nxblocks=1 ; fi
  blocksize=${blocksize:-32}

  # variables for controlling initialization of NCEP/NGGPS ICs
  filtered_terrain=${filtered_terrain:-".true."}
  gfs_dwinds=${gfs_dwinds:-".true."}

  # various debug options
  no_dycore=${no_dycore:-".false."}
  dycore_only=${adiabatic:-".false."}
  chksum_debug=${chksum_debug:-".false."}
  print_freq=${print_freq:-6}

  # the pre-conditioning of the solution
  # =0 implies no pre-conditioning
  # >0 means new adiabatic pre-conditioning
  # <0 means older adiabatic pre-conditioning
  na_init=${na_init:-1}

  local suite_file="${HOMEgfs}/sorc/ufs_model.fd/FV3/ccpp/suites/suite_${CCPP_SUITE}.xml"
  if [[ ! -f "${suite_file}" ]]; then
    echo "FATAL ERROR: CCPP Suite file ${suite_file} does not exist, ABORT!"
    exit 2
  fi

  # Scan suite file to determine whether it uses Noah-MP
  local num_noahmpdrv
  num_noahmpdrv=$(grep -c noahmpdrv "${suite_file}")
  if (( num_noahmpdrv > 0 )); then
    lsm="2"
    lheatstrg=".false."
    landice=".false."
    iopt_dveg=${iopt_dveg:-"4"}
    iopt_crs=${iopt_crs:-"2"}
    iopt_btr=${iopt_btr:-"1"}
    iopt_run=${iopt_run:-"1"}
    iopt_sfc=${iopt_sfc:-"1"}
    iopt_frz=${iopt_frz:-"1"}
    iopt_inf=${iopt_inf:-"1"}
    iopt_rad=${iopt_rad:-"3"}
    iopt_alb=${iopt_alb:-"1"}
    iopt_snf=${iopt_snf:-"4"}
    iopt_tbot=${iopt_tbot:-"2"}
    iopt_stc=${iopt_stc:-"3"}
    IALB=${IALB:-2}
    IEMS=${IEMS:-2}
  else
    lsm="1"
    lheatstrg=".true."
    landice=".true."
    iopt_dveg=${iopt_dveg:-"1"}
    iopt_crs=${iopt_crs:-"1"}
    iopt_btr=${iopt_btr:-"1"}
    iopt_run=${iopt_run:-"1"}
    iopt_sfc=${iopt_sfc:-"1"}
    iopt_frz=${iopt_frz:-"1"}
    iopt_inf=${iopt_inf:-"1"}
    iopt_rad=${iopt_rad:-"1"}
    iopt_alb=${iopt_alb:-"2"}
    iopt_snf=${iopt_snf:-"4"}
    iopt_tbot=${iopt_tbot:-"2"}
    iopt_stc=${iopt_stc:-"1"}
    IALB=${IALB:-1}
    IEMS=${IEMS:-1}
  fi

  if [[ "${TYPE}" == "nh" ]]; then  # non-hydrostatic options
    hydrostatic=".false."
    phys_hydrostatic=".false."     # enable heating in hydrostatic balance in non-hydrostatic simulation
    use_hydro_pressure=".false."   # use hydrostatic pressure for physics
    make_nh=".true."               # running in non-hydrostatic mode
    pass_full_omega_to_physics_in_non_hydrostatic_mode=".true."
  else  # hydrostatic options
    hydrostatic=".true."
    phys_hydrostatic=".false."     # ignored when hydrostatic = T
    use_hydro_pressure=".false."   # ignored when hydrostatic = T
    make_nh=".false."              # running in hydrostatic mode
  fi

  # Conserve total energy as heat globally
  consv_te=${consv_te:-1.} # range 0.-1., 1. will restore energy to orig. val. before physics
  if [[ "${DO_NEST:-NO}" == "YES" ]] ; then
    consv_te=0
    k_split=${k_split:-1}
    k_split_nest=${k_split_nest:-4}
  else
    consv_te=${consv_te:-1.} # range 0.-1., 1. will restore energy to orig. val. before physics
    k_split=${k_split:-2}
  fi

  # time step parameters in FV3
  n_split=${n_split:-5}

  if [[ "${MONO:0:4}" == "mono" ]]; then  # monotonic options
    d_con=${d_con_mono:-"0."}
    do_vort_damp=".false."
    if [[ "${TYPE}" == "nh" ]]; then  # monotonic and non-hydrostatic
      hord_mt=${hord_mt_nh_mono:-"10"}
      hord_xx=${hord_xx_nh_mono:-"10"}
      hord_dp=-${hord_xx_nh_nonmono:-"-10"}
    else  # monotonic and hydrostatic
      hord_mt=${hord_mt_hydro_mono:-"10"}
      hord_xx=${hord_xx_hydro_mono:-"10"}
      hord_dp=-${hord_xx_nh_nonmono:-"-10"}
    fi
  else  # non-monotonic options
    d_con=${d_con_nonmono:-"1."}
    do_vort_damp=".true."
    if [[ "${TYPE}" == "nh" ]]; then  # non-monotonic and non-hydrostatic
      hord_mt=${hord_mt_nh_nonmono:-"5"}
      hord_xx=${hord_xx_nh_nonmono:-"5"}
      hord_dp=${hord_xx_hydro_mono:-"-5"}
    else # non-monotonic and hydrostatic
      hord_mt=${hord_mt_hydro_nonmono:-"10"}
      hord_xx=${hord_xx_hydro_nonmono:-"10"}
      hord_dp=${hord_xx_hydro_mono:-"10"}
      kord_tm=${kord_tm_hydro_mono:-"-12"}
      kord_mt=${kord_mt_hydro_mono:-"12"}
      kord_wz=${kord_wz_hydro_mono:-"12"}
      kord_tr=${kord_tr_hydro_mono:-"12"}
    fi
  fi

  if [[ "${MONO:0:4}" != "mono" && "${TYPE}" == "nh" ]]; then
    vtdm4=${vtdm4_nh_nonmono:-"0.06"}
  else
    vtdm4=${vtdm4:-"0.05"}
  fi

  # Initial conditions are chgres-ed from GFS analysis file
  nggps_ic=${nggps_ic:-".true."}
  ncep_ic=${ncep_ic:-".false."}
  external_ic=".true."
  mountain=".false."
  warm_start=".false."
  read_increment=".false."
  res_latlon_dynamics='""'

  # Stochastic Physics Options
  do_skeb=".false."
  do_shum=".false."
  do_sppt=".false."
  do_ca=".false."
  ISEED=0
  if (( MEMBER > 0 )); then  # these are only applicable for ensemble members
    local imem=${MEMBER#0}
    local base_seed=$((current_cycle*10000 + imem*100))

    if [[ "${DO_SKEB:-}" == "YES" ]]; then
      do_skeb=".true."
      ISEED_SKEB=$((base_seed + 1))
    fi

    if [[ "${DO_SHUM:-}" == "YES" ]]; then
      do_shum=".true."
      ISEED_SHUM=$((base_seed + 2))
    fi

    if [[ "${DO_SPPT:-}" == "YES" ]]; then
      do_sppt=".true."
      ISEED_SPPT=$((base_seed + 3)),$((base_seed + 4)),$((base_seed + 5)),$((base_seed + 6)),$((base_seed + 7))
    fi

    if [[ "${DO_CA:-}" == "YES" ]]; then
      do_ca=".true."
      ISEED_CA=$(( (base_seed + 18) % 2147483647 ))
    fi

    if [[ "${DO_LAND_PERT:-}" == "YES" ]]; then
      lndp_type=${lndp_type:-2}
      ISEED_LNDP=$(( (base_seed + 5) % 2147483647 ))
      LNDP_TAU=${LNDP_TAU:-21600}
      LNDP_SCALE=${LNDP_SCALE:-500000}
      lndp_var_list=${lndp_var_list:-"'smc', 'vgf',"}
      lndp_prt_list=${lndp_prt_list:-"0.2,0.1"}
      n_var_lndp=$(echo "${lndp_var_list}" | wc -w)
    fi

  fi  # end of ensemble member specific options

  #--------------------------------------------------------------------------

  # Fix files
  FNGLAC=${FNGLAC:-"${FIXgfs}/am/global_glacier.2x2.grb"}
  FNMXIC=${FNMXIC:-"${FIXgfs}/am/global_maxice.2x2.grb"}
  FNTSFC=${FNTSFC:-"${FIXgfs}/am/RTGSST.1982.2012.monthly.clim.grb"}
  FNSNOC=${FNSNOC:-"${FIXgfs}/am/global_snoclim.1.875.grb"}
  FNZORC=${FNZORC:-"igbp"}
  FNAISC=${FNAISC:-"${FIXgfs}/am/IMS-NIC.blended.ice.monthly.clim.grb"}
  FNALBC2=${FNALBC2:-"${FIXorog}/${CASE}/sfc/${CASE}.mx${OCNRES}.facsf.tileX.nc"}
  FNTG3C=${FNTG3C:-"${FIXorog}/${CASE}/sfc/${CASE}.mx${OCNRES}.substrate_temperature.tileX.nc"}
  FNVEGC=${FNVEGC:-"${FIXorog}/${CASE}/sfc/${CASE}.mx${OCNRES}.vegetation_greenness.tileX.nc"}
  FNMSKH=${FNMSKH:-"${FIXgfs}/am/global_slmask.t1534.3072.1536.grb"}
  FNVMNC=${FNVMNC:-"${FIXorog}/${CASE}/sfc/${CASE}.mx${OCNRES}.vegetation_greenness.tileX.nc"}
  FNVMXC=${FNVMXC:-"${FIXorog}/${CASE}/sfc/${CASE}.mx${OCNRES}.vegetation_greenness.tileX.nc"}
  FNSLPC=${FNSLPC:-"${FIXorog}/${CASE}/sfc/${CASE}.mx${OCNRES}.slope_type.tileX.nc"}
  FNALBC=${FNALBC:-"${FIXorog}/${CASE}/sfc/${CASE}.mx${OCNRES}.snowfree_albedo.tileX.nc"}
  FNVETC=${FNVETC:-"${FIXorog}/${CASE}/sfc/${CASE}.mx${OCNRES}.vegetation_type.tileX.nc"}
  FNSOTC=${FNSOTC:-"${FIXorog}/${CASE}/sfc/${CASE}.mx${OCNRES}.soil_type.tileX.nc"}
  FNSOCC=${FNSOCC:-"${FIXorog}/${CASE}/sfc/${CASE}.mx${OCNRES}.soil_color.tileX.nc"}
  FNABSC=${FNABSC:-"${FIXorog}/${CASE}/sfc/${CASE}.mx${OCNRES}.maximum_snow_albedo.tileX.nc"}
  FNSMCC=${FNSMCC:-"${FIXgfs}/am/global_soilmgldas.statsgo.t${JCAP}.${LONB}.${LATB}.grb"}

  # If the appropriate resolution fix file is not present, use the highest resolution available (T1534)
  [[ ! -f "${FNSMCC}" ]] && FNSMCC="${FIXgfs}/am/global_soilmgldas.statsgo.t1534.3072.1536.grb"

  # Grid and orography data
  if [[ "${cplflx}" == ".false." ]] ; then
    ${NCP} "${FIXorog}/${CASE}/${CASE}_mosaic.nc" "${DATA}/INPUT/grid_spec.nc"
  else
    ${NCP} "${FIXorog}/${CASE}/${CASE}_mosaic.nc" "${DATA}/INPUT/${CASE}_mosaic.nc"
  fi

  # Files for GWD
  ${NCP} "${FIXugwd}/ugwp_limb_tau.nc" "${DATA}/ugwp_limb_tau.nc"

  # Files for orography, GWD tiles
  local tt
  for (( tt = 1; tt <= ntiles; tt++ )); do
    ${NCP} "${FIXorog}/${CASE}/${CASE}.mx${OCNRES}_oro_data.tile${tt}.nc" "${DATA}/INPUT/oro_data.tile${tt}.nc"
    ${NCP} "${FIXorog}/${CASE}/${CASE}_grid.tile${tt}.nc"                 "${DATA}/INPUT/${CASE}_grid.tile${tt}.nc"
    ${NCP} "${FIXugwd}/${CASE}/${CASE}_oro_data_ls.tile${tt}.nc"          "${DATA}/INPUT/oro_data_ls.tile${tt}.nc"
    ${NCP} "${FIXugwd}/${CASE}/${CASE}_oro_data_ss.tile${tt}.nc"          "${DATA}/INPUT/oro_data_ss.tile${tt}.nc"
  done
  if [[ "${DO_NEST:-NO}" == "YES" ]] ; then
    ${NLN} "${DATA}/INPUT/oro_data.tile7.nc" "${DATA}/INPUT/oro_data.nest02.tile7.nc"
    ${NLN} "${DATA}/INPUT/${CASE}_grid.tile7.nc"     "${DATA}/INPUT/${CASE}_grid.nest02.tile7.nc"
    ${NLN} "${DATA}/INPUT/${CASE}_grid.tile7.nc"     "${DATA}/INPUT/grid.nest02.tile7.nc"
    ${NLN} "${DATA}/INPUT/oro_data_ls.tile7.nc" "${DATA}/INPUT/oro_data_ls.nest02.tile7.nc"
    ${NLN} "${DATA}/INPUT/oro_data_ss.tile7.nc" "${DATA}/INPUT/oro_data_ss.nest02.tile7.nc"
  fi

  # NoahMP table
  local noahmptablefile="${PARMgfs}/ufs/noahmptable.tbl"
  if [[ ! -f "${noahmptablefile}" ]]; then
    echo "FATAL ERROR: missing noahmp table file '${noahmptablefile}'"
    exit 1
  else
    ${NCP} "${noahmptablefile}" "${DATA}/noahmptable.tbl"
  fi

  #  Thompson microphysics fix files
  if (( imp_physics == 8 )); then
    ${NCP} "${FIXgfs}/am/CCN_ACTIVATE.BIN" "${DATA}/CCN_ACTIVATE.BIN"
    ${NCP} "${FIXgfs}/am/freezeH2O.dat"    "${DATA}/freezeH2O.dat"
    ${NCP} "${FIXgfs}/am/qr_acr_qgV2.dat"  "${DATA}/qr_acr_qgV2.dat"
    ${NCP} "${FIXgfs}/am/qr_acr_qsV2.dat"  "${DATA}/qr_acr_qsV2.dat"
  fi

  if [[ "${new_o3forc:-YES}" == "YES" ]]; then
    O3FORC="ozprdlos_2015_new_sbuvO3_tclm15_nuchem.f77"
  else
    O3FORC="global_o3prdlos.f77"
  fi
  H2OFORC=${H2OFORC:-"global_h2o_pltc.f77"}
  ${NCP} "${FIXgfs}/am/${O3FORC}"  "${DATA}/global_o3prdlos.f77"
  ${NCP} "${FIXgfs}/am/${H2OFORC}" "${DATA}/global_h2oprdlos.f77"

  # GFS standard input data

  ISOL=${ISOL:-2}

  ${NCP} "${FIXgfs}/am/global_solarconstant_noaa_an.txt" "${DATA}/solarconstant_noaa_an.txt"
  ${NCP} "${FIXgfs}/am/global_sfc_emissivity_idx.txt"    "${DATA}/sfc_emissivity_idx.txt"

  # Aerosol options
  IAER=${IAER:-1011}

  ## merra2 aerosol climo
  if (( IAER == 1011 )); then
    local month mm
    for (( month = 1; month <=12; month++ )); do
      mm=$(printf %02d "${month}")
      ${NCP} "${FIXgfs}/aer/merra2.aerclim.2014-2023.m${mm}.nc" "aeroclim.m${mm}.nc"
    done
  fi

  ${NCP} "${FIXgfs}/am/global_climaeropac_global.txt" "${DATA}/aerosol.dat"
  if (( IAER > 0 )) ; then
    local file
    for file in "${FIXgfs}/am/global_volcanic_aerosols"* ; do
      ${NCP} "${file}" "${DATA}/$(basename "${file//global_}")"
    done
  fi

  ${NCP} "${FIXgfs}/lut/optics_BC.v1_3.dat"  "${DATA}/optics_BC.dat"
  ${NCP} "${FIXgfs}/lut/optics_OC.v1_3.dat"  "${DATA}/optics_OC.dat"
  ${NCP} "${FIXgfs}/lut/optics_DU.v15_3.dat" "${DATA}/optics_DU.dat"
  ${NCP} "${FIXgfs}/lut/optics_SS.v3_3.dat"  "${DATA}/optics_SS.dat"
  ${NCP} "${FIXgfs}/lut/optics_SU.v1_3.dat"  "${DATA}/optics_SU.dat"

  # CO2 options
  ICO2=${ICO2:-2}

  ${NCP} "${FIXgfs}/am/global_co2historicaldata_glob.txt" "${DATA}/co2historicaldata_glob.txt"
  ${NCP} "${FIXgfs}/am/co2monthlycyc.txt"                 "${DATA}/co2monthlycyc.txt"
  # Set historical CO2 values based on whether this is a reforecast run or not
  # Ref. issue 2403
  local co2dir
  co2dir="fix_co2_proj"
  if [[ "${reforecast:-}" == "YES" ]]; then
    co2dir="co2dat_4a"
  fi
  if (( ICO2 > 0 )); then
    local file
    for file in "${FIXgfs}/am/${co2dir}/global_co2historicaldata"* ; do
      ${NCP} "${file}" "${DATA}/$(basename "${file//global_}")"
    done
  fi

  # Inline UPP fix files
  if [[ "${WRITE_DOPOST:-}" == ".true." ]]; then
    ${NCP} "${POSTGRB2TBL:-${PARMgfs}/post/params_grib2_tbl_new}" "${DATA}/params_grib2_tbl_new"
    ${NCP} "${PARMgfs}/ufs/post_itag_gfs"                         "${DATA}/itag"  # TODO: Need a GEFS version when available in the UFS-weather-model
    # TODO: These should be replaced with ones from the ufs-weather-model when available there
    if [[ "${RUN}" =~ "gdas" || "${RUN}" =~ "gfs" ]]; then  # RUN = gdas | enkfgdas | gfs | enkfgfs
      ${NCP} "${PARMgfs}/post/gfs/postxconfig-NT-gfs-two.txt"     "${DATA}/postxconfig-NT.txt"
      ${NCP} "${PARMgfs}/post/gfs/postxconfig-NT-gfs-f00-two.txt" "${DATA}/postxconfig-NT_FH00.txt"
    elif [[ "${RUN}" == "gefs" && "${SFS_POST:-NO}" == "NO" ]]; then  # RUN = gefs
      ${NCP} "${PARMgfs}/post/gefs/postxconfig-NT-gefs.txt"       "${DATA}/postxconfig-NT.txt"
      ${NCP} "${PARMgfs}/post/gefs/postxconfig-NT-gefs-f00.txt"   "${DATA}/postxconfig-NT_FH00.txt"
    elif [[ "${RUN}" == "gefs" && "${SFS_POST:-NO}" == "YES" ]]; then  # RUN = sfs output
      ${NCP} "${PARMgfs}/post/sfs/postxconfig-NT-sfs.txt"       "${DATA}/postxconfig-NT.txt"
      ${NCP} "${PARMgfs}/post/sfs/postxconfig-NT-sfs.txt"       "${DATA}/postxconfig-NT_FH00.txt"
    fi
  fi
}

# Disable variable not used warnings
# shellcheck disable=SC2034
WW3_predet(){
  echo "SUB ${FUNCNAME[0]}: WW3 before run type determination"

  if [[ ! -d "${COMOUT_WAVE_HISTORY}" ]]; then mkdir -p "${COMOUT_WAVE_HISTORY}"; fi
  if [[ ! -d "${COMOUT_WAVE_RESTART}" ]]; then mkdir -p "${COMOUT_WAVE_RESTART}"; fi

  if [[ ! -d "${DATArestart}/WW3_RESTART" ]]; then mkdir -p "${DATArestart}/WW3_RESTART"; fi
  # Wave restarts are linked in postdet to only create links for files that will be created

  # Files from wave prep and wave init jobs
  # Copy mod_def files for wave grids
  local ww3_grid
  if [[ "${waveMULTIGRID}" == ".true." ]]; then
    local array=("${WAVECUR_FID}" "${WAVEICE_FID}" "${WAVEWND_FID}" "${waveuoutpGRD}" "${waveGRD}" "${waveesmfGRD}")
    echo "Wave Grids: ${array[*]}"
    local grdALL
    # shellcheck disable=SC2312
    grdALL=$(printf "%s\n" "${array[@]}" | sort -u | tr '\n' ' ')

    for ww3_grid in ${grdALL}; do
      ${NCP} "${COMIN_WAVE_PREP}/${RUN}wave.mod_def.${ww3_grid}" "${DATA}/mod_def.${ww3_grid}" \
      || ( echo "FATAL ERROR: Failed to copy '${RUN}wave.mod_def.${ww3_grid}' from '${COMIN_WAVE_PREP}'"; exit 1 )
    done
  else
    #if shel, only 1 waveGRD which is linked to mod_def.ww3
    ${NCP} "${COMIN_WAVE_PREP}/${RUN}wave.mod_def.${waveGRD}" "${DATA}/mod_def.ww3" \
    || ( echo "FATAL ERROR: Failed to copy '${RUN}wave.mod_def.${waveGRD}' from '${COMIN_WAVE_PREP}'"; exit 1 )
  fi

  if [[ "${WW3ICEINP}" == "YES" ]]; then
    local wavicefile="${COMIN_WAVE_PREP}/${RUN}wave.${WAVEICE_FID}.t${current_cycle:8:2}z.ice"
    if [[ ! -f "${wavicefile}" ]]; then
      echo "FATAL ERROR: WW3ICEINP='${WW3ICEINP}', but missing ice file '${wavicefile}', ABORT!"
      exit 1
    fi
    ${NCP} "${wavicefile}" "${DATA}/ice.${WAVEICE_FID}" \
    || ( echo "FATAL ERROR: Unable to copy '${wavicefile}', ABORT!"; exit 1 )
  fi

  if [[ "${WW3CURINP}" == "YES" ]]; then
    local wavcurfile="${COMIN_WAVE_PREP}/${RUN}wave.${WAVECUR_FID}.t${current_cycle:8:2}z.cur"
    if [[ ! -f "${wavcurfile}" ]]; then
      echo "FATAL ERROR: WW3CURINP='${WW3CURINP}', but missing current file '${wavcurfile}', ABORT!"
      exit 1
    fi
    ${NCP} "${wavcurfile}" "${DATA}/current.${WAVECUR_FID}" \
    || ( echo "FATAL ERROR: Unable to copy '${wavcurfile}', ABORT!"; exit 1 )
  fi

  # Fix files
  #if wave mesh is not the same as the ocean mesh, copy it in the file
  if [[ "${MESH_WAV}" == "${MESH_OCN:-mesh.mx${OCNRES}.nc}" ]]; then
    echo "Wave is on the same mesh as ocean"
  else
    echo "Wave is NOT on the same mesh as ocean"
    ${NCP} "${FIXgfs}/wave/${MESH_WAV}" "${DATA}/"
  fi

  WAV_MOD_TAG="${RUN}wave${waveMEMB}"
  if [[ "${USE_WAV_RMP:-YES}" == "YES" ]]; then
    local file file_array file_count
    # shellcheck disable=SC2312
    mapfile -t file_array < <(find "${FIXgfs}/wave" -name "rmp_src_to_dst_conserv_*" | sort)
    file_count=${#file_array[@]}
    if (( file_count > 0 )); then
      for file in "${file_array[@]}" ; do
        ${NCP} "${file}" "${DATA}/"
      done
    else
      echo 'FATAL ERROR : No rmp precomputed nc files found for wave model, ABORT!'
      exit 4
    fi
  fi
}

# shellcheck disable=SC2034
CICE_predet(){
  echo "SUB ${FUNCNAME[0]}: CICE before run type determination"

  if [[ ! -d "${COMOUT_ICE_HISTORY}" ]]; then mkdir -p "${COMOUT_ICE_HISTORY}"; fi
  if [[ ! -d "${COMOUT_ICE_RESTART}" ]]; then mkdir -p "${COMOUT_ICE_RESTART}"; fi
  if [[ ! -d "${COMIN_ICE_INPUT}" ]]; then mkdir -p "${COMIN_ICE_INPUT}"; fi

  if [[ ! -d "${DATA}/CICE_OUTPUT" ]]; then  mkdir -p "${DATA}/CICE_OUTPUT"; fi
  if [[ ! -d "${DATArestart}/CICE_RESTART" ]]; then mkdir -p "${DATArestart}/CICE_RESTART"; fi
  ${NLN} "${DATArestart}/CICE_RESTART" "${DATA}/CICE_RESTART"

  # CICE does not have a concept of high frequency output like FV3
  # Convert output settings into an explicit list for CICE
  # shellcheck disable=SC2312
  mapfile -t CICE_OUTPUT_FH < <(seq "${FHMIN}" "${FHOUT_ICE}" "${FHMAX}") || exit 10

  # Fix files
  ${NCP} "${FIXgfs}/cice/${ICERES}/${CICE_GRID}" "${DATA}/"
  ${NCP} "${FIXgfs}/cice/${ICERES}/${CICE_MASK}" "${DATA}/"
  ${NCP} "${FIXgfs}/cice/${ICERES}/${MESH_ICE}"  "${DATA}/"

}

# shellcheck disable=SC2034
MOM6_predet(){
  echo "SUB ${FUNCNAME[0]}: MOM6 before run type determination"

  if [[ ! -d "${COMOUT_OCEAN_HISTORY}" ]]; then mkdir -p "${COMOUT_OCEAN_HISTORY}"; fi
  if [[ ! -d "${COMOUT_OCEAN_RESTART}" ]]; then mkdir -p "${COMOUT_OCEAN_RESTART}"; fi
  if [[ ! -d "${COMIN_OCEAN_INPUT}" ]]; then mkdir -p "${COMIN_OCEAN_INPUT}"; fi

  if [[ ! -d "${DATA}/MOM6_OUTPUT" ]]; then mkdir -p "${DATA}/MOM6_OUTPUT"; fi
  if [[ ! -d "${DATArestart}/MOM6_RESTART" ]]; then mkdir -p "${DATArestart}/MOM6_RESTART"; fi
  ${NLN} "${DATArestart}/MOM6_RESTART" "${DATA}/MOM6_RESTART"

  # MOM6 does not have a concept of high frequency output like FV3
  # Convert output settings into an explicit list for MOM6
  MOM6_OUTPUT_FH=$(seq -s ' ' "${FHMIN}" "${FHOUT_OCN}" "${FHMAX}")

  # If using stochastic parameterizations, create a seed that does not exceed the
  # largest signed integer
  if (( MEMBER > 0 )); then  # these are only applicable for ensemble members
    local imem=${MEMBER#0}
    local base_seed=$((current_cycle*10000 + imem*100))

    if [[ "${DO_OCN_SPPT:-}" == "YES" ]]; then
      ISEED_OCNSPPT=$((base_seed + 8)),$((base_seed + 9)),$((base_seed + 10)),$((base_seed + 11)),$((base_seed + 12))
    fi

    if [[ "${DO_OCN_PERT_EPBL:-}" == "YES" ]]; then
      ISEED_EPBL=$((base_seed + 13)),$((base_seed + 14)),$((base_seed + 15)),$((base_seed + 16)),$((base_seed + 17))
    fi
  fi

  # Fix files
  ${NCP} "${FIXgfs}/mom6/${OCNRES}/"* "${DATA}/INPUT/"  # TODO: These need to be explicit

  # Copy coupled grid_spec
  local spec_file
  spec_file="${FIXgfs}/cpl/a${CASE}o${OCNRES}/grid_spec.nc"
  if [[ -s "${spec_file}" ]]; then
    ${NCP} "${spec_file}" "${DATA}/INPUT/"
  else
    echo "FATAL ERROR: coupled grid_spec file '${spec_file}' does not exist"
    exit 3
  fi

}

CMEPS_predet(){
  echo "SUB ${FUNCNAME[0]}: CMEPS before run type determination"

  if [[ ! -d "${COMOUT_MED_RESTART}" ]]; then mkdir -p "${COMOUT_MED_RESTART}"; fi

  if [[ ! -d "${DATArestart}/CMEPS_RESTART" ]]; then mkdir -p "${DATArestart}/CMEPS_RESTART"; fi
  ${NLN} "${DATArestart}/CMEPS_RESTART" "${DATA}/CMEPS_RESTART"

}

# shellcheck disable=SC2034
GOCART_predet(){
  echo "SUB ${FUNCNAME[0]}: GOCART before run type determination"

  if [[ ! -d "${COMOUT_CHEM_HISTORY}" ]]; then mkdir -p "${COMOUT_CHEM_HISTORY}"; fi

  # FHMAX gets modified when IAU is on, so keep origianl value for GOCART output
  GOCART_MAX=${FHMAX}

  # GOCART output times can't be computed here because they may depend on FHROT
}
