#! /usr/bin/env bash

#####
## This script defines functions for data I/O and namelist.
## different applications could share the same function
## or have their own.
##
## This is a child script of modular
## forecast script. This script is function definition.
## need to call these functions in the parent script
## for execution.
#####

# Disable variable not used warnings
# shellcheck disable=SC2034
FV3_postdet(){
  echo "SUB ${FUNCNAME[0]}: Entering for RUN = ${RUN}"

  echo "warm_start = ${warm_start}"
  echo "RERUN = ${RERUN}"

  #-------------------------------------------------------
  if [[ "${warm_start}" = ".true." ]] || [[ "${RERUN}" = "YES" ]]; then
    #-------------------------------------------------------
    #.............................
    if [[ ${RERUN} = "NO" ]]; then
      #.............................

      # Link all restart files from previous cycle
      for file in "${COM_ATMOS_RESTART_PREV}/${sPDY}.${scyc}0000."*.nc; do
        file2=$(echo $(basename "${file}"))
        file2=$(echo "${file2}" | cut -d. -f3-) # remove the date from file
        fsuf=$(echo "${file2}" | cut -d. -f1)
        ${NLN} "${file}" "${DATA}/INPUT/${file2}"
      done

      # Replace sfc_data with sfcanl_data restart files from current cycle (if found)
      for file in "${COM_ATMOS_RESTART}/${sPDY}.${scyc}0000."*.nc; do
        file2=$(basename "${file}")
        file2=$(echo "${file2}" | cut -d. -f3-) # remove the date from file
        fsufanl=$(echo "${file2}" | cut -d. -f1)
        file2=$(echo "${file2}" | sed -e "s/sfcanl_data/sfc_data/g")
        rm -f "${DATA}/INPUT/${file2}"
        ${NLN} "${file}" "${DATA}/INPUT/${file2}"
      done

      # Need a coupler.res when doing IAU  # FIXME: This is needed for warm_start, regardless of IAU.
      if [[ ${DOIAU} = "YES" ]]; then
        rm -f "${DATA}/INPUT/coupler.res"
        cat >> "${DATA}/INPUT/coupler.res" << EOF
        3        (Calendar: no_calendar=0, thirty_day_months=1, julian=2, gregorian=3, noleap=4)
        ${gPDY:0:4}  ${gPDY:4:2}  ${gPDY:6:2}  ${gcyc}     0     0        Model start time:   year, month, day, hour, minute, second
        ${sPDY:0:4}  ${sPDY:4:2}  ${sPDY:6:2}  ${scyc}     0     0        Current model time: year, month, day, hour, minute, second
EOF
      fi

      # Link increments
      if [[ ${DOIAU} = "YES" ]]; then
        for i in $(echo "${IAUFHRS}" | sed "s/,/ /g" | rev); do
          incfhr=$(printf %03i "${i}")
          if [[ ${incfhr} = "006" ]]; then
            increment_file="${COM_ATMOS_ANALYSIS}/${RUN}.t${cyc}z.${PREFIX_ATMINC}atminc.nc"
          else
            increment_file="${COM_ATMOS_ANALYSIS}/${RUN}.t${cyc}z.${PREFIX_ATMINC}atmi${incfhr}.nc"
          fi
          if [[ ! -f ${increment_file} ]]; then
            echo "FATAL ERROR: DOIAU = ${DOIAU}, but missing increment file for fhr ${incfhr} at ${increment_file}"
            echo "Abort!"
            exit 1
          fi
          ${NLN} "${increment_file}" "${DATA}/INPUT/fv_increment${i}.nc"
          IAU_INC_FILES="'fv_increment${i}.nc',${IAU_INC_FILES:-}"
        done
        read_increment=".false."
        res_latlon_dynamics=""
      else
        increment_file="${COM_ATMOS_ANALYSIS}/${RUN}.t${cyc}z.${PREFIX_ATMINC}atminc.nc"
        if [[ -f ${increment_file} ]]; then
          ${NLN} "${increment_file}" "${DATA}/INPUT/fv3_increment.nc"
          read_increment=".true."
          res_latlon_dynamics="fv3_increment.nc"
        fi
      fi

    #.............................
    else  ##RERUN
      export warm_start=".true."
      PDYT="${CDATE_RST:0:8}"
      cyct="${CDATE_RST:8:2}"
      for file in "${COM_ATMOS_RESTART}/${PDYT}.${cyct}0000."*; do
        file2=$(basename "${file}")
        file2=$(echo "${file2}" | cut -d. -f3-)
        ${NLN} "${file}" "${DATA}/INPUT/${file2}"
      done

      local hour_rst
      hour_rst=$(nhour "${CDATE_RST}" "${current_cycle}")
      # shellcheck disable=SC2034
      IAU_FHROT=$((IAU_OFFSET+hour_rst))
      if [[ ${DOIAU} = "YES" ]]; then
        IAUFHRS=-1
        # Ignore "not used" warning
        # shellcheck disable=SC2034
        IAU_DELTHRS=0
        IAU_INC_FILES="''"
      fi
    fi
    #.............................

  else ## cold start
    for file in "${COM_ATMOS_INPUT}/"*.nc; do
      file2=$(basename "${file}")
      fsuf="${file2:0:3}"
      if [[ "${fsuf}" = "gfs" ]] || [[ "${fsuf}" = "sfc" ]]; then
        ${NLN} "${file}" "${DATA}/INPUT/${file2}"
      fi
    done

  fi

  nfiles=$(ls -1 "${DATA}/INPUT/"* | wc -l)
  if [[ ${nfiles} -le 0 ]]; then
    echo SUB "${FUNCNAME[0]}": Initial conditions must exist in "${DATA}/INPUT", ABORT!
    exit 1
  fi

  # If doing IAU, change forecast hours
  if [[ "${DOIAU}" = "YES" ]]; then
    FHMAX=$((FHMAX+6))
    if [[ ${FHMAX_HF} -gt 0 ]]; then
      FHMAX_HF=$((FHMAX_HF+6))
    fi
  fi

  #--------------------------------------------------------------------------
  # Grid and orography data

  if [[ ${cplflx} = ".false." ]] ; then
    ${NLN} "${FIXgfs}/orog/${CASE}/${CASE}_mosaic.nc" "${DATA}/INPUT/grid_spec.nc"
  else
    ${NLN} "${FIXgfs}/orog/${CASE}/${CASE}_mosaic.nc" "${DATA}/INPUT/${CASE}_mosaic.nc"
  fi

  for n in $(seq 1 "${ntiles}"); do
    ${NLN} "${FIXgfs}/orog/${CASE}/${CASE}.mx${OCNRES}_oro_data.tile${n}.nc" "${DATA}/INPUT/oro_data.tile${n}.nc"
    ${NLN} "${FIXgfs}/orog/${CASE}/${CASE}_grid.tile${n}.nc"     "${DATA}/INPUT/${CASE}_grid.tile${n}.nc"
  done

  _suite_file="${HOMEgfs}/sorc/ufs_model.fd/FV3/ccpp/suites/suite_${CCPP_SUITE}.xml"
  if [[ ! -f ${_suite_file} ]]; then
    echo "FATAL ERROR: CCPP Suite file ${_suite_file} does not exist!"
    exit 2
  fi

  # Scan suite file to determine whether it uses Noah-MP
  if [[ $(grep noahmpdrv "${_suite_file}" | wc -l ) -gt 0 ]]; then
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

  # NoahMP table
  local noahmptablefile="${PARMgfs}/ufs/noahmptable.tbl"
  if [[ ! -f ${noahmptablefile} ]]; then
    echo "FATAL ERROR: missing noahmp table file ${noahmptablefile}"
    exit 1
  else
    ${NLN} "${noahmptablefile}" "${DATA}/noahmptable.tbl"
  fi

  # Files for GWD
  ${NLN} "${FIXgfs}/ugwd/ugwp_limb_tau.nc" "${DATA}/ugwp_limb_tau.nc"
  for n in $(seq 1 "${ntiles}"); do
    ${NLN} "${FIXgfs}/ugwd/${CASE}/${CASE}_oro_data_ls.tile${n}.nc" "${DATA}/INPUT/oro_data_ls.tile${n}.nc"
    ${NLN} "${FIXgfs}/ugwd/${CASE}/${CASE}_oro_data_ss.tile${n}.nc" "${DATA}/INPUT/oro_data_ss.tile${n}.nc"
  done

  # GFS standard input data

  ISOL=${ISOL:-2}
  IAER=${IAER:-1011}
  ICO2=${ICO2:-2}

  if [[ ${new_o3forc:-YES} = YES ]]; then
    O3FORC=ozprdlos_2015_new_sbuvO3_tclm15_nuchem.f77
  else
    O3FORC=global_o3prdlos.f77
  fi
  H2OFORC=${H2OFORC:-"global_h2o_pltc.f77"}
  ####
  #  Copy CCN_ACTIVATE.BIN for Thompson microphysics
  #  Thompson microphysics used when CCPP_SUITE set to FV3_GSD_v0 or FV3_GSD_noah
  #  imp_physics should be 8:
  ####
  if [[ ${imp_physics} -eq 8 ]]; then
    ${NLN} "${FIXgfs}/am/CCN_ACTIVATE.BIN"  "${DATA}/CCN_ACTIVATE.BIN"
    ${NLN} "${FIXgfs}/am/freezeH2O.dat"     "${DATA}/freezeH2O.dat"
    ${NLN} "${FIXgfs}/am/qr_acr_qgV2.dat"   "${DATA}/qr_acr_qgV2.dat"
    ${NLN} "${FIXgfs}/am/qr_acr_qsV2.dat"   "${DATA}/qr_acr_qsV2.dat"
  fi

  ${NLN} "${FIXgfs}/am/${O3FORC}"                         "${DATA}/global_o3prdlos.f77"
  ${NLN} "${FIXgfs}/am/${H2OFORC}"                        "${DATA}/global_h2oprdlos.f77"
  ${NLN} "${FIXgfs}/am/global_solarconstant_noaa_an.txt"  "${DATA}/solarconstant_noaa_an.txt"
  ${NLN} "${FIXgfs}/am/global_sfc_emissivity_idx.txt"     "${DATA}/sfc_emissivity_idx.txt"

  ## merra2 aerosol climo
  if [[ ${IAER} -eq "1011" ]]; then
    for month in $(seq 1 12); do
      MM=$(printf %02d "${month}")
      ${NLN} "${FIXgfs}/aer/merra2.aerclim.2003-2014.m${MM}.nc" "aeroclim.m${MM}.nc"
    done
  fi
    
  ${NLN} "${FIXgfs}/lut/optics_BC.v1_3.dat"  "${DATA}/optics_BC.dat"
  ${NLN} "${FIXgfs}/lut/optics_OC.v1_3.dat"  "${DATA}/optics_OC.dat"
  ${NLN} "${FIXgfs}/lut/optics_DU.v15_3.dat" "${DATA}/optics_DU.dat"
  ${NLN} "${FIXgfs}/lut/optics_SS.v3_3.dat"  "${DATA}/optics_SS.dat"
  ${NLN} "${FIXgfs}/lut/optics_SU.v1_3.dat"  "${DATA}/optics_SU.dat"

  ${NLN} "${FIXgfs}/am/global_co2historicaldata_glob.txt" "${DATA}/co2historicaldata_glob.txt"
  ${NLN} "${FIXgfs}/am/co2monthlycyc.txt"                 "${DATA}/co2monthlycyc.txt"
  # Set historical CO2 values based on whether this is a reforecast run or not
  # Ref. issue 2403
  local co2dir
  co2dir="fix_co2_proj"
  if [[ ${reforecast:-"NO"} == "YES" ]]; then
    co2dir="co2dat_4a"
  fi
  if (( ICO2 > 0 )); then
    for file in $(ls "${FIXgfs}/am/${co2dir}/global_co2historicaldata"*) ; do
      ${NLN} "${file}" "${DATA}/$(basename "${file//global_}")"
    done
  fi

  ${NLN} "${FIXgfs}/am/global_climaeropac_global.txt"     "${DATA}/aerosol.dat"
  if [[ ${IAER} -gt 0 ]] ; then
    for file in $(ls "${FIXgfs}/am/global_volcanic_aerosols"*) ; do
      ${NLN} "${file}" "${DATA}/$(basename "${file//global_}")"
    done
  fi

  # inline post fix files
  if [[ ${WRITE_DOPOST} = ".true." ]]; then
    ${NLN} "${PARMgfs}/post/post_tag_gfs${LEVS}"             "${DATA}/itag"
    ${NLN} "${FLTFILEGFS:-${PARMgfs}/post/postxconfig-NT-GFS-TWO.txt}"           "${DATA}/postxconfig-NT.txt"
    ${NLN} "${FLTFILEGFSF00:-${PARMgfs}/post/postxconfig-NT-GFS-F00-TWO.txt}"    "${DATA}/postxconfig-NT_FH00.txt"
    ${NLN} "${POSTGRB2TBL:-${PARMgfs}/post/params_grib2_tbl_new}"                "${DATA}/params_grib2_tbl_new"
  fi

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

  # Fix files
  FNGLAC=${FNGLAC:-"${FIXgfs}/am/global_glacier.2x2.grb"}
  FNMXIC=${FNMXIC:-"${FIXgfs}/am/global_maxice.2x2.grb"}
  FNTSFC=${FNTSFC:-"${FIXgfs}/am/RTGSST.1982.2012.monthly.clim.grb"}
  FNSNOC=${FNSNOC:-"${FIXgfs}/am/global_snoclim.1.875.grb"}
  FNZORC=${FNZORC:-"igbp"}
  FNAISC=${FNAISC:-"${FIXgfs}/am/IMS-NIC.blended.ice.monthly.clim.grb"}
  FNALBC2=${FNALBC2:-"${FIXgfs}/orog/${CASE}/sfc/${CASE}.mx${OCNRES}.facsf.tileX.nc"}
  FNTG3C=${FNTG3C:-"${FIXgfs}/orog/${CASE}/sfc/${CASE}.mx${OCNRES}.substrate_temperature.tileX.nc"}
  FNVEGC=${FNVEGC:-"${FIXgfs}/orog/${CASE}/sfc/${CASE}.mx${OCNRES}.vegetation_greenness.tileX.nc"}
  FNMSKH=${FNMSKH:-"${FIXgfs}/am/global_slmask.t1534.3072.1536.grb"}
  FNVMNC=${FNVMNC:-"${FIXgfs}/orog/${CASE}/sfc/${CASE}.mx${OCNRES}.vegetation_greenness.tileX.nc"}
  FNVMXC=${FNVMXC:-"${FIXgfs}/orog/${CASE}/sfc/${CASE}.mx${OCNRES}.vegetation_greenness.tileX.nc"}
  FNSLPC=${FNSLPC:-"${FIXgfs}/orog/${CASE}/sfc/${CASE}.mx${OCNRES}.slope_type.tileX.nc"}
  FNALBC=${FNALBC:-"${FIXgfs}/orog/${CASE}/sfc/${CASE}.mx${OCNRES}.snowfree_albedo.tileX.nc"}
  FNVETC=${FNVETC:-"${FIXgfs}/orog/${CASE}/sfc/${CASE}.mx${OCNRES}.vegetation_type.tileX.nc"}
  FNSOTC=${FNSOTC:-"${FIXgfs}/orog/${CASE}/sfc/${CASE}.mx${OCNRES}.soil_type.tileX.nc"}
  FNSOCC=${FNSOCC:-"${FIXgfs}/orog/${CASE}/sfc/${CASE}.mx${OCNRES}.soil_color.tileX.nc"}
  FNABSC=${FNABSC:-"${FIXgfs}/orog/${CASE}/sfc/${CASE}.mx${OCNRES}.maximum_snow_albedo.tileX.nc"}
  FNSMCC=${FNSMCC:-"${FIXgfs}/am/global_soilmgldas.statsgo.t${JCAP}.${LONB}.${LATB}.grb"}

  # If the appropriate resolution fix file is not present, use the highest resolution available (T1534)
  [[ ! -f ${FNSMCC} ]] && FNSMCC="${FIXgfs}/am/global_soilmgldas.statsgo.t1534.3072.1536.grb"

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

  # the pre-conditioning of the solution
  # =0 implies no pre-conditioning
  # >0 means new adiabatic pre-conditioning
  # <0 means older adiabatic pre-conditioning
  na_init=${na_init:-1}
  [[ ${warm_start} = ".true." ]] && na_init=0

  # variables for controlling initialization of NCEP/NGGPS ICs
  filtered_terrain=${filtered_terrain:-".true."}
  gfs_dwinds=${gfs_dwinds:-".true."}

  # various debug options
  no_dycore=${no_dycore:-".false."}
  dycore_only=${adiabatic:-".false."}
  chksum_debug=${chksum_debug:-".false."}
  print_freq=${print_freq:-6}

  if [[ ${TYPE} = "nh" ]]; then # non-hydrostatic options
    hydrostatic=".false."
    phys_hydrostatic=".false."     # enable heating in hydrostatic balance in non-hydrostatic simulation
    use_hydro_pressure=".false."   # use hydrostatic pressure for physics
    if [[ ${warm_start} = ".true." ]]; then
      make_nh=".false."              # restarts contain non-hydrostatic state
    else
      make_nh=".true."               # re-initialize non-hydrostatic state
    fi

  else # hydrostatic options
    hydrostatic=".true."
    phys_hydrostatic=".false."     # ignored when hydrostatic = T
    use_hydro_pressure=".false."   # ignored when hydrostatic = T
    make_nh=".false."              # running in hydrostatic mode
  fi

  # Conserve total energy as heat globally
  consv_te=${consv_te:-1.} # range 0.-1., 1. will restore energy to orig. val. before physics

  # time step parameters in FV3
  k_split=${k_split:-2}
  n_split=${n_split:-5}

  if [[ "${MONO:0:4}" = "mono" ]]; then # monotonic options
    d_con=${d_con_mono:-"0."}
    do_vort_damp=".false."
    if [[ ${TYPE} = "nh" ]]; then # non-hydrostatic
      hord_mt=${hord_mt_nh_mono:-"10"}
      hord_xx=${hord_xx_nh_mono:-"10"}
    else # hydrostatic
      hord_mt=${hord_mt_hydro_mono:-"10"}
      hord_xx=${hord_xx_hydro_mono:-"10"}
    fi

  else # non-monotonic options
    d_con=${d_con_nonmono:-"1."}
    do_vort_damp=".true."
    if [[ ${TYPE} = "nh" ]]; then # non-hydrostatic
      hord_mt=${hord_mt_nh_nonmono:-"5"}
      hord_xx=${hord_xx_nh_nonmono:-"5"}
    else # hydrostatic
      hord_mt=${hord_mt_hydro_nonmono:-"10"}
      hord_xx=${hord_xx_hydro_nonmono:-"10"}
    fi
  fi

  if [[ "${MONO:0:4}" != "mono" ]] && [[ "${TYPE}" = "nh" ]]; then
    vtdm4=${vtdm4_nh_nonmono:-"0.06"}
  else
    vtdm4=${vtdm4:-"0.05"}
  fi

  if [[ ${warm_start} = ".true." ]]; then # warm start from restart file
    nggps_ic=".false."
    ncep_ic=".false."
    external_ic=".false."
    mountain=".true."
    if [[ ${read_increment} = ".true." ]]; then # add increment on the fly to the restarts
      res_latlon_dynamics="fv3_increment.nc"
    else
      res_latlon_dynamics='""'
    fi

  else # CHGRES'd GFS analyses
    nggps_ic=${nggps_ic:-".true."}
    ncep_ic=${ncep_ic:-".false."}
    external_ic=".true."
    mountain=".false."
    read_increment=".false."
    res_latlon_dynamics='""'
  fi

  # Stochastic Physics Options
  if [[ ${DO_SPPT:-"NO"} = "YES" ]]; then
    do_sppt=".true."
    ISEED_SPPT=$((current_cycle*10000 + ${MEMBER#0}*100 + 3)),$((current_cycle*10000 + ${MEMBER#0}*100 + 4)),$((current_cycle*10000 + ${MEMBER#0}*100 + 5)),$((current_cycle*10000 + ${MEMBER#0}*100 + 6)),$((current_cycle*10000 + ${MEMBER#0}*100 + 7))
  else
    ISEED=${ISEED:-0}
  fi
  if (( MEMBER > 0 )) && [[ ${DO_CA:-"NO"} = "YES" ]]; then
    ISEED_CA=$(( (current_cycle*10000 + ${MEMBER#0}*100 + 18) % 2147483647 ))
  fi
  if [[ ${DO_SKEB} = "YES" ]]; then
    do_skeb=".true."
    ISEED_SKEB=$((current_cycle*10000 + ${MEMBER#0}*100 + 1))
  fi
  if [[ ${DO_SHUM} = "YES" ]]; then
    do_shum=".true."
    ISEED_SHUM=$((current_cycle*1000 + MEMBER*10 + 2))
  fi
  if [[ ${DO_LAND_PERT} = "YES" ]]; then
    lndp_type=${lndp_type:-2}
    ISEED_LNDP=$(( (current_cycle*1000 + MEMBER*10 + 5) % 2147483647 ))
    LNDP_TAU=${LNDP_TAU:-21600}
    LNDP_SCALE=${LNDP_SCALE:-500000}
    ISEED_LNDP=${ISEED_LNDP:-${ISEED}}
    lndp_var_list=${lndp_var_list:-"'smc', 'vgf',"}
    lndp_prt_list=${lndp_prt_list:-"0.2,0.1"}
    n_var_lndp=$(echo "${lndp_var_list}" | wc -w)
  fi
  JCAP_STP=${JCAP_STP:-${JCAP_CASE}}
  LONB_STP=${LONB_STP:-${LONB_CASE}}
  LATB_STP=${LATB_STP:-${LATB_CASE}}
  cd "${DATA}" || exit 1
  if [[ "${QUILTING}" = ".true." ]] && [[ "${OUTPUT_GRID}" = "gaussian_grid" ]]; then
    for fhr in ${FV3_OUTPUT_FH}; do
      local FH3=$(printf %03i "${fhr}")
      local FH2=$(printf %02i "${fhr}")
      ${NLN} "${COM_ATMOS_HISTORY}/${RUN}.t${cyc}z.atmf${FH3}.nc" "atmf${FH3}.nc"
      ${NLN} "${COM_ATMOS_HISTORY}/${RUN}.t${cyc}z.sfcf${FH3}.nc" "sfcf${FH3}.nc"
      ${NLN} "${COM_ATMOS_HISTORY}/${RUN}.t${cyc}z.atm.logf${FH3}.txt" "log.atm.f${FH3}"
      if [[ ${WRITE_DOPOST} = ".true." ]]; then
        ${NLN} "${COM_ATMOS_MASTER}/${RUN}.t${cyc}z.master.grb2f${FH3}" "GFSPRS.GrbF${FH2}"
        ${NLN} "${COM_ATMOS_MASTER}/${RUN}.t${cyc}z.sfluxgrbf${FH3}.grib2" "GFSFLX.GrbF${FH2}"
      fi
    done
  else  # TODO: Is this even valid anymore?
    for n in $(seq 1 "${ntiles}"); do
      ${NLN} "nggps2d.tile${n}.nc"       "${COM_ATMOS_HISTORY}/nggps2d.tile${n}.nc"
      ${NLN} "nggps3d.tile${n}.nc"       "${COM_ATMOS_HISTORY}/nggps3d.tile${n}.nc"
      ${NLN} "grid_spec.tile${n}.nc"     "${COM_ATMOS_HISTORY}/grid_spec.tile${n}.nc"
      ${NLN} "atmos_static.tile${n}.nc"  "${COM_ATMOS_HISTORY}/atmos_static.tile${n}.nc"
      ${NLN} "atmos_4xdaily.tile${n}.nc" "${COM_ATMOS_HISTORY}/atmos_4xdaily.tile${n}.nc"
    done
  fi
}

FV3_nml(){
  # namelist output for a certain component
  echo "SUB ${FUNCNAME[0]}: Creating name lists and model configure file for FV3"
  # Call child scripts in current script directory
  source "${USHgfs}/parsing_namelists_FV3.sh"
  FV3_namelists
  echo "SUB ${FUNCNAME[0]}: FV3 name lists and model configure file created"
}

FV3_out() {
  echo "SUB ${FUNCNAME[0]}: copying output data for FV3"

  # Copy FV3 restart files
  if [[ ${RUN} =~ "gdas" ]]; then
    cd "${DATA}/RESTART"
    local idate=$(date --utc -d "${current_cycle:0:8} ${current_cycle:8:2} + ${restart_interval} hours" +%Y%m%d%H)
    while [[ ${idate} -le ${forecast_end_cycle} ]]; do
      for file in "${idate:0:8}.${idate:8:2}0000."*; do
        ${NCP} "${file}" "${COM_ATMOS_RESTART}/${file}"
      done
      local idate=$(date --utc -d "${idate:0:8} ${idate:8:2} + ${restart_interval} hours" +%Y%m%d%H)
    done
  else
    # No need to copy FV3 restart files when RUN=gfs or gefs
    ${NCP} "${DATA}/input.nml" "${COM_CONF}/ufs.input.nml"
    ${NCP} "${DATA}/model_configure" "${COM_CONF}/ufs.model_configure"
    ${NCP} "${DATA}/ufs.configure" "${COM_CONF}/ufs.ufs.configure"
    ${NCP} "${DATA}/diag_table" "${COM_CONF}/ufs.diag_table"
  fi
  echo "SUB ${FUNCNAME[0]}: Output data for FV3 copied"
}

WW3_postdet() {
  echo "SUB ${FUNCNAME[0]}: Linking input data for WW3"
  COMPONENTwave=${COMPONENTwave:-${RUN}wave}

  #Link mod_def files for wave grids
  if [[ ${waveMULTIGRID} = ".true." ]]; then
    local array=(${WAVECUR_FID} ${WAVEICE_FID} ${WAVEWND_FID} ${waveuoutpGRD} ${waveGRD} ${waveesmfGRD})
    echo "Wave Grids: ${WAVECUR_FID} ${WAVEICE_FID} ${WAVEWND_FID} ${waveuoutpGRD} ${waveGRD} ${waveesmfGRD}"
    local grdALL=$(printf "%s\n" "${array[@]}" | sort -u | tr '\n' ' ')

    for wavGRD in ${grdALL}; do
      ${NCP} "${COM_WAVE_PREP}/${COMPONENTwave}.mod_def.${wavGRD}" "${DATA}/mod_def.${wavGRD}"
    done
  else
    #if shel, only 1 waveGRD which is linked to mod_def.ww3
    ${NCP} "${COM_WAVE_PREP}/${COMPONENTwave}.mod_def.${waveGRD}" "${DATA}/mod_def.ww3"
  fi


  #if wave mesh is not the same as the ocean mesh, link it in the file
  if [[ "${MESH_WAV}" == "${MESH_OCN:-mesh.mx${OCNRES}.nc}" ]]; then
    echo "Wave is on same mesh as ocean"
  else
    ${NLN} "${FIXgfs}/wave/${MESH_WAV}" "${DATA}/"
  fi

  export wavprfx=${RUNwave}${WAV_MEMBER:-}

  #Copy initial condition files:
  for wavGRD in ${waveGRD} ; do
    if [[ "${warm_start}" = ".true." ]] || [[ "${RERUN}" = "YES" ]]; then
      if [[ ${RERUN} = "NO" ]]; then
        local waverstfile="${COM_WAVE_RESTART_PREV}/${sPDY}.${scyc}0000.restart.${wavGRD}"
      else
        local waverstfile="${COM_WAVE_RESTART}/${PDYT}.${cyct}0000.restart.${wavGRD}"
      fi
    else
      local waverstfile="${COM_WAVE_RESTART}/${sPDY}.${scyc}0000.restart.${wavGRD}"
    fi
    if [[ ! -f ${waverstfile} ]]; then
      if [[ ${RERUN} = "NO" ]]; then
        echo "WARNING: NON-FATAL ERROR wave IC is missing, will start from rest"
      else
        echo "FATAL ERROR: Wave IC ${waverstfile} is missing in RERUN, exiting."
        exit 1
      fi
    else
      if [[ ${waveMULTIGRID} = ".true." ]]; then
        ${NLN} "${waverstfile}" "${DATA}/restart.${wavGRD}"
      else
        ${NLN} "${waverstfile}" "${DATA}/restart.ww3"
      fi
    fi
  done

  if [[ ${waveMULTIGRID} = ".true." ]]; then
    for wavGRD in ${waveGRD} ; do
      ${NLN} "${COM_WAVE_HISTORY}/${wavprfx}.log.${wavGRD}.${PDY}${cyc}" "log.${wavGRD}"
    done
  else
    ${NLN} "${COM_WAVE_HISTORY}/${wavprfx}.log.${waveGRD}.${PDY}${cyc}" "log.ww3"
  fi

  if [[ "${WW3ICEINP}" = "YES" ]]; then
    local wavicefile="${COM_WAVE_PREP}/${RUNwave}.${WAVEICE_FID}.${cycle}.ice"
    if [[ ! -f ${wavicefile} ]]; then
      echo "FATAL ERROR: WW3ICEINP = ${WW3ICEINP}, but missing ice file ${wavicefile}"
      echo "Abort!"
      exit 1
    fi
    ${NLN} "${wavicefile}" "${DATA}/ice.${WAVEICE_FID}"
  fi

  if [[ "${WW3CURINP}" = "YES" ]]; then
    local wavcurfile="${COM_WAVE_PREP}/${RUNwave}.${WAVECUR_FID}.${cycle}.cur"
    if [[ ! -f ${wavcurfile} ]]; then
      echo "FATAL ERROR: WW3CURINP = ${WW3CURINP}, but missing current file ${wavcurfile}"
      echo "Abort!"
      exit 1
    fi
    ${NLN} "${wavcurfile}" "${DATA}/current.${WAVECUR_FID}"
  fi

  # Link output files
  cd "${DATA}"
  if [[ ${waveMULTIGRID} = ".true." ]]; then
    ${NLN} "${COM_WAVE_HISTORY}/${wavprfx}.log.mww3.${PDY}${cyc}" "log.mww3"
  fi

  # Loop for gridded output (uses FHINC)
  local fhr vdate FHINC wavGRD
  fhr=${FHMIN_WAV}
  while [[ ${fhr} -le ${FHMAX_WAV} ]]; do
    vdate=$(date --utc -d "${current_cycle:0:8} ${current_cycle:8:2} + ${fhr} hours" +%Y%m%d%H)
    if [[ ${waveMULTIGRID} = ".true." ]]; then
      for wavGRD in ${waveGRD} ; do
        ${NLN} "${COM_WAVE_HISTORY}/${wavprfx}.out_grd.${wavGRD}.${vdate:0:8}.${vdate:8:2}0000" "${DATA}/${vdate:0:8}.${vdate:8:2}0000.out_grd.${wavGRD}"
      done
    else
      ${NLN} "${COM_WAVE_HISTORY}/${wavprfx}.out_grd.${waveGRD}.${vdate:0:8}.${vdate:8:2}0000" "${DATA}/${vdate:0:8}.${vdate:8:2}0000.out_grd.ww3"
    fi
    FHINC=${FHOUT_WAV}
    if (( FHMAX_HF_WAV > 0 && FHOUT_HF_WAV > 0 && fhr < FHMAX_HF_WAV )); then
      FHINC=${FHOUT_HF_WAV}
    fi
    fhr=$((fhr+FHINC))
  done

  # Loop for point output (uses DTPNT)
  fhr=${FHMIN_WAV}
  while [[ ${fhr} -le ${FHMAX_WAV} ]]; do
    vdate=$(date --utc -d "${current_cycle:0:8} ${current_cycle:8:2} + ${fhr} hours" +%Y%m%d%H)
    if [[ ${waveMULTIGRID} = ".true." ]]; then
      ${NLN} "${COM_WAVE_HISTORY}/${wavprfx}.out_pnt.${waveuoutpGRD}.${vdate:0:8}.${vdate:8:2}0000" "${DATA}/${vdate:0:8}.${vdate:8:2}0000.out_pnt.${waveuoutpGRD}"
    else
      ${NLN} "${COM_WAVE_HISTORY}/${wavprfx}.out_pnt.${waveuoutpGRD}.${vdate:0:8}.${vdate:8:2}0000" "${DATA}/${vdate:0:8}.${vdate:8:2}0000.out_pnt.ww3"
    fi

    FHINC=${FHINCP_WAV}
    fhr=$((fhr+FHINC))
  done
}

WW3_nml() {
  echo "SUB ${FUNCNAME[0]}: Copying input files for WW3"
  WAV_MOD_TAG=${RUN}wave${waveMEMB}
  if [[ "${USE_WAV_RMP:-YES}" = "YES" ]]; then
    if (( $( ls -1 "${FIXgfs}/wave/rmp_src_to_dst_conserv_"* 2> /dev/null | wc -l) > 0 )); then
      for file in $(ls "${FIXgfs}/wave/rmp_src_to_dst_conserv_"*) ; do
        ${NLN} "${file}" "${DATA}/"
      done
    else
      echo 'FATAL ERROR : No rmp precomputed nc files found for wave model'
      exit 4
    fi
  fi
  source "${USHgfs}/parsing_namelists_WW3.sh"
  WW3_namelists
}

WW3_out() {
  echo "SUB ${FUNCNAME[0]}: Copying output data for WW3"
}


CPL_out() {
  echo "SUB ${FUNCNAME[0]}: Copying output data for general cpl fields"
  if [[ "${esmf_profile:-}" = ".true." ]]; then
    ${NCP} "${DATA}/ESMF_Profile.summary" "${COM_ATMOS_HISTORY}/ESMF_Profile.summary"
  fi
}

MOM6_postdet() {
  echo "SUB ${FUNCNAME[0]}: MOM6 after run type determination"

  # Copy MOM6 ICs
  ${NLN} "${COM_OCEAN_RESTART_PREV}/${sPDY}.${scyc}0000.MOM.res.nc" "${DATA}/INPUT/MOM.res.nc"
  case ${OCNRES} in
    "025")
      local nn
      for nn in $(seq 1 4); do
        if [[ -f "${COM_OCEAN_RESTART_PREV}/${sPDY}.${scyc}0000.MOM.res_${nn}.nc" ]]; then
          ${NLN} "${COM_OCEAN_RESTART_PREV}/${sPDY}.${scyc}0000.MOM.res_${nn}.nc" "${DATA}/INPUT/MOM.res_${nn}.nc"
        fi
      done
    ;;
  esac

  # Link increment
  if [[ "${DO_JEDIOCNVAR:-NO}" = "YES" ]]; then
      if [[ ! -f "${COM_OCEAN_ANALYSIS}/${RUN}.t${cyc}z.ocninc.nc" ]]; then
          echo "FATAL ERROR: Ocean increment ${COM_OCEAN_ANALYSIS}/${RUN}.t${cyc}z.ocninc.nc not found, ABORT!"
          exit 111
      fi
      ${NLN} "${COM_OCEAN_ANALYSIS}/${RUN}.t${cyc}z.ocninc.nc" "${DATA}/INPUT/mom6_increment.nc"
  fi

  # GEFS perturbations
  # TODO if [[ $RUN} == "gefs" ]] block maybe be needed
  #     to ensure it does not interfere with the GFS
  if (( MEMBER > 0 )) && [[ "${ODA_INCUPD:-False}" == "True" ]]; then
     ${NLN} "${COM_OCEAN_RESTART_PREV}/${sPDY}.${scyc}0000.mom6_increment.nc" "${DATA}/INPUT/mom6_increment.nc"
  fi
  # Copy MOM6 fixed files
  ${NCP} "${FIXgfs}/mom6/${OCNRES}/"* "${DATA}/INPUT/"  # TODO: These need to be explicit

  # Copy coupled grid_spec
  spec_file="${FIXgfs}/cpl/a${CASE}o${OCNRES}/grid_spec.nc"
  if [[ -s ${spec_file} ]]; then
    ${NCP} "${spec_file}" "${DATA}/INPUT/"
  else
    echo "FATAL ERROR: grid_spec file '${spec_file}' does not exist"
    exit 3
  fi

  # If using stochatic parameterizations, create a seed that does not exceed the
  # largest signed integer
  if [[ ${DO_OCN_SPPT} = "YES" ]]; then
    ISEED_OCNSPPT=$((current_cycle*10000 + ${MEMBER#0}*100 + 8)),$((current_cycle*10000 + ${MEMBER#0}*100 + 9)),$((current_cycle*10000 + ${MEMBER#0}*100 + 10)),$((current_cycle*10000 + ${MEMBER#0}*100 + 11)),$((current_cycle*10000 + ${MEMBER#0}*100 + 12))
  fi
  if [[ ${DO_OCN_PERT_EPBL} = "YES" ]]; then
    ISEED_EPBL=$((current_cycle*10000 + ${MEMBER#0}*100 + 13)),$((current_cycle*10000 + ${MEMBER#0}*100 + 14)),$((current_cycle*10000 + ${MEMBER#0}*100 + 15)),$((current_cycle*10000 + ${MEMBER#0}*100 + 16)),$((current_cycle*10000 + ${MEMBER#0}*100 + 17))
  fi

  # Link output files
  if [[ "${RUN}" =~ "gfs" || "${RUN}" =~ "gefs" ]]; then
    # Link output files for RUN = gfs|gefs

    # Looping over MOM6 output hours
    local fhr fhr3 last_fhr interval midpoint vdate vdate_mid source_file dest_file
    for fhr in ${MOM6_OUTPUT_FH}; do
      fhr3=$(printf %03i "${fhr}")

      if [[ -z ${last_fhr:-} ]]; then
        last_fhr=${fhr}
        continue
      fi

      (( interval = fhr - last_fhr ))
      (( midpoint = last_fhr + interval/2 ))

      vdate=$(date --utc -d "${current_cycle:0:8} ${current_cycle:8:2} + ${fhr} hours" +%Y%m%d%H)
      vdate_mid=$(date --utc -d "${current_cycle:0:8} ${current_cycle:8:2} + ${midpoint} hours" +%Y%m%d%H)

      # Native model output uses window midpoint in the filename, but we are mapping that to the end of the period for COM
      source_file="ocn_${vdate_mid:0:4}_${vdate_mid:4:2}_${vdate_mid:6:2}_${vdate_mid:8:2}.nc"
      dest_file="${RUN}.ocean.t${cyc}z.${interval}hr_avg.f${fhr3}.nc"
      ${NLN} "${COM_OCEAN_HISTORY}/${dest_file}" "${DATA}/${source_file}"

      # Daily output
      if (( fhr > 0 & fhr % 24 == 0 )); then
        source_file="ocn_daily_${vdate:0:4}_${vdate:4:2}_${vdate:6:2}.nc"
        dest_file="${RUN}.ocean.t${cyc}z.daily.f${fhr3}.nc"
        ${NLN} "${COM_OCEAN_HISTORY}/${dest_file}" "${DATA}/${source_file}"
      fi

      last_fhr=${fhr}

    done

  elif [[ "${RUN}" =~ "gdas" ]]; then
    # Link output files for RUN = gdas

    # Save (instantaneous) MOM6 backgrounds
    for fhr in ${MOM6_OUTPUT_FH}; do
      local fhr3=$(printf %03i "${fhr}")
      local vdatestr=$(date --utc -d "${current_cycle:0:8} ${current_cycle:8:2} + ${fhr} hours" +%Y_%m_%d_%H)
      ${NLN} "${COM_OCEAN_HISTORY}/${RUN}.ocean.t${cyc}z.inst.f${fhr3}.nc" "${DATA}/ocn_da_${vdatestr}.nc"
    done
  fi

  # Link ocean restarts from DATA to COM
  # Coarser than 1/2 degree has a single MOM restart
  ${NLN} "${COM_OCEAN_RESTART}/${forecast_end_cycle:0:8}.${forecast_end_cycle:8:2}0000.MOM.res.nc" "${DATA}/MOM6_RESTART/"
  # 1/4 degree resolution has 4 additional restarts
  case ${OCNRES} in
    "025")
      for nn in $(seq 1 4); do
        ${NLN} "${COM_OCEAN_RESTART}/${forecast_end_cycle:0:8}.${forecast_end_cycle:8:2}0000.MOM.res_${nn}.nc" "${DATA}/MOM6_RESTART/"
      done
      ;;
    *)
    ;;
  esac

  if [[ "${RUN}" =~ "gdas" ]]; then
    local interval idate
    if [[ "${DOIAU}" = "YES" ]]; then
      # Link restarts at the beginning of the next cycle from DATA to COM
      interval=$(( assim_freq / 2 ))
      idate=$(date --utc -d "${next_cycle:0:8} ${next_cycle:8:2} - ${interval} hours" +%Y%m%d%H)
    else
      # Link restarts at the middle of the next cycle from DATA to COM
      idate="${next_cycle}"
    fi
    ${NLN} "${COM_OCEAN_RESTART}/${idate:0:8}.${idate:8:2}0000.MOM.res.nc" "${DATA}/MOM6_RESTART/"
    case ${OCNRES} in
      "025")
        for nn in $(seq 1 4); do
          ${NLN} "${COM_OCEAN_RESTART}/${idate:0:8}.${idate:8:2}0000.MOM.res_${nn}.nc" "${DATA}/MOM6_RESTART/"
        done
        ;;
    esac
  fi

  echo "SUB ${FUNCNAME[0]}: MOM6 input data linked/copied"

}

MOM6_nml() {
  echo "SUB ${FUNCNAME[0]}: Creating name list for MOM6"
  source "${USHgfs}/parsing_namelists_MOM6.sh"
  MOM6_namelists
}

MOM6_out() {
  echo "SUB ${FUNCNAME[0]}: Copying output data for MOM6"

  # Copy MOM_input from DATA to COM_OCEAN_INPUT after the forecast is run (and successfull)
  ${NCP} "${DATA}/INPUT/MOM_input" "${COM_CONF}/ufs.MOM_input"

}

CICE_postdet() {
  echo "SUB ${FUNCNAME[0]}: CICE after run type determination"

  # Copy CICE ICs
  echo "Link CICE ICs"
  cice_restart_file="${COM_ICE_RESTART_PREV}/${sPDY}.${scyc}0000.cice_model.res.nc"
  if [[ ! -f "${cice_restart_file}" ]]; then
    echo "FATAL ERROR: CICE restart file not found at '${cice_restart_file}', ABORT!"
    exit 112
  else
    ${NLN} "${cice_restart_file}" "${DATA}/cice_model.res.nc"
  fi
  rm -f "${DATA}/ice.restart_file"
  echo "${DATA}/cice_model.res.nc" > "${DATA}/ice.restart_file"

  echo "Link CICE fixed files"
  ${NLN} "${FIXgfs}/cice/${ICERES}/${CICE_GRID}" "${DATA}/"
  ${NLN} "${FIXgfs}/cice/${ICERES}/${CICE_MASK}" "${DATA}/"
  ${NLN} "${FIXgfs}/cice/${ICERES}/${MESH_ICE}"  "${DATA}/"

  # Link iceh_ic file to COM.  This is the initial condition file from CICE (f000)
  # TODO: Is this file needed in COM? Is this going to be used for generating any products?
  local vdate seconds vdatestr fhr fhr3 interval last_fhr
  seconds=$(to_seconds "${current_cycle:8:2}0000")  # convert HHMMSS to seconds
  vdatestr="${current_cycle:0:4}-${current_cycle:4:2}-${current_cycle:6:2}-${seconds}"
  ${NLN} "${COM_ICE_HISTORY}/${RUN}.ice.t${cyc}z.ic.nc" "${DATA}/CICE_OUTPUT/iceh_ic.${vdatestr}.nc"

  # Link CICE forecast output files from DATA/CICE_OUTPUT to COM
  local source_file dest_file
  for fhr in ${CICE_OUTPUT_FH}; do
    fhr3=$(printf %03i "${fhr}")

    if [[ -z ${last_fhr:-} ]]; then
      last_fhr=${fhr}
      continue
    fi

    (( interval = fhr - last_fhr ))

    vdate=$(date --utc -d "${current_cycle:0:8} ${current_cycle:8:2} + ${fhr} hours" +%Y%m%d%H)
    seconds=$(to_seconds "${vdate:8:2}0000")  # convert HHMMSS to seconds
    vdatestr="${vdate:0:4}-${vdate:4:2}-${vdate:6:2}-${seconds}"

    if [[ "${RUN}" =~ "gfs" || "${RUN}" =~ "gefs" ]]; then
      source_file="iceh_$(printf "%0.2d" "${interval}")h.${vdatestr}.nc"
      dest_file="${RUN}.ice.t${cyc}z.${interval}hr_avg.f${fhr3}.nc"
    elif [[ "${RUN}" =~ "gdas" ]]; then
      source_file="iceh_inst.${vdatestr}.nc"
      dest_file="${RUN}.ice.t${cyc}z.inst.f${fhr3}.nc"
    fi
    ${NLN} "${COM_ICE_HISTORY}/${dest_file}" "${DATA}/CICE_OUTPUT/${source_file}"

    last_fhr=${fhr}
  done

  # Link CICE restarts from CICE_RESTART to COMOUTice/RESTART
  # Loop over restart_interval and link restarts from DATA to COM
  local vdate vdatestr seconds
  vdate=$(date --utc -d "${current_cycle:0:8} ${current_cycle:8:2} + ${restart_interval} hours" +%Y%m%d%H)
  while [[ ${vdate} -le ${forecast_end_cycle} ]]; do
    seconds=$(to_seconds "${vdate:8:2}0000")  # convert HHMMSS to seconds
    vdatestr="${vdate:0:4}-${vdate:4:2}-${vdate:6:2}-${seconds}"
    ${NLN} "${COM_ICE_RESTART}/${vdate:0:8}.${vdate:8:2}0000.cice_model.res.nc" "${DATA}/CICE_RESTART/cice_model.res.${vdatestr}.nc"
    vdate=$(date --utc -d "${vdate:0:8} ${vdate:8:2} + ${restart_interval} hours" +%Y%m%d%H)
  done
}

CICE_nml() {
  echo "SUB ${FUNCNAME[0]}: Creating name list for CICE"
  source "${USHgfs}/parsing_namelists_CICE.sh"
  CICE_namelists
}

CICE_out() {
  echo "SUB ${FUNCNAME[0]}: Copying output data for CICE"

  # Copy ice_in namelist from DATA to COMOUTice after the forecast is run (and successfull)
  ${NCP} "${DATA}/ice_in" "${COM_CONF}/ufs.ice_in"
}

GOCART_rc() {
  echo "SUB ${FUNCNAME[0]}: Linking input data and copying config files for GOCART"
  # set input directory containing GOCART input data and configuration files
  # this variable is platform-dependent and should be set via a YAML file

  # link directory containing GOCART input dataset, if provided
  if [[ ! -z "${AERO_INPUTS_DIR}" ]]; then
    ${NLN} "${AERO_INPUTS_DIR}" "${DATA}/ExtData"
    status=$?
    [[ ${status} -ne 0 ]] && exit "${status}"
  fi

  # copying GOCART configuration files
  if [[ ! -z "${AERO_CONFIG_DIR}" ]]; then
    ${NCP} "${AERO_CONFIG_DIR}"/*.rc "${DATA}"
    status=$?
    [[ ${status} -ne 0 ]] && exit "${status}"
    # attempt to generate ExtData configuration file if not provided
    if [[ ! -f "${DATA}/AERO_ExtData.rc" ]]; then
      { \
        echo "PrimaryExports%%" ; \
        cat "${AERO_CONFIG_DIR}/ExtData.other" ; \
        cat "${AERO_CONFIG_DIR}/ExtData.${AERO_EMIS_FIRE:-none}" ; \
        echo "%%" ; \
      } > "${DATA}/AERO_ExtData.rc"
      status=$?
      if (( status != 0 )); then exit "${status}"; fi
    fi
  fi
}

GOCART_postdet() {
  echo "SUB ${FUNCNAME[0]}: Linking output data for GOCART"

  for fhr in ${GOCART_OUTPUT_FH}; do
    local vdate=$(date --utc -d "${current_cycle:0:8} ${current_cycle:8:2} + ${fhr} hours" +%Y%m%d%H)

    # Temporarily delete existing files due to noclobber in GOCART
    if [[ -e "${COM_CHEM_HISTORY}/gocart.inst_aod.${vdate:0:8}_${vdate:8:2}00z.nc4" ]]; then
      rm -f "${COM_CHEM_HISTORY}/gocart.inst_aod.${vdate:0:8}_${vdate:8:2}00z.nc4"
    fi

    #To Do: Temporarily removing this as this will crash gocart, adding copy statement at the end
    #${NLN} "${COM_CHEM_HISTORY}/gocart.inst_aod.${vdate:0:8}_${vdate:8:2}00z.nc4" \
    #       "${DATA}/gocart.inst_aod.${vdate:0:8}_${vdate:8:2}00z.nc4"
  done
}

GOCART_out() {
  echo "SUB ${FUNCNAME[0]}: Copying output data for GOCART"

  # Copy gocart.inst_aod after the forecast is run (and successfull)
  # TO DO: this should be linked but there were issues where gocart was crashing if it was linked
  local fhr
  local vdate
  for fhr in ${GOCART_OUTPUT_FH}; do
    if (( fhr == 0 )); then continue; fi
    vdate=$(date --utc -d "${current_cycle:0:8} ${current_cycle:8:2} + ${fhr} hours" +%Y%m%d%H)
    ${NCP} "${DATA}/gocart.inst_aod.${vdate:0:8}_${vdate:8:2}00z.nc4" \
      "${COM_CHEM_HISTORY}/gocart.inst_aod.${vdate:0:8}_${vdate:8:2}00z.nc4"
  done
}

CMEPS_postdet() {
  echo "SUB ${FUNCNAME[0]}: Linking output data for CMEPS mediator"

  # Copy mediator restart files to RUNDIR
  if [[ "${warm_start}" = ".true." ]]; then
    local mediator_file="${COM_MED_RESTART}/${PDY}.${cyc}0000.ufs.cpld.cpl.r.nc"
    if [[ -f "${mediator_file}" ]]; then
      ${NCP} "${mediator_file}" "${DATA}/ufs.cpld.cpl.r.nc"
      rm -f "${DATA}/rpointer.cpl"
      touch "${DATA}/rpointer.cpl"
      echo "ufs.cpld.cpl.r.nc" >> "${DATA}/rpointer.cpl"
    else
      # We have a choice to make here.
      # Either we can FATAL ERROR out, or we can let the coupling fields initialize from zero
      # cmeps_run_type is determined based on the availability of the mediator restart file
      echo "WARNING: ${mediator_file} does not exist for warm_start = .true., initializing!"
      #echo "FATAL ERROR: ${mediator_file} must exist for warm_start = .true. and does not, ABORT!"
      #exit 4
    fi
  fi

  # Link mediator restarts from DATA to COM
  # DANGER DANGER DANGER - Linking mediator restarts to COM causes the model to fail with a message like this below:
  # Abort with message NetCDF: File exists && NC_NOCLOBBER in file pio-2.5.7/src/clib/pioc_support.c at line 2173
  # Instead of linking, copy the mediator files after the model finishes.  See CMEPS_out() below.
  #local rdate rdatestr seconds mediator_file
  #rdate=${forecast_end_cycle}
  #seconds=$(to_seconds "${rdate:8:2}"0000)  # use function to_seconds from forecast_predet.sh to convert HHMMSS to seconds
  #rdatestr="${rdate:0:4}-${rdate:4:2}-${rdate:6:2}-${seconds}"
  #${NLN} "${COM_MED_RESTART}/${rdate:0:8}.${rdate:8:2}0000.ufs.cpld.cpl.r.nc" "${DATA}/CMEPS_RESTART/ufs.cpld.cpl.r.${rdatestr}.nc"

}

CMEPS_out() {
  echo "SUB ${FUNCNAME[0]}: Copying output data for CMEPS mediator"

  # Linking mediator restarts to COM causes the model to fail with a message.
  # Abort with message NetCDF: File exists && NC_NOCLOBBER in file pio-2.5.7/src/clib/pioc_support.c at line 2173
  # Copy mediator restarts from DATA to COM
  local rdate rdatestr seconds mediator_file
  rdate=${forecast_end_cycle}
  seconds=$(to_seconds "${rdate:8:2}"0000)  # use function to_seconds from forecast_predet.sh to convert HHMMSS to seconds
  rdatestr="${rdate:0:4}-${rdate:4:2}-${rdate:6:2}-${seconds}"
  mediator_file="${DATA}/CMEPS_RESTART/ufs.cpld.cpl.r.${rdatestr}.nc"
  if [[ -f ${mediator_file} ]]; then
    ${NCP} "${mediator_file}" "${COM_MED_RESTART}/${rdate:0:8}.${rdate:8:2}0000.ufs.cpld.cpl.r.nc"
  else
    echo "Mediator restart ${mediator_file} not found."
  fi

}
