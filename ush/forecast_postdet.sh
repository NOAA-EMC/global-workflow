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

FV3_GEFS_postdet(){
  echo SUB ${FUNCNAME[0]}: Linking input data for FV3 $RUN
  # soft link commands insert here
}

DATM_postdet(){
  ######################################################################
  # Link DATM  inputs (ie forcing files)                           #
  ######################################################################

  #TODO: This should be some loop through CDATE-> CDATE+ FORECAST length
  #and get input from either CFSR or GEFS or Whatever...
  #Currently assumes you only need the month of DATM input for IC date
  #DATMINPUTDIR should be machine specific

  # DATM forcing file name convention is ${DATM_FILENAME_BASE}.$YYYYMMDDHH.nc
  echo "Link DATM forcing files"
  DATMINPUTDIR="/scratch2/NCEPDEV/marineda/DATM_INPUT/CFSR/${SYEAR}${SMONTH}"
  $NLN -sf ${DATMINPUTDIR}/${DATM_FILENAME_BASE}*.nc $DATA/DATM_INPUT/
}

FV3_GFS_postdet(){
  echo "SUB ${FUNCNAME[0]}: $RERUN and $warm_start determined for $RUN"

  echo $warm_start
  echo $RERUN

  #-------------------------------------------------------
  if [ $warm_start = ".true." -o $RERUN = "YES" ]; then
    #-------------------------------------------------------
    #.............................
    if [ $RERUN = "NO" ]; then
      #.............................

      # Link all (except sfc_data) restart files from $gmemdir
      for file in $(ls $gmemdir/RESTART/${sPDY}.${scyc}0000.*.nc); do
        file2=$(echo $(basename $file))
        file2=$(echo $file2 | cut -d. -f3-) # remove the date from file
        fsuf=$(echo $file2 | cut -d. -f1)
        if [ $fsuf != "sfc_data" ]; then
          $NLN $file $DATA/INPUT/$file2
        fi
      done

      # Link sfcanl_data restart files from $memdir
      for file in $(ls $memdir/RESTART/${sPDY}.${scyc}0000.*.nc); do
        file2=$(echo $(basename $file))
        file2=$(echo $file2 | cut -d. -f3-) # remove the date from file
        fsufanl=$(echo $file2 | cut -d. -f1)
        if [ $fsufanl = "sfcanl_data" ]; then
          file2=$(echo $file2 | sed -e "s/sfcanl_data/sfc_data/g")
          $NLN $file $DATA/INPUT/$file2
        fi
      done

      # Need a coupler.res when doing IAU
      if [ $DOIAU = "YES" ]; then
        rm -f $DATA/INPUT/coupler.res
        cat >> $DATA/INPUT/coupler.res << EOF
        2        (Calendar: no_calendar=0, thirty_day_months=1, julian=2, gregorian=3, noleap=4)
        ${gPDY:0:4}  ${gPDY:4:2}  ${gPDY:6:2}  ${gcyc}     0     0        Model start time:   year, month, day, hour, minute, second
        ${sPDY:0:4}  ${sPDY:4:2}  ${sPDY:6:2}  ${scyc}     0     0        Current model time: year, month, day, hour, minute, second
EOF
      fi

      # Link increments
      if [ $DOIAU = "YES" ]; then
        for i in $(echo $IAUFHRS | sed "s/,/ /g" | rev); do
          incfhr=$(printf %03i $i)
          if [ $incfhr = "006" ]; then
            increment_file=$memdir/${CDUMP}.t${cyc}z.${PREFIX_ATMINC}atminc.nc
          else
            increment_file=$memdir/${CDUMP}.t${cyc}z.${PREFIX_ATMINC}atmi${incfhr}.nc
          fi
          if [ ! -f $increment_file ]; then
            echo "ERROR: DOIAU = $DOIAU, but missing increment file for fhr $incfhr at $increment_file"
            echo "Abort!"
            exit 1
          fi
          $NLN $increment_file $DATA/INPUT/fv_increment$i.nc
          IAU_INC_FILES="'fv_increment$i.nc',${IAU_INC_FILES:-}"
        done
        read_increment=".false."
        res_latlon_dynamics=""
      else
        increment_file=$memdir/${CDUMP}.t${cyc}z.${PREFIX_ATMINC}atminc.nc
        if [ -f $increment_file ]; then
          $NLN $increment_file $DATA/INPUT/fv3_increment.nc
          read_increment=".true."
          res_latlon_dynamics="fv3_increment.nc"
        fi
      fi

    #.............................
    else  ##RERUN
      export warm_start=".true."
      PDYT=$(echo $CDATE_RST | cut -c1-8)
      cyct=$(echo $CDATE_RST | cut -c9-10)
      for file in $(ls $RSTDIR_ATM/${PDYT}.${cyct}0000.*); do
        file2=$(echo $(basename $file))
        file2=$(echo $file2 | cut -d. -f3-)
        $NLN $file $DATA/INPUT/$file2
      done

      hour_rst=$($NHOUR $CDATE_RST $CDATE)
      IAU_FHROT=$((IAU_OFFSET+hour_rst))
      if [ $DOIAU = "YES" ]; then
        IAUFHRS=-1
        IAU_DELTHRS=0
        IAU_INC_FILES="''"
      fi

      rst_list_rerun=""
      xfh=$restart_interval_gfs
      while [ $xfh -le $FHMAX_GFS ]; do
        rst_list_rerun="$rst_list_rerun $xfh"
        xfh=$((xfh+restart_interval_gfs))
      done
      restart_interval="$rst_list_rerun"
    fi
    #.............................

  else ## cold start
    for file in $(ls $memdir/INPUT/*.nc); do
      file2=$(echo $(basename $file))
      fsuf=$(echo $file2 | cut -c1-3)
      if [ $fsuf = "gfs" -o $fsuf = "sfc" ]; then
        $NLN $file $DATA/INPUT/$file2
      fi
    done

  fi

  if [ $machine = 'sandbox' ]; then
    echo SUB ${FUNCNAME[0]}: Checking initial condition, overriden in sandbox mode!
  else
    nfiles=$(ls -1 $DATA/INPUT/* | wc -l)
    if [ $nfiles -le 0 ]; then
      echo SUB ${FUNCNAME[0]}: Initial conditions must exist in $DATA/INPUT, ABORT!
      msg="SUB ${FUNCNAME[0]}: Initial conditions must exist in $DATA/INPUT, ABORT!"
      postmsg "$jlogfile" "$msg"
      exit 1
    fi
  fi

  # If doing IAU, change forecast hours
  if [[ "$DOIAU" = "YES" ]]; then
    FHMAX=$((FHMAX+6))
    if [ $FHMAX_HF -gt 0 ]; then
      FHMAX_HF=$((FHMAX_HF+6))
    fi
  fi

  #--------------------------------------------------------------------------
  # Grid and orography data
  for n in $(seq 1 $ntiles); do
    $NLN $FIXfv3/$CASE/${CASE}_grid.tile${n}.nc     $DATA/INPUT/${CASE}_grid.tile${n}.nc
    $NLN $FIXfv3/$CASE/${CASE}_oro_data.tile${n}.nc $DATA/INPUT/oro_data.tile${n}.nc
  done

  if [ $cplflx = ".false." ] ; then
    $NLN $FIXfv3/$CASE/${CASE}_mosaic.nc $DATA/INPUT/grid_spec.nc
  else
    $NLN $FIXfv3/$CASE/${CASE}_mosaic.nc $DATA/INPUT/${CASE}_mosaic.nc
  fi

  # Fractional grid related
  if [ $FRAC_GRID = ".true." ]; then
    OROFIX=${OROFIX:-"${FIX_DIR}/fix_fv3_fracoro/${CASE}.mx${OCNRES}_frac"}
    FIX_SFC=${FIX_SFC:-"${OROFIX}/fix_sfc"}
    for n in $(seq 1 $ntiles); do
      $NLN ${OROFIX}/oro_${CASE}.mx${OCNRES}.tile${n}.nc $DATA/INPUT/oro_data.tile${n}.nc
    done
  else
    OROFIX=${OROFIX:-"${FIXfv3}/${CASE}"}
    FIX_SFC=${FIX_SFC:-"${OROFIX}/fix_sfc"}
    for n in $(seq 1 $ntiles); do
      $NLN ${OROFIX}/${CASE}_oro_data.tile${n}.nc $DATA/INPUT/oro_data.tile${n}.nc
    done
  fi

  export CCPP_SUITE=${CCPP_SUITE:-"FV3_GFS_v16"}
  _suite_file=$HOMEgfs/sorc/ufs_model.fd/FV3/ccpp/suites/suite_${CCPP_SUITE}.xml

  if [ ! -f ${_suite_file} ]; then
    echo "FATAL: CCPP Suite file ${_suite_file} does not exist!"
    exit 2
  fi

  # Scan suite file to determine whether it uses Noah-MP
  if [ $(grep noahmpdrv ${_suite_file} | wc -l ) -gt 0 ]; then
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

  # Files for GWD
  OROFIX_ugwd=${OROFIX_ugwd:-"${FIX_DIR}/fix_ugwd"}
  $NLN ${OROFIX_ugwd}/ugwp_limb_tau.nc $DATA/ugwp_limb_tau.nc
  for n in $(seq 1 $ntiles); do
    $NLN ${OROFIX_ugwd}/$CASE/${CASE}_oro_data_ls.tile${n}.nc $DATA/INPUT/oro_data_ls.tile${n}.nc
    $NLN ${OROFIX_ugwd}/$CASE/${CASE}_oro_data_ss.tile${n}.nc $DATA/INPUT/oro_data_ss.tile${n}.nc
  done

  # GFS standard input data

  ISOL=${ISOL:-2}
  IAER=${IAER:-1011}
  ICO2=${ICO2:-2}

  if [ ${new_o3forc:-YES} = YES ]; then
    O3FORC=ozprdlos_2015_new_sbuvO3_tclm15_nuchem.f77
  else
    O3FORC=global_o3prdlos.f77
  fi
  H2OFORC=${H2OFORC:-"global_h2o_pltc.f77"}
  ####
  #  Copy CCN_ACTIVATE.BIN for Thompson microphysics
  #  Thompson microphysics used when CCPP_SUITE set to FV3_GSD_v0 or FV3_GSD_noah
  #  imp_physics should be 8
  ####
  if [ $imp_physics -eq 8 ]; then
    $NLN $FIX_AM/CCN_ACTIVATE.BIN  $DATA/CCN_ACTIVATE.BIN
    $NLN $FIX_AM/freezeH2O.dat     $DATA/freezeH2O.dat
    $NLN $FIX_AM/qr_acr_qg.dat     $DATA/qr_acr_qg.dat 
    $NLN $FIX_AM/qr_acr_qs.dat     $DATA/qr_acr_qs.dat
  fi

  $NLN $FIX_AM/${O3FORC}                         $DATA/global_o3prdlos.f77
  $NLN $FIX_AM/${H2OFORC}                        $DATA/global_h2oprdlos.f77
  $NLN $FIX_AM/global_solarconstant_noaa_an.txt  $DATA/solarconstant_noaa_an.txt
  $NLN $FIX_AM/global_sfc_emissivity_idx.txt     $DATA/sfc_emissivity_idx.txt

  ## merra2 aerosol climo
  if [ $IAER -eq "1011" ]; then
    FIX_AER="${FIX_DIR}/fix_aer"
    for month in $(seq 1 12); do
      MM=$(printf %02d $month)
      $NLN "${FIX_AER}/merra2.aerclim.2003-2014.m${MM}.nc" "aeroclim.m${MM}.nc"
    done
    FIX_LUT="${FIX_DIR}/fix_lut"
    $NLN $FIX_LUT/optics_BC.v1_3.dat $DATA/optics_BC.dat
    $NLN $FIX_LUT/optics_OC.v1_3.dat $DATA/optics_OC.dat
    $NLN $FIX_LUT/optics_DU.v15_3.dat $DATA/optics_DU.dat
    $NLN $FIX_LUT/optics_SS.v3_3.dat $DATA/optics_SS.dat
    $NLN $FIX_LUT/optics_SU.v1_3.dat $DATA/optics_SU.dat
  fi

  $NLN $FIX_AM/global_co2historicaldata_glob.txt $DATA/co2historicaldata_glob.txt
  $NLN $FIX_AM/co2monthlycyc.txt                 $DATA/co2monthlycyc.txt
  if [ $ICO2 -gt 0 ]; then
    for file in $(ls $FIX_AM/fix_co2_proj/global_co2historicaldata*) ; do
      $NLN $file $DATA/$(echo $(basename $file) | sed -e "s/global_//g")
    done
  fi

  $NLN $FIX_AM/global_climaeropac_global.txt     $DATA/aerosol.dat
  if [ $IAER -gt 0 ] ; then
    for file in $(ls $FIX_AM/global_volcanic_aerosols*) ; do
      $NLN $file $DATA/$(echo $(basename $file) | sed -e "s/global_//g")
    done
  fi

  # inline post fix files
  if [ $WRITE_DOPOST = ".true." ]; then
    $NLN $PARM_POST/post_tag_gfs${LEVS}             $DATA/itag
    $NLN $PARM_POST/postxconfig-NT-GFS-TWO.txt      $DATA/postxconfig-NT.txt
    $NLN $PARM_POST/postxconfig-NT-GFS-F00-TWO.txt  $DATA/postxconfig-NT_FH00.txt
    $NLN $PARM_POST/params_grib2_tbl_new            $DATA/params_grib2_tbl_new
  fi

  #------------------------------------------------------------------
  # changeable parameters
  # dycore definitions
  res=$(echo $CASE |cut -c2-5)
  resp=$((res+1))
  npx=$resp
  npy=$resp
  npz=$((LEVS-1))
  io_layout="1,1"
  #ncols=$(( (${npx}-1)*(${npy}-1)*3/2 ))

  # spectral truncation and regular grid resolution based on FV3 resolution
  JCAP_CASE=$((2*res-2))
  LONB_CASE=$((4*res))
  LATB_CASE=$((2*res))

  JCAP=${JCAP:-$JCAP_CASE}
  LONB=${LONB:-$LONB_CASE}
  LATB=${LATB:-$LATB_CASE}

  LONB_IMO=${LONB_IMO:-$LONB_CASE}
  LATB_JMO=${LATB_JMO:-$LATB_CASE}

  # Fix files
  FNGLAC=${FNGLAC:-"$FIX_AM/global_glacier.2x2.grb"}
  FNMXIC=${FNMXIC:-"$FIX_AM/global_maxice.2x2.grb"}
  FNTSFC=${FNTSFC:-"$FIX_AM/RTGSST.1982.2012.monthly.clim.grb"}
  FNSNOC=${FNSNOC:-"$FIX_AM/global_snoclim.1.875.grb"}
  FNZORC=${FNZORC:-"igbp"}
  FNAISC=${FNAISC:-"$FIX_AM/IMS-NIC.blended.ice.monthly.clim.grb"}
  FNALBC2=${FNALBC2:-"${FIX_SFC}/${CASE}.facsf.tileX.nc"}
  FNTG3C=${FNTG3C:-"${FIX_SFC}/${CASE}.substrate_temperature.tileX.nc"}
  FNVEGC=${FNVEGC:-"${FIX_SFC}/${CASE}.vegetation_greenness.tileX.nc"}
  FNMSKH=${FNMSKH:-"$FIX_AM/global_slmask.t1534.3072.1536.grb"}
  FNVMNC=${FNVMNC:-"${FIX_SFC}/${CASE}.vegetation_greenness.tileX.nc"}
  FNVMXC=${FNVMXC:-"${FIX_SFC}/${CASE}.vegetation_greenness.tileX.nc"}
  FNSLPC=${FNSLPC:-"${FIX_SFC}/${CASE}.slope_type.tileX.nc"}
  FNALBC=${FNALBC:-"${FIX_SFC}/${CASE}.snowfree_albedo.tileX.nc"}
  FNVETC=${FNVETC:-"${FIX_SFC}/${CASE}.vegetation_type.tileX.nc"}
  FNSOTC=${FNSOTC:-"${FIX_SFC}/${CASE}.soil_type.tileX.nc"}
  FNABSC=${FNABSC:-"${FIX_SFC}/${CASE}.maximum_snow_albedo.tileX.nc"}
  FNSMCC=${FNSMCC:-"$FIX_AM/global_soilmgldas.statsgo.t${JCAP}.${LONB}.${LATB}.grb"}

  # If the appropriate resolution fix file is not present, use the highest resolution available (T1534)
  [[ ! -f $FNSMCC ]] && FNSMCC="$FIX_AM/global_soilmgldas.statsgo.t1534.3072.1536.grb"

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
  nstf_name=${nstf_name:-"$NST_MODEL,$NST_SPINUP,$NST_RESV,$ZSEA1,$ZSEA2"}
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
  [[ $warm_start = ".true." ]] && na_init=0

  # variables for controlling initialization of NCEP/NGGPS ICs
  filtered_terrain=${filtered_terrain:-".true."}
  gfs_dwinds=${gfs_dwinds:-".true."}

  # various debug options
  no_dycore=${no_dycore:-".false."}
  dycore_only=${adiabatic:-".false."}
  chksum_debug=${chksum_debug:-".false."}
  print_freq=${print_freq:-6}

  if [ ${TYPE} = "nh" ]; then # non-hydrostatic options
    hydrostatic=".false."
    phys_hydrostatic=".false."     # enable heating in hydrostatic balance in non-hydrostatic simulation
    use_hydro_pressure=".false."   # use hydrostatic pressure for physics
    if [ $warm_start = ".true." ]; then
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
  n_split=${n_split:-6}

  if [ $(echo $MONO | cut -c-4) = "mono" ];  then # monotonic options
    d_con=${d_con_mono:-"0."}
    do_vort_damp=".false."
    if [ ${TYPE} = "nh" ]; then # non-hydrostatic
      hord_mt=${hord_mt_nh_mono:-"10"}
      hord_xx=${hord_xx_nh_mono:-"10"}
    else # hydrostatic
      hord_mt=${hord_mt_hydro_mono:-"10"}
      hord_xx=${hord_xx_hydro_mono:-"10"}
    fi

  else # non-monotonic options
    d_con=${d_con_nonmono:-"1."}
    do_vort_damp=".true."
    if [ ${TYPE} = "nh" ]; then # non-hydrostatic
      hord_mt=${hord_mt_nh_nonmono:-"5"}
      hord_xx=${hord_xx_nh_nonmono:-"5"}
    else # hydrostatic
      hord_mt=${hord_mt_hydro_nonmono:-"10"}
      hord_xx=${hord_xx_hydro_nonmono:-"10"}
    fi
  fi

  if [ $(echo $MONO | cut -c-4) != "mono" -a $TYPE = "nh" ]; then
    vtdm4=${vtdm4_nh_nonmono:-"0.06"}
  else
    vtdm4=${vtdm4:-"0.05"}
  fi

  if [ $warm_start = ".true." ]; then # warm start from restart file
    nggps_ic=".false."
    ncep_ic=".false."
    external_ic=".false."
    mountain=".true."
    if [ $read_increment = ".true." ]; then # add increment on the fly to the restarts
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
  if [ ${SET_STP_SEED:-"YES"} = "YES" ]; then
    ISEED_SKEB=$((CDATE*1000 + MEMBER*10 + 1))
    ISEED_SHUM=$((CDATE*1000 + MEMBER*10 + 2))
    ISEED_SPPT=$((CDATE*1000 + MEMBER*10 + 3))
    ISEED_CA=$(( (CDATE*1000 + MEMBER*10 + 4) % 2147483647 ))
    ISEED_LNDP=$(( (CDATE*1000 + MEMBER*10 + 5) % 2147483647 ))
  else
    ISEED=${ISEED:-0}
  fi
  if [ $DO_SKEB = "YES" ]; then
    do_skeb=".true."
  fi
  if [ $DO_SPPT = "YES" ]; then
    do_sppt=".true."
  fi
  if [ $DO_SHUM = "YES" ]; then
    do_shum=".true."
  fi
  if [ $DO_LAND_PERT = "YES" ]; then
    lndp_type=${lndp_type:-2}
    LNDP_TAU=${LNDP_TAU:-21600}
    LNDP_SCALE=${LNDP_SCALE:-500000}
    ISEED_LNDP=${ISEED_LNDP:-$ISEED}
    lndp_var_list=${lndp_var_list:-"'smc', 'vgf',"}
    lndp_prt_list=${lndp_prt_list:-"0.2,0.1"}
    n_var_lndp=$(echo "$lndp_var_list" | wc -w)
  fi
  JCAP_STP=${JCAP_STP:-$JCAP_CASE}
  LONB_STP=${LONB_STP:-$LONB_CASE}
  LATB_STP=${LATB_STP:-$LATB_CASE}

  cd $DATA

  affix="nc"
  if [ "$OUTPUT_FILE" = "nemsio" ]; then
    affix="nemsio"
  fi

  if [ $QUILTING = ".true." -a $OUTPUT_GRID = "gaussian_grid" ]; then
    fhr=$FHMIN
    for fhr in $OUTPUT_FH; do
      FH3=$(printf %03i $fhr)
      FH2=$(printf %02i $fhr)
      atmi=atmf${FH3}.$affix
      sfci=sfcf${FH3}.$affix
      logi=logf${FH3}
      pgbi=GFSPRS.GrbF${FH2}
      flxi=GFSFLX.GrbF${FH2}
      atmo=$memdir/${CDUMP}.t${cyc}z.atmf${FH3}.$affix
      sfco=$memdir/${CDUMP}.t${cyc}z.sfcf${FH3}.$affix
      logo=$memdir/${CDUMP}.t${cyc}z.logf${FH3}.txt
      pgbo=$memdir/${CDUMP}.t${cyc}z.master.grb2f${FH3}
      flxo=$memdir/${CDUMP}.t${cyc}z.sfluxgrbf${FH3}.grib2
      eval $NLN $atmo $atmi
      eval $NLN $sfco $sfci
      eval $NLN $logo $logi
      if [ $WRITE_DOPOST = ".true." ]; then
        eval $NLN $pgbo $pgbi
        eval $NLN $flxo $flxi
      fi
    done
  else
    for n in $(seq 1 $ntiles); do
      eval $NLN nggps2d.tile${n}.nc       $memdir/nggps2d.tile${n}.nc
      eval $NLN nggps3d.tile${n}.nc       $memdir/nggps3d.tile${n}.nc
      eval $NLN grid_spec.tile${n}.nc     $memdir/grid_spec.tile${n}.nc
      eval $NLN atmos_static.tile${n}.nc  $memdir/atmos_static.tile${n}.nc
      eval $NLN atmos_4xdaily.tile${n}.nc $memdir/atmos_4xdaily.tile${n}.nc
    done
  fi
}

FV3_GFS_nml(){
  # namelist output for a certain component
  echo SUB ${FUNCNAME[0]}: Creating name lists and model configure file for FV3
  if [ $machine = 'sandbox' ]; then
    cd $SCRIPTDIR
    echo "MAIN: !!!Sandbox mode, writing to current directory!!!"
  fi
  # Call child scripts in current script directory
  source $SCRIPTDIR/parsing_namelists_FV3.sh
  FV3_namelists
  echo SUB ${FUNCNAME[0]}: FV3 name lists and model configure file created
}

DATM_nml(){
  source $SCRIPTDIR/parsing_namelists_DATM.sh
  DATM_namelists
  echo SUB ${FUNCNAME[0]}: DATM name lists and model configure file created
}

data_out_GFS() {
  # data in take for FV3GFS
  # Arguments: None
  #
  #------------------------------------------------------------------
  # make symbolic links to write forecast files directly in memdir
  echo "SUB ${FUNCNAME[0]}: copying output data for FV3"
  #------------------------------------------------------------------

  if [ $SEND = "YES" ]; then
    # Copy model restart files
    if [ $CDUMP = "gdas" -a $rst_invt1 -gt 0 ]; then
      cd $DATA/RESTART
      mkdir -p $memdir/RESTART
      for rst_int in $restart_interval ; do
        if [ $rst_int -ge 0 ]; then
          RDATE=$($NDATE +$rst_int $CDATE)
          rPDY=$(echo $RDATE | cut -c1-8)
          rcyc=$(echo $RDATE | cut -c9-10)
          for file in $(ls ${rPDY}.${rcyc}0000.*) ; do
            $NCP $file $memdir/RESTART/$file
          done
        fi
      done
      if [ $DOIAU = "YES" ] || [ $DOIAU_coldstart = "YES" ]; then
        # if IAU is on, save restart at start of IAU window
        rst_iau=$(( ${IAU_OFFSET} - (${IAU_DELTHRS}/2) ))
        if [ $rst_iau -lt 0 ];then
          rst_iau=$(( (${IAU_DELTHRS}) - ${IAU_OFFSET} ))
        fi
        RDATE=$($NDATE +$rst_iau $CDATE)
        rPDY=$(echo $RDATE | cut -c1-8)
        rcyc=$(echo $RDATE | cut -c9-10)
        for file in $(ls ${rPDY}.${rcyc}0000.*) ; do
          $NCP $file $memdir/RESTART/$file
        done
      fi
    elif [ $CDUMP = "gfs" ]; then
      $NCP $DATA/input.nml $ROTDIR/${CDUMP}.${PDY}/${cyc}/atmos/
    fi
  fi

  echo "SUB ${FUNCNAME[0]}: Output data for FV3 copied"
}

WW3_postdet() {
  echo "SUB ${FUNCNAME[0]}: Linking input data for WW3"
  COMPONENTwave=${COMPONENTwave:-${RUN}wave}

  #Link mod_def files for wave grids
  if [ $waveMULTIGRID = ".true." ]; then
    array=($WAVECUR_FID $WAVEICE_FID $WAVEWND_FID $waveuoutpGRD $waveGRD $waveesmfGRD)
    echo "Wave Grids: $WAVECUR_FID $WAVEICE_FID $WAVEWND_FID $waveuoutpGRD $waveGRD $waveesmfGRD"
    grdALL=$(printf "%s\n" "${array[@]}" | sort -u | tr '\n' ' ')

    for wavGRD in ${grdALL}; do
      $NCP $ROTDIR/${CDUMP}.${PDY}/${cyc}/wave/rundata/${COMPONENTwave}.mod_def.$wavGRD $DATA/mod_def.$wavGRD
    done
  else 
    #if shel, only 1 waveGRD which is linked to mod_def.ww3 
    $NCP $ROTDIR/${CDUMP}.${PDY}/${cyc}/wave/rundata/${COMPONENTwave}.mod_def.$waveGRD $DATA/mod_def.ww3
  fi


  #if wave mesh is not the same as the ocn/ice mesh, linkk it in the file
  comparemesh=${MESH_OCN_ICE:-"mesh.mx${ICERES}.nc"}
  if [ "$MESH_WAV" = "$comparemesh" ]; then 
    echo "Wave is on same mesh as ocean/ice"
  else 
    $NLN -sf $FIXwave/$MESH_WAV $DATA/
  fi 

  export WAVHCYC=${WAVHCYC:-6}
  export WRDATE=$($NDATE -${WAVHCYC} $CDATE)
  export WRPDY=$(echo $WRDATE | cut -c1-8)
  export WRcyc=$(echo $WRDATE | cut -c9-10)
  export WRDIR=${ROTDIR}/${CDUMPRSTwave}.${WRPDY}/${WRcyc}/wave/restart
  export RSTDIR_WAVE=$ROTDIR/${CDUMP}.${PDY}/${cyc}/wave/restart
  export datwave=$COMOUTwave/rundata
  export wavprfx=${CDUMPwave}${WAV_MEMBER:-}

  #Copy initial condition files:
  for wavGRD in $waveGRD ; do
    if [ $warm_start = ".true." -o $RERUN = "YES" ]; then
      if [ $RERUN = "NO" ]; then
        waverstfile=${WRDIR}/${sPDY}.${scyc}0000.restart.${wavGRD}
      else 
        waverstfile=${RSTDIR_WAVE}/${PDYT}.${cyct}0000.restart.${wavGRD}
      fi
    else 
      waverstfile=${RSTDIR_WAVE}/${sPDY}.${scyc}0000.restart.${wavGRD}
    fi
    if [ ! -f ${waverstfile} ]; then
      if [ $RERUN = "NO" ]; then
        echo "WARNING: NON-FATAL ERROR wave IC is missing, will start from rest"
      else 
        echo "ERROR: Wave IC is missing in RERUN, exiting." 
        exit 1 
      fi 
    else
      if [ $waveMULTIGRID = ".true." ]; then
        $NLN ${waverstfile} $DATA/restart.${wavGRD}
      else 
        $NLN ${waverstfile} $DATA/restart.ww3
      fi
    fi
  done  

  if [ $waveMULTIGRID = ".true." ]; then
    for wavGRD in $waveGRD ; do
      $NLN $datwave/${wavprfx}.log.${wavGRD}.${PDY}${cyc} log.${wavGRD}
    done
  else 
    $NLN $datwave/${wavprfx}.log.${waveGRD}.${PDY}${cyc} log.ww3 
  fi 

  if [ "$WW3ICEINP" = "YES" ]; then
    wavicefile=$COMINwave/rundata/${CDUMPwave}.${WAVEICE_FID}.${cycle}.ice
    if [ ! -f $wavicefile ]; then
      echo "ERROR: WW3ICEINP = ${WW3ICEINP}, but missing ice file"
      echo "Abort!"
      exit 1
    fi
    $NLN ${wavicefile} $DATA/ice.${WAVEICE_FID}
  fi

  if [ "$WW3CURINP" = "YES" ]; then
    wavcurfile=$COMINwave/rundata/${CDUMPwave}.${WAVECUR_FID}.${cycle}.cur
    if [ ! -f $wavcurfile ]; then
      echo "ERROR: WW3CURINP = ${WW3CURINP}, but missing current file"
      echo "Abort!"
      exit 1
    fi
    $NLN $wavcurfile $DATA/current.${WAVECUR_FID}
  fi

  # Link output files
  cd $DATA
  if [ $waveMULTIGRID = ".true." ]; then
    $NLN $datwave/${wavprfx}.log.mww3.${PDY}${cyc} log.mww3
  fi 

  # Loop for gridded output (uses FHINC)
  fhr=$FHMIN_WAV
  while [ $fhr -le $FHMAX_WAV ]; do
    YMDH=$($NDATE $fhr $CDATE)
    YMD=$(echo $YMDH | cut -c1-8)
    HMS="$(echo $YMDH | cut -c9-10)0000"
    if [ $waveMULTIGRID = ".true." ]; then
      for wavGRD in ${waveGRD} ; do
        $NLN $datwave/${wavprfx}.out_grd.${wavGRD}.${YMD}.${HMS} $DATA/${YMD}.${HMS}.out_grd.${wavGRD}
      done
    else 
      $NLN $datwave/${wavprfx}.out_grd.${waveGRD}.${YMD}.${HMS} $DATA/${YMD}.${HMS}.out_grd.ww3
    fi 
    FHINC=$FHOUT_WAV
    if [ $FHMAX_HF_WAV -gt 0 -a $FHOUT_HF_WAV -gt 0 -a $fhr -lt $FHMAX_HF_WAV ]; then
      FHINC=$FHOUT_HF_WAV
    fi
    fhr=$((fhr+FHINC))
  done

  # Loop for point output (uses DTPNT)
  fhr=$FHMIN_WAV
  while [ $fhr -le $FHMAX_WAV ]; do
    YMDH=$($NDATE $fhr $CDATE)
    YMD=$(echo $YMDH | cut -c1-8)
    HMS="$(echo $YMDH | cut -c9-10)0000"
    if [ $waveMULTIGRID = ".true." ]; then
      $NLN $datwave/${wavprfx}.out_pnt.${waveuoutpGRD}.${YMD}.${HMS} $DATA/${YMD}.${HMS}.out_pnt.${waveuoutpGRD}
    else 
      $NLN $datwave/${wavprfx}.out_pnt.${waveuoutpGRD}.${YMD}.${HMS} $DATA/${YMD}.${HMS}.out_pnt.ww3
    fi 

    FHINC=$FHINCP_WAV
    fhr=$((fhr+FHINC))
  done
}

WW3_nml() {
  echo "SUB ${FUNCNAME[0]}: Copying input files for WW3"
  WAV_MOD_TAG=${CDUMP}wave${waveMEMB}
  if [ "${USE_WAV_RMP:-YES}" = "YES" ]; then
    if (( $( ls -1 $FIXwave/rmp_src_to_dst_conserv_* > /dev/null | wc -l) > 0 )); then
      for file in $(ls $FIXwave/rmp_src_to_dst_conserv_*) ; do
        $NLN $file $DATA/ 
      done
    else
      echo 'FATAL ERROR : No rmp precomputed nc files found for wave model' 
      exit 4 
    fi
  fi
  source $SCRIPTDIR/parsing_namelists_WW3.sh
  WW3_namelists
}

WW3_out() {
  echo "SUB ${FUNCNAME[0]}: Copying output data for WW3"
}


CPL_out() {
  echo "SUB ${FUNCNAME[0]}: Copying output data for general cpl fields"
  if [ $esmf_profile = ".true." ]; then
    $NCP $DATA/ESMF_Profile.summary $ROTDIR/$CDUMP.$PDY/$cyc/
  fi
}

MOM6_postdet() {
  echo "SUB ${FUNCNAME[0]}: MOM6 after run type determination"

  OCNRES=${OCNRES:-"025"}

  # Copy MOM6 ICs
  $NCP -pf $ICSDIR/$CDATE/ocn/MOM*nc $DATA/INPUT/

  # Copy MOM6 fixed files
  $NCP -pf $FIXmom/$OCNRES/* $DATA/INPUT/

  # Copy coupled grid_spec
  spec_file="$FIX_DIR/fix_cpl/a${CASE}o${OCNRES}/grid_spec.nc"
  if [ -s $spec_file ]; then
    $NCP -pf $spec_file $DATA/INPUT/
  else
    echo "FATAL ERROR: grid_spec file '$spec_file' does not exist"
    exit 3
  fi

  # Copy mediator restart files to RUNDIR
  if [ $warm_start = ".true." -o $RERUN = "YES" ]; then
    $NCP $ROTDIR/$CDUMP.$PDY/$cyc/med/ufs.cpld*.nc $DATA/
    $NCP $ROTDIR/$CDUMP.$PDY/$cyc/med/rpointer.cpl $DATA/
  fi

  if [ $DO_OCN_SPPT = "YES" -o $DO_OCN_PERT_EPBL = "YES" ]; then
    if [ ${SET_STP_SEED:-"YES"} = "YES" ]; then
      ISEED_OCNSPPT=$(( (CDATE*1000 + MEMBER*10 + 6) % 2147483647 ))
      ISEED_EPBL=$(( (CDATE*1000 + MEMBER*10 + 7) % 2147483647 ))
    else
      ISEED=${ISEED:-0}
    fi
  fi

  # Link output files

  export ENSMEM=${ENSMEM:-01}
  export IDATE=$CDATE

  [[ ! -d $COMOUTocean ]] && mkdir -p $COMOUTocean

  fhrlst=$OUTPUT_FH

  for fhr in $fhrlst; do
    if [ $fhr = 'anl' ]; then
      continue
    fi
    if [ -z ${last_fhr:-} ]; then
      last_fhr=$fhr
      continue
    fi
    (( interval = fhr - last_fhr ))
    (( midpoint = last_fhr + interval/2 ))
    VDATE=$($NDATE $fhr $IDATE)
    YYYY=$(echo $VDATE | cut -c1-4)
    MM=$(echo $VDATE | cut -c5-6)
    DD=$(echo $VDATE | cut -c7-8)
    HH=$(echo $VDATE | cut -c9-10)
    SS=$((10#$HH*3600))

    VDATE_MID=$($NDATE $midpoint $IDATE)
    YYYY_MID=$(echo $VDATE_MID | cut -c1-4)
    MM_MID=$(echo $VDATE_MID | cut -c5-6)
    DD_MID=$(echo $VDATE_MID | cut -c7-8)
    HH_MID=$(echo $VDATE_MID | cut -c9-10)
    SS_MID=$((10#$HH_MID*3600))

    source_file="ocn_${YYYY_MID}_${MM_MID}_${DD_MID}_${HH_MID}.nc"
    dest_file="ocn${VDATE}.${ENSMEM}.${IDATE}.nc"
    ${NLN} ${COMOUTocean}/${dest_file} ${DATA}/${source_file}

    source_file="wavocn_${YYYY_MID}_${MM_MID}_${DD_MID}_${HH_MID}.nc"
    dest_file=${source_file}
    ${NLN} ${COMOUTocean}/${dest_file} ${DATA}/${source_file}

    source_file="ocn_daily_${YYYY}_${MM}_${DD}.nc"
    dest_file=${source_file}
    if [ ! -a "${DATA}/${source_file}" ]; then
      $NLN ${COMOUTocean}/${dest_file} ${DATA}/${source_file}
    fi

    last_fhr=$fhr
  done
  $NLN ${COMOUTocean}/MOM_input $DATA/INPUT/MOM_input

  echo "SUB ${FUNCNAME[0]}: MOM6 input data linked/copied"

}

MOM6_nml() {
  echo "SUB ${FUNCNAME[0]}: Creating name list for MOM6"
  source $SCRIPTDIR/parsing_namelists_MOM6.sh
  MOM6_namelists
}

MOM6_out() {
  echo "SUB ${FUNCNAME[0]}: Copying output data for MOM6"
}

CICE_postdet() {
  echo "SUB ${FUNCNAME[0]}: CICE after run type determination"

  year=$(echo $CDATE|cut -c 1-4)
  month=$(echo $CDATE|cut -c 5-6)
  day=$(echo $CDATE|cut -c 7-8)
  sec=$(echo $CDATE|cut -c 9-10)  
  stepsperhr=$((3600/$ICETIM))
  nhours=$($NHOUR $CDATE ${year}010100)
  steps=$((nhours*stepsperhr))
  npt=$((FHMAX*$stepsperhr))      # Need this in order for dump_last to work

  histfreq_n=${histfreq_n:-6}
  dumpfreq_n=${dumpfreq_n:-840}  # restart write interval in seconds, default 35 days
  dumpfreq=${dumpfreq:-"h"} #  "h","d","m" or "y" for restarts at intervals of "hours", "days", "months" or "years"
  cice_hist_avg=${cice_hist_avg:-".true."}

  FRAZIL_FWSALT=${FRAZIL_FWSALT:-".true."}
  ktherm=${ktherm:-2}
  tfrz_option=${tfrz_option:-"'mushy'"}
  tr_pond_lvl=${tr_pond_lvl:-".true."} # Use level melt ponds tr_pond_lvl=true

  # restart_pond_lvl (if tr_pond_lvl=true):
  #   -- if true, initialize the level ponds from restart (if runtype=continue)
  #   -- if false, re-initialize level ponds to zero (if runtype=initial or continue)

  #TODO: Determine the proper way to determine if it's a 'hot start' or not
  #note this is not mediator cold start or not
  #if [ hotstart ]; then
  #  #continuing run "hot start"
  #  RUNTYPE='continue'
  #  USE_RESTART_TIME='.true.'
  #fi
  RUNTYPE='initial'
  USE_RESTART_TIME='.false.'
  restart_pond_lvl=${restart_pond_lvl:-".false."}

  ICERES=${ICERES:-"025"}
  if [ $ICERES = '025' ]; then
    ICERESdec="0.25"
  fi
  if [ $ICERES = '050' ]; then
    ICERESdec="0.50"
  fi
  if [ $ICERES = '100' ]; then
    ICERESdec="1.00"
  fi

  ice_grid_file=${ice_grid_file:-"grid_cice_NEMS_mx${ICERES}.nc"}
  ice_kmt_file=${ice_kmt_file:-"kmtu_cice_NEMS_mx${ICERES}.nc"}
  export MESH_OCN_ICE=${MESH_OCN_ICE:-"mesh.mx${ICERES}.nc"}

  iceic="cice_model.res_$CDATE.nc"

  # Copy CICE IC
  $NCP -p $ICSDIR/$CDATE/ice/cice_model_${ICERESdec}.res_$CDATE.nc $DATA/$iceic

  echo "Link CICE fixed files"
  $NLN -sf $FIXcice/$ICERES/${ice_grid_file} $DATA/
  $NLN -sf $FIXcice/$ICERES/${ice_kmt_file} $DATA/
  $NLN -sf $FIXcice/$ICERES/$MESH_OCN_ICE $DATA/

  # Link output files
  export ENSMEM=${ENSMEM:-01}
  export IDATE=$CDATE
  [[ ! -d $COMOUTice ]] && mkdir -p $COMOUTice
  $NLN $COMOUTice/ice_in $DATA/ice_in
  fhrlst=$OUTPUT_FH

  for fhr in $fhrlst; do
    if [ $fhr = 'anl' ]; then
      continue
    fi
    VDATE=$($NDATE $fhr $IDATE)
    YYYY=$(echo $VDATE | cut -c1-4)
    MM=$(echo $VDATE | cut -c5-6)
    DD=$(echo $VDATE | cut -c7-8)
    HH=$(echo $VDATE | cut -c9-10)
    SS=$((10#$HH*3600))

    if [[ 10#$fhr -eq 0 ]]; then
      $NLN $COMOUTice/iceic$VDATE.$ENSMEM.$IDATE.nc $DATA/history/iceh_ic.${YYYY}-${MM}-${DD}-$(printf "%5.5d" ${SS}).nc
    else
      (( interval = fhr - last_fhr ))
      $NLN $COMOUTice/ice$VDATE.$ENSMEM.$IDATE.nc $DATA/history/iceh_$(printf "%0.2d" $interval)h.${YYYY}-${MM}-${DD}-$(printf "%5.5d" ${SS}).nc
    fi
    last_fhr=$fhr
  done
}

CICE_nml() {
  echo "SUB ${FUNCNAME[0]}: Creating name list for CICE"
  source $SCRIPTDIR/parsing_namelists_CICE.sh
  CICE_namelists
}

CICE_out() {
  echo "SUB ${FUNCNAME[0]}: Copying output data for CICE"
}

GOCART_rc() {
  echo "SUB ${FUNCNAME[0]}: Linking input data and copying config files for GOCART"
  # set input directory containing GOCART input data and configuration files
  # this variable is platform-dependent and should be set via a YAML file

  # link directory containing GOCART input dataset, if provided
  if [ ! -z "${AERO_INPUTS_DIR}" ]; then
    $NLN -sf ${AERO_INPUTS_DIR} $DATA/ExtData
    status=$?
    [[ $status -ne 0 ]] && exit $status
  fi

  # copying GOCART configuration files
  if [ ! -z "${AERO_CONFIG_DIR}" ]; then
    $NCP     ${AERO_CONFIG_DIR}/*.rc $DATA
    status=$?
    [[ $status -ne 0 ]] && exit $status
    # attempt to generate ExtData configuration file if not provided
    if [ ! -f $DATA/AERO_ExtData.rc ]; then
      { \
        echo "PrimaryExports%%" ; \
        cat ${AERO_CONFIG_DIR}/ExtData.other ; \
        cat ${AERO_CONFIG_DIR}/ExtData.${AERO_EMIS_FIRE:-none} ; \
        echo "%%" ; \
      } > $DATA/AERO_ExtData.rc
      status=$?
      if (( status != 0 )); then exit $status; fi
    fi
  fi
}

GOCART_postdet() {
  echo "SUB ${FUNCNAME[0]}: Linking output data for GOCART"

  [[ ! -d $COMOUTaero ]] && mkdir -p $COMOUTaero

  fhrlst=$OUTPUT_FH
  for fhr in $fhrlst; do
    if [ $fhr = 'anl' ]; then
      continue
    fi
    VDATE=$($NDATE $fhr $CDATE)
    YYYY=$(echo $VDATE | cut -c1-4)
    MM=$(echo $VDATE | cut -c5-6)
    DD=$(echo $VDATE | cut -c7-8)
    HH=$(echo $VDATE | cut -c9-10)
    SS=$((10#$HH*3600))

    $NLN $COMOUTaero/gocart.inst_aod.${YYYY}${MM}${DD}_${HH}00z.nc4 $DATA/gocart.inst_aod.${YYYY}${MM}${DD}_${HH}00z.nc4
  done
}
