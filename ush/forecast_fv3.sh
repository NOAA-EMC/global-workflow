#! /usr/bin/env bash

#===============================================================================
#
#   FILE: forecast_fv3.sh
#
#   DESCRIPTION: Functions for the FV3 atmosphere component of the UFS.
#                Defines variables and stages fix files before run type
#                determination (FV3_predet), checks restart availability (FV3_det),
#                stages initial conditions and links output after run type
#                determination (FV3_postdet), writes namelists (FV3_nml), and copies
#                output to COM (FV3_out).

FV3_coldstarts() {
    # Function to return an comma-separated string of cold-start input files for FV3
    # Create an array of chgres-ed FV3 files
    local fv3_input_files tile_files
    fv3_input_files=(gfs_ctrl.nc)
    tile_files=(gfs_data sfc_data)
    local nn tt
    for ((nn = 1; nn <= ntiles; nn++)); do
        for tt in "${tile_files[@]}"; do
            fv3_input_files+=("${tt}.tile${nn}.nc")
        done
    done
    # Create a comma separated string from array using IFS
    IFS=, echo "${fv3_input_files[*]}"
}

FV3_restarts() {
    # Function to return an comma-separated string of warm-start input files for FV3
    # Create an array of FV3 restart files
    local fv3_restart_files tile_files
    fv3_restart_files=(coupler.res fv_core.res.nc)
    tile_files=(fv_core.res fv_srf_wnd.res fv_tracer.res phy_data sfc_data)
    if [[ ${DO_CA:-"NO"} == "YES" ]]; then
        tile_files+=(ca_data)
    fi
    local nn tt
    for ((nn = 1; nn <= ntiles; nn++)); do
        for tt in "${tile_files[@]}"; do
            fv3_restart_files+=("${tt}.tile${nn}.nc")
        done
    done
    # Create a comma separated string from array using IFS
    IFS=, echo "${fv3_restart_files[*]}"
}

stoch_restarts() {
    # These only get copied for reruns
    local stoch_restart_files
    stoch_restart_files=()

    if [[ "${DO_SPPT:-}" == "YES" || "${DO_SKEB:-}" == "YES" ||
        "${DO_SHUM:-}" == "YES" || "${DO_LAND_PERT:-}" == "YES" ]]; then
        stoch_restart_files+=(atm_stoch.res.nc)
    fi
    if [[ "${DO_OCN:-}" == "YES" && ("${DO_OCN_SPPT:-}" == "YES" || "${DO_OCN_PERT_EPBL}" == "YES") ]]; then
        stoch_restart_files+=(ocn_stoch.res.nc)
    fi
    # Create a comma separated string from array using IFS
    IFS=, echo "${stoch_restart_files[*]}"
}

# shellcheck disable=SC2034
FV3_predet() {
    echo "SUB ${FUNCNAME[0]}: Defining variables for FV3"

    if [[ ! -d "${COMOUT_ATMOS_HISTORY}" ]]; then mkdir -p "${COMOUT_ATMOS_HISTORY}"; fi
    if [[ ! -d "${DATAoutput}/FV3ATM_OUTPUT" ]]; then mkdir -p "${DATAoutput}/FV3ATM_OUTPUT"; fi
    if [[ ! -d "${DATArestart}/FV3_RESTART" ]]; then mkdir -p "${DATArestart}/FV3_RESTART"; fi
    if [[ "${WRITE_DOPOST:-}" == ".true." ]] && [[ ! -d "${COMOUT_ATMOS_MASTER}" ]]; then
        mkdir -p "${COMOUT_ATMOS_MASTER}"
    fi

    # The $DATA/RESTART directory is used for writing FV3 restart files (it is hard-wired in the model)
    # Link the output and restart directories to the DATA directory
    ${NLN} "${DATAoutput}/FV3ATM_OUTPUT" "${DATA}/FV3ATM_OUTPUT"
    ${NLN} "${DATArestart}/FV3_RESTART" "${DATA}/RESTART"

    FHZERO=${FHZERO:-6}
    FHCYC=${FHCYC:-24}

    # Convert output settings into an explicit list for FV3
    # Create an FV3 fhr list to be used in the filenames
    FV3_OUTPUT_FH=""
    local fhr=${FHMIN}
    if [[ "${FHOUT_HF}" -gt 0 && "${FHMAX_HF}" -gt 0 ]]; then
        FV3_OUTPUT_FH="${FV3_OUTPUT_FH} $(seq -s ' ' "${FHMIN}" "${FHOUT_HF}" "${FHMAX_HF}")"
        fhr=${FHMAX_HF}
    fi
    FV3_OUTPUT_FH="${FV3_OUTPUT_FH} $(seq -s ' ' "${fhr}" "${FHOUT}" "${FHMAX}")"

    if [[ "${FHOUT_HF}" -eq "${FHOUT}" ]]; then
        FV3_OUTPUT_FH_NML="${FHOUT} -1"
    else
        FV3_OUTPUT_FH_NML="${FV3_OUTPUT_FH}"
    fi

    # IAU options
    IAUFHRS=${IAUFHRS:-0}
    IAU_DELTHRS=${IAU_DELTHRS:-0}

    #------------------------------------------------------------------
    # changeable parameters
    # dycore definitions
    res="${CASE:1}"
    resh="${CASE_HIST:1}"
    resp=$((res + 1))
    npx=${resp}
    npy=${resp}
    npz=$((LEVS - 1))
    io_layout="1,1"
    #ncols=$(( (${npx}-1)*(${npy}-1)*3/2 ))

    # spectral truncation and regular grid resolution based on FV3 resolution
    JCAP_CASE=$((2 * res - 2))
    LONB_CASE=$((4 * res))
    LATB_CASE=$((2 * res))

    JCAP_CASE_HIST=$((2 * resh - 2))
    LONB_CASE_HIST=$((4 * resh))
    LATB_CASE_HIST=$((2 * resh))

    JCAP=${JCAP:-${JCAP_CASE}}
    LONB=${LONB:-${LONB_CASE}}
    LATB=${LATB:-${LATB_CASE}}

    LONB_IMO=${LONB_IMO:-${LONB_CASE_HIST}}
    LATB_JMO=${LATB_JMO:-${LATB_CASE_HIST}}

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
    #if [[ $nxblocks -le 0 ]]; then nxblocks=1 ; fi
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

    local suite_file="${HOMEglobal}/parm/ufs/suite_${CCPP_SUITE}.xml"
    if [[ ! -f "${suite_file}" ]]; then
        echo "FATAL ERROR: CCPP Suite file ${suite_file} does not exist, ABORT!"
        exit 2
    fi

    # Scan suite file to determine whether it uses Noah-MP
    local num_noahmpdrv
    num_noahmpdrv=$(grep -c noahmpdrv "${suite_file}")
    if [[ "${num_noahmpdrv}" -gt 0 ]]; then
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

    if [[ "${TYPE}" == "nh" ]]; then # non-hydrostatic options
        hydrostatic=".false."
        phys_hydrostatic=".false."   # enable heating in hydrostatic balance in non-hydrostatic simulation
        use_hydro_pressure=".false." # use hydrostatic pressure for physics
        make_nh=".true."             # running in non-hydrostatic mode
        pass_full_omega_to_physics_in_non_hydrostatic_mode=".true."
    else # hydrostatic options
        hydrostatic=".true."
        phys_hydrostatic=".false."   # ignored when hydrostatic = T
        use_hydro_pressure=".false." # ignored when hydrostatic = T
        make_nh=".false."            # running in hydrostatic mode
    fi

    # Conserve total energy as heat globally
    consv_te=${consv_te:-1.} # range 0.-1., 1. will restore energy to orig. val. before physics
    if [[ "${DO_NEST:-NO}" == "YES" ]]; then
        consv_te=0
        k_split=${k_split:-1}
        k_split_nest=${k_split_nest:-4}
    else
        consv_te=${consv_te:-1.} # range 0.-1., 1. will restore energy to orig. val. before physics
        k_split=${k_split:-2}
    fi

    # time step parameters in FV3
    n_split=${n_split:-5}

    if [[ "${MONO:0:4}" == "mono" ]]; then # monotonic options
        d_con=${d_con_mono:-"0."}
        do_vort_damp=".false."
        if [[ "${TYPE}" == "nh" ]]; then # monotonic and non-hydrostatic
            hord_mt=${hord_mt_nh_mono:-"10"}
            hord_xx=${hord_xx_nh_mono:-"10"}
            hord_dp=${hord_xx_nh_mono:-"10"}
        else # monotonic and hydrostatic
            hord_mt=${hord_mt_hydro_mono:-"10"}
            hord_xx=${hord_xx_hydro_mono:-"10"}
            hord_dp=${hord_xx_hydro_mono:-"10"}
            kord_tm=${kord_tm_hydro_mono:-"-12"}
            kord_mt=${kord_mt_hydro_mono:-"12"}
            kord_wz=${kord_wz_hydro_mono:-"12"}
            kord_tr=${kord_tr_hydro_mono:-"12"}
        fi
    else # non-monotonic options
        d_con=${d_con_nonmono:-"1."}
        do_vort_damp=".true."
        if [[ "${TYPE}" == "nh" ]]; then # non-monotonic and non-hydrostatic
            hord_mt=${hord_mt_nh_nonmono:-"5"}
            hord_xx=${hord_xx_nh_nonmono:-"5"}
            hord_dp=${hord_dp_nh_nonmono:-"-5"}
        else # non-monotonic and hydrostatic
            hord_mt=${hord_mt_hydro_nonmono:-"10"}
            hord_xx=${hord_xx_hydro_nonmono:-"10"}
            hord_dp=${hord_xx_hydro_nonmono:-"10"}
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
    increment_file_on_native_grid=".false."

    # Stochastic Physics Options
    do_skeb=".false."
    do_shum=".false."
    do_sppt=".false."
    do_ca=".false."
    ISEED=0
    local imem=${MEMBER#0}
    local base_seed=$((current_cycle * 10000 + imem * 100))

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
        ISEED_CA=$(((base_seed + 18) % 2147483647))
    fi

    if [[ "${DO_LAND_PERT:-}" == "YES" ]]; then
        lndp_type=${lndp_type:-2}
        ISEED_LNDP=$(((base_seed + 5) % 2147483647))
        LNDP_TAU=${LNDP_TAU:-21600}
        LNDP_SCALE=${LNDP_SCALE:-500000}
        lndp_var_list=${lndp_var_list:-"'smc', 'vgf',"}
        lndp_prt_list=${lndp_prt_list:-"0.2,0.1"}
        n_var_lndp=$(echo "${lndp_var_list}" | wc -w)
    fi

    #--------------------------------------------------------------------------

    # Fix files
    FNGLAC=${FNGLAC:-"${FIXglobal}/am/global_glacier.2x2.grb"}
    FNMXIC=${FNMXIC:-"${FIXglobal}/am/global_maxice.2x2.grb"}
    FNTSFC=${FNTSFC:-"${FIXglobal}/am/RTGSST.1982.2012.monthly.clim.grb"}
    FNSNOC=${FNSNOC:-"${FIXglobal}/am/global_snoclim.1.875.grb"}
    FNZORC=${FNZORC:-"igbp"}
    FNAISC=${FNAISC:-"${FIXglobal}/am/IMS-NIC.blended.ice.monthly.clim.grb"}
    FNSMCC=${FNSMCC:-"${FIXglobal}/am/global_soilmgldas.statsgo.t${JCAP}.${LONB}.${LATB}.grb"}
    FNMSKH=${FNMSKH:-"${FIXglobal}/am/global_slmask.t1534.3072.1536.grb"}
    if [[ "${RUN}" == "sfs" ]]; then
        FNALBC2=${FNALBC2:-"${FIXorog}/${CASE}_gwd_hr3/sfc/${CASE}.mx${OCNRES}.facsf.tileX.nc"}
        FNTG3C=${FNTG3C:-"${FIXorog}/${CASE}_gwd_hr3/sfc/${CASE}.mx${OCNRES}.substrate_temperature.tileX.nc"}
        FNVEGC=${FNVEGC:-"${FIXorog}/${CASE}_gwd_hr3/sfc/${CASE}.mx${OCNRES}.vegetation_greenness.tileX.nc"}
        FNVMNC=${FNVMNC:-"${FIXorog}/${CASE}_gwd_hr3/sfc/${CASE}.mx${OCNRES}.vegetation_greenness.tileX.nc"}
        FNVMXC=${FNVMXC:-"${FIXorog}/${CASE}_gwd_hr3/sfc/${CASE}.mx${OCNRES}.vegetation_greenness.tileX.nc"}
        FNSLPC=${FNSLPC:-"${FIXorog}/${CASE}_gwd_hr3/sfc/${CASE}.mx${OCNRES}.slope_type.tileX.nc"}
        FNALBC=${FNALBC:-"${FIXorog}/${CASE}_gwd_hr3/sfc/${CASE}.mx${OCNRES}.snowfree_albedo.tileX.nc"}
        FNVETC=${FNVETC:-"${FIXorog}/${CASE}_gwd_hr3/sfc/${CASE}.mx${OCNRES}.vegetation_type.tileX.nc"}
        FNSOTC=${FNSOTC:-"${FIXorog}/${CASE}_gwd_hr3/sfc/${CASE}.mx${OCNRES}.soil_type.tileX.nc"}
        FNSOCC=${FNSOCC:-"${FIXorog}/${CASE}_gwd_hr3/sfc/${CASE}.mx${OCNRES}.soil_color.tileX.nc"}
        FNABSC=${FNABSC:-"${FIXorog}/${CASE}_gwd_hr3/sfc/${CASE}.mx${OCNRES}.maximum_snow_albedo.tileX.nc"}
    else
        FNALBC2=${FNALBC2:-"${FIXorog}/${CASE}/sfc/${CASE}.mx${OCNRES}.facsf.tileX.nc"}
        FNTG3C=${FNTG3C:-"${FIXorog}/${CASE}/sfc/${CASE}.mx${OCNRES}.substrate_temperature.tileX.nc"}
        FNVEGC=${FNVEGC:-"${FIXorog}/${CASE}/sfc/${CASE}.mx${OCNRES}.vegetation_greenness.tileX.nc"}
        FNVMNC=${FNVMNC:-"${FIXorog}/${CASE}/sfc/${CASE}.mx${OCNRES}.vegetation_greenness.tileX.nc"}
        FNVMXC=${FNVMXC:-"${FIXorog}/${CASE}/sfc/${CASE}.mx${OCNRES}.vegetation_greenness.tileX.nc"}
        FNSLPC=${FNSLPC:-"${FIXorog}/${CASE}/sfc/${CASE}.mx${OCNRES}.slope_type.tileX.nc"}
        FNALBC=${FNALBC:-"${FIXorog}/${CASE}/sfc/${CASE}.mx${OCNRES}.snowfree_albedo.tileX.nc"}
        FNVETC=${FNVETC:-"${FIXorog}/${CASE}/sfc/${CASE}.mx${OCNRES}.vegetation_type.tileX.nc"}
        FNSOTC=${FNSOTC:-"${FIXorog}/${CASE}/sfc/${CASE}.mx${OCNRES}.soil_type.tileX.nc"}
        FNSOCC=${FNSOCC:-"${FIXorog}/${CASE}/sfc/${CASE}.mx${OCNRES}.soil_color.tileX.nc"}
        FNABSC=${FNABSC:-"${FIXorog}/${CASE}/sfc/${CASE}.mx${OCNRES}.maximum_snow_albedo.tileX.nc"}
    fi

    # If the appropriate resolution fix file is not present, use the highest resolution available (T1534)
    if [[ ! -f "${FNSMCC}" ]]; then
        FNSMCC="${FIXglobal}/am/global_soilmgldas.statsgo.t1534.3072.1536.grb"
    fi

    # Grid and orography data
    if [[ "${RUN}" == "sfs" ]]; then
        if [[ "${cplflx}" == ".false." ]]; then
            cpreq "${FIXorog}/${CASE}_gwd_hr3/${CASE}_mosaic.nc" "${DATA}/INPUT/grid_spec.nc"
        else
            cpreq "${FIXorog}/${CASE}_gwd_hr3/${CASE}_mosaic.nc" "${DATA}/INPUT/${CASE}_mosaic.nc"
        fi
    else
        if [[ "${cplflx}" == ".false." ]]; then
            cpreq "${FIXorog}/${CASE}/${CASE}_mosaic.nc" "${DATA}/INPUT/grid_spec.nc"
        else
            cpreq "${FIXorog}/${CASE}/${CASE}_mosaic.nc" "${DATA}/INPUT/${CASE}_mosaic.nc"
        fi
    fi

    # Files for GWD
    cpreq "${FIXugwd}/ugwp_limb_tau.nc" "${DATA}/ugwp_limb_tau.nc"

    # Files for orography, GWD tiles
    local tt
    if [[ "${RUN}" == "sfs" ]]; then
        for ((tt = 1; tt <= ntiles; tt++)); do
            cpreq "${FIXorog}/${CASE}_gwd_hr3/${CASE}.mx${OCNRES}_oro_data.tile${tt}.nc" "${DATA}/INPUT/oro_data.tile${tt}.nc"
            cpreq "${FIXorog}/${CASE}_gwd_hr3/${CASE}_grid.tile${tt}.nc" "${DATA}/INPUT/${CASE}_grid.tile${tt}.nc"
            cpreq "${FIXugwd}/${CASE}/${CASE}_oro_data_ls.tile${tt}.nc" "${DATA}/INPUT/oro_data_ls.tile${tt}.nc"
            cpreq "${FIXugwd}/${CASE}/${CASE}_oro_data_ss.tile${tt}.nc" "${DATA}/INPUT/oro_data_ss.tile${tt}.nc"
        done
    else
        for ((tt = 1; tt <= ntiles; tt++)); do
            cpreq "${FIXorog}/${CASE}/${CASE}.mx${OCNRES}_oro_data.tile${tt}.nc" "${DATA}/INPUT/oro_data.tile${tt}.nc"
            cpreq "${FIXorog}/${CASE}/${CASE}_grid.tile${tt}.nc" "${DATA}/INPUT/${CASE}_grid.tile${tt}.nc"
            cpreq "${FIXugwd}/${CASE}/${CASE}_oro_data_ls.tile${tt}.nc" "${DATA}/INPUT/oro_data_ls.tile${tt}.nc"
            cpreq "${FIXugwd}/${CASE}/${CASE}_oro_data_ss.tile${tt}.nc" "${DATA}/INPUT/oro_data_ss.tile${tt}.nc"
        done
    fi

    if [[ "${DO_NEST:-NO}" == "YES" ]]; then
        ${NLN} "${DATA}/INPUT/oro_data.tile7.nc" "${DATA}/INPUT/oro_data.nest02.tile7.nc"
        ${NLN} "${DATA}/INPUT/${CASE}_grid.tile7.nc" "${DATA}/INPUT/${CASE}_grid.nest02.tile7.nc"
        ${NLN} "${DATA}/INPUT/${CASE}_grid.tile7.nc" "${DATA}/INPUT/grid.nest02.tile7.nc"
        ${NLN} "${DATA}/INPUT/oro_data_ls.tile7.nc" "${DATA}/INPUT/oro_data_ls.nest02.tile7.nc"
        ${NLN} "${DATA}/INPUT/oro_data_ss.tile7.nc" "${DATA}/INPUT/oro_data_ss.nest02.tile7.nc"
    fi

    # NoahMP table
    local noahmptablefile="${PARMglobal}/ufs/noahmptable.tbl"
    cpreq "${noahmptablefile}" "${DATA}/noahmptable.tbl"

    #  Thompson microphysics fix files
    if ((imp_physics == 8)); then
        cpreq "${FIXglobal}/am/CCN_ACTIVATE.BIN" "${DATA}/CCN_ACTIVATE.BIN"
        cpreq "${FIXglobal}/am/freezeH2O.dat" "${DATA}/freezeH2O.dat"
        cpreq "${FIXglobal}/am/qr_acr_qgV2.dat" "${DATA}/qr_acr_qgV2.dat"
        cpreq "${FIXglobal}/am/qr_acr_qsV2.dat" "${DATA}/qr_acr_qsV2.dat"
    fi

    if [[ "${new_o3forc:-YES}" == "YES" ]]; then
        if [[ "${o3forc_params:-McCormack}" == "McCormack-empirical-sh-ozh" ]]; then
            O3FORC="ozprdlos_2015_new_sbuvO3_tclm15_nuchem_shozhvlogp.f77"
        else
            O3FORC="ozprdlos_2015_new_sbuvO3_tclm15_nuchem.f77"
        fi
    else
        O3FORC="global_o3prdlos.f77"
    fi
    H2OFORC=${H2OFORC:-"global_h2o_pltc.f77"}
    cpreq "${FIXglobal}/am/${O3FORC}" "${DATA}/global_o3prdlos.f77"
    cpreq "${FIXglobal}/am/${H2OFORC}" "${DATA}/global_h2oprdlos.f77"

    # GFS standard input data

    ISOL=${ISOL:-2}

    cpreq "${FIXglobal}/am/global_solarconstant_noaa_an.txt" "${DATA}/solarconstant_noaa_an.txt"
    cpreq "${FIXglobal}/am/global_sfc_emissivity_idx.txt" "${DATA}/sfc_emissivity_idx.txt"

    # Aerosol options
    IAER=${IAER:-1011}
    MERRA2_6ym=${MERRA2_6ym:-".false."}
    ## merra2 aerosol climo
    if ((IAER == 1011)); then
        if [[ "${MERRA2_6ym}" == ".false." ]]; then
            #   local month mm
            for ((month = 1; month <= 12; month++)); do
                mm=$(printf %02d "${month}")
                cpreq "${FIXglobal}/aer/merra2.aerclim.2014-2023.m${mm}.nc" "aeroclim.m${mm}.nc"
            done
        elif [[ "${MERRA2_6ym}" == ".true." ]]; then
            year=${current_cycle:0:4}
            for i in {1980..2300..5}; do
                if [[ ${year} -le ${i} ]]; then
                    Eyear=$(printf %04d "${i}")
                    Syear=$((i - 5))
                    break
                fi
            done
            if ((Syear >= 2025)); then
                Eyear=$((Eyear - 5))
                Syear=$((Syear - 5))
            fi
            for ((month = 1; month <= 12; month++)); do
                mm=$(printf %02d "${month}")
                cpreq "${FIXglobal}/aer/y${Syear}-${Eyear}/merra2_${Syear}-${Eyear}_${mm}.nc" "aeroclim.m${mm}.nc"
            done
        fi # if [[ "${MERRA2_6ym}" == ".true." ]];
    fi     # if (( IAER == 1011 ))

    cpreq "${FIXglobal}/am/global_climaeropac_global.txt" "${DATA}/aerosol.dat"
    if ((IAER > 0)); then
        local file
        for file in "${FIXglobal}/am/global_volcanic_aerosols"*; do
            cpreq "${file}" "${DATA}/$(basename "${file//global_/}")"
        done
    fi

    cpreq "${FIXglobal}/lut/optics_BC.v1_3.dat" "${DATA}/optics_BC.dat"
    cpreq "${FIXglobal}/lut/optics_OC.v1_3.dat" "${DATA}/optics_OC.dat"
    cpreq "${FIXglobal}/lut/optics_DU.v15_3.dat" "${DATA}/optics_DU.dat"
    cpreq "${FIXglobal}/lut/optics_SS.v3_3.dat" "${DATA}/optics_SS.dat"
    cpreq "${FIXglobal}/lut/optics_SU.v1_3.dat" "${DATA}/optics_SU.dat"

    # CO2 options
    ICO2=${ICO2:-2}

    cpreq "${FIXglobal}/am/global_co2historicaldata_glob.txt" "${DATA}/co2historicaldata_glob.txt"
    cpreq "${FIXglobal}/am/co2monthlycyc.txt" "${DATA}/co2monthlycyc.txt"
    # Set historical CO2 values based on whether this is a reforecast run or not
    # Ref. issue 2403
    local co2dir
    co2dir="fix_co2_proj"
    if [[ "${reforecast:-}" == "YES" ]]; then
        co2dir="co2dat_4a"
    fi
    if ((ICO2 > 0)); then
        local file
        for file in "${FIXglobal}/am/${co2dir}/global_co2historicaldata"*; do
            cpreq "${file}" "${DATA}/$(basename "${file//global_/}")"
        done
    fi

    # Inline UPP fix files
    if [[ "${WRITE_DOPOST:-}" == ".true." ]]; then
        cpreq "${POSTGRB2TBL:-${PARMglobal}/post/params_grib2_tbl_new}" "${DATA}/params_grib2_tbl_new"
        cpreq "${PARMglobal}/ufs/post_itag_gfs" "${DATA}/itag" # TODO: Need a GEFS version when available in the UFS-weather-model
        # TODO: These should be replaced with ones from the ufs-weather-model when available there
        case ${NET} in
            gfs)
                cpreq "${PARMglobal}/post/gfs/postxconfig-NT-gfs-two.txt" "${DATA}/postxconfig-NT.txt"
                cpreq "${PARMglobal}/post/gfs/postxconfig-NT-gfs-f00-two.txt" "${DATA}/postxconfig-NT_FH00.txt"
                ;;
            gcafs)
                cpreq "${PARMglobal}/post/gcafs/postxconfig-NT-gcafs.txt" "${DATA}/postxconfig-NT.txt"
                cpreq "${PARMglobal}/post/gcafs/postxconfig-NT-gcafs.txt" "${DATA}/postxconfig-NT_FH00.txt"
                cpreq "${PARMglobal}/ufs/post_itag_gcafs" "${DATA}/itag"
                cpreq "${PARMglobal}/post/optics_luts_DUST_nasa.dat" "${DATA}/."
                cpreq "${PARMglobal}/post/optics_luts_NITR_nasa.dat" "${DATA}/."
                cpreq "${PARMglobal}/post/optics_luts_SALT_nasa.dat" "${DATA}/."
                cpreq "${PARMglobal}/post/optics_luts_SOOT_nasa.dat" "${DATA}/."
                cpreq "${PARMglobal}/post/optics_luts_SUSO_nasa.dat" "${DATA}/."
                cpreq "${PARMglobal}/post/optics_luts_WASO_nasa.dat" "${DATA}/."
                ;;
            gefs)
                cpreq "${PARMglobal}/post/gefs/postxconfig-NT-gefs.txt" "${DATA}/postxconfig-NT.txt"
                cpreq "${PARMglobal}/post/gefs/postxconfig-NT-gefs-f00.txt" "${DATA}/postxconfig-NT_FH00.txt"
                # Provide ensemble header information for GEFS
                if [[ "${ENSMEM}" == "000" ]]; then
                    export e1=1
                else
                    export e1=3
                fi
                export e2="${ENSMEM:1:2}"
                export e3="${NMEM_ENS}"
                ;;
            sfs)
                cpreq "${PARMglobal}/post/sfs/postxconfig-NT-sfs.txt" "${DATA}/postxconfig-NT.txt"
                cpreq "${PARMglobal}/post/sfs/postxconfig-NT-sfs.txt" "${DATA}/postxconfig-NT_FH00.txt"
                # Provide ensemble header information for SFS
                if [[ "${ENSMEM}" == "000" ]]; then
                    export e1=1
                else
                    export e1=3
                fi
                export e2="${ENSMEM:1:2}"
                export e3="${NMEM_ENS}"
                ;;
            *)
                echo "FATAL ERROR: Unknown NET ${NET}, unable to determine appropriate post files"
                exit 20
                ;;
        esac
    fi
}

FV3_det() {
    # Check for FV3 restart availability at the date given by $1 (YYYYMMDDHH)
    # Returns 0 if the restarts are available, 1 otherwise
    local rdate=${1:?}
    # TODO: add checks for other FV3 restarts as well
    if [[ ! -f "${DATArestart}/FV3_RESTART/${rdate:0:8}.${rdate:8:2}0000.coupler.res" ]]; then
        return 1
    fi
    return 0
}

# Disable variable not used warnings
# shellcheck disable=SC2034
# shellcheck disable=SC2178
FV3_postdet() {
    echo "SUB ${FUNCNAME[0]}: Entering for RUN = ${RUN}"

    echo "warm_start = ${warm_start}"
    echo "RERUN = ${RERUN}"

    #============================================================================
    # First copy initial conditions
    # cold start case
    if [[ "${warm_start}" == ".false." ]]; then

        # Get list of FV3 cold start files
        local file_list
        file_list=$(FV3_coldstarts)
        echo "Copying FV3 cold start files for 'RUN=${RUN}' at '${current_cycle}' from '${COMIN_ATMOS_INPUT}'"
        local fv3_file
        for fv3_file in ${file_list}; do
            cpreq "${COMIN_ATMOS_INPUT}/${fv3_file}" "${DATA}/INPUT/${fv3_file}"
        done

    # warm start case
    elif [[ "${warm_start}" == ".true." ]]; then

        # Determine restart date and directory containing restarts
        local restart_date restart_dir
        if [[ "${RERUN}" == "YES" ]]; then
            restart_date="${RERUN_DATE}"
            restart_dir="${DATArestart}/FV3_RESTART"
        else # "${RERUN}" == "NO"
            restart_date="${model_start_date_current_cycle}"
            restart_dir="${COMIN_ATMOS_RESTART_PREV}"
        fi

        # Get list of FV3 restart files
        local file_list
        file_list=$(FV3_restarts)
        echo "Copying FV3 restarts for 'RUN=${RUN}' at '${restart_date}' from '${restart_dir}'"
        local fv3_file restart_file
        for fv3_file in ${file_list}; do
            restart_file="${restart_date:0:8}.${restart_date:8:2}0000.${fv3_file}"
            cpreq "${restart_dir}/${restart_file}" "${DATA}/INPUT/${fv3_file}"
        done

        if [[ "${RERUN}" == "YES" ]]; then
            if [[ "${DO_SPPT:-}" == "YES" || "${DO_SKEB:-}" == "YES" ||
                "${DO_SHUM:-}" == "YES" || "${DO_LAND_PERT:-}" == "YES" ]]; then
                stochini=".true."
                file_list=$(stoch_restarts)
                echo "Copying stochastic restarts for 'RUN=${RUN}' at '${restart_date}' from '${restart_dir}'"
                for stoch_file in $(stoch_restarts); do
                    restart_file="${restart_date:0:8}.${restart_date:8:2}0000.${stoch_file}"
                    cpreq "${restart_dir}/${restart_file}" "${DATA}/INPUT/${stoch_file}"
                done
            fi
        else
            # Replace sfc_data with sfcanl_data restart files from current cycle (if found)
            local nn
            for ((nn = 1; nn <= ntiles; nn++)); do
                if [[ -f "${COMIN_ATMOS_RESTART}/${restart_date:0:8}.${restart_date:8:2}0000.sfcanl_data.tile${nn}.nc" ]]; then
                    rm -f "${DATA}/INPUT/sfc_data.tile${nn}.nc"
                    cpreq "${COMIN_ATMOS_RESTART}/${restart_date:0:8}.${restart_date:8:2}0000.sfcanl_data.tile${nn}.nc" \
                        "${DATA}/INPUT/sfc_data.tile${nn}.nc"
                # GCAFS does not run the sfcanl, only GCDAS
                elif [[ ${DO_AERO_FCST} == "YES" && -f "${COMIN_TRACER_RESTART}/${restart_date:0:8}.${restart_date:8:2}0000.sfcanl_data.tile${nn}.nc" ]]; then
                    rm -f "${DATA}/INPUT/sfc_data.tile${nn}.nc"
                    cpreq "${COMIN_TRACER_RESTART}/${restart_date:0:8}.${restart_date:8:2}0000.sfcanl_data.tile${nn}.nc" \
                        "${DATA}/INPUT/sfc_data.tile${nn}.nc"
                else
                    echo "'sfcanl_data.tile1.nc' not found in '${COMIN_ATMOS_RESTART}', using 'sfc_data.tile1.nc'"
                    break
                fi
            done

            # If aerosol analysis is to be done, replace fv_tracer with aeroanl_fv_tracer
            # restart files from current cycle (if found)
            if [[ "${DO_AERO_FCST}" == "YES" ]]; then
                local nn
                local use_anl_aero="YES"
                for ((nn = 1; nn <= ntiles; nn++)); do
                    test_tracer_file="${COMIN_TRACER_RESTART}/${restart_date:0:8}.${restart_date:8:2}0000.aeroanl_fv_tracer.res.tile${nn}.nc"
                    if [[ ! -f "${test_tracer_file}" ]]; then
                        use_anl_aero="NO"
                        echo "WARNING: File ${test_tracer_file} does not exist, will not replace any files from the aerosol analysis"
                        break
                    fi
                done
                if [[ "${use_anl_aero}" == "YES" ]]; then
                    for ((nn = 1; nn <= ntiles; nn++)); do
                        rm -f "${DATA}/INPUT/fv_tracer.res.tile${nn}.nc"
                        cpreq "${COMIN_TRACER_RESTART}/${restart_date:0:8}.${restart_date:8:2}0000.aeroanl_fv_tracer.res.tile${nn}.nc" \
                            "${DATA}/INPUT/fv_tracer.res.tile${nn}.nc"
                    done
                fi # if [[ ${use_anl_aero} == "YES" ]]; then

            fi # [[ ${DO_AERO_FCST} == "YES" ]]; then

        fi # if [[ "${RERUN}" == "YES" ]]; then

    fi # if [[ "${warm_start}" == ".true." ]]; then

    # Regardless of warm_start or not, the sfc_data and orography files should be consistent
    # Check for land-mask consistency, valid veg/soil types and soil moisture in valid range
    # TODO: the checker has a --fatal option, which is not used here.  This needs to be decided how to handle.
    if [[ "${CHECK_LAND_RESTART_FIELDS:-NO}" == "YES" ]]; then
        "${USHglobal}/check_land_input_fields.py" \
            --input_dir "${DATA}/INPUT" --orog_dir "${DATA}/INPUT" --soilparm_dir "${PARMglobal}"
        err=$?
        if [[ ${err} -ne 0 ]]; then
            echo "FATAL ERROR: check_land_input_fields.py returned error code ${err}, ABORT!"
            exit "${err}"
        fi
    fi

    #============================================================================
    # Determine increment files when doing cold start
    if [[ "${warm_start}" == ".false." ]] && [[ "${DOIAU_COLDSTART:-NO}" == "NO" ]]; then

        if [[ "${USE_ATM_ENS_PERTURB_FILES:-NO}" == "YES" ]]; then
            if ((MEMBER == 0)); then
                inc_files=()
            else
                inc_files=("increment.atm.i006.nc")
                read_increment=".true."
                res_latlon_dynamics="increment.atm.i006.nc"
            fi
            increment_file_on_native_grid=".false."
            local increment_file
            for inc_file in "${inc_files[@]}"; do
                increment_file="${COMIN_ATMOS_ANALYSIS}/${RUN}.t${cyc}z.${inc_file}"
                cpreq "${increment_file}" "${DATA}/INPUT/${inc_file}"
            done
        fi

    # Determine IAU and increment files when doing warm start
    elif [[ "${warm_start}" == ".true." ]] || [[ "${DOIAU_COLDSTART:-NO}" == "YES" ]]; then

        #--------------------------------------------------------------------------
        if [[ "${RERUN}" == "YES" ]]; then

            local restart_fhr
            restart_fhr=$(nhour "${RERUN_DATE}" "${current_cycle}")
            IAU_FHROT=$((IAU_OFFSET + restart_fhr))
            if [[ "${DOIAU}" == "YES" ]]; then
                IAUFHRS=-1
                IAU_DELTHRS=0
                IAU_INC_FILES="''"
            fi
            DO_LAND_IAU=".false."
        #--------------------------------------------------------------------------
        else # "${RERUN}" == "NO"

            # Need a coupler.res that is consistent with the model start time
            if [[ "${DOIAU:-NO}" == "YES" ]]; then
                local model_start_time="${previous_cycle}"
            else
                local model_start_time="${current_cycle}"
            fi
            local model_current_time="${model_start_date_current_cycle}"
            rm -f "${DATA}/INPUT/coupler.res"
            cat >> "${DATA}/INPUT/coupler.res" << EOF
      3        (Calendar: no_calendar=0, thirty_day_months=1, julian=2, gregorian=3, noleap=4)
      ${model_start_time:0:4}  ${model_start_time:4:2}  ${model_start_time:6:2}  ${model_start_time:8:2}  0  0        Model start time: year, month, day, hour, minute, second
      ${model_current_time:0:4}  ${model_current_time:4:2}  ${model_current_time:6:2}  ${model_current_time:8:2}  0  0        Current model time: year, month, day, hour, minute, second
EOF
            # Create a array of increment files
            local inc_files inc_file iaufhrs iaufhr
            if [[ "${DOIAU}" == "YES" || "${DOIAU_COLDSTART:-NO}" == "YES" ]]; then
                if [[ "${DOIAU_COLDSTART:-NO}" == "YES" ]]; then
                    IAUFHRS=0,3,6
                    IAU_DELTHRS=6
                    DO_LAND_IAU=".true."
                fi
                # create an array of inc_files for each IAU hour
                IFS=',' read -ra iaufhrs <<< "${IAUFHRS}"
                inc_files=()
                delimiter=""
                IAU_INC_FILES=""
                for iaufhr in "${iaufhrs[@]}"; do
                    if [[ "${DO_JEDIATMVAR:-NO}" == "YES" ]]; then
                        for tile in {1..6}; do
                            inc_file="jedi_increment.atm.i$(printf %03i "${iaufhr}").tile${tile}.nc"
                            inc_files+=("${inc_file}")
                            IAU_INC_FILES="${IAU_INC_FILES}${delimiter}'${inc_file}'"
                        done
                    else
                        inc_file="increment.atm.i$(printf %03i "${iaufhr}").nc"
                        inc_files+=("${inc_file}")
                        IAU_INC_FILES="${IAU_INC_FILES}${delimiter}'${inc_file}'"
                    fi

                    delimiter=","
                done
            else # "${DOIAU}" == "NO"
                read_increment=".true."

                if [[ "${DO_JEDIATMVAR:-NO}" == "YES" ]]; then
                    increment_file_on_native_grid=".true."
                    if [[ "${DOENKFONLY_ATM:-NO}" == "YES" ]]; then
                        inc_files=("jedi_increment.atm.i006.tile1.nc" "jedi_increment.atm.i006.tile2.nc" "jedi_increment.atm.i006.tile3.nc" "jedi_increment.atm.i006.tile4.nc" "jedi_increment.atm.i006.tile5.nc" "jedi_increment.atm.i006.tile6.nc")
                        res_latlon_dynamics="jedi_increment.atm.i006"
                    else
                        inc_files=("jedi_increment.atm.i006.tile1.nc" "jedi_increment.atm.i006.tile2.nc" "jedi_increment.atm.i006.tile3.nc" "jedi_increment.atm.i006.tile4.nc" "jedi_increment.atm.i006.tile5.nc" "jedi_increment.atm.i006.tile6.nc")
                        res_latlon_dynamics="jedi_increment.atm.i006"
                    fi
                    if [[ "${DO_JEDIATMENS:-NO}" == "NO" ]]; then
                        inc_files=("increment.atm.i006.nc")
                        res_latlon_dynamics="increment.atm.i006.nc"
                        increment_file_on_native_grid=".false."
                    fi
                else
                    if [[ "${DO_JEDIATMENS:-NO}" == "NO" ]]; then
                        inc_files=("increment.atm.i006.nc")
                        res_latlon_dynamics="increment.atm.i006.nc"
                        increment_file_on_native_grid=".false."
                    else
                        increment_file_on_native_grid=".true."
                        if [[ "${DOENKFONLY_ATM:-NO}" == "YES" ]]; then
                            inc_files=("jedi_increment.atm.i006.tile1.nc" "jedi_increment.atm.i006.tile2.nc" "jedi_increment.atm.i006.tile3.nc" "jedi_increment.atm.i006.tile4.nc" "jedi_increment.atm.i006.tile5.nc" "jedi_increment.atm.i006.tile6.nc")
                            res_latlon_dynamics="jedi_increment.atm.i006"
                        else
                            inc_files=("jedi_increment.atm.i006.tile1.nc" "jedi_increment.atm.i006.tile2.nc" "jedi_increment.atm.i006.tile3.nc" "jedi_increment.atm.i006.tile4.nc" "jedi_increment.atm.i006.tile5.nc" "jedi_increment.atm.i006.tile6.nc")
                            res_latlon_dynamics="jedi_increment.atm.i006"
                        fi
                    fi
                fi
                if [[ "${USE_ATM_ENS_PERTURB_FILES:-NO}" == "YES" ]]; then
                    # Control member has no perturbation
                    if ((MEMBER == 0)); then
                        inc_files=()
                        read_increment=".false."
                        res_latlon_dynamics='""'
                    fi
                fi
            fi

            if [[ "${RUN}" == "enkfgfs" || "${RUN}" == "enkfgdas" ]] || [[ "${DOIAU_COLDSTART:-NO}" == "YES" && ${MEMBER} -gt 0 ]]; then
                if [[ "${DOENKFONLY_ATM:-NO}" == "YES" ]]; then
                    prefix_atminc=""
                else
                    prefix_atminc="recentered_"
                fi
            else
                prefix_atminc=""
            fi

            local increment_file
            for inc_file in "${inc_files[@]}"; do
                if [[ "${DO_JEDIATMVAR:-NO}" == "YES" ]]; then
                    increment_file="${COMIN_ATMOS_ANALYSIS}/${RUN}.t${cyc}z.${prefix_atminc}${inc_file}"
                    if [[ "${DO_JEDIATMENS:-NO}" == "NO" ]]; then
                        increment_file="${COMIN_ATMOS_ANALYSIS}/${RUN}.t${cyc}z.${prefix_atminc}${inc_file}"
                    fi
                else
                    if [[ "${RUN}" == "gcafs" ]]; then
                        increment_file="${COMIN_ATMOS_ANALYSIS}/gcdas.t${cyc}z.${prefix_atminc}${inc_file}"
                    else
                        increment_file="${COMIN_ATMOS_ANALYSIS}/${RUN}.t${cyc}z.${prefix_atminc}${inc_file}"
                    fi
                fi
                cpreq "${increment_file}" "${DATA}/INPUT/${inc_file}"
            done

            # Land IAU increments: sfc_inc in FV3 grid, all timesteps in one file per tile
            if [[ ${DO_LAND_IAU} == ".true." ]]; then
                local TN sfc_increment_file
                for TN in $(seq 1 "${ntiles}"); do
                    sfc_increment_file="${COMIN_ATMOS_ANALYSIS}/increment.sfc.tile${TN}.nc"
                    if [[ ! -f "${sfc_increment_file}" ]]; then
                        export err=1
                        err_exit "FATAL ERROR: DO_LAND_IAU=${DO_LAND_IAU}, but missing increment file ${sfc_increment_file}, ABORT!"
                    else
                        cpreq "${sfc_increment_file}" "${DATA}/INPUT/sfc_inc.tile${TN}.nc"
                    fi
                done
            fi
        fi # if [[ "${RERUN}" == "YES" ]]; then
        #--------------------------------------------------------------------------
    fi # if [[ "${warm_start}" == ".true." ]]; then
    #============================================================================

    #============================================================================
    # If doing IAU, change forecast hours
    if [[ "${DOIAU:-NO}" == "YES" ]]; then
        FHMAX=$((FHMAX + 6))
        if [[ ${FHMAX_HF} -gt 0 ]]; then
            FHMAX_HF=$((FHMAX_HF + 6))
        fi
    fi
    #============================================================================

    #============================================================================
    # If warm starting from restart files, set the following flags
    if [[ "${warm_start}" == ".true." ]]; then

        # start from restart file
        nggps_ic=".false."
        ncep_ic=".false."
        external_ic=".false."
        mountain=".true."

        # restarts contain non-hydrostatic state
        if [[ "${TYPE}" == "nh" ]]; then
            make_nh=".false."
        fi

        # do not pre-condition the solution
        na_init=0

    fi # warm_start == .true.
    #============================================================================

    #============================================================================
    if [[ "${QUILTING}" == ".true." ]] && [[ "${OUTPUT_GRID}" == "gaussian_grid" || "${OUTPUT_GRID}" == "global_latlon" ]]; then
        local FH2 FH3
        for fhr in ${FV3_OUTPUT_FH}; do
            FH3=$(printf %03i "${fhr}")
            FH2=$(printf %02i "${fhr}")
            # TODO: remove deadlinks and refer https://github.com/NOAA-EMC/global-workflow/issues/4405
            ${NLN} "${COMOUT_ATMOS_HISTORY}/${RUN}.t${cyc}z.atm.f${FH3}.nc" "${DATAoutput}/FV3ATM_OUTPUT/atmf${FH3}.nc"
            ${NLN} "${COMOUT_ATMOS_HISTORY}/${RUN}.t${cyc}z.sfc.f${FH3}.nc" "${DATAoutput}/FV3ATM_OUTPUT/sfcf${FH3}.nc"
            ${NLN} "${COMOUT_ATMOS_HISTORY}/${RUN}.t${cyc}z.log.f${FH3}.txt" "${DATAoutput}/FV3ATM_OUTPUT/log.atm.f${FH3}"
            if [[ "${DO_JEDIATMVAR:-}" == "YES" || "${DO_HISTORY_FILE_ON_NATIVE_GRID:-"NO"}" == "YES" ]]; then
                ${NLN} "${COMOUT_ATMOS_HISTORY}/${RUN}.t${cyc}z.csg_atm.f${FH3}.nc" "${DATAoutput}/FV3ATM_OUTPUT/cubed_sphere_grid_atmf${FH3}.nc"
                ${NLN} "${COMOUT_ATMOS_HISTORY}/${RUN}.t${cyc}z.csg_sfc.f${FH3}.nc" "${DATAoutput}/FV3ATM_OUTPUT/cubed_sphere_grid_sfcf${FH3}.nc"
            fi
            if [[ "${WRITE_DOPOST}" == ".true." ]]; then
                if [[ "${RUN}" == "sfs" ]]; then
                    # SFS hourly forecast data is not needed in COM as it is not used by downstream
                    # applications or distributed. Instead, save it to an umbrella directory under DATAROOT
                    # so it can be used to calculate averages by the <job name> job.
                    if [[ "${MEMBER}" -eq 0 ]]; then
                        mkdir -p "${DATAROOT}/${RUN}efcs000.${PDY:-}${cyc}/output/FV3ATM_OUTPUT/model/master"
                        ${NLN} "${DATAROOT}/${RUN}efcs000.${PDY:-}${cyc}/output/FV3ATM_OUTPUT/model/master/${RUN}.t${cyc}z.master.f${FH3}.grib2" "${DATAoutput}/FV3ATM_OUTPUT/GFSPRS.GrbF${FH2}"
                        ${NLN} "${DATAROOT}/${RUN}efcs000.${PDY:-}${cyc}/output/FV3ATM_OUTPUT/model/master/${RUN}.t${cyc}z.sflux.f${FH3}.grib2" "${DATAoutput}/FV3ATM_OUTPUT/GFSFLX.GrbF${FH2}"
                    else
                        mkdir -p "${DATAoutput}/FV3ATM_OUTPUT/model/master"
                        ${NLN} "${DATAoutput}/FV3ATM_OUTPUT/model/master/${RUN}.t${cyc}z.master.f${FH3}.grib2" "${DATAoutput}/FV3ATM_OUTPUT/GFSPRS.GrbF${FH2}"
                        ${NLN} "${DATAoutput}/FV3ATM_OUTPUT/model/master/${RUN}.t${cyc}z.sflux.f${FH3}.grib2" "${DATAoutput}/FV3ATM_OUTPUT/GFSFLX.GrbF${FH2}"
                    fi
                else
                    ${NLN} "${COMOUT_ATMOS_MASTER}/${RUN}.t${cyc}z.master.f${FH3}.grib2" "${DATAoutput}/FV3ATM_OUTPUT/GFSPRS.GrbF${FH2}"
                    ${NLN} "${COMOUT_ATMOS_MASTER}/${RUN}.t${cyc}z.sflux.f${FH3}.grib2" "${DATAoutput}/FV3ATM_OUTPUT/GFSFLX.GrbF${FH2}"
                    if [[ "${DO_NEST:-NO}" == "YES" ]]; then
                        ${NLN} "${COMOUT_ATMOS_MASTER}/${RUN}.t${cyc}z.master.nest.f${FH3}.grib2" "${DATAoutput}/FV3ATM_OUTPUT/GFSPRS.GrbF${FH2}.nest02"
                        ${NLN} "${COMOUT_ATMOS_MASTER}/${RUN}.t${cyc}z.sflux.nest.f${FH3}.grib2" "${DATAoutput}/FV3ATM_OUTPUT/GFSFLX.GrbF${FH2}.nest02"
                    fi
                fi
            fi
        done
    fi
    #============================================================================
    restart_interval=${restart_interval:-${FHMAX}}
    if [[ ${restart_interval} -gt ${FHMAX} ]]; then
        restart_interval=${FHMAX}
    fi
    # restart_interval = 0 implies write restart at the END of the forecast i.e. at FHMAX
    # Convert restart interval into an explicit list for CMEPS/CICE/MOM6/WW3
    # Note, this must be computed after determination IAU in UFS_det and fhrot.
    if [[ ${restart_interval} -eq 0 ]]; then
        if [[ "${DOIAU:-NO}" == "YES" ]]; then
            FV3_RESTART_FH=$((FHMAX + assim_freq))
        else
            FV3_RESTART_FH=("${FHMAX}")
        fi
    else
        if [[ "${DOIAU:-NO}" == "YES" ]]; then
            if [[ "${MODE}" = "cycled" && "${SDATE}" = "${PDY}${cyc}" && ${EXP_WARM_START} = ".false." ]]; then
                local restart_interval_start=${restart_interval}
                local restart_interval_end=${FHMAX}
            else
                local restart_interval_start=$((restart_interval + assim_freq))
                local restart_interval_end=$((FHMAX + assim_freq))
            fi
        else
            local restart_interval_start=${restart_interval}
            local restart_interval_end=${FHMAX}
        fi
        FV3_RESTART_FH="$(seq -s ' ' "${restart_interval_start}" "${restart_interval}" "${restart_interval_end}")"
    fi
    export FV3_RESTART_FH
    if [[ -n "${FV3_RESTART_FH}" ]]; then mkdir -p "${DATArestart}/FV3_RESTART"; fi
    #============================================================================
}

FV3_nml() {
    # namelist output for a certain component
    echo "SUB ${FUNCNAME[0]}: Creating name lists and model configure file for FV3"

    source "${USHglobal}/parsing_namelists_fv3.sh"
    source "${USHglobal}/parsing_model_configure_fv3.sh"

    # Call the appropriate namelist functions
    if [[ "${DO_NEST:-NO}" == "YES" ]]; then
        source "${USHglobal}/parsing_namelists_fv3_nest.sh"
        FV3_namelists_nest global
        FV3_namelists_nest nest
    else
        FV3_namelists
    fi
    FV3_model_configure

    echo "SUB ${FUNCNAME[0]}: FV3 name lists and model configure file created"
}

FV3_out() {
    echo "SUB ${FUNCNAME[0]}: copying output data for FV3"

    # Copy configuration files
    cpfs "${DATA}/input.nml" "${COMOUT_CONF}/ufs.input.nml"
    cpfs "${DATA}/model_configure" "${COMOUT_CONF}/ufs.model_configure"
    cpfs "${DATA}/ufs.configure" "${COMOUT_CONF}/ufs.ufs.configure"
    cpfs "${DATA}/diag_table" "${COMOUT_CONF}/ufs.diag_table"

    # Determine the dates for restart files to be copied to COM
    local restart_date restart_dates
    restart_dates=()

    case ${RUN} in
        gdas | enkfgdas | enkfgfs | enkfgcafs | gcdas) # Copy restarts in the assimilation window for RUN=gdas|enkfgdas|enkfgfs
            restart_date="${model_start_date_next_cycle}"
            while ((restart_date <= forecast_end_cycle)); do
                restart_dates+=("${restart_date:0:8}.${restart_date:8:2}0000")
                restart_date=$(date --utc -d "${restart_date:0:8} ${restart_date:8:2} + ${restart_interval} hours" +%Y%m%d%H)
            done
            ;;
        gfs | gefs | sfs | gcafs) # Copy restarts at the end of the forecast segment for RUN=gfs|gefs|sfs|gcafs
            if [[ "${COPY_FINAL_RESTARTS}" == "YES" ]]; then
                restart_dates+=("${forecast_end_cycle:0:8}.${forecast_end_cycle:8:2}0000")
            fi
            ;;
        *)
            echo "FATAL ERROR: Not sure how to copy restart files for RUN ${RUN}"
            exit 25
            ;;
    esac

    ### Check that there are restart files to copy
    if [[ ${#restart_dates[@]} -gt 0 ]]; then
        # Get list of FV3 restart files
        local file_list fv3_file
        file_list=$(FV3_restarts)

        # Build MPMD cmdfile to copy restarts in parallel
        local cmdfile="${DATA}/cmdfile_fv3_out"
        rm -f "${cmdfile}"
        for restart_date in "${restart_dates[@]}"; do
            echo "Copying FV3 restarts for 'RUN=${RUN}' at ${restart_date}"
            for fv3_file in ${file_list}; do
                echo "cpfs ${DATArestart}/FV3_RESTART/${restart_date}.${fv3_file} ${COMOUT_ATMOS_RESTART}/${restart_date}.${fv3_file}" >> "${cmdfile}"
            done
        done

        if [[ -s "${cmdfile}" ]]; then
            if [[ ! -d "${COMOUT_ATMOS_RESTART}" ]]; then
                echo "INFO: Directory ${COMOUT_ATMOS_RESTART} does not exist, creating..."
                mkdir -p "${COMOUT_ATMOS_RESTART}"
            fi

            "${USHglobal}/run_mpmd.sh" "${cmdfile}" && true
            export err=$?
            if [[ ${err} -ne 0 ]]; then
                err_exit "run_mpmd.sh failed to copy FV3 restart files!"
            fi

            echo "SUB ${FUNCNAME[0]}: Output data for FV3 copied"
        fi
    fi
}
