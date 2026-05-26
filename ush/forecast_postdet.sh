#! /usr/bin/env bash

#===============================================================================
#
#   FILE: forecast_postdet.sh
#
#   DESCRIPTION: A suite of handler functions for managing the data flow and
#                configuration of various Unified Forecast System (UFS)
#                coupled components. It handles the staging of initial conditions,
#                namelist generation, and output/restart file management for:
#                FV3 (Atmosphere), WW3 (Waves), MOM6 (Ocean), CICE (Sea Ice),
#                GOCART (Aerosols), and CMEPS (Coupler/Mediator)

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
    # Check for consistency
    # TODO: the checker has a --fatal option, which is not used here.  This needs to be decided how to handle.
    if [[ "${CHECK_LAND_RESTART_OROG:-NO}" == "YES" ]]; then
        "${USHgfs}/check_land_input_orography.py" \
            --input_dir "${DATA}/INPUT" --orog_dir "${DATA}/INPUT"
        err=$?
        if [[ ${err} -ne 0 ]]; then
            echo "FATAL ERROR: check_land_input_orography.py returned error code ${err}, ABORT!"
            exit "${err}"
        fi
    fi

    #============================================================================
    # Determine increment files when doing cold start
    if [[ "${warm_start}" == ".false." ]]; then

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
    elif [[ "${warm_start}" == ".true." ]]; then

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
            if [[ "${DOIAU}" == "YES" ]]; then
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
                    inc_files=("jedi_increment.atm.i006.tile1.nc" "jedi_increment.atm.i006.tile2.nc" "jedi_increment.atm.i006.tile3.nc" "jedi_increment.atm.i006.tile4.nc" "jedi_increment.atm.i006.tile5.nc" "jedi_increment.atm.i006.tile6.nc")
                    increment_file_on_native_grid=".true."
                    res_latlon_dynamics="jedi_increment.atm.i006"
                    if [[ "${DO_JEDIATMENS:-NO}" == "NO" ]]; then
                        inc_files=("increment.atm.i006.nc")
                        res_latlon_dynamics="increment.atm.i006.nc"
                        increment_file_on_native_grid=".false."
                    fi
                else
                    inc_files=("increment.atm.i006.nc")
                    res_latlon_dynamics="increment.atm.i006.nc"
                    increment_file_on_native_grid=".false."
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

            if [[ "${RUN}" == "enkfgfs" ]] || [[ "${RUN}" == "enkfgdas" ]]; then
                prefix_atminc="recentered_"
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
    if [[ "${QUILTING}" == ".true." ]] && [[ "${OUTPUT_GRID}" == "gaussian_grid" ]]; then
        local FH2 FH3
        # Build a product table consumed by the forecast manager.
        # The model writes real files to DATAoutput; the manager copies them to COM.
        local use_mgr="NO"
        case "${RUN}" in
            gfs | gdas | enkfgdas) use_mgr="YES" ;;
                # TODO: enable forecast manager for gefs, sfs, gcafs once tested
                # gefs | sfs | gcafs) use_mgr="YES" ;;
            *) ;;
        esac

        # Per-product tables: MGR_NATM_INST instance groups, each with one rank
        # per ATM file type plus one barrier rank (5 ranks per instance, 10 total
        # for the default of 2). Forecast hours are distributed round-robin across
        # instances so all groups copy in parallel, halving the serial copy time.
        local natm_inst="${MGR_NATM_INST:-2}"
        local seg="${FCST_SEGMENT:-0}"
        local inst fhr_idx
        local -a atm_atmf_tables atm_sfcf_tables atm_grib_tables atm_flux_tables atm_barrier_tables
        for ((inst = 0; inst < natm_inst; inst++)); do
            atm_atmf_tables[inst]="${DATAjob}/atm_atmf_products_seg${seg}_inst${inst}.txt"
            atm_sfcf_tables[inst]="${DATAjob}/atm_sfcf_products_seg${seg}_inst${inst}.txt"
            atm_grib_tables[inst]="${DATAjob}/atm_grib_products_seg${seg}_inst${inst}.txt"
            atm_flux_tables[inst]="${DATAjob}/atm_flux_products_seg${seg}_inst${inst}.txt"
            atm_barrier_tables[inst]="${DATAjob}/atm_barrier_seg${seg}_inst${inst}.txt"
        done
        if [[ "${use_mgr}" == "YES" ]]; then
            for ((inst = 0; inst < natm_inst; inst++)); do
                rm -f "${atm_atmf_tables[inst]}" "${atm_sfcf_tables[inst]}" \
                    "${atm_grib_tables[inst]}" "${atm_flux_tables[inst]}" "${atm_barrier_tables[inst]}"
            done
            # Remove the table-ready sentinel so the forecast manager does not trigger from a
            # previous run when this segment is rewound and re-queued.
            rm -f "${DATAjob}/fcst_table_ready_seg${seg}"
        fi

        fhr_idx=0
        for fhr in ${FV3_OUTPUT_FH}; do
            FH3=$(printf %03i "${fhr}")
            FH2=$(printf %02i "${fhr}")
            inst=$((fhr_idx % natm_inst))
            ((fhr_idx++)) || true

            # Build (local_file, com_file) pairs once; used for both the manager
            # product table and the NLN symlink paths.
            local local_files=() com_files=()
            local_files+=("${DATAoutput}/FV3ATM_OUTPUT/atmf${FH3}.nc")
            com_files+=("${COMOUT_ATMOS_HISTORY}/${RUN}.t${cyc}z.atm.f${FH3}.nc")
            local_files+=("${DATAoutput}/FV3ATM_OUTPUT/sfcf${FH3}.nc")
            com_files+=("${COMOUT_ATMOS_HISTORY}/${RUN}.t${cyc}z.sfc.f${FH3}.nc")
            if [[ "${DO_JEDIATMVAR:-}" == "YES" || "${DO_HISTORY_FILE_ON_NATIVE_GRID:-"NO"}" == "YES" ]]; then
                local_files+=("${DATAoutput}/FV3ATM_OUTPUT/cubed_sphere_grid_atmf${FH3}.nc")
                com_files+=("${COMOUT_ATMOS_HISTORY}/${RUN}.t${cyc}z.csg_atm.f${FH3}.nc")
                local_files+=("${DATAoutput}/FV3ATM_OUTPUT/cubed_sphere_grid_sfcf${FH3}.nc")
                com_files+=("${COMOUT_ATMOS_HISTORY}/${RUN}.t${cyc}z.csg_sfc.f${FH3}.nc")
            fi
            if [[ "${WRITE_DOPOST}" == ".true." ]]; then
                local_files+=("${DATAoutput}/FV3ATM_OUTPUT/GFSPRS.GrbF${FH2}")
                com_files+=("${COMOUT_ATMOS_MASTER}/${RUN}.t${cyc}z.master.f${FH3}.grib2")
                local_files+=("${DATAoutput}/FV3ATM_OUTPUT/GFSFLX.GrbF${FH2}")
                com_files+=("${COMOUT_ATMOS_MASTER}/${RUN}.t${cyc}z.sflux.f${FH3}.grib2")
                if [[ "${DO_NEST:-NO}" == "YES" ]]; then
                    local_files+=("${DATAoutput}/FV3ATM_OUTPUT/GFSPRS.GrbF${FH2}.nest02")
                    com_files+=("${COMOUT_ATMOS_MASTER}/${RUN}.t${cyc}z.master.nest.f${FH3}.grib2")
                    local_files+=("${DATAoutput}/FV3ATM_OUTPUT/GFSFLX.GrbF${FH2}.nest02")
                    com_files+=("${COMOUT_ATMOS_MASTER}/${RUN}.t${cyc}z.sflux.nest.f${FH3}.grib2")
                fi
            fi

            # log.atm.fHHH is the sentinel written by the write component after
            # atmfHHH.nc and sfcfHHH.nc are fully flushed to disk.
            local local_log="${DATAoutput}/FV3ATM_OUTPUT/log.atm.f${FH3}"
            local com_log="${COMOUT_ATMOS_HISTORY}/${RUN}.t${cyc}z.log.f${FH3}.txt"
            local i
            if [[ "${use_mgr}" == "YES" ]]; then
                # Per-product com_logs for parallel copy ranks.
                local com_log_atmf="${COMOUT_ATMOS_HISTORY}/${RUN}.t${cyc}z.log.atm.atmf.f${FH3}.txt"
                local com_log_sfcf="${COMOUT_ATMOS_HISTORY}/${RUN}.t${cyc}z.log.atm.sfcf.f${FH3}.txt"
                # Atmospheric state netCDF rank
                echo "${DATAoutput}/FV3ATM_OUTPUT/atmf${FH3}.nc ${local_log} ${COMOUT_ATMOS_HISTORY}/${RUN}.t${cyc}z.atm.f${FH3}.nc ${com_log_atmf}" >> "${atm_atmf_tables[inst]}"
                # Surface state netCDF rank
                echo "${DATAoutput}/FV3ATM_OUTPUT/sfcf${FH3}.nc ${local_log} ${COMOUT_ATMOS_HISTORY}/${RUN}.t${cyc}z.sfc.f${FH3}.nc ${com_log_sfcf}" >> "${atm_sfcf_tables[inst]}"
                # Optional cubed-sphere grid files share the same nc rank as their Gaussian counterpart.
                if [[ "${DO_JEDIATMVAR:-}" == "YES" || "${DO_HISTORY_FILE_ON_NATIVE_GRID:-"NO"}" == "YES" ]]; then
                    echo "${DATAoutput}/FV3ATM_OUTPUT/cubed_sphere_grid_atmf${FH3}.nc ${local_log} ${COMOUT_ATMOS_HISTORY}/${RUN}.t${cyc}z.csg_atm.f${FH3}.nc ${com_log_atmf}" >> "${atm_atmf_tables[inst]}"
                    echo "${DATAoutput}/FV3ATM_OUTPUT/cubed_sphere_grid_sfcf${FH3}.nc ${local_log} ${COMOUT_ATMOS_HISTORY}/${RUN}.t${cyc}z.csg_sfc.f${FH3}.nc ${com_log_sfcf}" >> "${atm_sfcf_tables[inst]}"
                fi
                local barrier_deps="${com_log_atmf} ${com_log_sfcf}"
                if [[ "${WRITE_DOPOST}" == ".true." ]]; then
                    local com_log_grib="${COMOUT_ATMOS_MASTER}/${RUN}.t${cyc}z.log.atm.grib.f${FH3}.txt"
                    local com_log_flux="${COMOUT_ATMOS_MASTER}/${RUN}.t${cyc}z.log.atm.flux.f${FH3}.txt"
                    # GRIB2 gridded rank
                    echo "${DATAoutput}/FV3ATM_OUTPUT/GFSPRS.GrbF${FH2} ${local_log} ${COMOUT_ATMOS_MASTER}/${RUN}.t${cyc}z.master.f${FH3}.grib2 ${com_log_grib}" >> "${atm_grib_tables[inst]}"
                    # GRIB2 flux rank
                    echo "${DATAoutput}/FV3ATM_OUTPUT/GFSFLX.GrbF${FH2} ${local_log} ${COMOUT_ATMOS_MASTER}/${RUN}.t${cyc}z.sflux.f${FH3}.grib2 ${com_log_flux}" >> "${atm_flux_tables[inst]}"
                    if [[ "${DO_NEST:-NO}" == "YES" ]]; then
                        echo "${DATAoutput}/FV3ATM_OUTPUT/GFSPRS.GrbF${FH2}.nest02 ${local_log} ${COMOUT_ATMOS_MASTER}/${RUN}.t${cyc}z.master.nest.f${FH3}.grib2 ${com_log_grib}" >> "${atm_grib_tables[inst]}"
                        echo "${DATAoutput}/FV3ATM_OUTPUT/GFSFLX.GrbF${FH2}.nest02 ${local_log} ${COMOUT_ATMOS_MASTER}/${RUN}.t${cyc}z.sflux.nest.f${FH3}.grib2 ${com_log_flux}" >> "${atm_flux_tables[inst]}"
                    fi
                    barrier_deps="${barrier_deps} ${com_log_grib} ${com_log_flux}"
                fi
                # Barrier row: final combined com_log followed by all per-product deps.
                echo "${com_log} ${barrier_deps}" >> "${atm_barrier_tables[inst]}"
            else
                # Remaining runs (gefs, sfs, gcafs, enkfgfs): build a copy cmdfile;
                # FV3_out will copy files from DATA to COM after the forecast completes.
                for ((i = 0; i < ${#local_files[@]}; i++)); do
                    echo "cpfs ${local_files[i]} ${com_files[i]}" >> "${atm_hist_cmdfile}"
                done
                echo "cpfs ${local_log} ${com_log}" >> "${atm_hist_cmdfile}"
            fi
        done

        ##############################################################
        # Release the forecast manager once the product table is ready
        # so it can begin copying output files to COM as they appear.
        ##############################################################
        if [[ "${SENDECF}" == "YES" && "${use_mgr}" == "YES" ]]; then
            ecflow_client --event release_fcst_manager
        fi

    fi
    #============================================================================
    restart_interval=${restart_interval:-${FHMAX}}
    # restart_interval = 0 implies write restart at the END of the forecast i.e. at FHMAX
    # Convert restart interval into an explicit list for CMEPS/CICE/MOM6/WW3
    # Note, this must be computed after determination IAU in forecast_det and fhrot.
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
    if [[ -n "${FV3_RESTART_FH}" ]]; then
        mkdir -p "${DATArestart}/FV3_RESTART"
    fi
    #============================================================================
}

FV3_nml() {
    # namelist output for a certain component
    echo "SUB ${FUNCNAME[0]}: Creating name lists and model configure file for FV3"

    source "${USHgfs}/parsing_namelists_FV3.sh"
    source "${USHgfs}/parsing_model_configure_FV3.sh"

    # Call the appropriate namelist functions
    if [[ "${DO_NEST:-NO}" == "YES" ]]; then
        source "${USHgfs}/parsing_namelists_FV3_nest.sh"
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
            "${USHgfs}/run_mpmd.sh" "${cmdfile}" && true
            export err=$?
            if [[ ${err} -ne 0 ]]; then
                err_exit "run_mpmd.sh failed to copy FV3 restart files!"
            fi

            echo "SUB ${FUNCNAME[0]}: Output data for FV3 copied"
        fi
    fi

    # Copy FV3 history files and log sentinels from DATA to COM for non-manager runs
    # (gefs, sfs, gcafs, enkfgfs). For manager runs this is handled by the forecast manager.
    local atm_hist_cmdfile="${DATA}/cmdfile_fv3_hist"
    if [[ -s "${atm_hist_cmdfile}" ]]; then
        if [[ ! -d "${COMOUT_ATMOS_HISTORY}" ]]; then
            echo "INFO: Directory ${COMOUT_ATMOS_HISTORY} does not exist, creating..."
            mkdir -p "${COMOUT_ATMOS_HISTORY}"
        fi
        if [[ ! -d "${COMOUT_ATMOS_MASTER}" ]]; then
            echo "INFO: Directory ${COMOUT_ATMOS_MASTER} does not exist, creating..."
            mkdir -p "${COMOUT_ATMOS_MASTER}"
        fi
        "${USHgfs}/run_mpmd.sh" "${atm_hist_cmdfile}" && true
        export err=$?
        if [[ ${err} -ne 0 ]]; then
            err_exit "run_mpmd.sh failed to copy FV3 history files!"
        fi
        echo "SUB ${FUNCNAME[0]}: FV3 history files copied to COM"
    fi
}

################################################################################
# forecast_postdet_ww3.sh
################################################################################

# shellcheck disable=SC2034
# shellcheck disable=SC2178

WW3_postdet() {
    echo "SUB ${FUNCNAME[0]}: Linking input data for WW3"
    # Copy initial condition files:
    local restart_date restart_dir
    if [[ "${RERUN}" == "YES" ]]; then
        restart_date="${RERUN_DATE}"
        restart_dir="${DATArestart}/WW3_RESTART"
    else
        restart_date="${model_start_date_current_cycle}"
        restart_dir="${COMIN_WAVE_RESTART_PREV}"
    fi

    echo "Copying WW3 restarts for 'RUN=${RUN}' at '${restart_date}' from '${restart_dir}'"

    local ww3_restart_file ww3_restart_dest_file seconds
    seconds=$(to_seconds "${restart_date:8:2}0000") # convert HHMMSS to seconds
    ww3_restart_file="${restart_dir}/${restart_date:0:8}.${restart_date:8:2}0000.restart.ww3"
    ww3_restart_dest_file="ufs.cpld.ww3.r.${restart_date:0:4}-${restart_date:4:2}-${restart_date:6:2}-${seconds}"
    if [[ -s "${ww3_restart_file}.nc" ]]; then # First check to see if netcdf restart exists:
        export WW3_restart_from_binary=false
        cpreq "${ww3_restart_file}.nc" "${DATA}/${ww3_restart_dest_file}.nc"
    elif [[ -s "${ww3_restart_file}" ]]; then # If not, check to see if binary restart exists:
        export WW3_restart_from_binary=true
        cpreq "${ww3_restart_file}" "${DATA}/${ww3_restart_dest_file}"
    else
        if [[ "${RERUN}" == "YES" ]] || [[ -f "${DATA}/ufs.cpld.cpl.r.nc" ]]; then # The || part requires CMEPS_postdet to be called before WW3_postdet
            # In the case of a RERUN, the WW3 restart file is required
            # In the case of runtype=continue, if no wave restart when using PIO, the model will fail
            echo "FATAL ERROR: WW3 binary | netcdf restart file '${ww3_restart_file}' | '${ww3_restart_file}.nc' not found for RERUN='${RERUN}' or runtype=continue, ABORT!"
            exit 1
        else
            export WW3_restart_from_binary=false
            echo "WARNING: WW3 binary | netcdf restart file '${ww3_restart_file}' | '${ww3_restart_file}.nc' not found for warm_start='${warm_start}', will start from rest!"
        fi
    fi

    local first_ww3_restart_out
    first_ww3_restart_out=$(date --utc -d "${restart_date:0:8} ${restart_date:8:2} + ${restart_interval} hours" +%Y%m%d%H)
    if [[ "${DOIAU:-NO}" == "YES" ]]; then
        first_ww3_restart_out=$(date --utc -d "${first_ww3_restart_out:0:8} ${first_ww3_restart_out:8:2} + ${half_window} hours" +%Y%m%d%H)
    fi

    # Link restart files to their expected names in DATArestart/WW3_RESTART
    # TODO: Have the UFSWM write out the WW3 restart files in the expected format of 'YYYYMMDD.HHmmSS.restart.ww3.nc'
    local cwd vdate ww3_ufs_restart_file ww3_netcdf_restart_file
    cwd="${PWD}"
    cd "${DATArestart}/WW3_RESTART" || exit 1
    for ((vdate = first_ww3_restart_out; vdate <= forecast_end_cycle;  \
    vdate = $(date --utc -d "${vdate:0:8} ${vdate:8:2} + ${restart_interval} hours" +%Y%m%d%H))); do
        seconds=$(to_seconds "${vdate:8:2}0000")                                                   # convert HHMMSS to seconds
        ww3_ufs_restart_file="ufs.cpld.ww3.r.${vdate:0:4}-${vdate:4:2}-${vdate:6:2}-${seconds}.nc" # UFS restart file name
        ww3_netcdf_restart_file="${vdate:0:8}.${vdate:8:2}0000.restart.ww3.nc"                     # WW3 restart file name in COM
        ${NLN} "${ww3_netcdf_restart_file}" "${ww3_ufs_restart_file}"
    done

    # TODO: link GEFS restart for next cycle IC
    #if [[ "${RUN}" == "gefs" ]]; then
    #  vdate=${model_start_date_next_cycle}
    #  seconds=$(to_seconds "${vdate:8:2}0000")  # convert HHMMSS to seconds
    #  ww3_ufs_restart_file="ufs.cpld.ww3.r.${vdate:0:4}-${vdate:4:2}-${vdate:6:2}-${seconds}.nc"
    #  ww3_netcdf_restart_file="${vdate:0:8}.${vdate:8:2}0000.restart.ww3.nc"
    #  ${NLN} "${ww3_netcdf_restart_file}" "${ww3_ufs_restart_file}"
    #fi
    cd "${cwd}" || exit 1

    # Build product tables for the forecast manager.
    local use_mgr_ww3="NO"
    case "${RUN}" in
        gfs | gdas | enkfgdas) use_mgr_ww3="YES" ;;
            # TODO: enable forecast manager for gefs, sfs, gcafs once tested
            # gefs | sfs | gcafs) use_mgr_ww3="YES" ;;
        *) ;;
    esac

    # log.ww3 is the WW3 run log written to DATA. For manager runs it becomes a
    # real file (copied to COM in WW3_out). Others symlink it to COM here.
    if [[ "${use_mgr_ww3}" == "YES" ]]; then
        : # log.ww3 will be a real file in DATA; WW3_out copies it after the run
    else
        ${NLN} "${COMOUT_WAVE_HISTORY}/${RUN}.t${cyc}z.${waveGRD}.${PDY}${cyc}.log" "log.ww3"
    fi

    # Loop for gridded output (uses FHINC)
    local fhr fhr3 FHINC
    # shellcheck disable=SC2153
    fhr="${FHMIN_WAV}"
    if [[ ${FHMAX_HF_WAV} -gt 0 && ${FHOUT_HF_WAV} -gt 0 && ${fhr} -lt ${FHMAX_HF_WAV} ]]; then
        fhinc=${FHOUT_HF_WAV}
    else
        fhinc=${FHOUT_WAV}
    fi
    local ww3_table="${DATAjob}/ww3_products_seg${FCST_SEGMENT:-0}.txt"
    if [[ "${use_mgr_ww3}" == "YES" ]]; then
        rm -f "${ww3_table}"
    fi
    while [[ ${fhr} -le ${FHMAX_WAV} ]]; do
        fhr3=$(printf '%03d' "${fhr}")
        vdate=$(date --utc -d "${current_cycle:0:8} ${current_cycle:8:2} + ${fhr} hours" +%Y%m%d.%H0000)
        local ww3_grd_local="${DATAoutput}/WW3_OUTPUT/${vdate}.out_grd.ww3"
        local ww3_grd_local_log="${DATAoutput}/WW3_OUTPUT/log.${vdate}.out_grd.ww3.txt"
        local ww3_grd_com="${COMOUT_WAVE_HISTORY}/${RUN}.t${cyc}z.${waveGRD}.f${fhr3}.bin"
        local ww3_grd_com_log="${COMOUT_WAVE_HISTORY}/${RUN}.t${cyc}z.${waveGRD}.f${fhr3}.log"
        if [[ "${use_mgr_ww3}" == "YES" ]]; then
            # Each WW3 gridded file has its own per-file sentinel log
            echo "${ww3_grd_local} ${ww3_grd_local_log} ${ww3_grd_com} ${ww3_grd_com_log}" >> "${ww3_table}"
        else
            ${NLN} "${ww3_grd_com}" "${ww3_grd_local}"
            ${NLN} "${ww3_grd_com_log}" "${ww3_grd_local_log}"
        fi

        if [[ ${fhr} -ge ${FHMAX_HF_WAV} ]]; then
            fhinc=${FHOUT_WAV}
        fi
        fhr=$((fhr + fhinc))
    done

    # Loop for point output (uses DTPNT)
    fhr=${FHMIN_WAV}
    fhinc=${FHINCP_WAV}
    while [[ ${fhr} -le ${FHMAX_WAV} ]]; do
        fhr3=$(printf '%03d' "${fhr}")
        vdate=$(date --utc -d "${current_cycle:0:8} ${current_cycle:8:2} + ${fhr} hours" +%Y%m%d.%H0000)
        local ww3_pnt_local="${DATAoutput}/WW3_OUTPUT/${vdate}.out_pnt.ww3.nc"
        local ww3_pnt_local_log="${DATAoutput}/WW3_OUTPUT/log.${vdate}.out_pnt.ww3.txt"
        local ww3_pnt_com="${COMOUT_WAVE_HISTORY}/${RUN}.t${cyc}z.points.f${fhr3}.nc"
        local ww3_pnt_com_log="${COMOUT_WAVE_HISTORY}/${RUN}.t${cyc}z.points.f${fhr3}.log"
        if [[ "${use_mgr_ww3}" == "YES" ]]; then
            # Each WW3 point file has its own per-file sentinel log
            echo "${ww3_pnt_local} ${ww3_pnt_local_log} ${ww3_pnt_com} ${ww3_pnt_com_log}" >> "${ww3_table}"
        else
            ${NLN} "${ww3_pnt_com}" "${ww3_pnt_local}"
            ${NLN} "${ww3_pnt_com_log}" "${ww3_pnt_local_log}"
        fi

        fhr=$((fhr + fhinc))
    done

}

WW3_nml() {
    echo "SUB ${FUNCNAME[0]}: Copying input files for WW3"
    source "${USHgfs}/parsing_namelists_WW3.sh"
    WW3_namelists
}

WW3_out() {
    echo "SUB ${FUNCNAME[0]}: Copying output data for WW3"

    # Copy wave namelist from DATA to COMOUT_CONF after the forecast is run (and successfull)
    cpfs "${DATA}/ww3_shel.nml" "${COMOUT_CONF}/ufs.ww3_shel.nml"

    # Copy WW3 run log (no pre-run symlink; model writes a real file in DATA
    # which is copied to COM here at end of run for manager-enabled runs)
    case "${RUN}" in
        gfs | gdas | enkfgdas | gefs | sfs | gcafs)
            if [[ -f "${DATA}/log.ww3" ]]; then
                mkdir -p "${COMOUT_WAVE_HISTORY}"
                cpfs "${DATA}/log.ww3" "${COMOUT_WAVE_HISTORY}/${RUN}.t${cyc}z.${waveGRD}.${PDY}${cyc}.log"
            fi
            ;;
        *) ;;
    esac

    # Build MPMD cmdfile to copy WW3 restarts in parallel
    local cmdfile="${DATA}/cmdfile_ww3_out"
    rm -f "${cmdfile}"

    # Copy WW3 restarts at the end of the forecast segment to COM for RUN=gfs|gefs
    if [[ "${COPY_FINAL_RESTARTS}" == "YES" ]]; then
        local restart_file
        if [[ "${RUN}" == "gfs" || "${RUN}" == "gefs" || "${RUN}" == "gcafs" ]]; then
            echo "Copying WW3 restarts for 'RUN=${RUN}' at ${forecast_end_cycle}"
            restart_file="${forecast_end_cycle:0:8}.${forecast_end_cycle:8:2}0000.restart.ww3.nc"
            echo "cpfs ${DATArestart}/WW3_RESTART/${restart_file} ${COMOUT_WAVE_RESTART}/${restart_file}" >> "${cmdfile}"
        fi
    fi

    # Copy restarts for next cycle for RUN=gdas|gefs
    # TODO: GEFS needs to be added here
    if [[ "${RUN}" == "gdas" ]]; then
        local restart_date restart_file
        restart_date="${model_start_date_next_cycle}"
        echo "Copying WW3 restarts for 'RUN=${RUN}' at ${restart_date}"
        restart_file="${restart_date:0:8}.${restart_date:8:2}0000.restart.ww3.nc"
        echo "cpfs ${DATArestart}/WW3_RESTART/${restart_file} ${COMOUT_WAVE_RESTART}/${restart_file}" >> "${cmdfile}"
    fi

    # Copy restarts for downstream usage in HAFS
    if [[ "${RUN}" == "gdas" ]]; then
        local restart_date restart_file
        restart_date="${next_cycle}"
        echo "Copying WW3 restarts for 'RUN=${RUN}' at ${restart_date}"
        restart_file="${restart_date:0:8}.${restart_date:8:2}0000.restart.ww3.nc"
        echo "cpfs ${DATArestart}/WW3_RESTART/${restart_file} ${COMOUT_WAVE_RESTART}/${restart_file}" >> "${cmdfile}"
    fi

    if [[ -s "${cmdfile}" ]]; then
        if [[ ! -d "${COMOUT_WAVE_RESTART}" ]]; then
            echo "INFO: Directory ${COMOUT_WAVE_RESTART} does not exist, creating..."
            mkdir -p "${COMOUT_WAVE_RESTART}"
        fi
        "${USHgfs}/run_mpmd.sh" "${cmdfile}" && true
        export err=$?
        if [[ ${err} -ne 0 ]]; then
            err_exit "run_mpmd.sh failed to copy WW3 restart files!"
        fi
    fi

}

################################################################################
# forecast_postdet_cmeps.sh
################################################################################

# shellcheck disable=SC2034
# shellcheck disable=SC2178

CPL_out() {
    echo "SUB ${FUNCNAME[0]}: Copying output data for general cpl fields"
    if [[ "${esmf_profile:-.false.}" == ".true." ]]; then
        if [[ ! -d "${COMOUT_ATMOS_HISTORY}" ]]; then
            echo "INFO: Directory ${COMOUT_ATMOS_HISTORY} does not exist, creating..."
            mkdir -p "${COMOUT_ATMOS_HISTORY}"
        fi
        cpfs "${DATA}/ESMF_Profile.summary" "${COMOUT_ATMOS_HISTORY}/ESMF_Profile.summary"
    fi
}
################################################################################
# forecast_postdet_mom6.sh
################################################################################

# shellcheck disable=SC2034
# shellcheck disable=SC2178

MOM6_postdet() {
    echo "SUB ${FUNCNAME[0]}: MOM6 after run type determination"

    local restart_dir restart_date
    if [[ "${RERUN}" == "YES" ]]; then
        if [[ ! -d "${COMOUT_ATMOS_HISTORY}" ]]; then
            echo "INFO: Directory ${COMOUT_ATMOS_HISTORY} does not exist, creating..."
            mkdir -p "${COMOUT_ATMOS_HISTORY}"
        fi
        restart_dir="${DATArestart}/MOM6_RESTART"
        restart_date="${RERUN_DATE}"
    else # "${RERUN}" == "NO"
        restart_dir="${COMIN_OCEAN_RESTART_PREV}"
        restart_date="${model_start_date_current_cycle}"
    fi

    # Copy MOM6 ICs
    cpreq "${restart_dir}/${restart_date:0:8}.${restart_date:8:2}0000.MOM.res.nc" "${DATA}/INPUT/MOM.res.nc"
    case ${OCNRES} in
        "025")
            local nn
            for ((nn = 1; nn <= 4; nn++)); do
                if [[ -f "${restart_dir}/${restart_date:0:8}.${restart_date:8:2}0000.MOM.res_${nn}.nc" ]]; then
                    cpreq "${restart_dir}/${restart_date:0:8}.${restart_date:8:2}0000.MOM.res_${nn}.nc" "${DATA}/INPUT/MOM.res_${nn}.nc"
                fi
            done
            ;;
        *) ;;
    esac

    # Copy increment (only when RERUN=NO)
    if [[ "${RERUN}" == "NO" ]]; then
        if [[ "${DO_JEDIOCNVAR:-NO}" == "YES" ]] || [[ ${MEMBER} -gt 0 && "${ODA_INCUPD:-False}" == "True" ]]; then
            cpreq "${COMIN_OCEAN_ANALYSIS}/${RUN}.t${cyc}z.mom6_increment.i006.nc" "${DATA}/INPUT/mom6_increment.nc"
        fi
    fi # if [[ "${RERUN}" == "NO" ]]; then

    # Link output files
    case ${RUN} in
        gfs | enkfgfs | gefs | sfs | gcafs | gdas | enkfgdas) # Set up MOM6 output files
            local fhr fhr3 last_fhr interval midpoint vdate vdate_mid ihour source_file dest_file source_file_log dest_file_log
            local ocn_local ocn_com ocn_table ocn_hist_cmdfile use_mgr_ocn
            # TODO: enable forecast manager for enkfgfs, gefs, sfs, gcafs once tested
            case "${RUN}" in
                gfs | gdas | enkfgdas) use_mgr_ocn="YES" ;;
                *) use_mgr_ocn="NO" ;;
            esac
            ocn_table="${DATAjob}/ocn_products_seg${FCST_SEGMENT}.txt"
            ocn_hist_cmdfile="${DATA}/cmdfile_mom6_hist"
            rm -f "${ocn_table}" "${ocn_hist_cmdfile}"
            for fhr in ${MOM6_OUTPUT_FH}; do
                fhr3=$(printf %03i "${fhr}")

                if [[ -z ${last_fhr:-} ]]; then
                    last_fhr=${fhr}
                    continue
                fi

                ((interval = fhr - last_fhr))
                ihour=$(printf %02i "${interval}")
                vdate=$(date --utc -d "${current_cycle:0:8} ${current_cycle:8:2} + ${fhr} hours" +%Y%m%d%H)
                source_file_log="${DATA}/${vdate:0:8}.${vdate:8:2}0000.mom6.${ihour}h"

                case "${RUN}" in
                    gdas | enkfgdas)
                        # Instantaneous MOM6 backgrounds; filename uses underscore-separated date.
                        local vdatestr_da="${vdate:0:4}_${vdate:4:2}_${vdate:6:2}_${vdate:8:2}"
                        source_file="ocn_da_${vdatestr_da}.nc"
                        dest_file="${RUN}.t${cyc}z.inst.f${fhr3}.nc"
                        dest_file_log="${COMOUT_OCEAN_HISTORY}/${RUN}.t${cyc}z.inst.log.f${fhr3}.txt"
                        ;;
                    gfs | enkfgfs | sfs | gcafs)
                        # Period averages; model uses midpoint timestamp in filename.
                        ((midpoint = last_fhr + interval / 2))
                        # If OFFSET_START_HOUR > 0, add offset to midpoint for first lead time.
                        # Native model uses midpoint in filename; we map that to the end of the period for COM.
                        if ((OFFSET_START_HOUR > 0)) && ((fhr == FHOUT_OCN)); then
                            vdate_mid=$(date --utc -d "${current_cycle:0:8} ${current_cycle:8:2} + $((midpoint + OFFSET_START_HOUR)) hours" +%Y%m%d%H)
                            source_file="ocn_lead1_${vdate_mid:0:4}_${vdate_mid:4:2}_${vdate_mid:6:2}_${vdate_mid:8:2}.nc"
                        else
                            vdate_mid=$(date --utc -d "${current_cycle:0:8} ${current_cycle:8:2} + ${midpoint} hours" +%Y%m%d%H)
                            source_file="ocn_${vdate_mid:0:4}_${vdate_mid:4:2}_${vdate_mid:6:2}_${vdate_mid:8:2}_00.nc"
                        fi
                        dest_file="${RUN}.t${cyc}z.${interval}hr_avg.f${fhr3}.nc"
                        dest_file_log="${COMOUT_OCEAN_HISTORY}/${RUN}.t${cyc}z.${interval}hr_avg.log.f${fhr3}.txt"
                        ;;
                    gefs)
                        ((midpoint = last_fhr + interval / 2))
                        vdate_mid=$(date --utc -d "${current_cycle:0:8} ${current_cycle:8:2} + ${midpoint} hours" +%Y%m%d%H)
                        source_file="ocn_${vdate_mid:0:4}_${vdate_mid:4:2}_${vdate_mid:6:2}_${vdate_mid:8:2}_00.nc"
                        dest_file="${RUN}.t${cyc}z.${interval}hr_avg.f${fhr3}.nc"
                        dest_file_log="${COMOUT_OCEAN_HISTORY}/${RUN}.t${cyc}z.${interval}hr_avg.log.f${fhr3}.txt"
                        ;;
                    *)
                        echo "FATAL ERROR: Unsupported RUN ${RUN} in MOM6 postdet"
                        exit 25
                        ;;
                esac

                ocn_local="${DATAoutput}/MOM6_OUTPUT/${source_file}"
                ocn_com="${COMOUT_OCEAN_HISTORY}/${dest_file}"

                # Forecast manager copies from DATA to COM; register in product table.
                # Others: build a copy cmdfile; MOM6_out will copy files from DATA to COM
                # after the forecast completes.
                if [[ "${use_mgr_ocn}" == "YES" ]]; then
                    # Model-log-triggered: local_log (source_file_log) is the MOM6 period log
                    # written by the model after the .nc is complete. Manager polls for it,
                    # copies the .nc to COM, then copies the log to COM as the Rocoto sentinel.
                    echo "${ocn_local} ${source_file_log} ${ocn_com} ${dest_file_log}" >> "${ocn_table}"
                else
                    echo "cpfs ${ocn_local} ${ocn_com}" >> "${ocn_hist_cmdfile}"
                    echo "cpfs ${source_file_log} ${dest_file_log}" >> "${ocn_hist_cmdfile}"
                fi

                last_fhr=${fhr}
            done
            ;;

        *)
            echo "FATAL ERROR: Don't know how to copy MOM output files for RUN ${RUN}"
            exit 25
            ;;
    esac

    echo "SUB ${FUNCNAME[0]}: MOM6 input data linked/copied"

}

MOM6_nml() {
    echo "SUB ${FUNCNAME[0]}: Creating name list for MOM6"
    source "${USHgfs}/parsing_namelists_MOM6.sh"
    MOM6_namelists
}

MOM6_out() {
    echo "SUB ${FUNCNAME[0]}: Copying output data for MOM6"

    # Copy MOM_input from DATA to COMOUT_CONF after the forecast is run (and successfull)
    cpfs "${DATA}/INPUT/MOM_input" "${COMOUT_CONF}/ufs.MOM_input"
    # Copy runtime configuration of MOM: MOM_parameter_doc.all that was used in the forecast
    if [[ -f "${DATA}/MOM6_OUTPUT/MOM_parameter_doc.all" ]]; then
        cpfs "${DATA}/MOM6_OUTPUT/MOM_parameter_doc.all" "${COMOUT_CONF}/MOM_parameter_doc.all"
    fi

    # Create a list of MOM6 restart files
    # Coarser than 1/2 degree has a single MOM restart
    local mom6_restart_files mom6_restart_file restart_file
    mom6_restart_files=(MOM.res.nc)
    # 1/4 degree resolution has 3 additional restarts
    case "${OCNRES}" in
        "025")
            local nn
            for ((nn = 1; nn <= 3; nn++)); do
                mom6_restart_files+=("MOM.res_${nn}.nc")
            done
            ;;
        *) ;;
    esac

    # Build MPMD cmdfile to copy MOM6 restarts in parallel
    local cmdfile="${DATA}/cmdfile_mom6_out"
    rm -f "${cmdfile}"

    case ${RUN} in
        gdas | enkfgdas | enkfgfs) # Copy restarts for the next cycle for RUN=gdas|enkfgdas|enkfgfs
            local restart_date
            restart_date="${model_start_date_next_cycle}"
            echo "Copying MOM6 restarts for 'RUN=${RUN}' at ${restart_date}"
            for mom6_restart_file in "${mom6_restart_files[@]}"; do
                restart_file="${restart_date:0:8}.${restart_date:8:2}0000.${mom6_restart_file}"
                echo "cpfs ${DATArestart}/MOM6_RESTART/${restart_file} ${COMOUT_OCEAN_RESTART}/${restart_file}" >> "${cmdfile}"
            done
            ;;
        gfs | gefs | sfs | gcafs) # Copy MOM6 restarts at the end of the forecast segment to COM for RUN=gfs|gefs|sfs
            if [[ "${COPY_FINAL_RESTARTS}" == "YES" ]]; then
                local restart_file
                echo "Copying MOM6 restarts for 'RUN=${RUN}' at ${forecast_end_cycle}"
                for mom6_restart_file in "${mom6_restart_files[@]}"; do
                    restart_file="${forecast_end_cycle:0:8}.${forecast_end_cycle:8:2}0000.${mom6_restart_file}"
                    echo "cpfs ${DATArestart}/MOM6_RESTART/${restart_file} ${COMOUT_OCEAN_RESTART}/${restart_file}" >> "${cmdfile}"
                done
            fi
            ;;
        *)
            echo "FATAL ERROR: Not sure how to copy restart files for RUN ${RUN}"
            exit 25
            ;;
    esac

    if [[ -s "${cmdfile}" ]]; then
        if [[ ! -d "${COMOUT_OCEAN_RESTART}" ]]; then
            echo "INFO: Directory ${COMOUT_OCEAN_RESTART} does not exist, creating..."
            mkdir -p "${COMOUT_OCEAN_RESTART}"
        fi
        "${USHgfs}/run_mpmd.sh" "${cmdfile}" && true
        export err=$?
        if [[ ${err} -ne 0 ]]; then
            err_exit "run_mpmd.sh failed to copy MOM6 restart files!"
        fi
    fi

    # Copy MOM6 history files and log sentinels from DATA to COM for non-manager runs
    # (gefs, sfs, gcafs, enkfgfs). For manager runs this is handled by the forecast manager.
    local ocn_hist_cmdfile="${DATA}/cmdfile_mom6_hist"
    if [[ -s "${ocn_hist_cmdfile}" ]]; then
        if [[ ! -d "${COMOUT_OCEAN_HISTORY}" ]]; then
            echo "INFO: Directory ${COMOUT_OCEAN_HISTORY} does not exist, creating..."
            mkdir -p "${COMOUT_OCEAN_HISTORY}"
        fi
        "${USHgfs}/run_mpmd.sh" "${ocn_hist_cmdfile}" && true
        export err=$?
        if [[ ${err} -ne 0 ]]; then
            err_exit "run_mpmd.sh failed to copy MOM6 history files!"
        fi
        echo "SUB ${FUNCNAME[0]}: MOM6 history files copied to COM"
    fi
}

################################################################################
# forecast_postdet_cice.sh
################################################################################

# shellcheck disable=SC2034
# shellcheck disable=SC2178

CICE_postdet() {
    echo "SUB ${FUNCNAME[0]}: CICE after run type determination"

    local restart_date cice_restart_file
    if [[ "${RERUN}" == "YES" ]]; then
        restart_date="${RERUN_DATE}"
        local seconds
        seconds=$(to_seconds "${restart_date:8:2}0000") # convert HHMMSS to seconds
        cice_restart_file="${DATArestart}/CICE_RESTART/cice_model.res.${restart_date:0:4}-${restart_date:4:2}-${restart_date:6:2}-${seconds}.nc"
    else # "${RERUN}" == "NO"
        restart_date="${model_start_date_current_cycle}"
        cice_restart_file="${COMIN_ICE_RESTART_PREV}/${restart_date:0:8}.${restart_date:8:2}0000.cice_model.res.nc"
        if [[ "${DO_JEDIOCNVAR:-NO}" == "YES" ]]; then
            if [[ "${MEMBER}" -eq 0 ]]; then
                # Start the deterministic from the JEDI/SOCA analysis if the Marine DA in ON
                cice_restart_file="${COMIN_ICE_ANALYSIS}/${restart_date:0:8}.${restart_date:8:2}0000.analysis.cice_model.res.nc"
            elif [[ "${MEMBER}" -gt 0 ]] && [[ "${DO_STARTMEM_FROM_JEDIICE:-NO}" == "YES" ]]; then
                # Ignore the JEDI/SOCA ensemble analysis for the ensemble members if DO_START_FROM_JEDIICE is OFF
                cice_restart_file="${COMIN_ICE_ANALYSIS}/${restart_date:0:8}.${restart_date:8:2}0000.analysis.cice_model.res.nc"
            fi
        fi
    fi

    # Copy CICE ICs
    cpreq "${cice_restart_file}" "${DATA}/cice_model.res.nc"

    # Determine whether to use the forecast manager for CICE output.
    local use_mgr_ice="NO"
    case "${RUN}" in
        gfs) use_mgr_ice="YES" ;;
        # TODO: enable forecast manager for gdas, enkfgdas, enkfgfs, gefs, sfs, gcafs once tested
        *) ;;
    esac
    local ice_table="${DATAjob}/ice_products_seg${FCST_SEGMENT:-0}.txt"
    local ice_hist_cmdfile="${DATA}/cmdfile_cice_hist"
    rm -f "${ice_table}" "${ice_hist_cmdfile}"

    local vdate seconds vdatestr fhr fhr3 interval

    # iceh_ic: CICE initial condition snapshot (write_ic=.true. in namelist).
    # No per-period sentinel exists; the manager cannot track it in-flight.
    seconds=$(to_seconds "${model_start_date_current_cycle:8:2}0000") # convert HHMMSS to seconds
    vdatestr="${model_start_date_current_cycle:0:4}-${model_start_date_current_cycle:4:2}-${model_start_date_current_cycle:6:2}-${seconds}"
    if [[ "${use_mgr_ice}" == "YES" ]]; then
        # iceh_ic is written during CICE initialization before any time stepping.
        # Use the first forecast-hour ice output as the trigger (same pattern as
        # non-last entries in the loop below). iceh_ic is fully written before
        # f006 appears, ensuring a complete copy.
        local ic_trigger_fhr=${CICE_OUTPUT_FH[1]}
        local ic_trigger_vdate ic_trigger_sec ic_trigger_vdstr ic_trigger
        ic_trigger_vdate=$(date --utc -d "${current_cycle:0:8} ${current_cycle:8:2} + ${ic_trigger_fhr} hours" +%Y%m%d%H)
        ic_trigger_sec=$(to_seconds "${ic_trigger_vdate:8:2}0000")
        ic_trigger_vdstr="${ic_trigger_vdate:0:4}-${ic_trigger_vdate:4:2}-${ic_trigger_vdate:6:2}-${ic_trigger_sec}"
        case "${RUN}" in
            gfs | enkfgfs | sfs | gcafs)
                ic_trigger="${DATAoutput}/CICE_OUTPUT/iceh_$(printf "%0.2d" "${FHOUT_ICE}")h.${ic_trigger_vdstr}.nc"
                ;;
            gefs)
                ic_trigger="${DATAoutput}/CICE_OUTPUT/iceh.${ic_trigger_vdstr}.nc"
                ;;
            *)
                echo "FATAL ERROR: Unsupported RUN ${RUN} for iceh_ic trigger in CICE postdet" >&2
                exit 10
                ;;
        esac
        echo "${DATAoutput}/CICE_OUTPUT/iceh_ic.${vdatestr}.nc" \
            "${ic_trigger}" \
            "${COMOUT_ICE_HISTORY}/${RUN}.t${cyc}z.ic.nc" \
            "${COMOUT_ICE_HISTORY}/${RUN}.t${cyc}z.log.ice.ic.txt" >> "${ice_table}"
    else
        # Non-manager: NLN so the model writes directly into COM via symlink.
        if [[ ! -d "${COMOUT_ICE_HISTORY}" ]]; then mkdir -p "${COMOUT_ICE_HISTORY}"; fi
        ${NLN} "${COMOUT_ICE_HISTORY}/${RUN}.t${cyc}z.ic.nc" "${DATAoutput}/CICE_OUTPUT/iceh_ic.${vdatestr}.nc"
    fi

    # Build CICE product table entries for each forecast hour.
    # Column layout: local_data  local_trigger  com_data  com_log
    #   local_trigger: next hour's output file for non-last entries, or
    #                  fcst_done_seg for the last entry.  The manager writes
    #                  com_log synthetically when the trigger appears.
    local source_file dest_file
    local n_fhr=${#CICE_OUTPUT_FH[@]}
    for ((idx = 1; idx < n_fhr; idx++)); do
        fhr=${CICE_OUTPUT_FH[idx]}
        local prev_fhr=${CICE_OUTPUT_FH[idx - 1]}
        ((interval = fhr - prev_fhr))
        fhr3=$(printf %03i "${fhr}")

        vdate=$(date --utc -d "${current_cycle:0:8} ${current_cycle:8:2} + ${fhr} hours" +%Y%m%d%H)
        seconds=$(to_seconds "${vdate:8:2}0000") # convert HHMMSS to seconds
        vdatestr="${vdate:0:4}-${vdate:4:2}-${vdate:6:2}-${seconds}"

        case "${RUN}" in
            gdas | enkfgdas)
                source_file="iceh_inst.${vdatestr}.nc"
                dest_file="${RUN}.t${cyc}z.${interval}hr_avg.f${fhr3}.nc"
                ;;
            gfs | enkfgfs | sfs | gcafs)
                source_file="iceh_$(printf "%0.2d" "${FHOUT_ICE}")h.${vdatestr}.nc"
                dest_file="${RUN}.t${cyc}z.${interval}hr_avg.f${fhr3}.nc"
                ;;
            gefs)
                source_file="iceh.${vdatestr}.nc"
                dest_file="${RUN}.t${cyc}z.${interval}hr_avg.f${fhr3}.nc"
                ;;
            *)
                echo "FATAL ERROR: Unsupported RUN ${RUN} in CICE postdet"
                exit 10
                ;;
        esac

        local ice_local="${DATAoutput}/CICE_OUTPUT/${source_file}"
        local ice_com="${COMOUT_ICE_HISTORY}/${dest_file}"
        local ice_log_local ice_log_com
        ice_log_com="${COMOUT_ICE_HISTORY}/${RUN}.t${cyc}z.log.ice.f${fhr3}.txt"
        if [[ "${use_mgr_ice}" == "YES" ]]; then
            if [[ $((idx + 1)) -lt n_fhr ]]; then
                # Non-last: trigger = next forecast hour's ice output on DATA.
                local next_fhr=${CICE_OUTPUT_FH[idx + 1]}
                local next_vdate next_sec next_vdstr
                next_vdate=$(date --utc -d "${current_cycle:0:8} ${current_cycle:8:2} + ${next_fhr} hours" +%Y%m%d%H)
                next_sec=$(to_seconds "${next_vdate:8:2}0000")
                next_vdstr="${next_vdate:0:4}-${next_vdate:4:2}-${next_vdate:6:2}-${next_sec}"
                case "${RUN}" in
                    gfs | enkfgfs | sfs | gcafs)
                        ice_log_local="${DATAoutput}/CICE_OUTPUT/iceh_$(printf "%0.2d" "${FHOUT_ICE}")h.${next_vdstr}.nc"
                        ;;
                    gefs)
                        ice_log_local="${DATAoutput}/CICE_OUTPUT/iceh.${next_vdstr}.nc"
                        ;;
                    *)
                        echo "FATAL ERROR: Unsupported RUN ${RUN} in CICE postdet ice trigger"
                        exit 10
                        ;;
                esac
            else
                # Last forecast hour: trigger = forecast completion sentinel.
                ice_log_local="${DATAjob}/fcst_done_seg${FCST_SEGMENT:-0}"
            fi
            echo "${ice_local} ${ice_log_local} ${ice_com} ${ice_log_com}" >> "${ice_table}"
        else
            echo "cpfs ${ice_local} ${ice_com}" >> "${ice_hist_cmdfile}"
        fi
    done

    # When the manager is not used for ICE, create the (empty) product table so
    # the forecast manager does not wait for a file that is never written.
    if [[ "${use_mgr_ice}" == "NO" ]]; then
        touch "${ice_table}"
    fi
}

CICE_nml() {
    echo "SUB ${FUNCNAME[0]}: Creating name list for CICE"
    source "${USHgfs}/parsing_namelists_CICE.sh"
    CICE_namelists
}

CICE_out() {
    echo "SUB ${FUNCNAME[0]}: Copying output data for CICE"

    # Copy ice_in namelist from DATA to COMOUT_CONF after the forecast is run (and successfull)
    cpfs "${DATA}/ice_in" "${COMOUT_CONF}/ufs.ice_in"

    # Build MPMD cmdfile to copy CICE restarts in parallel
    local cmdfile="${DATA}/cmdfile_cice_out"
    rm -f "${cmdfile}"

    case ${RUN} in
        gdas | enkfgdas | enkfgfs) # Copy restarts for next cycle for RUN=gdas|enkfgdas|enkfgfs
            local restart_date
            restart_date="${model_start_date_next_cycle}"
            echo "Copying CICE restarts for 'RUN=${RUN}' at ${restart_date}"
            seconds=$(to_seconds "${restart_date:8:2}0000") # convert HHMMSS to seconds
            source_file="cice_model.res.${restart_date:0:4}-${restart_date:4:2}-${restart_date:6:2}-${seconds}.nc"
            target_file="${restart_date:0:8}.${restart_date:8:2}0000.cice_model.res.nc"
            echo "cpfs ${DATArestart}/CICE_RESTART/${source_file} ${COMOUT_ICE_RESTART}/${target_file}" >> "${cmdfile}"
            ;;
        gfs | gefs | sfs | gcafs) # Copy CICE restarts at the end of the forecast segment to COM for RUN=gfs|gefs|sfs|gcafs
            if [[ "${COPY_FINAL_RESTARTS}" == "YES" ]]; then
                local seconds source_file target_file
                echo "Copying CICE restarts for 'RUN=${RUN}' at ${forecast_end_cycle}"
                seconds=$(to_seconds "${forecast_end_cycle:8:2}0000") # convert HHMMSS to seconds
                source_file="cice_model.res.${forecast_end_cycle:0:4}-${forecast_end_cycle:4:2}-${forecast_end_cycle:6:2}-${seconds}.nc"
                target_file="${forecast_end_cycle:0:8}.${forecast_end_cycle:8:2}0000.cice_model.res.nc"
                echo "cpfs ${DATArestart}/CICE_RESTART/${source_file} ${COMOUT_ICE_RESTART}/${target_file}" >> "${cmdfile}"
            fi
            ;;
        *)
            echo "FATAL ERROR: Not sure how to copy restart files for RUN ${RUN}"
            exit 25
            ;;
    esac

    if [[ -s "${cmdfile}" ]]; then
        if [[ ! -d "${COMOUT_ICE_RESTART}" ]]; then
            echo "INFO: Directory ${COMOUT_ICE_RESTART} does not exist, creating..."
            mkdir -p "${COMOUT_ICE_RESTART}"
        fi
        "${USHgfs}/run_mpmd.sh" "${cmdfile}" && true
        export err=$?
        if [[ ${err} -ne 0 ]]; then
            err_exit "run_mpmd.sh failed to copy CICE restart files!"
        fi
    fi

    # Copy CICE history files and log sentinels from DATA to COM for non-manager runs
    # (gefs, sfs, gcafs, enkfgfs). For manager runs this is handled by the forecast manager.
    local ice_hist_cmdfile="${DATA}/cmdfile_cice_hist"
    if [[ -s "${ice_hist_cmdfile}" ]]; then
        if [[ ! -d "${COMOUT_ICE_HISTORY}" ]]; then
            echo "INFO: Directory ${COMOUT_ICE_HISTORY} does not exist, creating..."
            mkdir -p "${COMOUT_ICE_HISTORY}"
        fi
        "${USHgfs}/run_mpmd.sh" "${ice_hist_cmdfile}" && true
        export err=$?
        if [[ ${err} -ne 0 ]]; then
            err_exit "run_mpmd.sh failed to copy CICE history files!"
        fi
        echo "SUB ${FUNCNAME[0]}: CICE history files copied to COM"
    fi
}

################################################################################
# forecast_postdet_gocart.sh
################################################################################

# shellcheck disable=SC2034
# shellcheck disable=SC2178

GOCART_rc() {
    echo "SUB ${FUNCNAME[0]}: Linking input data and copying config files for GOCART"
    # set input directory containing GOCART input data and configuration files
    # this variable is platform-dependent and should be set via a YAML file

    # link directory containing GOCART input dataset, if provided
    if [[ -n "${AERO_INPUTS_DIR}" ]]; then
        #TODO: add only necessary files and remove unneeded ones to minimize data volume
        ${NLN} "${AERO_INPUTS_DIR}" "${DATA}/ExtData"
        status=$?
        if [[ ${status} -ne 0 ]]; then
            exit "${status}"
        fi
    fi

    source "${USHgfs}/parsing_namelists_GOCART.sh"
    GOCART_namelists
}

GOCART_postdet() {
    echo "SUB ${FUNCNAME[0]}: Linking output data for GOCART"

    local vdate
    for fhr in $(GOCART_output_fh); do
        vdate=$(date --utc -d "${current_cycle:0:8} ${current_cycle:8:2} + ${fhr} hours" +%Y%m%d%H)

        # Temporarily delete existing files due to noclobber in GOCART
        local file_types=("inst_aod" "inst_du_ss" "inst_ca" "inst_ni" "inst_su"
            "inst_du_bin" "inst_ss_bin" "inst_ca_bin" "inst_ni_bin" "inst_su_bin"
            "inst_2d" "inst_3d" "tavg_du_ss" "tavg_du_bin" "tavg_2d_rad" "tavg_3d_rad")
        for file_type in "${file_types[@]}"; do
            if [[ -e "${COMOUT_CHEM_HISTORY}/gocart.${file_type}.${vdate:0:8}_${vdate:8:2}00z.nc4" ]]; then
                rm -f "${COMOUT_CHEM_HISTORY}/gocart.${file_type}.${vdate:0:8}_${vdate:8:2}00z.nc4"
            fi
        done

        #TODO: Temporarily removing this as this will crash gocart, adding copy statement at the end
        #${NLN} "${COMOUT_CHEM_HISTORY}/gocart.inst_aod.${vdate:0:8}_${vdate:8:2}00z.nc4" \
        #       "${DATA}/gocart.inst_aod.${vdate:0:8}_${vdate:8:2}00z.nc4"
    done
}

GOCART_output_fh() {
    # This has to be called during postdet after FHROT has been set
    local aero_min
    local gocart_output_fh
    # GOCART produces no AOD files at the initial forecast time, so start the time
    #   after the forecast start (accounting for FHROT)
    aero_min=$((${IAU_FHROT:-0} > FHMIN ? IAU_FHROT + FHOUT_AERO : FHMIN + FHOUT_AERO))
    gocart_output_fh=$(seq -s ' ' "$((aero_min))" "${FHOUT_AERO}" "${GOCART_MAX}")

    echo "${gocart_output_fh}"
}

GOCART_out() {
    echo "SUB ${FUNCNAME[0]}: Copying output data for GOCART"

    # Copy gocart.inst_aod after the forecast is run (and successfull)
    # TODO: this should be linked but there are issues where gocart crashing if it is linked
    local fhr
    local vdate

    local file_types=("inst_aod" "inst_du_ss" "inst_ca" "inst_ni" "inst_su"
        "inst_du_bin" "inst_ss_bin" "inst_ca_bin" "inst_ni_bin" "inst_su_bin"
        "inst_2d" "inst_3d" "tavg_du_ss" "tavg_du_bin" "tavg_2d_rad" "tavg_3d_rad")

    # Build MPMD cmdfile to copy GOCART output files in parallel
    local cmdfile="${DATA}/cmdfile_gocart_out"
    rm -f "${cmdfile}"

    for fhr in $(GOCART_output_fh); do
        vdate=$(date --utc -d "${current_cycle:0:8} ${current_cycle:8:2} + ${fhr} hours" +%Y%m%d%H)

        for file_type in "${file_types[@]}"; do
            if [[ -e "${DATA}/gocart.${file_type}.${vdate:0:8}_${vdate:8:2}00z.nc4" ]]; then
                echo "cpfs ${DATA}/gocart.${file_type}.${vdate:0:8}_${vdate:8:2}00z.nc4 ${COMOUT_CHEM_HISTORY}/gocart.${file_type}.${vdate:0:8}_${vdate:8:2}00z.nc4" >> "${cmdfile}"
            fi
        done
    done

    if [[ -s "${cmdfile}" ]]; then
        if [[ ! -d "${COMOUT_CHEM_HISTORY}" ]]; then
            echo "INFO: Directory ${COMOUT_CHEM_HISTORY} does not exist, creating..."
            mkdir -p "${COMOUT_CHEM_HISTORY}"
        fi
        "${USHgfs}/run_mpmd.sh" "${cmdfile}" && true
        export err=$?
        if [[ ${err} -ne 0 ]]; then
            err_exit "run_mpmd.sh failed to copy GOCART output files!"
        fi
    fi
}

# shellcheck disable=SC2178
CMEPS_postdet() {
    echo "SUB ${FUNCNAME[0]}: Linking output data for CMEPS mediator"

    if [[ "${warm_start}" == ".true." ]]; then

        # Determine the appropriate restart file
        local restart_date cmeps_restart_file
        if [[ "${RERUN}" == "YES" ]]; then
            restart_date="${RERUN_DATE}"
            local seconds
            seconds=$(to_seconds "${restart_date:8:2}0000") # convert HHMMSS to seconds
            cmeps_restart_file="${DATArestart}/CMEPS_RESTART/ufs.cpld.cpl.r.${restart_date:0:4}-${restart_date:4:2}-${restart_date:6:2}-${seconds}.nc"
        else # "${RERUN}" == "NO"
            restart_date="${model_start_date_current_cycle}"
            cmeps_restart_file="${COMIN_MED_RESTART_PREV}/${restart_date:0:8}.${restart_date:8:2}0000.ufs.cpld.cpl.r.nc"
        fi

        # Copy CMEPS restarts
        if [[ -f "${cmeps_restart_file}" ]]; then
            cpreq "${cmeps_restart_file}" "${DATA}/ufs.cpld.cpl.r.nc"
            rm -f "${DATA}/rpointer.cpl"
            touch "${DATA}/rpointer.cpl"
            echo "ufs.cpld.cpl.r.nc" >> "${DATA}/rpointer.cpl"
        else
            # We have a choice to make here.
            # Either we can FATAL ERROR out, or we can let the coupling fields initialize from zero
            # cmeps_run_type is determined based on the availability of the CMEPS restart file
            echo "WARNING: CMEPS restart file '${cmeps_restart_file}' not found for warm_start='${warm_start}', will initialize!"
            if [[ "${RERUN}" == "YES" ]]; then
                # In the case of a RERUN, the CMEPS restart file is required
                echo "FATAL ERROR: CMEPS restart file '${cmeps_restart_file}' not found for RERUN='${RERUN}', ABORT!"
                exit 1
            fi
        fi

    fi # [[ "${warm_start}" == ".true." ]];

    # For CMEPS, CICE, MOM6 and WW3 determine restart writes
    # Note FV3 has its own restart intervals
    cmeps_restart_interval=${restart_interval:-${FHMAX}}
    # restart_interval = 0 implies write restart at the END of the forecast i.e. at FHMAX
    # Convert restart interval into an explicit list for CMEPS/CICE/MOM6/WW3
    # Note, this must be computed after determination IAU in forecast_det and fhrot.
    if ((cmeps_restart_interval == 0)); then
        if [[ "${DOIAU:-NO}" == "YES" ]]; then
            CMEPS_RESTART_FH=$((FHMAX + half_window))
        else
            CMEPS_RESTART_FH=("${FHMAX}")
        fi
    else
        if [[ "${DOIAU:-NO}" == "YES" ]]; then
            if [[ "${MODE}" = "cycled" && "${SDATE}" = "${PDY}${cyc}" && ${EXP_WARM_START} = ".false." ]]; then
                local restart_interval_start=${cmeps_restart_interval}
                local restart_interval_end=${FHMAX}
            else
                local restart_interval_start=$((cmeps_restart_interval + half_window))
                local restart_interval_end=$((FHMAX + half_window))
            fi
        else
            local restart_interval_start=${cmeps_restart_interval}
            local restart_interval_end=${FHMAX}
        fi
        CMEPS_RESTART_FH="$(seq -s ' ' "${restart_interval_start}" "${cmeps_restart_interval}" "${restart_interval_end}")"
    fi
    export CMEPS_RESTART_FH
    # TODO: For GEFS, once cycling waves "self-cycles" and therefore needs to have a restart at 6 hour

}

CMEPS_out() {
    echo "SUB ${FUNCNAME[0]}: Copying output data for CMEPS mediator"

    # Build MPMD cmdfile to copy CMEPS mediator restarts in parallel
    local cmdfile="${DATA}/cmdfile_cmeps_out"
    rm -f "${cmdfile}"

    case ${RUN} in
        gdas | enkfgdas | enkfgfs) # Copy restarts for the next cycle to COM
            local restart_date
            restart_date="${model_start_date_next_cycle}"
            echo "Copying mediator restarts for 'RUN=${RUN}' at ${restart_date}"
            seconds=$(to_seconds "${restart_date:8:2}"0000)
            source_file="ufs.cpld.cpl.r.${restart_date:0:4}-${restart_date:4:2}-${restart_date:6:2}-${seconds}.nc"
            target_file="${restart_date:0:8}.${restart_date:8:2}0000.ufs.cpld.cpl.r.nc"
            if [[ -f "${DATArestart}/CMEPS_RESTART/${source_file}" ]]; then
                echo "cpfs ${DATArestart}/CMEPS_RESTART/${source_file} ${COMOUT_MED_RESTART}/${target_file}" >> "${cmdfile}"
            else
                echo "Mediator restart '${DATArestart}/CMEPS_RESTART/${source_file}' not found."
            fi
            ;;
        gfs | gefs | sfs | gcafs) # Copy mediator restarts at the end of the forecast segment
            if [[ "${COPY_FINAL_RESTARTS}" == "YES" ]]; then
                echo "Copying mediator restarts for 'RUN=${RUN}' at ${forecast_end_cycle}"
                local seconds source_file target_file
                seconds=$(to_seconds "${forecast_end_cycle:8:2}"0000)
                source_file="ufs.cpld.cpl.r.${forecast_end_cycle:0:4}-${forecast_end_cycle:4:2}-${forecast_end_cycle:6:2}-${seconds}.nc"
                target_file="${forecast_end_cycle:0:8}.${forecast_end_cycle:8:2}0000.ufs.cpld.cpl.r.nc"
                if [[ -f "${DATArestart}/CMEPS_RESTART/${source_file}" ]]; then
                    echo "cpfs ${DATArestart}/CMEPS_RESTART/${source_file} ${COMOUT_MED_RESTART}/${target_file}" >> "${cmdfile}"
                else
                    echo "Mediator restart '${DATArestart}/CMEPS_RESTART/${source_file}' not found."
                fi
            fi
            ;;
        *)
            echo "FATAL ERROR: Not sure how to copy restart files for RUN ${RUN}"
            exit 25
            ;;
    esac

    if [[ -s "${cmdfile}" ]]; then
        if [[ ! -d "${COMOUT_MED_RESTART}" ]]; then
            mkdir -p "${COMOUT_MED_RESTART}"
        fi
        "${USHgfs}/run_mpmd.sh" "${cmdfile}" && true
        export err=$?
        if [[ ${err} -ne 0 ]]; then
            err_exit "run_mpmd.sh failed to copy CMEPS mediator restart files!"
        fi
    fi
}
