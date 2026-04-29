#! /usr/bin/env bash

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

            if [[ "${RUN}" == "enkfgfs" ]] || [[ "${RUN}" == "enkfgdas" ]]; then
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
    if [[ "${QUILTING}" == ".true." ]] && [[ "${OUTPUT_GRID}" == "gaussian_grid" ]]; then
        local FH2 FH3
        # For GFS/GEFS/SFS/GCAFS: build a product table consumed by the forecast manager.
        # The model writes real files to DATAoutput; the manager copies them to COM.
        # For GDAS/enkfGDAS: keep NLN symlinks so analysis jobs can read outputs during the run.
        local use_mgr="NO"
        case "${RUN}" in
            gfs) use_mgr="YES" ;;
            # TODO: enable forecast manager for gefs, sfs, gcafs once tested
            # gefs | sfs | gcafs) use_mgr="YES" ;;
        esac

        local atm_table="${DATAjob}/atm_products_seg${FCST_SEGMENT:-0}.txt"
        if [[ "${use_mgr}" == "YES" ]]; then
            rm -f "${atm_table}"
            # Remove the started sentinel so the forecast manager does not trigger from a
            # previous run when this segment is rewound and re-queued.
            rm -f "${DATAjob}/fcst_started_seg${FCST_SEGMENT:-0}"
        fi

        for fhr in ${FV3_OUTPUT_FH}; do
            FH3=$(printf %03i "${fhr}")
            FH2=$(printf %02i "${fhr}")

            # Build (local_file, com_file) pairs once; used for both the manager
            # product table and the NLN symlink paths.
            local local_files=() com_files=()
            local_files+=( "${DATAoutput}/FV3ATM_OUTPUT/atmf${FH3}.nc" )
            com_files+=( "${COMOUT_ATMOS_HISTORY}/${RUN}.t${cyc}z.atm.f${FH3}.nc" )
            local_files+=( "${DATAoutput}/FV3ATM_OUTPUT/sfcf${FH3}.nc" )
            com_files+=( "${COMOUT_ATMOS_HISTORY}/${RUN}.t${cyc}z.sfc.f${FH3}.nc" )
            if [[ "${DO_JEDIATMVAR:-}" == "YES" || "${DO_HISTORY_FILE_ON_NATIVE_GRID:-"NO"}" == "YES" ]]; then
                local_files+=( "${DATAoutput}/FV3ATM_OUTPUT/cubed_sphere_grid_atmf${FH3}.nc" )
                com_files+=( "${COMOUT_ATMOS_HISTORY}/${RUN}.t${cyc}z.csg_atm.f${FH3}.nc" )
                local_files+=( "${DATAoutput}/FV3ATM_OUTPUT/cubed_sphere_grid_sfcf${FH3}.nc" )
                com_files+=( "${COMOUT_ATMOS_HISTORY}/${RUN}.t${cyc}z.csg_sfc.f${FH3}.nc" )
            fi
            if [[ "${WRITE_DOPOST}" == ".true." ]]; then
                local_files+=( "${DATAoutput}/FV3ATM_OUTPUT/GFSPRS.GrbF${FH2}" )
                com_files+=( "${COMOUT_ATMOS_MASTER}/${RUN}.t${cyc}z.master.f${FH3}.grib2" )
                local_files+=( "${DATAoutput}/FV3ATM_OUTPUT/GFSFLX.GrbF${FH2}" )
                com_files+=( "${COMOUT_ATMOS_MASTER}/${RUN}.t${cyc}z.sflux.f${FH3}.grib2" )
                if [[ "${DO_NEST:-NO}" == "YES" ]]; then
                    local_files+=( "${DATAoutput}/FV3ATM_OUTPUT/GFSPRS.GrbF${FH2}.nest02" )
                    com_files+=( "${COMOUT_ATMOS_MASTER}/${RUN}.t${cyc}z.master.nest.f${FH3}.grib2" )
                    local_files+=( "${DATAoutput}/FV3ATM_OUTPUT/GFSFLX.GrbF${FH2}.nest02" )
                    com_files+=( "${COMOUT_ATMOS_MASTER}/${RUN}.t${cyc}z.sflux.nest.f${FH3}.grib2" )
                fi
            fi

            # log.atm.fHHH is the sentinel written by the write component after
            # atmfHHH.nc and sfcfHHH.nc are fully flushed to disk.
            local local_log="${DATAoutput}/FV3ATM_OUTPUT/log.atm.f${FH3}"
            local com_log="${COMOUT_ATMOS_HISTORY}/${RUN}.t${cyc}z.log.f${FH3}.txt"
            local i
            if [[ "${use_mgr}" == "YES" ]]; then
                # Product table entries: local_data  local_log  com_data  com_log
                for (( i = 0; i < ${#local_files[@]}; i++ )); do
                    echo "${local_files[i]} ${local_log} ${com_files[i]} ${com_log}" >> "${atm_table}"
                done
            else
                # GDAS/enkfGDAS: NLN symlinks to COM so analysis jobs can read outputs during run
                for (( i = 0; i < ${#local_files[@]}; i++ )); do
                    ${NLN} "${com_files[i]}" "${local_files[i]}"
                done
                ${NLN} "${com_log}" "${local_log}"
            fi
        done

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
    if [[ -n "${FV3_RESTART_FH}" ]]; then mkdir -p "${DATArestart}/FV3_RESTART"; fi
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
}
