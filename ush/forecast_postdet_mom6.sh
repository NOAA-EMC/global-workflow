#! /usr/bin/env bash

# shellcheck disable=SC2034
# shellcheck disable=SC2178

MOM6_postdet() {
    echo "SUB ${FUNCNAME[0]}: MOM6 after run type determination"

    local restart_dir restart_date
    if [[ "${RERUN}" == "YES" ]]; then
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
        gfs | enkfgfs | gefs | sfs | gcafs) # Link output files for RUN=gfs|enkfgfs|gefs|sfs
            # Looping over MOM6 output hours
            local fhr fhr3 last_fhr interval midpoint vdate vdate_mid source_file dest_file
            local use_mgr_ocn="NO"
            case "${RUN}" in
                gfs) use_mgr_ocn="YES" ;;
                # TODO: enable forecast manager for enkfgfs, gefs, sfs, gcafs once tested
                # enkfgfs | gefs | sfs | gcafs) use_mgr_ocn="YES" ;;
            esac
            local ocn_table="${DATA}/ocn_products.txt"
            if [[ "${use_mgr_ocn}" == "YES" ]]; then
                rm -f "${ocn_table}"
                rm -f "${COMOUT_CONF}/ocn_products_seg${FCST_SEGMENT:-0}.txt"
            fi
            for fhr in ${MOM6_OUTPUT_FH}; do
                fhr3=$(printf %03i "${fhr}")

                if [[ -z ${last_fhr:-} ]]; then
                    last_fhr=${fhr}
                    continue
                fi

                ((interval = fhr - last_fhr))
                ((midpoint = last_fhr + interval / 2))

                vdate=$(date --utc -d "${current_cycle:0:8} ${current_cycle:8:2} + ${fhr} hours" +%Y%m%d%H)
                #If OFFSET_START_HOUR is greater than 0, OFFSET_START_HOUR should be added to the midpoint for first lead time
                if ((OFFSET_START_HOUR > 0)) && ((fhr == FHOUT_OCN)); then
                    vdate_mid=$(date --utc -d "${current_cycle:0:8} ${current_cycle:8:2} + $((midpoint + OFFSET_START_HOUR)) hours" +%Y%m%d%H)
                else
                    vdate_mid=$(date --utc -d "${current_cycle:0:8} ${current_cycle:8:2} + ${midpoint} hours" +%Y%m%d%H)
                fi

                # Native model output uses window midpoint in the filename, but we are mapping that to the end of the period for COM
                if ((OFFSET_START_HOUR > 0)) && ((fhr == FHOUT_OCN)); then
                    source_file="ocn_lead1_${vdate_mid:0:4}_${vdate_mid:4:2}_${vdate_mid:6:2}_${vdate_mid:8:2}.nc"
                else
                    source_file="ocn_${vdate_mid:0:4}_${vdate_mid:4:2}_${vdate_mid:6:2}_${vdate_mid:8:2}.nc"
                fi
                dest_file="${RUN}.t${cyc}z.${interval}hr_avg.f${fhr3}.nc"
                local ocn_local="${DATAoutput}/MOM6_OUTPUT/${source_file}"
                local ocn_com="${COMOUT_OCEAN_HISTORY}/${dest_file}"
                if [[ "${use_mgr_ocn}" == "YES" ]]; then
                    # Self-sentinel: MOM6 writes complete netCDF files atomically per output
                    # period. The file itself signals readiness; no separate log needed.
                    echo "${ocn_local} ${ocn_local} ${ocn_com} ${ocn_com}" >> "${ocn_table}"
                fi
                # For enkfgfs/gefs/sfs/gcafs: MOM6_out copies files to COM after the run.

                last_fhr=${fhr}

            done
            if [[ "${use_mgr_ocn}" == "YES" ]] && [[ -s "${ocn_table}" ]]; then
                mkdir -p "${COMOUT_CONF}"
                cpfs "${ocn_table}" "${COMOUT_CONF}/ocn_products_seg${FCST_SEGMENT:-0}.txt"
            fi
            ;;

        gdas | enkfgdas) # Link output files for RUN=gdas|enkfgdas
            # Save (instantaneous) MOM6 backgrounds
            local fhr3 vdatestr
            for fhr in ${MOM6_OUTPUT_FH}; do
                fhr3=$(printf %03i "${fhr}")
                vdatestr=$(date --utc -d "${current_cycle:0:8} ${current_cycle:8:2} + ${fhr} hours" +%Y_%m_%d_%H)
                # NLN symlink: GDAS analysis jobs need ocean backgrounds during the run
                ${NLN} "${COMOUT_OCEAN_HISTORY}/${RUN}.t${cyc}z.inst.f${fhr3}.nc" "${DATAoutput}/MOM6_OUTPUT/ocn_da_${vdatestr}.nc"
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
    source "${USHglobal}/parsing_namelists_MOM6.sh"
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

        "${USHglobal}/run_mpmd.sh" "${cmdfile}" && true
        export err=$?
        if [[ ${err} -ne 0 ]]; then
            err_exit "run_mpmd.sh failed to copy MOM6 restart files!"
        fi
    fi

    # Copy MOM6 history files for GFS/GEFS/SFS/GCAFS (no pre-run symlinks;
    # model writes real files to DATAoutput/MOM6_OUTPUT).
    # For GFS: if the OCN product table was written during pre-run, the
    # forecast manager handles copy in real-time; write the ready sentinel here.
    # For other systems (enkfgfs/gefs/sfs/gcafs): copy directly as before.
    local mom6_hist_helper
    mom6_hist_helper() {
        local cmdfile_mom6_hist="${DATA}/cmdfile_mom6_hist"
        rm -f "${cmdfile_mom6_hist}"
        local last_fhr_hist fhr_hist fhr3_hist interval_hist midpoint_hist vdate_hist vdate_mid_hist source_file_hist dest_file_hist
        for fhr_hist in ${MOM6_OUTPUT_FH}; do
            fhr3_hist=$(printf %03i "${fhr_hist}")
            if [[ -z ${last_fhr_hist:-} ]]; then
                last_fhr_hist=${fhr_hist}
                continue
            fi
            (( interval_hist = fhr_hist - last_fhr_hist ))
            (( midpoint_hist = last_fhr_hist + interval_hist / 2 ))
            vdate_hist=$(date --utc -d "${current_cycle:0:8} ${current_cycle:8:2} + ${fhr_hist} hours" +%Y%m%d%H)
            if (( OFFSET_START_HOUR > 0 )) && (( fhr_hist == FHOUT_OCN )); then
                vdate_mid_hist=$(date --utc -d "${current_cycle:0:8} ${current_cycle:8:2} + $(( midpoint_hist + OFFSET_START_HOUR )) hours" +%Y%m%d%H)
            else
                vdate_mid_hist=$(date --utc -d "${current_cycle:0:8} ${current_cycle:8:2} + ${midpoint_hist} hours" +%Y%m%d%H)
            fi
            if (( OFFSET_START_HOUR > 0 )) && (( fhr_hist == FHOUT_OCN )); then
                source_file_hist="ocn_lead1_${vdate_mid_hist:0:4}_${vdate_mid_hist:4:2}_${vdate_mid_hist:6:2}_${vdate_mid_hist:8:2}.nc"
            else
                source_file_hist="ocn_${vdate_mid_hist:0:4}_${vdate_mid_hist:4:2}_${vdate_mid_hist:6:2}_${vdate_mid_hist:8:2}.nc"
            fi
            dest_file_hist="${RUN}.t${cyc}z.${interval_hist}hr_avg.f${fhr3_hist}.nc"
            echo "cpfs ${DATAoutput}/MOM6_OUTPUT/${source_file_hist} ${COMOUT_OCEAN_HISTORY}/${dest_file_hist}" >> "${cmdfile_mom6_hist}"
            last_fhr_hist=${fhr_hist}
        done
        if [[ -s "${cmdfile_mom6_hist}" ]]; then
            mkdir -p "${COMOUT_OCEAN_HISTORY}"
            "${USHglobal}/run_mpmd.sh" "${cmdfile_mom6_hist}" && true
            export err=$?
            if [[ ${err} -ne 0 ]]; then
                err_exit "run_mpmd.sh failed to copy MOM6 history files!"
            fi
        fi
    }
    case "${RUN}" in
        gfs)
            if [[ -f "${COMOUT_CONF}/ocn_products_seg${FCST_SEGMENT:-0}.txt" ]]; then
                echo "INFO: OCN product table found; forecast manager handles history copy"
            else
                mom6_hist_helper
            fi
            ;;
        enkfgfs | gefs | sfs | gcafs)
            mom6_hist_helper
            ;;
    esac
    unset -f mom6_hist_helper
}

