#! /usr/bin/env bash

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
        # TODO: enable forecast manager for enkfgfs, gefs, sfs, gcafs once tested
        # enkfgfs | gefs | sfs | gcafs) use_mgr_ice="YES" ;;
    esac
    local ice_table="${DATAjob}/ice_products_seg${FCST_SEGMENT:-0}.txt"
    rm -f "${ice_table}"

    # Register/link iceh_ic (f000 initial condition snapshot written by CICE at start of run).
    local vdate seconds vdatestr fhr fhr3 interval last_fhr
    seconds=$(to_seconds "${model_start_date_current_cycle:8:2}0000") # convert HHMMSS to seconds
    vdatestr="${model_start_date_current_cycle:0:4}-${model_start_date_current_cycle:4:2}-${model_start_date_current_cycle:6:2}-${seconds}"
    local ic_local="${DATAoutput}/CICE_OUTPUT/iceh_ic.${vdatestr}.nc"
    local ic_com="${COMOUT_ICE_HISTORY}/${RUN}.t${cyc}z.ic.nc"
    if [[ "${use_mgr_ice}" == "YES" ]]; then
        # Add to product table; forecast manager will copy the real file to COM after the run.
        echo "${ic_local} ${ic_local} ${ic_com} ${ic_com}" >> "${ice_table}"
    else
        # NLN: model writes directly into COM via symlink; create the directory first.
        if [[ ! -d "${COMOUT_ICE_HISTORY}" ]]; then
            mkdir -p "${COMOUT_ICE_HISTORY}"
        fi
        ${NLN} "${ic_com}" "${ic_local}"
    fi

    # Link/register regular CICE forecast output files.
    local source_file dest_file
    for fhr in "${CICE_OUTPUT_FH[@]}"; do

        if [[ -z ${last_fhr:-} ]]; then
            last_fhr=${fhr}
            continue
        fi

        fhr3=$(printf %03i "${fhr}")
        ((interval = fhr - last_fhr))

        vdate=$(date --utc -d "${current_cycle:0:8} ${current_cycle:8:2} + ${fhr} hours" +%Y%m%d%H)
        seconds=$(to_seconds "${vdate:8:2}0000") # convert HHMMSS to seconds
        vdatestr="${vdate:0:4}-${vdate:4:2}-${vdate:6:2}-${seconds}"

        case "${RUN}" in
            gdas | enkfgdas)
                source_file="iceh_inst.${vdatestr}.nc"
                dest_file="${RUN}.t${cyc}z.inst.f${fhr3}.nc"
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
        if [[ "${use_mgr_ice}" == "YES" ]]; then
            # Self-sentinel: CICE writes complete netCDF files atomically per output period.
            echo "${ice_local} ${ice_local} ${ice_com} ${ice_com}" >> "${ice_table}"
        else
            ${NLN} "${ice_com}" "${ice_local}"
        fi

        last_fhr=${fhr}
    done
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
}
