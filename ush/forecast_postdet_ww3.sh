#! /usr/bin/env bash

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

    # For GFS/GEFS/SFS/GCAFS: build product tables for the forecast manager.
    # For GDAS: keep NLN symlinks so downstream analysis jobs can read WW3 outputs.
    local use_mgr_ww3="NO"
    case "${RUN}" in
        gfs) use_mgr_ww3="YES" ;;
            # TODO: enable forecast manager for gefs, sfs, gcafs once tested
            # gefs | sfs | gcafs) use_mgr_ww3="YES" ;;
        *) ;;
    esac

    # log.ww3 is the WW3 run log written to DATA. For GFS it becomes a real file
    # (copied to COM in WW3_out). For GDAS it is symlinked to COM here.
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

    # Copy WW3 run log for GFS/GEFS/SFS/GCAFS (no pre-run symlink; model writes a real
    # file in DATA which is copied to COM here at end of run)
    case "${RUN}" in
        gfs | gefs | sfs | gcafs)
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
