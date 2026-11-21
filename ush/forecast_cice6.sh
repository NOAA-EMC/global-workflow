#! /usr/bin/env bash

#===============================================================================
#
#   FILE: forecast_cice6.sh
#
#   DESCRIPTION: Functions for the CICE6 sea ice component of the UFS.
#                Stages fix files before run type determination (CICE_predet),
#                checks restart availability (CICE_det), stages restarts and links
#                output after run type determination (CICE_postdet), writes
#                namelists (CICE_nml), and copies output to COM (CICE_out).

# shellcheck disable=SC2034
CICE_predet() {
    echo "SUB ${FUNCNAME[0]}: CICE before run type determination"

    if [[ ! -d "${COMOUT_ICE_HISTORY}" ]]; then mkdir -p "${COMOUT_ICE_HISTORY}"; fi
    if [[ ! -d "${DATAoutput}/CICE_OUTPUT" ]]; then mkdir -p "${DATAoutput}/CICE_OUTPUT"; fi
    if [[ ! -d "${DATArestart}/CICE_RESTART" ]]; then mkdir -p "${DATArestart}/CICE_RESTART"; fi

    # Link the output and restart directories to the DATA directory
    ${NLN} "${DATAoutput}/CICE_OUTPUT" "${DATA}/CICE_OUTPUT"
    ${NLN} "${DATArestart}/CICE_RESTART" "${DATA}/CICE_RESTART"

    # CICE does not have a concept of high frequency output like FV3
    # Convert output settings into an explicit list for CICE
    mapfile -t CICE_OUTPUT_FH < <(seq "${FHMIN}" "${FHOUT_ICE}" "${FHMAX}") || exit 10

    # Fix files
    cpreq "${FIXglobal}/cice/${ICERES}/${CICE_GRID}" "${DATA}/"
    cpreq "${FIXglobal}/cice/${ICERES}/${CICE_MASK}" "${DATA}/"
    cpreq "${FIXglobal}/cice/${ICERES}/${MESH_ICE}" "${DATA}/"

}

CICE_det() {
    # Check for CICE6 restart availability at the date given by $1 (YYYYMMDDHH)
    # Returns 0 if the restarts are available, 1 otherwise
    local rdate=${1:?}
    local seconds
    seconds=$(to_seconds "${rdate:8:2}0000")
    if [[ ! -f "${DATArestart}/CICE_RESTART/cice_model.res.${rdate:0:4}-${rdate:4:2}-${rdate:6:2}-${seconds}.nc" ]]; then
        return 1
    fi
    return 0
}

CICE_postdet() {
    echo "SUB ${FUNCNAME[0]}: CICE after run type determination"

    local restart_date cice_restart_file
    if [[ "${RERUN}" == "YES" ]]; then
        restart_date="${RERUN_DATE}"
        local seconds
        seconds=$(to_seconds "${restart_date:8:2}0000") # convert HHMMSS to seconds
        cice_restart_file="${DATArestart}/CICE_RESTART/cice_model.res.${restart_date:0:4}-${restart_date:4:2}-${restart_date:6:2}-${seconds}.nc"
    else # "${RERUN}" == "NO"
        if [[ "${DOIAU_COLDSTART:-NO}" == "YES" ]]; then
            restart_date="${current_cycle_begin}"
        else
            restart_date="${model_start_date_current_cycle}"
        fi
        cice_restart_file="${COMIN_ICE_RESTART_PREV}/${restart_date:0:8}.${restart_date:8:2}0000.cice_model.res.nc"
        if [[ "${DOIAU_COLDSTART:-NO}" == "YES" ]]; then
            cice_restart_file="${COMIN_ICE_ANALYSIS}/${restart_date:0:8}.${restart_date:8:2}0000.analysis.cice_model.res.nc"
        fi
        if [[ "${DO_JEDIOCNVAR:-NO}" == "YES" || "${DOIAU_COLDSTART:-NO}" == "YES" ]]; then
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

    # Link iceh_ic file to COM.  This is the initial condition file from CICE (f000)
    # TODO: Is this file needed in COM? Is this going to be used for generating any products?
    local vdate seconds vdatestr fhr fhr3 interval last_fhr
    seconds=$(to_seconds "${model_start_date_current_cycle:8:2}0000") # convert HHMMSS to seconds
    vdatestr="${model_start_date_current_cycle:0:4}-${model_start_date_current_cycle:4:2}-${model_start_date_current_cycle:6:2}-${seconds}"
    ${NLN} "${COMOUT_ICE_HISTORY}/${RUN}.t${cyc}z.ic.nc" "${DATAoutput}/CICE_OUTPUT/iceh_ic.${vdatestr}.nc"

    # Link CICE forecast output files from DATAoutput/CICE_OUTPUT to COM
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
                ${NLN} "${COMOUT_ICE_HISTORY}/${dest_file}" "${DATAoutput}/CICE_OUTPUT/${source_file}"
                ;;
            gfs | enkfgfs | gcafs)
                source_file="iceh_$(printf "%0.2d" "${FHOUT_ICE}")h.${vdatestr}.nc"
                dest_file="${RUN}.t${cyc}z.${interval}hr_avg.f${fhr3}.nc"
                ${NLN} "${COMOUT_ICE_HISTORY}/${dest_file}" "${DATAoutput}/CICE_OUTPUT/${source_file}"
                ;;
            sfs)
                source_file="iceh_$(printf "%0.2d" "${FHOUT_ICE}")h.${vdatestr}.nc"
                dest_file="${RUN}.t${cyc}z.${interval}hr_avg.f${fhr3}.nc"
                if [[ "${MEMBER}" -eq 0 ]]; then
                    mkdir -p "${DATAROOT}/${RUN}efcs000.${PDY:-}${cyc}/output/CICE_OUTPUT/model/history"
                    ${NLN} "${DATAROOT}/${RUN}efcs000.${PDY:-}${cyc}/output/CICE_OUTPUT/model/history/${dest_file}" "${DATAoutput}/CICE_OUTPUT/${source_file}"
                else
                    mkdir -p "${DATAoutput}/CICE_OUTPUT/model/history"
                    ${NLN} "${DATAoutput}/CICE_OUTPUT/model/history/${dest_file}" "${DATAoutput}/CICE_OUTPUT/${source_file}"
                fi
                ;;
            gefs)
                source_file="iceh.${vdatestr}.nc"
                dest_file="${RUN}.t${cyc}z.${interval}hr_avg.f${fhr3}.nc"
                ${NLN} "${COMOUT_ICE_HISTORY}/${dest_file}" "${DATAoutput}/CICE_OUTPUT/${source_file}"
                ;;
            *)
                echo "FATAL ERROR: Unsupported RUN ${RUN} in CICE postdet"
                exit 10
                ;;
        esac

        last_fhr=${fhr}
    done

}

CICE_nml() {
    echo "SUB ${FUNCNAME[0]}: Creating name list for CICE"
    source "${USHglobal}/parsing_namelists_cice.sh"
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

        "${USHglobal}/run_mpmd.sh" "${cmdfile}" && true
        export err=$?
        if [[ ${err} -ne 0 ]]; then
            err_exit "run_mpmd.sh failed to copy CICE restart files!"
        fi
    fi
}
