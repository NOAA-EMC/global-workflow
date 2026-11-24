#! /usr/bin/env bash

#===============================================================================
#
#   FILE: forecast_cmeps.sh
#
#   DESCRIPTION: Functions for the CMEPS mediator (coupler) of the UFS.
#                Links the restart directory before run type determination
#                (CMEPS_predet), checks restart availability (CMEPS_det), stages
#                restarts and determines restart output times after run type
#                determination (CMEPS_postdet), and copies restarts to COM
#                (CMEPS_out).

# shellcheck disable=SC2178
CMEPS_predet() {
    echo "SUB ${FUNCNAME[0]}: CMEPS before run type determination"

    if [[ ! -d "${DATArestart}/CMEPS_RESTART" ]]; then mkdir -p "${DATArestart}/CMEPS_RESTART"; fi

    # Link the restart directory to the DATA directory
    ${NLN} "${DATArestart}/CMEPS_RESTART" "${DATA}/CMEPS_RESTART"
}

CMEPS_det() {
    # Check for CMEPS restart availability at the date given by $1 (YYYYMMDDHH)
    # Returns 0 if the restarts are available, 1 otherwise
    local rdate=${1:?}
    local seconds
    seconds=$(to_seconds "${rdate:8:2}0000")
    if [[ ! -f "${DATArestart}/CMEPS_RESTART/ufs.cpld.cpl.r.${rdate:0:4}-${rdate:4:2}-${rdate:6:2}-${seconds}.nc" ]]; then
        return 1
    fi
    return 0
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
    # Note, this must be computed after determination IAU in UFS_det and fhrot.
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
        if [[ ! -d "${COMOUT_MED_RESTART}" ]]; then mkdir -p "${COMOUT_MED_RESTART}"; fi

        "${USHglobal}/run_mpmd.sh" "${cmdfile}" && true
        export err=$?
        if [[ ${err} -ne 0 ]]; then
            err_exit "run_mpmd.sh failed to copy CMEPS mediator restart files!"
        fi
    fi
}
