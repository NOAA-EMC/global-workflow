#! /usr/bin/env bash

#===============================================================================
#
#   FILE: forecast_mom6.sh
#
#   DESCRIPTION: Functions for the MOM6 ocean component of the UFS.
#                Stages fix files before run type determination (MOM6_predet),
#                checks restart availability (MOM6_det), stages restarts and
#                increments and links output after run type determination
#                (MOM6_postdet), writes namelists (MOM6_nml), and copies output to
#                COM (MOM6_out).

# shellcheck disable=SC2034
MOM6_predet() {
    echo "SUB ${FUNCNAME[0]}: MOM6 before run type determination"

    if [[ ! -d "${COMOUT_OCEAN_HISTORY}" ]]; then mkdir -p "${COMOUT_OCEAN_HISTORY}"; fi
    if [[ ! -d "${DATAoutput}/MOM6_OUTPUT" ]]; then mkdir -p "${DATAoutput}/MOM6_OUTPUT"; fi
    if [[ ! -d "${DATArestart}/MOM6_RESTART" ]]; then mkdir -p "${DATArestart}/MOM6_RESTART"; fi

    # rm ocean_geometry.nc as model fails if zero length exist
    rm -f "${DATAoutput}/MOM6_OUTPUT/ocean_geometry.nc"

    # Link the output and restart directories to the DATA directory
    ${NLN} "${DATAoutput}/MOM6_OUTPUT" "${DATA}/MOM6_OUTPUT"
    ${NLN} "${DATArestart}/MOM6_RESTART" "${DATA}/MOM6_RESTART"

    # MOM6 does not have a concept of high frequency output like FV3
    # Convert output settings into an explicit list for MOM6
    MOM6_OUTPUT_FH=$(seq -s ' ' "${FHMIN}" "${FHOUT_OCN}" "${FHMAX}")

    # If using stochastic parameterizations, create a seed that does not exceed the
    # largest signed integer
    local imem=${MEMBER#0}
    local base_seed=$((current_cycle * 10000 + imem * 100))

    if [[ "${DO_OCN_SPPT:-}" == "YES" ]]; then
        ISEED_OCNSPPT=$((base_seed + 8)),$((base_seed + 9)),$((base_seed + 10)),$((base_seed + 11)),$((base_seed + 12))
    fi

    if [[ "${DO_OCN_PERT_EPBL:-}" == "YES" ]]; then
        ISEED_EPBL=$((base_seed + 13)),$((base_seed + 14)),$((base_seed + 15)),$((base_seed + 16)),$((base_seed + 17))
    fi

    # Fix files for ocean; ocean_hgrid, ocean_mosaic, ocean_mask, etc.
    # MOM_channels is configurable based on resolution, but is treated as a fix file
    # MOM_override is a template that allows user to override default namelist settings, but is also treated as a fix file
    cpreq "${FIXglobal}/mom6/${OCNRES}/"* "${DATA}/INPUT/" # TODO: These need to be explicit

    # Add to the MOM_override file, to have ISO timestamp
    cat >> "${DATA}/INPUT/MOM_override" << EOF
!#Write ISO date stamped output with 3-hourly (0.125) frequency to ocean.stats ascii file
#override ISO_DATE_STAMPED_STDOUT = True
#override ENERGYSAVEDAYS = 0.125
EOF

    # Copy coupled grid_spec
    local spec_file
    spec_file="${FIXglobal}/cpl/a${CASE}o${OCNRES}/grid_spec.nc"
    # Test that the file exists and is not zero-sized
    if [[ -s "${spec_file}" ]]; then
        cpreq "${spec_file}" "${DATA}/INPUT/"
    else
        echo "FATAL ERROR: coupled grid_spec file '${spec_file}' does not exist or is size 0"
        exit 3
    fi

}

# shellcheck disable=SC2034
MOM6_det() {
    # Check for MOM6 restart availability at the date given by $1 (YYYYMMDDHH)
    # Returns 0 if the restarts are available, 1 otherwise
    local rdate=${1:?}
    local rst_ok="YES"
    local hdate hdatep1 fhout_ocn_by_2

    # TODO: add checks for other MOM6 restarts as well
    if [[ ! -f "${DATArestart}/MOM6_RESTART/${rdate:0:8}.${rdate:8:2}0000.MOM.res.nc" ]]; then
        rst_ok="NO"
    else
        # Also check for MOM6 history file availability
        # This is done because MOM6 output and restart files are asynchronous
        # TODO: We now have MOM6 log files, and so this logic can be updated to only check that 1 time level is written
        # and confirm that the file is fully written via the log file.
        # TODO: SFS runs with 24-hr averaging of ocean output, which causes issues with restart checks,
        # TODO: so we will skip them for now, and revisit this logic later
        if [[ "${FHOUT_OCN}" -le 6 ]]; then
            fhout_ocn_by_2=$((FHOUT_OCN / 2))
            hdate=$(date -u -d "${rdate:0:8} ${rdate:8:2} + ${fhout_ocn_by_2} hours" +"%Y%m%d%H")
            if [[ ! -f "${DATAoutput}/MOM6_OUTPUT/ocn_${hdate:0:4}_${hdate:4:2}_${hdate:6:2}_${hdate:8:2}_00.nc" ]]; then
                rst_ok="NO"
            else
                # Also check for the next MOM6 history file (hdate + FHOUT_OCN hours)
                hdatep1=$(date -u -d "${hdate:0:8} ${hdate:8:2} + ${FHOUT_OCN} hours" +"%Y%m%d%H")
                if [[ ! -f "${DATAoutput}/MOM6_OUTPUT/ocn_${hdatep1:0:4}_${hdatep1:4:2}_${hdatep1:6:2}_${hdatep1:8:2}_00.nc" ]]; then
                    rst_ok="NO"
                fi
            fi
        fi
    fi

    # Restarts from a previous attempt exist in DATArestart, so set MOM6 to read restarts
    MOM6_RESTART_SETTING='r'
    MOM6_INIT_FROM_Z=True
    MOM6_WARMSTART_FILE="none"
    MOM6_INIT_UV="zero"
    ODA_INCUPD="False"

    if [[ "${rst_ok}" == "NO" ]]; then
        return 1
    fi
    return 0
}

MOM6_postdet() {
    echo "SUB ${FUNCNAME[0]}: MOM6 after run type determination"

    local restart_dir restart_date
    if [[ "${RERUN}" == "YES" ]]; then
        restart_dir="${DATArestart}/MOM6_RESTART"
        restart_date="${RERUN_DATE}"
    else # "${RERUN}" == "NO"
        restart_dir="${COMIN_OCEAN_RESTART_PREV}"
        if [[ "${DOIAU_COLDSTART:-NO}" == "YES" ]]; then
            restart_date="${current_cycle_begin}"
        else
            restart_date="${model_start_date_current_cycle}"
        fi
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
        if [[ "${DO_JEDIOCNVAR:-NO}" == "YES" ]] || [[ "${DOIAU_COLDSTART:-NO}" == "YES" ]] || [[ ${MEMBER} -gt 0 && "${ODA_INCUPD:-False}" == "True" ]]; then
            cpreq "${COMIN_OCEAN_ANALYSIS}/${RUN}.t${cyc}z.mom6_increment.i006.nc" "${DATA}/INPUT/mom6_increment.nc"
        fi
    fi # if [[ "${RERUN}" == "NO" ]]; then

    # Link output files
    case ${RUN} in
        gfs | enkfgfs | gefs | gcafs) # Link output files for RUN=gfs|enkfgfs|gefs|gcafs
            # Looping over MOM6 output hours
            local fhr fhr3 last_fhr interval midpoint vdate vdate_mid source_file dest_file ihour source_file_log dest_file_log
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
                    source_file="ocn_${vdate_mid:0:4}_${vdate_mid:4:2}_${vdate_mid:6:2}_${vdate_mid:8:2}_00.nc"
                fi
                ihour=$(printf %02i "${interval}")
                source_file_log="${vdate:0:8}.${vdate:8:2}0000.mom6.${ihour}h"
                dest_file="${RUN}.t${cyc}z.${interval}hr_avg.f${fhr3}.nc"
                dest_file_log="${RUN}.t${cyc}z.${interval}hr_avg.log.f${fhr3}.txt"
                ${NLN} "${COMOUT_OCEAN_HISTORY}/${dest_file}" "${DATAoutput}/MOM6_OUTPUT/${source_file}"
                ${NLN} "${COMOUT_OCEAN_HISTORY}/${dest_file_log}" "${DATA}/${source_file_log}"

                last_fhr=${fhr}

            done
            ;;

        sfs) # Link output files for RUN=sfs
            # Looping over MOM6 output hours
            local fhr fhr3 last_fhr interval midpoint vdate vdate_mid source_file dest_file ihour source_file_log dest_file_log
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
                ihour=$(printf %02i "${interval}")
                source_file_log="${vdate:0:8}.${vdate:8:2}0000.mom6.${ihour}h"
                dest_file="${RUN}.t${cyc}z.${interval}hr_avg.f${fhr3}.nc"
                dest_file_log="${RUN}.t${cyc}z.${interval}hr_avg.log.f${fhr3}.txt"
                if [[ "${MEMBER}" -eq 0 ]]; then
                    mkdir -p "${DATAROOT}/${RUN}efcs000.${PDY:-}${cyc}/output/MOM6_OUTPUT/model/history"
                    ${NLN} "${DATAROOT}/${RUN}efcs000.${PDY:-}${cyc}/output/MOM6_OUTPUT/model/history/${dest_file}" "${DATAoutput}/MOM6_OUTPUT/${source_file}"
                    ${NLN} "${DATAROOT}/${RUN}efcs000.${PDY:-}${cyc}/output/MOM6_OUTPUT/model/history/${dest_file_log}" "${DATA}/${source_file_log}"
                else
                    mkdir -p "${DATAoutput}/MOM6_OUTPUT/model/history"
                    ${NLN} "${DATAoutput}/MOM6_OUTPUT/model/history/${dest_file}" "${DATAoutput}/MOM6_OUTPUT/${source_file}"
                    ${NLN} "${DATAoutput}/MOM6_OUTPUT/model/history/${dest_file_log}" "${DATA}/${source_file_log}"
                fi

                last_fhr=${fhr}

            done
            ;;

        gdas | enkfgdas) # Link output files for RUN=gdas|enkfgdas
            # Save (instantaneous) MOM6 backgrounds
            local fhr3 vdatestr
            for fhr in ${MOM6_OUTPUT_FH}; do
                fhr3=$(printf %03i "${fhr}")
                vdatestr=$(date --utc -d "${current_cycle:0:8} ${current_cycle:8:2} + ${fhr} hours" +%Y_%m_%d_%H)
                ${NLN} "${COMOUT_OCEAN_HISTORY}/${RUN}.t${cyc}z.inst.f${fhr3}.nc" "${DATAoutput}/MOM6_OUTPUT/ocn_${vdatestr}.nc"
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
    source "${USHglobal}/parsing_namelists_mom6.sh"
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
}
