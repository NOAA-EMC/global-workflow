#! /usr/bin/env bash

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
