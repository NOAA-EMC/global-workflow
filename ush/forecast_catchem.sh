#! /usr/bin/env bash

#===============================================================================
#
#   FILE: forecast_catchem.sh
#
#   DESCRIPTION: Functions for the CATChem chemistry component of the UFS.
#                Links chemistry input before run type determination
#                (CATCHEM_predet), pre-cleans output after run type
#                determination (CATCHEM_postdet), stages configuration files
#                (CATCHEM_rc), and copies output to COM (CATCHEM_out).
#
#   NOTE: CATChem is not yet supported. This file is intentionally not sourced
#         by exglobal_fcst.sh and these functions are not called anywhere.
#         Enabling CATChem requires the remaining configuration, build, and
#         workflow changes (see NOAA-EMC/global-workflow#5279).

# shellcheck disable=SC2034
CATCHEM_predet() {
    echo "SUB ${FUNCNAME[0]}: CATChem before run type determination"

    # FHMAX gets modified when IAU is on, so keep original value for CATChem output
    GOCART_MAX=${FHMAX}

    # Stage the prepared chemistry input (emissions) when available
    if [[ -d "${COMIN_CHEM_INPUT}" ]]; then
        ${NLN} "${COMIN_CHEM_INPUT}" "${DATA}/ChemInput"
    fi
}

CATCHEM_rc() {
    echo "SUB ${FUNCNAME[0]}: Staging CATChem runtime configuration files"

    # Link directory containing the chemistry input dataset (ExtData), if provided.
    # CATChem reads Mie optics tables from ./ExtData/monochromatic/ (see the
    # 'mie.directory' key in CATChem_config.yml).
    if [[ -n "${AERO_INPUTS_DIR}" ]]; then
        ${NLN} "${AERO_INPUTS_DIR}" "${DATA}/ExtData"
        status=$?
        if [[ ${status} -ne 0 ]]; then
            exit "${status}"
        fi
    fi

    # Stage the selected CATChem configuration set under the fixed names expected
    # by the CATChem NUOPC cap and by CATChem_config.yml itself:
    #   - the cap hardcodes 'CATChem_new_config.yml' and 'CATChem_field_mapping.yml'
    #   - the config file references './CATChem_species.yml' and './CATChem_emission.yml'
    local -a catchem_src=(
        "CATChem_config_${CATCHEM_CONFIG}.yaml"
        "CATChem_species_${CATCHEM_CONFIG}.yaml"
        "CATChem_emissions_${CATCHEM_CONFIG}.yaml"
        "CATChem_field_mapping_${CATCHEM_CONFIG}.yaml"
    )
    local -a catchem_dst=(
        "CATChem_new_config.yml"
        "CATChem_species.yml"
        "CATChem_emission.yml"
        "CATChem_field_mapping.yml"
    )

    local idx
    for idx in "${!catchem_src[@]}"; do
        if [[ ! -r "${CATCHEM_CONFIG_DIR}/${catchem_src[idx]}" ]]; then
            err_exit "FATAL ERROR: CATChem configuration file '${CATCHEM_CONFIG_DIR}/${catchem_src[idx]}' not found (CATCHEM_CONFIG='${CATCHEM_CONFIG}'), ABORT!"
        fi
        cpreq "${CATCHEM_CONFIG_DIR}/${catchem_src[idx]}" "${DATA}/${catchem_dst[idx]}"
    done
}

CATCHEM_postdet() {
    echo "SUB ${FUNCNAME[0]}: Pre-cleaning CATChem output files"

    # CATChem diagnostics (prefix 'catchem_diag' per CATChem_config.yml) cannot
    # overwrite existing files, so remove any pre-existing output in COM.
    local outfile
    for outfile in "${COMOUT_CHEM_HISTORY}"/catchem*; do
        if [[ -e "${outfile}" ]]; then
            rm -f "${outfile}"
        fi
    done
}

CATCHEM_out() {
    echo "SUB ${FUNCNAME[0]}: Copying output data for CATChem"

    # CATChem writes diagnostics into the output directory configured in
    # CATChem_config.yml ('directory: "./output"', relative to DATA).
    local diag_dir="${DATA}/output"
    if [[ ! -d "${diag_dir}" ]]; then
        diag_dir="${DATA}"
    fi

    # Build MPMD cmdfile to copy CATChem output files in parallel
    local cmdfile="${DATA}/cmdfile_catchem_out"
    rm -f "${cmdfile}"

    local outfile
    for outfile in "${diag_dir}"/catchem*; do
        if [[ -e "${outfile}" ]]; then
            echo "cpfs ${outfile} ${COMOUT_CHEM_HISTORY}/$(basename "${outfile}")" >> "${cmdfile}"
        fi
    done

    if [[ -s "${cmdfile}" ]]; then
        if [[ ! -d "${COMOUT_CHEM_HISTORY}" ]]; then
            echo "INFO: Directory ${COMOUT_CHEM_HISTORY} does not exist, creating..."
            mkdir -p "${COMOUT_CHEM_HISTORY}"
        fi

        "${USHglobal}/run_mpmd.sh" "${cmdfile}" && true
        export err=$?
        if [[ ${err} -ne 0 ]]; then
            err_exit "run_mpmd.sh failed to copy CATChem output files!"
        fi
    fi
}
