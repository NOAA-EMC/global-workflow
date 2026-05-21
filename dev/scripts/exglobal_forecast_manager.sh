#! /usr/bin/env bash

################################################################################
####  UNIX Script Documentation Block
#                      .                                             .
# Script name:         exglobal_forecast_manager.sh
# Script description:  Launches MPMD component managers for JGLOBAL_FORECAST_MANAGER
#
# Abstract: Waits for product tables written by JGLOBAL_FORECAST during its
#           pre-run setup phase, then launches one forecast_manager.sh process per
#           active model component via run_mpmd.sh. Each manager polls for
#           per-file sentinel logs and copies output files to COM as the model
#           writes them.
#
# $Id$
#
# Attributes:
#   Language: POSIX shell
#
################################################################################

#  Set environment.
# shellcheck source=ush/wait_for_file.sh
source "${USHgfs}/wait_for_file.sh"

cd "${DATA}" || exit 8

# Default segment index to 0 if not set by the caller (multi-segment forecasts set this).
FCST_SEGMENT=${FCST_SEGMENT:-0}

MGR_INIT_TIMEOUT="${FCST_MANAGER_INIT_TIMEOUT:-7200}"
# Poll every 30 seconds up to the timeout.
mgr_sleep_interval=30
mgr_max_tries=$((MGR_INIT_TIMEOUT / mgr_sleep_interval))

# Number of parallel manager ranks for WW3/OCN/ICE components. Each product
# table is split into n sub-tables (one per rank) by sentinel group so that all
# rows for a given sentinel land in the same sub-table, preserving the
# data-first-log-last copy contract inside each manager process.
# ATM uses dedicated per-product ranks instead (see below).
MGR_NTASKS_WW3=${MGR_NTASKS_WW3:-2}
MGR_NTASKS_OCN=${MGR_NTASKS_OCN:-1}
MGR_NTASKS_ICE=${MGR_NTASKS_ICE:-1}

# split_table_by_sentinel <table_file> <n_ranks> <output_prefix>
# Reads a 4-column product table and distributes rows across n_ranks output
# files. All rows sharing a sentinel log (field 2) are kept together and each
# sentinel group is assigned to a rank in round-robin order.
split_table_by_sentinel() {
    local table_file="${1}" n_ranks="${2}" output_prefix="${3}"
    local r ld ll cd cl
    local rank=0
    declare -A sentinel_rank

    for ((r = 0; r < n_ranks; r++)); do
        : > "${output_prefix}${r}.txt"
    done

    while read -r ld ll cd cl; do
        [[ -z "${ld}" || "${ld:0:1}" == "#" ]] && continue
        if [[ -z "${sentinel_rank[${ll}]+_}" ]]; then
            sentinel_rank["${ll}"]=${rank}
            ((rank = (rank + 1) % n_ranks)) || true
        fi
        r="${sentinel_rank[${ll}]}"
        echo "${ld} ${ll} ${cd} ${cl}" >> "${output_prefix}${r}.txt"
    done < "${table_file}"
}

# Build a command file with one line per manager rank. Each component's product
# table is split into MGR_NTASKS_<component> sub-tables and one manager rank is
# launched per sub-table via run_mpmd.sh in MPMD mode.
FCST_MANAGER_CMDFILE="${DATA}/fcst_manager_cmdfile"
rm -f "${FCST_MANAGER_CMDFILE}"

# ATM: one dedicated manager rank per product type + one barrier rank.
# The barrier rank writes the final combined com_log only after all per-product
# ranks have confirmed their data files are in COM.
ATM_ATMF_TABLE="${DATAjob}/atm_atmf_products_seg${FCST_SEGMENT}.txt"
ATM_SFCF_TABLE="${DATAjob}/atm_sfcf_products_seg${FCST_SEGMENT}.txt"
ATM_GRIB_TABLE="${DATAjob}/atm_grib_products_seg${FCST_SEGMENT}.txt"
ATM_FLUX_TABLE="${DATAjob}/atm_flux_products_seg${FCST_SEGMENT}.txt"
ATM_BARRIER_TABLE="${DATAjob}/atm_barrier_seg${FCST_SEGMENT}.txt"

echo "INFO: Waiting for ATM per-product tables"
for _atm_tbl in "${ATM_ATMF_TABLE}" "${ATM_SFCF_TABLE}" \
    "${ATM_GRIB_TABLE}" "${ATM_FLUX_TABLE}" "${ATM_BARRIER_TABLE}"; do
    if ! wait_for_file "${_atm_tbl}" "${mgr_sleep_interval}" "${mgr_max_tries}"; then
        echo "FATAL ERROR: Timed out after ${MGR_INIT_TIMEOUT}s waiting for ${_atm_tbl}" >&2
        exit 1
    fi
done
echo "INFO: ATM per-product tables found; adding 5 ATM rank(s) (4 product + 1 barrier)"
{
    echo "${USHgfs}/forecast_manager.sh atm_atmf ${ATM_ATMF_TABLE}"
    echo "${USHgfs}/forecast_manager.sh atm_sfcf ${ATM_SFCF_TABLE}"
    echo "${USHgfs}/forecast_manager.sh atm_grib ${ATM_GRIB_TABLE}"
    echo "${USHgfs}/forecast_manager.sh atm_flux ${ATM_FLUX_TABLE}"
    echo "${USHgfs}/forecast_atm_barrier.sh ${ATM_BARRIER_TABLE}"
} >> "${FCST_MANAGER_CMDFILE}"

if [[ "${DO_WAVE}" == "YES" ]]; then
    WW3_TABLE="${DATAjob}/ww3_products_seg${FCST_SEGMENT}.txt"
    echo "INFO: Waiting for WW3 product table at ${WW3_TABLE}"
    if ! wait_for_file "${WW3_TABLE}" "${mgr_sleep_interval}" "${mgr_max_tries}"; then
        echo "FATAL ERROR: Timed out after ${MGR_INIT_TIMEOUT}s waiting for ${WW3_TABLE}" >&2
        exit 1
    fi
    echo "INFO: WW3 product table found (${MGR_NTASKS_WW3} rank(s))"
    split_table_by_sentinel "${WW3_TABLE}" "${MGR_NTASKS_WW3}" "${DATA}/ww3_mgr_rank"
    for ((r = 0; r < MGR_NTASKS_WW3; r++)); do
        echo "${USHgfs}/forecast_manager.sh ww3 ${DATA}/ww3_mgr_rank${r}.txt" >> "${FCST_MANAGER_CMDFILE}"
    done
fi

if [[ "${DO_OCN:-NO}" == "YES" ]]; then
    OCN_TABLE="${DATAjob}/ocn_products_seg${FCST_SEGMENT}.txt"
    echo "INFO: Waiting for OCN product table at ${OCN_TABLE}"
    if ! wait_for_file "${OCN_TABLE}" "${mgr_sleep_interval}" "${mgr_max_tries}"; then
        echo "FATAL ERROR: Timed out after ${MGR_INIT_TIMEOUT}s waiting for ${OCN_TABLE}" >&2
        exit 1
    fi
    echo "INFO: OCN product table found (${MGR_NTASKS_OCN} rank(s))"
    split_table_by_sentinel "${OCN_TABLE}" "${MGR_NTASKS_OCN}" "${DATA}/ocn_mgr_rank"
    for ((r = 0; r < MGR_NTASKS_OCN; r++)); do
        echo "${USHgfs}/forecast_manager.sh ocn ${DATA}/ocn_mgr_rank${r}.txt" >> "${FCST_MANAGER_CMDFILE}"
    done
fi

if [[ "${DO_ICE:-NO}" == "YES" ]]; then
    ICE_TABLE="${DATAjob}/ice_products_seg${FCST_SEGMENT}.txt"
    echo "INFO: Waiting for ICE product table at ${ICE_TABLE}"
    if ! wait_for_file "${ICE_TABLE}" "${mgr_sleep_interval}" "${mgr_max_tries}"; then
        echo "FATAL ERROR: Timed out after ${MGR_INIT_TIMEOUT}s waiting for ${ICE_TABLE}" >&2
        exit 1
    fi
    echo "INFO: ICE product table found (${MGR_NTASKS_ICE} rank(s))"
    split_table_by_sentinel "${ICE_TABLE}" "${MGR_NTASKS_ICE}" "${DATA}/ice_mgr_rank"
    for ((r = 0; r < MGR_NTASKS_ICE; r++)); do
        echo "${USHgfs}/forecast_manager.sh ice ${DATA}/ice_mgr_rank${r}.txt" >> "${FCST_MANAGER_CMDFILE}"
    done
fi

num_ranks=$(wc -l < "${FCST_MANAGER_CMDFILE}")
echo "INFO: Launching ${num_ranks} MPMD component manager rank(s)"

# Tell forecast_manager.sh where to find the model-completion sentinel so it can
# exit gracefully when the model is done but some product files were not produced.
export FCST_TABLE_READY_SENTINEL="${DATAjob}/fcst_table_ready_seg${FCST_SEGMENT}"
export FCST_DONE_SENTINEL="${DATAjob}/fcst_done_seg${FCST_SEGMENT}"

# Launch all component managers concurrently via run_mpmd.sh
export USE_CFP=YES
"${USHgfs}/run_mpmd.sh" "${FCST_MANAGER_CMDFILE}"
