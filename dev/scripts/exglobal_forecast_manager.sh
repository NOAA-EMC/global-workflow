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
source "${USHglobal}/wait_for_file.sh"

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

# ATM: MGR_NATM_INST instance groups, each with 4 product ranks + 1 barrier rank.
# Forecast hours are distributed round-robin across instances (postdet splits the tables)
# so all groups copy in parallel.
natm_inst="${MGR_NATM_INST:-2}"
echo "INFO: Waiting for ATM per-product tables (${natm_inst} instance(s))"
for ((inst = 0; inst < natm_inst; inst++)); do
    atm_atmf_tbl="${DATAjob}/atm_atmf_products_seg${FCST_SEGMENT}_inst${inst}.txt"
    atm_sfcf_tbl="${DATAjob}/atm_sfcf_products_seg${FCST_SEGMENT}_inst${inst}.txt"
    atm_barrier_tbl="${DATAjob}/atm_barrier_seg${FCST_SEGMENT}_inst${inst}.txt"
    for _atm_tbl in "${atm_atmf_tbl}" "${atm_sfcf_tbl}" "${atm_barrier_tbl}"; do
        if ! wait_for_file "${_atm_tbl}" "${mgr_sleep_interval}" "${mgr_max_tries}"; then
            echo "FATAL ERROR: Timed out after ${MGR_INIT_TIMEOUT}s waiting for ${_atm_tbl}" >&2
            exit 1
        fi
    done
    # GRIB2/flux tables only exist when inline post-processing is enabled.
    if [[ "${WRITE_DOPOST:-}" == ".true." ]]; then
        atm_grib_tbl="${DATAjob}/atm_grib_products_seg${FCST_SEGMENT}_inst${inst}.txt"
        atm_flux_tbl="${DATAjob}/atm_flux_products_seg${FCST_SEGMENT}_inst${inst}.txt"
        for _atm_tbl in "${atm_grib_tbl}" "${atm_flux_tbl}"; do
            if ! wait_for_file "${_atm_tbl}" "${mgr_sleep_interval}" "${mgr_max_tries}"; then
                echo "FATAL ERROR: Timed out after ${MGR_INIT_TIMEOUT}s waiting for ${_atm_tbl}" >&2
                exit 1
            fi
        done
    fi
    {
        echo "${USHglobal}/forecast_manager.sh atm_atmf ${atm_atmf_tbl}"
        echo "${USHglobal}/forecast_manager.sh atm_sfcf ${atm_sfcf_tbl}"
        if [[ "${WRITE_DOPOST:-}" == ".true." ]]; then
            echo "${USHglobal}/forecast_manager.sh atm_grib ${atm_grib_tbl}"
            echo "${USHglobal}/forecast_manager.sh atm_flux ${atm_flux_tbl}"
        fi
        echo "${USHglobal}/forecast_atm_barrier.sh ${atm_barrier_tbl}"
    } >> "${FCST_MANAGER_CMDFILE}"
done
if [[ "${WRITE_DOPOST:-}" == ".true." ]]; then
    echo "INFO: ATM tables found; added $((natm_inst * 5)) ATM rank(s) (${natm_inst} x 4 product + 1 barrier)"
else
    echo "INFO: ATM tables found; added $((natm_inst * 3)) ATM rank(s) (${natm_inst} x 2 history + 1 barrier; no inline post)"
fi

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
        echo "${USHglobal}/forecast_manager.sh ww3 ${DATA}/ww3_mgr_rank${r}.txt" >> "${FCST_MANAGER_CMDFILE}"
    done
fi

if [[ "${DO_OCN:-NO}" == "YES" && "${RUN}" == "gfs" ]]; then
    OCN_TABLE="${DATAjob}/ocn_products_seg${FCST_SEGMENT}.txt"
    echo "INFO: Waiting for OCN product table at ${OCN_TABLE}"
    if ! wait_for_file "${OCN_TABLE}" "${mgr_sleep_interval}" "${mgr_max_tries}"; then
        echo "FATAL ERROR: Timed out after ${MGR_INIT_TIMEOUT}s waiting for ${OCN_TABLE}" >&2
        exit 1
    fi
    echo "INFO: OCN product table found (${MGR_NTASKS_OCN} rank(s))"
    split_table_by_sentinel "${OCN_TABLE}" "${MGR_NTASKS_OCN}" "${DATA}/ocn_mgr_rank"
    for ((r = 0; r < MGR_NTASKS_OCN; r++)); do
        echo "${USHglobal}/forecast_manager.sh ocn ${DATA}/ocn_mgr_rank${r}.txt" >> "${FCST_MANAGER_CMDFILE}"
    done
fi

if [[ "${DO_ICE:-NO}" == "YES" && "${RUN}" == "gfs" ]]; then
    ICE_TABLE="${DATAjob}/ice_products_seg${FCST_SEGMENT}.txt"
    echo "INFO: Waiting for ICE product table at ${ICE_TABLE}"
    if ! wait_for_file "${ICE_TABLE}" "${mgr_sleep_interval}" "${mgr_max_tries}"; then
        echo "FATAL ERROR: Timed out after ${MGR_INIT_TIMEOUT}s waiting for ${ICE_TABLE}" >&2
        exit 1
    fi
    echo "INFO: ICE product table found (${MGR_NTASKS_ICE} rank(s))"
    split_table_by_sentinel "${ICE_TABLE}" "${MGR_NTASKS_ICE}" "${DATA}/ice_mgr_rank"
    for ((r = 0; r < MGR_NTASKS_ICE; r++)); do
        echo "${USHglobal}/forecast_manager.sh ice ${DATA}/ice_mgr_rank${r}.txt" >> "${FCST_MANAGER_CMDFILE}"
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
"${USHglobal}/run_mpmd.sh" "${FCST_MANAGER_CMDFILE}"
