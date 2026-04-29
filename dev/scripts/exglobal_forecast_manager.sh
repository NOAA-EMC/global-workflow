#! /usr/bin/env bash

################################################################################
####  UNIX Script Documentation Block
#                      .                                             .
# Script name:         exglobal_forecast_manager.sh
# Script description:  Launches component managers for JGLOBAL_FORECAST_MGR
#
# Abstract: Waits for product tables written by JGLOBAL_FORECAST during its
#           pre-run setup phase, then manages real-time file copies to COM.
#           Two modes are available via FCST_MGR_MPMD (default: YES):
#
#           FCST_MGR_MPMD=YES  - One forecast_mgr.sh process per active model
#           component launched concurrently via run_mpmd.sh (MPMD mode).
#           Requires one core per active component.
#
#           FCST_MGR_MPMD=NO   - All component product tables are concatenated
#           into a single table and processed by one forecast_mgr.sh process
#           on a single core (serial mode).
#
# $Id$
#
# Attributes:
#   Language: POSIX shell
#
################################################################################

#  Set environment.
cd "${DATA}" || exit 8

# Remove the started sentinel left by the forecast segment job.
# This ensures a rewound segment does not re-trigger the manager from a stale sentinel.
rm -f "${DATAjob}/fcst_started_seg${FCST_SEGMENT:-0}"

MGR_INIT_TIMEOUT="${FCST_MGR_INIT_TIMEOUT:-7200}"

# Build a command file with one line per active component.
# Each line is a complete command passed to run_mpmd.sh for MPMD execution.
FCST_MGR_CMDFILE="${DATA}/fcst_mgr_cmdfile"
rm -f "${FCST_MGR_CMDFILE}"

ATM_TABLE="${DATAjob}/atm_products_seg${FCST_SEGMENT:-0}.txt"
"${USHglobal}/wait_for_table.sh" "ATM" "${ATM_TABLE}" "${MGR_INIT_TIMEOUT}"
echo "${USHglobal}/forecast_mgr.sh atm ${ATM_TABLE}" >> "${FCST_MGR_CMDFILE}"

if [[ "${DO_WAVE}" == "YES" ]]; then
    WW3_TABLE="${DATAjob}/ww3_products_seg${FCST_SEGMENT:-0}.txt"
    "${USHglobal}/wait_for_table.sh" "WW3" "${WW3_TABLE}" "${MGR_INIT_TIMEOUT}"
    echo "${USHglobal}/forecast_mgr.sh ww3 ${WW3_TABLE}" >> "${FCST_MGR_CMDFILE}"
fi

if [[ "${DO_OCN:-NO}" == "YES" ]]; then
    OCN_TABLE="${DATAjob}/ocn_products_seg${FCST_SEGMENT:-0}.txt"
    "${USHglobal}/wait_for_table.sh" "OCN" "${OCN_TABLE}" "${MGR_INIT_TIMEOUT}"
    echo "${USHglobal}/forecast_mgr.sh ocn ${OCN_TABLE}" >> "${FCST_MGR_CMDFILE}"
fi

if [[ "${DO_ICE:-NO}" == "YES" ]]; then
    ICE_TABLE="${DATAjob}/ice_products_seg${FCST_SEGMENT:-0}.txt"
    "${USHglobal}/wait_for_table.sh" "ICE" "${ICE_TABLE}" "${MGR_INIT_TIMEOUT}"
    echo "${USHglobal}/forecast_mgr.sh ice ${ICE_TABLE}" >> "${FCST_MGR_CMDFILE}"
fi

FCST_MGR_MPMD="${FCST_MGR_MPMD:-YES}"

if [[ "${FCST_MGR_MPMD}" == "YES" ]]; then
    num_ranks=$(wc -l < "${FCST_MGR_CMDFILE}")
    echo "INFO: Launching ${num_ranks} MPMD component manager rank(s)"
    export USE_CFP=YES
    "${USHglobal}/run_mpmd.sh" "${FCST_MGR_CMDFILE}"
else
    # Serial mode: concatenate all component tables into one and run a single manager.
    COMBINED_TABLE="${DATA}/all_products_seg${FCST_SEGMENT:-0}.txt"
    rm -f "${COMBINED_TABLE}"
    while IFS= read -r cmd; do
        # Each cmd line is: <path>/forecast_mgr.sh <component> <table_file>
        # Extract the table_file (third token) and append its contents.
        table_file="${cmd##* }"
        cat "${table_file}" >> "${COMBINED_TABLE}"
    done < "${FCST_MGR_CMDFILE}"
    echo "INFO: Launching single serial manager on combined table ($(wc -l < "${COMBINED_TABLE}") entries)"
    "${USHglobal}/forecast_mgr.sh" "all" "${COMBINED_TABLE}"
fi
