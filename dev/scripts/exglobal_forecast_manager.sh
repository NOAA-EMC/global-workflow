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

MGR_INIT_TIMEOUT="${FCST_MANAGER_INIT_TIMEOUT:-7200}"
# Poll every 30 seconds up to the timeout.
mgr_sleep_interval=30
mgr_max_tries=$((MGR_INIT_TIMEOUT / mgr_sleep_interval))

# Build a command file with one line per active component.
# Each line is a complete command passed to run_mpmd.sh for MPMD execution.
FCST_MANAGER_CMDFILE="${DATA}/fcst_manager_cmdfile"
rm -f "${FCST_MANAGER_CMDFILE}"

ATM_TABLE="${DATAjob}/atm_products_seg${FCST_SEGMENT:-0}.txt"
echo "INFO: Waiting for ATM product table at ${ATM_TABLE}"
if ! wait_for_file "${ATM_TABLE}" "${mgr_sleep_interval}" "${mgr_max_tries}"; then
    echo "FATAL ERROR: Timed out after ${MGR_INIT_TIMEOUT}s waiting for ${ATM_TABLE}" >&2
    exit 1
fi
echo "INFO: ATM product table found"
echo "${USHgfs}/forecast_manager.sh atm ${ATM_TABLE}" >> "${FCST_MANAGER_CMDFILE}"

if [[ "${DO_WAVE}" == "YES" ]]; then
    WW3_TABLE="${DATAjob}/ww3_products_seg${FCST_SEGMENT:-0}.txt"
    echo "INFO: Waiting for WW3 product table at ${WW3_TABLE}"
    if ! wait_for_file "${WW3_TABLE}" "${mgr_sleep_interval}" "${mgr_max_tries}"; then
        echo "FATAL ERROR: Timed out after ${MGR_INIT_TIMEOUT}s waiting for ${WW3_TABLE}" >&2
        exit 1
    fi
    echo "INFO: WW3 product table found"
    echo "${USHgfs}/forecast_manager.sh ww3 ${WW3_TABLE}" >> "${FCST_MANAGER_CMDFILE}"
fi

if [[ "${DO_OCN:-NO}" == "YES" ]]; then
    OCN_TABLE="${DATAjob}/ocn_products_seg${FCST_SEGMENT:-0}.txt"
    echo "INFO: Waiting for OCN product table at ${OCN_TABLE}"
    if ! wait_for_file "${OCN_TABLE}" "${mgr_sleep_interval}" "${mgr_max_tries}"; then
        echo "FATAL ERROR: Timed out after ${MGR_INIT_TIMEOUT}s waiting for ${OCN_TABLE}" >&2
        exit 1
    fi
    echo "INFO: OCN product table found"
    echo "${USHgfs}/forecast_manager.sh ocn ${OCN_TABLE}" >> "${FCST_MANAGER_CMDFILE}"
fi

if [[ "${DO_ICE:-NO}" == "YES" ]]; then
    ICE_TABLE="${DATAjob}/ice_products_seg${FCST_SEGMENT:-0}.txt"
    echo "INFO: Waiting for ICE product table at ${ICE_TABLE}"
    if ! wait_for_file "${ICE_TABLE}" "${mgr_sleep_interval}" "${mgr_max_tries}"; then
        echo "FATAL ERROR: Timed out after ${MGR_INIT_TIMEOUT}s waiting for ${ICE_TABLE}" >&2
        exit 1
    fi
    echo "INFO: ICE product table found"
    echo "${USHgfs}/forecast_manager.sh ice ${ICE_TABLE}" >> "${FCST_MANAGER_CMDFILE}"
fi

num_ranks=$(wc -l < "${FCST_MANAGER_CMDFILE}")
echo "INFO: Launching ${num_ranks} MPMD component manager rank(s)"

# Tell forecast_manager.sh where to find the model-completion sentinel so it can
# exit gracefully when the model is done but some product files were not produced.
export FCST_DONE_SENTINEL="${DATAjob}/fcst_done_seg${FCST_SEGMENT:-0}"

# Launch all component managers concurrently via run_mpmd.sh
export USE_CFP=YES
"${USHgfs}/run_mpmd.sh" "${FCST_MANAGER_CMDFILE}"

# Segment copy complete — remove sentinels so a rewound forecast can write fresh ones.
rm -f "${DATAjob}/fcst_started_seg${FCST_SEGMENT:-0}" "${DATAjob}/fcst_done_seg${FCST_SEGMENT:-0}"
