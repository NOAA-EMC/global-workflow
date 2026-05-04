#! /usr/bin/env bash

################################################################################
####  UNIX Script Documentation Block
#                      .                                             .
# Script name:         exglobal_forecast_manager.sh
# Script description:  Launches MPMD component managers for JGLOBAL_FORECAST_MGR
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
cd "${DATA}" || exit 8

MANAGER_INIT_TIMEOUT="${FCST_MANAGER_INIT_TIMEOUT:-${FCST_MGR_INIT_TIMEOUT:-7200}}"

# Build a command file with one line per active component.
# Each line is a complete command passed to run_mpmd.sh for MPMD execution.
FCST_MANAGER_CMDFILE="${DATA}/fcst_manager_cmdfile"
rm -f "${FCST_MANAGER_CMDFILE}"

ATM_TABLE="${DATAjob}/atm_products_seg${FCST_SEGMENT:-0}.txt"
"${USHgfs}/wait_for_table.sh" "ATM" "${ATM_TABLE}" "${MANAGER_INIT_TIMEOUT}"
echo "${USHgfs}/forecast_manager.sh atm ${ATM_TABLE}" >> "${FCST_MANAGER_CMDFILE}"

if [[ "${DO_WAVE}" == "YES" ]]; then
    WW3_TABLE="${DATAjob}/ww3_products_seg${FCST_SEGMENT:-0}.txt"
    "${USHgfs}/wait_for_table.sh" "WW3" "${WW3_TABLE}" "${MANAGER_INIT_TIMEOUT}"
    echo "${USHgfs}/forecast_manager.sh ww3 ${WW3_TABLE}" >> "${FCST_MANAGER_CMDFILE}"
fi

if [[ "${DO_OCN:-NO}" == "YES" ]]; then
    OCN_TABLE="${DATAjob}/ocn_products_seg${FCST_SEGMENT:-0}.txt"
    "${USHgfs}/wait_for_table.sh" "OCN" "${OCN_TABLE}" "${MANAGER_INIT_TIMEOUT}"
    echo "${USHgfs}/forecast_manager.sh ocn ${OCN_TABLE}" >> "${FCST_MANAGER_CMDFILE}"
fi

if [[ "${DO_ICE:-NO}" == "YES" ]]; then
    ICE_TABLE="${DATAjob}/ice_products_seg${FCST_SEGMENT:-0}.txt"
    "${USHgfs}/wait_for_table.sh" "ICE" "${ICE_TABLE}" "${MANAGER_INIT_TIMEOUT}"
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
