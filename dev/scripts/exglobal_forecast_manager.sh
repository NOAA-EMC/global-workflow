#! /usr/bin/env bash

################################################################################
####  UNIX Script Documentation Block
#                      .                                             .
# Script name:         exglobal_forecast_manager.sh
# Script description:  Launches MPMD component managers for JGLOBAL_FORECAST_MGR
#
# Abstract: Waits for product tables written by JGLOBAL_FORECAST during its
#           pre-run setup phase, then launches one forecast_mgr.sh process per
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

MGR_INIT_TIMEOUT="${FCST_MGR_INIT_TIMEOUT:-7200}"

# Build a command file with one line per active component.
# Each line is a complete command passed to run_mpmd.sh for MPMD execution.
FCST_MGR_CMDFILE="${DATA}/fcst_mgr_cmdfile"
rm -f "${FCST_MGR_CMDFILE}"

ATM_TABLE="${COMOUT_CONF}/atm_products_seg${FCST_SEGMENT:-0}.txt"
"${USHglobal}/wait_for_table.sh" "ATM" "${ATM_TABLE}" "${MGR_INIT_TIMEOUT}"
echo "${USHglobal}/forecast_mgr.sh atm ${ATM_TABLE}" >> "${FCST_MGR_CMDFILE}"

if [[ "${DO_WAVE}" == "YES" ]]; then
    WW3_TABLE="${COMOUT_CONF}/ww3_products_seg${FCST_SEGMENT:-0}.txt"
    "${USHglobal}/wait_for_table.sh" "WW3" "${WW3_TABLE}" "${MGR_INIT_TIMEOUT}"
    echo "${USHglobal}/forecast_mgr.sh ww3 ${WW3_TABLE}" >> "${FCST_MGR_CMDFILE}"
fi

if [[ "${DO_OCN:-NO}" == "YES" ]]; then
    OCN_TABLE="${COMOUT_CONF}/ocn_products_seg${FCST_SEGMENT:-0}.txt"
    "${USHglobal}/wait_for_table.sh" "OCN" "${OCN_TABLE}" "${MGR_INIT_TIMEOUT}"
    echo "${USHglobal}/forecast_mgr.sh ocn ${OCN_TABLE}" >> "${FCST_MGR_CMDFILE}"
fi

if [[ "${DO_ICE:-NO}" == "YES" ]]; then
    ICE_TABLE="${COMOUT_CONF}/ice_products_seg${FCST_SEGMENT:-0}.txt"
    "${USHglobal}/wait_for_table.sh" "ICE" "${ICE_TABLE}" "${MGR_INIT_TIMEOUT}"
    echo "${USHglobal}/forecast_mgr.sh ice ${ICE_TABLE}" >> "${FCST_MGR_CMDFILE}"
fi

num_ranks=$(wc -l < "${FCST_MGR_CMDFILE}")
echo "INFO: Launching ${num_ranks} MPMD component manager rank(s)"

# Launch all component managers concurrently via run_mpmd.sh
export USE_CFP=YES
"${USHglobal}/run_mpmd.sh" "${FCST_MGR_CMDFILE}"
