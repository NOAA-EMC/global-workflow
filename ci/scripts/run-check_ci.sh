#!/bin/bash

set -eu

#####################################################################################
# Script description: script to check the status of an experiment as reported
#                     by Rocoto
#####################################################################################

TEST_DIR=${1:-${TEST_DIR:-?}}  # Location of the root of the testing directory
pslot=${2:-${pslot:-?}}        # Name of the experiment being tested by this script
SYSTEM_BUILD_DIR=${3:-"global-workflow"}  # Name of the system build directory, default is "global-workflow

# TEST_DIR contains 2 directories;
# 1. HOMEgfs: clone of the global-workflow
# 2. RUNTESTS: A directory containing EXPDIR and COMROOT for experiments
# # e.g. $> tree ./TEST_DIR
# ./TEST_DIR
# ├── HOMEgfs
# └── RUNTESTS
#     ├── COMROOT
#     │   └── ${pslot}
#     └── EXPDIR
#         └── ${pslot}
# Two system build directories created at build time gfs, and gdas
# TODO: Make this configurable (for now all scripts run from gfs for CI at runtime)
HOMEgfs="${TEST_DIR}/${SYSTEM_BUILD_DIR}"
RUNTESTS="${TEST_DIR}/RUNTESTS"
run_check_logfile="${RUNTESTS}/ci-run_check.log"

# Source modules and setup logging
echo "Source modules."
source "${HOMEgfs}/workflow/gw_setup.sh"

# cd into the experiment directory
echo "cd ${RUNTESTS}/EXPDIR/${pslot}"
cd "${RUNTESTS}/EXPDIR/${pslot}" || (echo "FATAL ERROR: Unable to cd into '${RUNTESTS}/EXPDIR/${pslot}', ABORT!"; exit 1)

# Name of the Rocoto XML and database files
xml="${pslot}.xml"
db="${pslot}.db"

# Ensure the XML is present for the experiment
if [[ ! -f "${xml}" ]]; then
  echo "FATAL ERROR: XML file ${xml} not found in '${pslot}', experiment ${pslot} failed, ABORT!"
  exit 1
fi

# Launch experiment
echo "Launch experiment with Rocoto."
rocotorun -v "${ROCOTO_VERBOSE:-0}" -w "${xml}" -d "${db}"
sleep 10
if [[ ! -f "${db}" ]]; then
  echo "FATAL ERROR: Rocoto database file ${db} not found, experiment ${pslot} failed, ABORT!"
  exit 2
fi

# Experiment launched
rc=99
set +e
while true; do

  echo "Run rocotorun."
  rocotorun -v "${ROCOTO_VERBOSE:-0}" -w "${xml}" -d "${db}"

  # Wait before running rocotostat
  sleep 60

  # Get job statistics
  echo "Gather Rocoto statistics"
  # shellcheck disable=SC2312 # We want to use the exit code of the command
  full_state=$("${HOMEgfs}/ci/scripts/utils/rocotostat.py" -w "${xml}" -d "${db}" -v)
  error_stat=$?

  for state in CYCLES_TOTAL CYCLES_DONE SUCCEEDED FAIL DEAD; do
    declare "${state}"="$(echo "${full_state}" | grep "${state}" | cut -d: -f2)" || true
  done
  ROCOTO_STATE=$(echo "${full_state}" | tail -1) || exit 1

  echo -e "(${pslot} on ${MACHINE_ID^})\n\tTotal Cycles: ${CYCLES_TOTAL}\n\tNumber Cycles done: ${CYCLES_DONE}\n\tState: ${ROCOTO_STATE}"

  if [[ ${error_stat} -ne 0 ]]; then
    {
      echo "Experiment ${pslot} Terminated with ${FAIL} tasks failed and ${DEAD} dead at $(date)" || true
      echo "Experiment ${pslot} Terminated: *${ROCOTO_STATE}*"
    } | tee -a "${run_check_logfile}"
    if [[ "${DEAD}" -ne 0 ]]; then
      error_logs=$(rocotostat -d "${db}" -w "${xml}" | grep -E 'FAIL|DEAD' | awk '{print "-c", $1, "-t", $2}' | xargs rocotocheck -d "${db}" -w "${xml}" | grep join | awk '{print $2}') || true
      {
        echo "Error logs:"
        echo "${error_logs}"
      } | tee -a  "${run_check_logfile}"
      rm -f "${RUNTESTS}/${pslot}_error.logs"
      for log in ${error_logs}; do
        echo "RUNTESTS${log#*RUNTESTS}" >> "${RUNTESTS}/${pslot}_error.logs"
      done
   fi
   rc=1
   break
  fi

  if [[ "${ROCOTO_STATE}" == "DONE" ]]; then
    {
      echo "Experiment ${pslot} Completed ${CYCLES_DONE} Cycles: *SUCCESS* at $(date)" || true
    } | tee -a "${run_check_logfile}"
    rc=0
    break
  fi

  # Wait before running rocotorun again
  sleep 300

done

exit "${rc}"

