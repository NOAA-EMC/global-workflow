#!/bin/bash

set -eu

#####################################################################################
# Script description: script to check the status of an experiment as reported
#                     by Rocoto
#####################################################################################

TEST_DIR=${1:-${TEST_DIR:-?}}  # Location of the root of the testing directory
pslot=${2:-${pslot:-?}}        # Name of the experiment being tested by this script

# TEST_DIR contains 2 directories;
# 1. HOMEgfs: clone of the global-workflow
# 2. RUNTESTS: A directory containing EXPDIR and ROTDIR for experiments
# # e.g. $> tree ./TEST_DIR
# ./TEST_DIR
# ├── HOMEgfs
# └── RUNTESTS
#     ├── ROTDIR
#     │   └── ${pslot}
#     └── EXPDIR
#         └── ${pslot}
HOMEgfs="${TEST_DIR}/HOMEgfs"
RUNTESTS="${TEST_DIR}/RUNTESTS"

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
sleep 30
if [[ ! -f "${db}" ]]; then
  echo "FATAL ERROR: Rocoto database file ${db} not found, experiment ${pslot} failed, ABORT!"
  exit 2
fi

# Experiment launched
rc=99
while true; do

  echo "Run rocotorun."
  rocotorun -v "${ROCOTO_VERBOSE:-0}" -w "${xml}" -d "${db}"

  # Wait before running rocotostat
  sleep 30

  # Get job statistics
  echo "Gather Rocoto statistics"
  rocotostat_output=$(rocotostat -w "${xml}" -d "${db}" -s | grep -v CYCLE) || true
  num_cycles=$(echo "${rocotostat_output}" | wc -l) || true
  num_done=$(echo "${rocotostat_output}" | grep -c Done) || true
  num_succeeded=$(rocotostat -w "${xml}" -d "${db}" -a | grep -c SUCCEEDED) || true
  num_failed=$(rocotostat -w "${xml}" -d "${db}" -a | grep -c -E 'FAIL|DEAD') || true

  echo "${pslot} Total Cycles: ${num_cycles} number done: ${num_done}"

  if [[ ${num_failed} -ne 0 ]]; then
    {
      echo "Experiment ${pslot} Terminated with ${num_failed} tasks failed at $(date)" || true
      echo "Experiment ${pslot} Terminated: *FAILED*"
    } >> "${RUNTESTS}/ci.log"

    error_logs=$(rocotostat -d "${db}" -w "${xml}" | grep -E 'FAIL|DEAD' | awk '{print "-c", $1, "-t", $2}' | xargs rocotocheck -d "${db}" -w "${xml}" | grep join | awk '{print $2}') || true
    {
     echo "Error logs:"
     echo "${error_logs}"
    } >> "${RUNTESTS}/ci.log"
    sed -i "s/\`\`\`//2g" "${RUNTESTS}/ci.log"
    sacct --format=jobid,jobname%35,WorkDir%100,stat | grep "${pslot}" | grep "${pr}\/RUNTESTS" |  awk '{print $1}' | xargs scancel || true
    rc=1
    break
  fi

  if [[ "${num_done}" -eq "${num_cycles}" ]]; then
    {
      echo "Experiment ${pslot} Completed at $(date)" || true
      echo "with ${num_succeeded} successfully completed jobs" || true
      echo "Experiment ${pslot} Completed: *SUCCESS*"
    } >> "${RUNTESTS}/ci.log"
    sed -i "s/\`\`\`//2g" "${RUNTESTS}/ci.log"
    rc=0
    break
  fi

  # Wait before running rocotorun again
  sleep 300

done

exit "${rc}"
