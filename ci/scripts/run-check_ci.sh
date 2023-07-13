#!/bin/bash

#####################################################################################
#
# Script description: BASH script for checking for cases in a given PR and
#                     simply running rocotorun on each.  This script is intended
#                     to run from within a cron job in the CI Managers account
#####################################################################################

HOMEgfs=${1:-${HOMEgfs:-?}}
EXPDIR=${2:-${EXPDIR:-?}}
pslot=${3:-${pslot:-?}}

source "${HOMEgfs}/ush/preamble.sh"
source "${HOMEgfs}/workflow/gw_setup.sh"

xml="${EXPDIR}/${pslot}/${pslot}.xml"
db="${EXPDIR}/${pslot}/${pslot}.db"

# Ensure the XML is present for the experiment
if [[ ! -f "${xml}" ]]; then
  echo "XML file ${xml} not found, experiment ${pslot} failed"
  exit 1
fi

rc=99
while true; do

  echo "Running: rocotorun -v 10 -w ${xml} -d ${db}"
  rocotorun -v 10 -w "${xml}" -d "${db}"

  # Wait before running rocotostat
  sleep 30
  if [[ ! -f "${db}" ]]; then
    echo "Database file ${db} not found, experiment ${pslot} failed"
    exit 2
  fi

  # Get job statistics
  rocotostat_output=$(rocotostat -w "${xml}" -d "${db}" -s | grep -v CYCLE) || true
  num_cycles=$(echo "${rocotostat_output}" | wc -l) || true
  num_done=$(echo "${rocotostat_output}" | grep -c Done) || true
  num_succeeded=$(rocotostat -w "${xml}" -d "${db}" -a | grep -c SUCCEEDED) || true
  num_failed=$(rocotostat -w "${xml}" -d "${db}" -a | grep -c -E 'FAIL|DEAD') || true

  echo "${pslot} Total Cycles: ${num_cycles} number done: ${num_done}"

  if [[ ${num_failed} -ne 0 ]]; then
    {
      echo "Experiment ${pslot} Terminated: *FAILED*"
      echo "Experiment ${pslot} Terminated with ${num_failed} tasks failed at $(date)" || true
    } >> "${HOMEgfs}/global-workflow.log"

    error_logs=$(rocotostat -d "${db}" -w "${xml}" | grep -E 'FAIL|DEAD' | awk '{print "-c", $1, "-t", $2}' | xargs rocotocheck -d "${db}" -w "${xml}" | grep join | awk '{print $2}') || true
    {
     echo "Error logs:"
     echo "${error_logs}"
    } >> "${HOMEgfs}/global-workflow.log"
    sed -i "s/\`\`\`//2g" "${HOMEgfs}/global-workflow.log"
    sacct --format=jobid,jobname%35,WorkDir%100,stat | grep "${pslot}" | grep "${pr}\/RUNTESTS" |  awk '{print $1}' | xargs scancel || true
    rc=1
    break
  fi

  if [[ "${num_done}" -eq "${num_cycles}" ]]; then
    {
      echo "Experiment ${pslot} completed: *SUCCESS*"
      echo "Experiment ${pslot} Completed at $(date)" || true
      echo "with ${num_succeeded} successfully completed jobs" || true
    } >> "${HOMEgfs}/global-workflow.log"
    sed -i "s/\`\`\`//2g" "${HOMEgfs}/global-workflow.log"
    rc=0
    break
  fi

  # Wait before running rocotorun again
  sleep 300

done

exit "${rc}"
