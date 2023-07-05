#!/bin/bash
set -eux

#####################################################################################
#
# Script description: BASH script for checking for cases in a given PR and 
#                     simply running rocotorun on each.  This script is intended
#                     to run from within a cron job in the CI Managers account
# Abstract TODO
#####################################################################################

scriptname=$(basename "${BASH_SOURCE[0]}")
echo "Begin ${scriptname} at $(date -u)" || true
export PS4='+ $(basename ${BASH_SOURCE})[${LINENO}]'

#########################################################################
#  Set up runtime environment varibles for accounts on supproted machines
#########################################################################

source "${HOMEgfs}/ush/detect_machine.sh"
case ${MACHINE_ID} in
  hera | orion)
   echo "Running Automated Testing on ${MACHINE_ID}"
   source "${HOMEgfs}/ci/platforms/${MACHINE_ID}.sh"
   ;;
 *)
   echo "Unsupported platform. Exiting with error."
   exit 1
   ;;
esac
set +x
source "${HOMEgfs}/ush/module-setup.sh"
module use "${HOMEgfs}/modulefiles"
module load "module_gwsetup.${MACHINE_ID}"
module list
set -eux
rocotorun=$(command -v rocotorun)
if [[ -z ${rocotorun} ]]; then
  echo "rocotorun not found on system"
  exit 1
else
  echo "rocotorun being used from ${rocotorun}"
fi
rocotostat=$(command -v rocotostat)
if [[ -z ${rocotostat+x} ]]; then
  echo "rocotostat not found on system"
  exit 1
else
  echo "rocotostat being used from ${rocotostat}"
fi
rocotocheck=$(command -v rocotocheck)
if [[ -z ${rocotocheck+x} ]]; then
  echo "rocotocheck not found on system"
  exit 1
else
  echo "rocotocheck being used from ${rocotocheck}"
fi

RUN_COMPLETE="FALSE"
while [ ${RUN_COMPLETE} == "FALSE" ]; do

  xml="${EXPDIR}/${pslot}/${pslot}.xml"
  db="${EXPDIR}/${pslot}/${pslot}.db"

  if [[ ! -f "${xml}" ]]; then
    echo "XML file ${xml} not found, experment ${pslot} failed"
    exit 1 
  fi
  echo "Running: ${rocotorun} -v 6 -w ${xml} -d ${db}"
  "${rocotorun}" -v 10 -w "${xml}" -d "${db}"

  # Wait before running rocotostat
  sleep 60
  if [[ ! -f "${db}" ]]; then
    echo "Database file ${db} not found, experment ${pslot} failed"
    exit 1 
  fi

  id=$("${GH}" pr view "${pr}" --repo "${REPO_URL}" --json id --jq '.id')
  rocoto_stat_output=$("${rocotostat}" -w "${xml}" -d "${db}" -s | grep -v CYCLE) || true
  num_cycles=$(echo "${rocoto_stat_output}" | wc -l) || true
  num_done=$(echo "${rocoto_stat_output}" | grep -c Done) || true
  num_succeeded=$("${rocotostat}" -w "${xml}" -d "${db}" -a | grep -c SUCCEEDED) || true
  echo "${pslot} Total Cycles: ${num_cycles} number done: ${num_done}" || true
  num_failed=$("${rocotostat}" -w "${xml}" -d "${db}" -a | grep -c -E 'FAIL|DEAD') || true

  if [[ ${num_failed} -ne 0 ]]; then
    {
      echo "Experiment ${pslot} Terminated: *FAILED*"
      echo "Experiment ${pslot} Terminated with ${num_failed} tasks failed at $(date)" || true
    } >> "${HOMEgfs}/output_${id}"
    error_logs=$("${rocotostat}" -d "${db}" -w "${xml}" | grep -E 'FAIL|DEAD' | awk '{print "-c", $1, "-t", $2}' | xargs "${rocotocheck}" -d "${db}" -w "${xml}" | grep join | awk '{print $2}') || true
    "${GH}" pr edit --repo "${REPO_URL}" "${pr}" --remove-label "CI-${MACHINE_ID^}-Running" --add-label "CI-${MACHINE_ID^}-Failed"
    {
     echo "Error logs:"
     echo "${error_logs}"
    } >> "${HOMEgfs}/output_${id}" 
    sed -i "s/\`\`\`//2g" "${GFS_CI_ROOT}/PR/${pr}/output_${id}"
    "${GH}" pr comment "${pr}" --repo "${REPO_URL}" --body-file "${HOMEgfs}/output_${id}"
    sacct --format=jobid,jobname%35,WorkDir%100,stat | grep "${pslot}" | grep "PR\/${pr}\/RUNTESTS" |  awk '{print $1}' | xargs scancel || true
  fi
  
  if [[ "${num_done}" -eq  "${num_cycles}" ]]; then
    {
      echo "Experiment ${pslot} completed: *SUCCESS*"
      echo "Experiment ${pslot} Completed at $(date)" || true
      echo "with ${num_succeeded} successfully completed jobs" || true
    } >> "${HOMEgfs}/output_${id}"
    sed -i "s/\`\`\`//2g" "${HOMEgfs}/output_${id}"
    "${GH}" pr comment "${pr}" --repo "${REPO_URL}" --body-file "${HOMEgfs}/output_${id}"
    RUN_COMPLETE="TRUE"
  fi

  # Wait before running rocotorun again
  sleep 240

done