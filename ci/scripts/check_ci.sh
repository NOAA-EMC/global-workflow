#!/bin/bash
set -eux
#####################################################################################
#
# Script description: BASH script for checking for cases in a given PR and 
#                     running rocotostat on each to determine if the experiment has
#                     succeeded or faild.  This script is intended
#                     to run from within a cron job in the CI Managers account
# Abstract TODO
#####################################################################################

HOMEgfs="$(cd "$(dirname  "${BASH_SOURCE[0]}")/../.." >/dev/null 2>&1 && pwd )"
scriptname=$(basename "${BASH_SOURCE[0]}")
echo "Begin ${scriptname} at $(date -u)" || true
export PS4='+ $(basename ${BASH_SOURCE})[${LINENO}]'

GH=${HOME}/bin/gh
REPO_URL=${REPO_URL:-"https://github.com/NOAA-EMC/global-workflow.git"}

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
set -x
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

pr_list_dbfile="${GFS_CI_ROOT}/open_pr_list.db"

pr_list=""
if [[ -f "${pr_list_dbfile}" ]]; then
  pr_list=$("${HOMEgfs}/ci/scripts/pr_list_database.py" --display "${pr_list_dbfile}" | grep -v Failed | grep Running | awk '{print $1}') || true
fi
if [[ -z "${pr_list}" ]]; then
  echo "no PRs open and ready to run cases on .. exiting"
  exit 0
fi

#############################################################
# Loop throu all PRs in PR List and look for expirments in
# the RUNTESTS dir and for each one run runcotorun on them
#############################################################

for pr in ${pr_list}; do
  id=$("${GH}" pr view "${pr}" --repo "${REPO_URL}" --json id --jq '.id')
  echo "Processing Pull Request #${pr} and looking for cases"
  pr_dir="${GFS_CI_ROOT}/PR/${pr}"

  # If there is no RUNTESTS dir for this PR then cases have not been made yet
  if [[ ! -d "${pr_dir}/RUNTESTS" ]]; then
     continue
  fi
  num_cases=$(find "${pr_dir}/RUNTESTS" -mindepth 1 -maxdepth 1 -type d | wc -l) || true

  #Check for PR success  when ${pr_dir}/RUNTESTS is void of subfolders
  # since all successfull ones where previously removed
  if [[ "${num_cases}" -eq 0 ]] && [[ -d "${pr_dir}/RUNTESTS" ]]; then
    "${GH}" pr edit --repo "${REPO_URL}" "${pr}" --remove-label "CI-${MACHINE_ID^}-Running" --add-label "CI-${MACHINE_ID^}-Passed"
    "${GH}" pr comment "${pr}" --repo "${REPO_URL}" --body-file "${GFS_CI_ROOT}/PR/${pr}/output_${id}"
    "${HOMEgfs}/ci/scripts/pr_list_database.py" --remove_pr "${pr}" "${pr_list_dbfile}"
    # Completely remove the PR and its cloned repo on sucess of all cases
    rm -Rf "${pr_dir}" 
    continue 
  fi

  for cases in "${pr_dir}/RUNTESTS/"*; do
    pslot=$(basename "${cases}") || true
    xml="${pr_dir}/RUNTESTS/${pslot}/EXPDIR/${pslot}/${pslot}.xml"
    db="${pr_dir}/RUNTESTS/${pslot}/EXPDIR/${pslot}/${pslot}.db"
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
      } >> "${GFS_CI_ROOT}/PR/${pr}/output_${id}"
      error_logs=$("${rocotostat}" -d "${db}" -w "${xml}" | grep -E 'FAIL|DEAD' | awk '{print "-c", $1, "-t", $2}' | xargs "${rocotocheck}" -d "${db}" -w "${xml}" | grep join | awk '{print $2}') || true
      "${GH}" pr edit --repo "${REPO_URL}" "${pr}" --remove-label "CI-${MACHINE_ID^}-Running" --add-label "CI-${MACHINE_ID^}-Failed"
      {
       echo "Error logs:"
       echo "${error_logs}"
      } >> "${GFS_CI_ROOT}/PR/${pr}/output_${id}" 
      "${GH}" pr comment "${pr}" --repo "${REPO_URL}" --body-file "${GFS_CI_ROOT}/PR/${pr}/output_${id}"
      "${HOMEgfs}/ci/scripts/pr_list_database.py" --remove_pr "${pr}" "${pr_list_dbfile}"
      for kill_cases in "${pr_dir}/RUNTESTS/"*; do
         pslot=$(basename "${kill_cases}")
         sacct --format=jobid,jobname%35,WorkDir%100,stat | grep "${pslot}" | grep "PR\/${pr}\/RUNTESTS" |  awk '{print $1}' | xargs scancel || true
      done
      break
    fi
    if [[ "${num_done}" -eq  "${num_cycles}" ]]; then
      {
        echo "Experiment ${pslot} completed: *SUCCESS*"
        echo "Experiment ${pslot} Completed at $(date)" || true
        echo "with ${num_succeeded} successfully completed jobs" || true
      } >> "${GFS_CI_ROOT}/PR/${pr}/output_${id}"
      "${GH}" pr comment "${pr}" --repo "${REPO_URL}" --body-file "${GFS_CI_ROOT}/PR/${pr}/output_${id}"
      #Remove Experment cases that completed successfully
      rm -Rf "${pr_dir}/RUNTESTS/${pslot}"
    fi
  done
done
