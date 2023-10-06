#!/bin/bash
set -eux
#####################################################################################
#
# Script description: BASH script for checking for cases in a given PR and 
#                     running rocotostat on each to determine if the experiment has
#                     succeeded or faild.  This script is intended
#                     to run from within a cron job in the CI Managers account
#####################################################################################

HOMEgfs="$(cd "$(dirname  "${BASH_SOURCE[0]}")/../.." >/dev/null 2>&1 && pwd )"
scriptname=$(basename "${BASH_SOURCE[0]}")
echo "Begin ${scriptname} at $(date -u)" || true
export PS4='+ $(basename ${BASH_SOURCE})[${LINENO}]'

GH=${HOME}/bin/gh
REPO_URL="https://github.com/NOAA-EMC/global-workflow.git"

#########################################################################
#  Set up runtime environment varibles for accounts on supproted machines
#########################################################################

source "${HOMEgfs}/ush/detect_machine.sh"
case ${MACHINE_ID} in
  hera | orion)
   echo "Running Automated Testing on ${MACHINE_ID}"
   source "${HOMEgfs}/ci/platforms/config.${MACHINE_ID}"
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
  pr_list=$("${HOMEgfs}/ci/scripts/pr_list_database.py" --dbfile "${pr_list_dbfile}" --display | grep -v Failed | grep Running | awk '{print $1}') || true
fi
if [[ -z "${pr_list+x}" ]]; then
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

  #Check for PR success  when ${pr_dir}/RUNTESTS/EXPDIR is void of subfolders
  # since all successfull ones where previously removed
  # shellcheck disable=SC2312
  if [[ -z $(ls -A "${pr_dir}/RUNTESTS/EXPDIR") ]] ; then
    "${GH}" pr edit --repo "${REPO_URL}" "${pr}" --remove-label "CI-${MACHINE_ID^}-Running" --add-label "CI-${MACHINE_ID^}-Passed"
    sed -i "s/\`\`\`//2g" "${GFS_CI_ROOT}/PR/${pr}/output_${id}"
    "${GH}" pr comment "${pr}" --repo "${REPO_URL}" --body-file "${GFS_CI_ROOT}/PR/${pr}/output_${id}"
    "${HOMEgfs}/ci/scripts/pr_list_database.py" --remove_pr "${pr}" --dbfile "${pr_list_dbfile}"
    # Check to see if this PR that was opened by the weekly tests and if so close it if it passed on all platforms
    weekly_labels=$(${GH} pr view ${pr} --repo ${REPO_URL}  --json headRefName,labels,author --jq 'select(.author.login | contains("emcbot")) | select(.headRefName | contains("weekly_ci")) | .labels[].name ') || true
    if [[ -n "${weekly_labels}" ]]; then
      num_platforms=$(ls -1 ../platforms/config.* | wc -l)
      passed=0
      for platforms in ../platforms/config.*; do
        machine=$(basename $platforms | cut -d. -f2)
        if [[ "${weekly_labels}" == *"CI-${machine^}-Passed"* ]]; then
          ((passed=passed+1))
        fi
      done
      if [[ "${passed}" == "${num_platforms}" ]]; then
        "${GH}" pr close --repo "${REPO_URL}" "${pr}"
      fi
    fi
    # Completely remove the PR and its cloned repo on sucess
    # of all cases on this platform
    rm -Rf "${pr_dir}" 
    continue 
  fi

  for pslot_dir in "${pr_dir}/RUNTESTS/EXPDIR/"*; do
    pslot=$(basename "${pslot_dir}") || true
    if [[ -z "${pslot+x}" ]]; then
      echo "No experiments found in ${pslot_dir} .. exiting"
      exit 0
    fi
    xml="${pslot_dir}/${pslot}.xml"
    db="${pslot_dir}/${pslot}.db"
    if [[ ! -f "${db}" ]]; then
       continue
    fi
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
      sed -i "s/\`\`\`//2g" "${GFS_CI_ROOT}/PR/${pr}/output_${id}"
      "${GH}" pr comment "${pr}" --repo "${REPO_URL}" --body-file "${GFS_CI_ROOT}/PR/${pr}/output_${id}"
      "${HOMEgfs}/ci/scripts/pr_list_database.py" --remove_pr "${pr}" --dbfile "${pr_list_dbfile}"
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
      sed -i "s/\`\`\`//2g" "${GFS_CI_ROOT}/PR/${pr}/output_${id}"
      "${GH}" pr comment "${pr}" --repo "${REPO_URL}" --body-file "${GFS_CI_ROOT}/PR/${pr}/output_${id}"
      #Remove Experment cases that completed successfully
      rm -Rf "${pslot_dir}"
      rm -Rf "${pr_dir}/RUNTESTS/COMROT/${pslot}"
    fi
  done
done
