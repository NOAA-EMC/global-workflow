#!/bin/bash
set -eux

#####################################################################################
#
# Script description: BASH script for checking for cases in a given PR and
#                     simply running rocotorun on each.  This script is intended
#                     to run from within a cron job in the CI Managers account
# Abstract TODO
#####################################################################################

ROOT_DIR="$(cd "$(dirname  "${BASH_SOURCE[0]}")/../.." >/dev/null 2>&1 && pwd )"
scriptname=$(basename "${BASH_SOURCE[0]}")
echo "Begin ${scriptname} at $(date -u)" || true
export PS4='+ $(basename ${BASH_SOURCE})[${LINENO}]'
GH=${HOME}/bin/gh
REPO_URL="https://github.com/NOAA-EMC/global-workflow.git"

#########################################################################
#  Set up runtime environment varibles for accounts on supproted machines
#########################################################################

source "${ROOT_DIR}/ush/detect_machine.sh"
case ${MACHINE_ID} in
  hera | orion)
   echo "Running Automated Testing on ${MACHINE_ID}"
   source "${ROOT_DIR}/ci/platforms/config.${MACHINE_ID}"
   ;;
 *)
   echo "Unsupported platform. Exiting with error."
   exit 1
   ;;
esac
set +x
source "${ROOT_DIR}/ush/module-setup.sh"
module use "${ROOT_DIR}/modulefiles"
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

pr_list_dbfile="${GFS_CI_ROOT}/open_pr_list.db"

pr_list=""
if [[ -f "${pr_list_dbfile}" ]]; then
  pr_list=$("${ROOT_DIR}/ci/scripts/pr_list_database.py" --display --dbfile "${pr_list_dbfile}" | grep -v Failed | grep Open | grep Running | awk '{print $1}' | head -"${max_concurrent_pr}") || true
fi
if [[ -z "${pr_list}" ]]; then
  echo "no open and built PRs that are ready for the cases to advance with rocotorun .. exiting"
  exit 0
fi

#############################################################
# Loop throu all PRs in PR List and look for expirments in
# the RUNTESTS dir and for each one run runcotorun on them
# only up to $max_concurrent_cases will advance at a time
#############################################################

for pr in ${pr_list}; do
  echo "Processing Pull Request #${pr} and looking for cases"
  pr_dir="${GFS_CI_ROOT}/PR/${pr}"
  # If the directory RUNTESTS is not present then
  # setupexpt.py has no been run yet for this PR
  if [[ ! -d "${pr_dir}/RUNTESTS" ]]; then
     continue
  fi
  num_cases=0
  for pslot_dir in "${pr_dir}/RUNTESTS/EXPDIR/"*; do
    if [[ ! -d "${pslot_dir}" ]]; then
       continue
    fi
    ((num_cases=num_cases+1))
    # No more than two cases are going forward at a time for each PR
    if [[ "${num_cases}" -gt "${max_concurrent_cases}" ]]; then
       continue
    fi
    pslot=$(basename "${pslot_dir}")
    xml="${pslot_dir}/${pslot}.xml"
    db="${pslot_dir}/${pslot}.db"
    echo "Running: ${rocotorun} -v 10 -w ${xml} -d ${db}"
    "${rocotorun}" -v 10 -w "${xml}" -d "${db}"
    set +e
    "${ROOT_DIR}"/ci/scripts/utils/rocoto_statcount.py -d "${db}" -w "${xml}"
    rc=$?
    
    if [[ "${rc}" -ne 0 ]]; then
      output_ci="${pr_dir}/output_runtime_single.log"
      {
        echo "${pslot} has *** STALLED **** on ${MACHINE_ID^}"
        echo "A job in experiment ${pslot} in ${pslot_dir}"
        echo "may have depenencies that are not being met"
      } >> "${output_ci}"
      sed -i "1 i\`\`\`" "${output_ci}"
      "${GH}" pr comment "${pr}" --repo "${REPO_URL}" --body-file "${output_ci}"
      "${GH}" pr edit --repo "${REPO_URL}" "${pr}" --remove-label "CI-${MACHINE_ID^}-Running" --add-label "CI-${MACHINE_ID^}-Failed"
      "${ROOT_DIR}/ci/scripts/pr_list_database.py" --remove_pr "${pr}" --dbfile "${pr_list_dbfile}"
    fi
  done
done
