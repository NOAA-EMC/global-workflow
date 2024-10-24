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

REPO_URL=${REPO_URL:-"git@github.com:NOAA-EMC/global-workflow.git"}

#########################################################################
#  Set up runtime environment varibles for accounts on supproted machines
#########################################################################

source "${HOMEgfs}/ush/detect_machine.sh"
case ${MACHINE_ID} in
  hera | orion | hercules | wcoss2 | gaea)
   echo "Running Automated Testing on ${MACHINE_ID}"
   source "${HOMEgfs}/ci/platforms/config.${MACHINE_ID}"
   ;;
 *)
   echo "Unsupported platform. Exiting with error."
   exit 1
   ;;
esac
set +x
export HOMEgfs
source "${HOMEgfs}/ush/module-setup.sh"
source "${HOMEgfs}/ci/scripts/utils/ci_utils.sh"
module use "${HOMEgfs}/modulefiles"
module load "module_gwsetup.${MACHINE_ID}"
module list
# Load machine specific modules for ci (only wcoss2 is current)
if [[ "${MACHINE_ID}" == "wcoss2" ]]; then
  module load "module_gwci.${MACHINE_ID}"
fi
set -x
if ! command -v gh > /dev/null; then
   GH="${HOME}/bin/gh"
else
   GH=$(command -v gh)
fi
export GH

rocotostat=$(command -v rocotostat)
if [[ -z ${rocotostat} ]]; then
  echo "rocotostat not found on system"
  exit 1
else
  echo "rocotostat being used from ${rocotostat}"
fi
rocotocheck=$(command -v rocotocheck)
if [[ -z ${rocotocheck} ]]; then
  echo "rocotocheck not found on system"
  exit 1
else
  echo "rocotocheck being used from ${rocotocheck}"
fi

pr_list_dbfile="${GFS_CI_ROOT}/open_pr_list.db"

pr_list=""
if [[ -f "${pr_list_dbfile}" ]]; then
  pr_list=$("${HOMEgfs}/ci/scripts/utils/pr_list_database.py" --dbfile "${pr_list_dbfile}" --list Open Running) || true
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
  output_ci="${GFS_CI_ROOT}/PR/${pr}/output_runtime_${id}"
  output_ci_single="${GFS_CI_ROOT}/PR/${pr}/output_runtime_single.log"
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
    sed -i "1 i\`\`\`" "${output_ci}"
    sed -i "1 i\All CI Test Cases Passed on ${MACHINE_ID^}:" "${output_ci}"
    "${GH}" pr comment "${pr}" --repo "${REPO_URL}" --body-file "${output_ci}"
    "${HOMEgfs}/ci/scripts/utils/pr_list_database.py" --remove_pr "${pr}" --dbfile "${pr_list_dbfile}"
    # Check to see if this PR that was opened by the weekly tests and if so close it if it passed on all platforms
    weekly_labels=$(${GH} pr view "${pr}" --repo "${REPO_URL}"  --json headRefName,labels,author --jq 'select(.author.login | contains("emcbot")) | select(.headRefName | contains("weekly_ci")) | .labels[].name ') || true
    if [[ -n "${weekly_labels}" ]]; then
      num_platforms=$(find "${HOMEgfs}/ci/platforms" -type f -name "config.*" | wc -l)
      passed=0
      for platforms in "${HOMEgfs}"/ci/platforms/config.*; do
        machine=$(basename "${platforms}" | cut -d. -f2)
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
    if [[ -z "${pslot}" ]]; then
      echo "No experiments found in ${pslot_dir} .. exiting"
      exit 0
    fi
    xml="${pslot_dir}/${pslot}.xml"
    db="${pslot_dir}/${pslot}.db"
    if [[ ! -f "${db}" ]]; then
       continue
    fi

    set +e
    rocoto_state="$("${HOMEgfs}/ci/scripts/utils/rocotostat.py" -w "${xml}" -d "${db}")"
    rocoto_error=$?
    rm -f "${output_ci_single}"
    if [[ "${rocoto_error}" -ne 0 ]]; then
        "${GH}" pr edit --repo "${REPO_URL}" "${pr}" --remove-label "CI-${MACHINE_ID^}-Running" --add-label "CI-${MACHINE_ID^}-Failed"
        if [[ "${rocoto_state}" == "STALLED" ]]; then
          # shellcheck disable=SC2312
          "${GH}" pr comment "${pr}" --repo "${REPO_URL}" --body "Experiment ${pslot} **${rocoto_state}** on ${MACHINE_ID^} at $(date +'%D %r')"
          "${HOMEgfs}/ci/scripts/utils/pr_list_database.py" --remove_pr "${pr}" --dbfile "${pr_list_dbfile}"
          cancel_all_batch_jobs "${pr_dir}/RUNTESTS"
          exit "${rocoto_error}"
        fi
        error_logs=$("${rocotostat}" -d "${db}" -w "${xml}" | grep -E 'FAIL|DEAD' | awk '{print "-c", $1, "-t", $2}' | xargs "${rocotocheck}" -d "${db}" -w "${xml}" | grep join | awk '{print $2}') || true
        # shellcheck disable=SC2086
        ${HOMEgfs}/ci/scripts/utils/publish_logs.py --file ${error_logs} --repo "PR_${pr}" > /dev/null
        # shellcheck disable=SC2086
        gist_url="$("${HOMEgfs}/ci/scripts/utils/publish_logs.py" --file ${error_logs} --gist "PR_${pr}")"
        {
          echo "Experiment ${pslot} **${rocoto_state}** on ${MACHINE_ID^} at $(date +'%D %r')" || true
          echo ""
          echo "Error logs:"
          echo "\`\`\`"
          echo "${error_logs}"
          echo "\`\`\`"
          echo "Follow link here to view the contents of the above file(s): [(link)](${gist_url})"
        } >> "${output_ci_single}"
        "${GH}" pr comment "${pr}" --repo "${REPO_URL}" --body-file "${output_ci_single}"
        "${HOMEgfs}/ci/scripts/utils/pr_list_database.py" --remove_pr "${pr}" --dbfile "${pr_list_dbfile}"
        cancel_all_batch_jobs "${pr_dir}/RUNTESTS"
        exit "${rocoto_error}"
    fi
    if [[ "${rocoto_state}" == "DONE" ]]; then
      #Remove Experment cases that completed successfully
      "${HOMEgfs}/ci/scripts/utils/ci_utils_wrapper.sh" cleanup_experiment "${pslot_dir}"
      rm -f "${output_ci_single}"
      # echo "\`\`\`" > "${output_ci_single}"
      DATE=$(date +'%D %r')
      echo "Experiment ${pslot} **SUCCESS** on ${MACHINE_ID^} at ${DATE}" >> "${output_ci_single}"
      echo "Experiment ${pslot} *** SUCCESS *** at ${DATE}" >> "${output_ci}"
      # "${GH}" pr comment "${pr}" --repo "${REPO_URL}" --body-file "${output_ci_single}"
    fi
  done
done
