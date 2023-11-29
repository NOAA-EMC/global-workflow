#!/bin/bash
set -eux

#####################################################################################
#
# Script description: Top level driver script for checking PR
#                     ready for CI regression testing
#
# Abstract:
#
# This script uses GitHub CLI to check for Pull Requests with CI-Ready-${machine} tags on the
# development branch for the global-workflow repo.  It then stages tests directories per
# PR number and calls clone-build_ci.sh to perform a clone and full build from the PR.
# It then is ready to run a suite of regression tests with various configurations
#######################################################################################

#################################################################
# TODO using static build for GitHub CLI until fixed in HPC-Stack
#################################################################
export GH=${HOME}/bin/gh
export REPO_URL=${REPO_URL:-"https://github.com/NOAA-EMC/global-workflow.git"}

################################################################
# Setup the reletive paths to scripts and PS4 for better logging
################################################################
ROOT_DIR="$(cd "$(dirname  "${BASH_SOURCE[0]}")/../.." >/dev/null 2>&1 && pwd )"
scriptname=$(basename "${BASH_SOURCE[0]}")
echo "Begin ${scriptname} at $(date  +'%D %r')" || true
export PS4='+ $(basename ${BASH_SOURCE})[${LINENO}]'

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

######################################################
# setup runtime env for correct python install and git
######################################################
set +x
source "${ROOT_DIR}/ci/scripts/utils/ci_utils.sh"
source "${ROOT_DIR}/ush/module-setup.sh"
module use "${ROOT_DIR}/modulefiles"
module load "module_gwsetup.${MACHINE_ID}"
set -x

############################################################
# query repo and get list of open PRs with tags {machine}-CI
############################################################

pr_list_dbfile="${GFS_CI_ROOT}/open_pr_list.db"

if [[ ! -f "${pr_list_dbfile}" ]]; then
  "${ROOT_DIR}/ci/scripts/pr_list_database.py" --create --dbfile "${pr_list_dbfile}"
fi

pr_list=$(${GH} pr list --repo "${REPO_URL}" --label "CI-${MACHINE_ID^}-Ready" --state "open" | awk '{print $1}') || true

for pr in ${pr_list}; do
  pr_dir="${GFS_CI_ROOT}/PR/${pr}"
  db_list=$("${ROOT_DIR}/ci/scripts/pr_list_database.py" --add_pr "${pr}" --dbfile "${pr_list_dbfile}")
  output_ci_single="${GFS_CI_ROOT}/PR/${pr}/output_single.log"
  #############################################################
  # Check if a Ready labeled PR has changed back from once set
  # and in that case remove all previous jobs in scheduler and
  # and remove PR from filesystem to start clean
  #############################################################
  if [[ "${db_list}" == *"already is in list"* ]]; then
    driver_ID=$("${ROOT_DIR}/ci/scripts/pr_list_database.py" --dbfile "${pr_list_dbfile}" --display "${pr}" | awk '{print $4}') || true
    driver_PID=$(echo "${driver_ID}" | cut -d":" -f1) || true
    driver_HOST=$(echo "${driver_ID}" | cut -d":" -f2) || true
    host_name=$(hostname -s)
    rm -f "${output_ci_single}"
    {
      echo "CI Update on ${MACHINE_ID^} at $(date +'%D %r')" || true
      echo "================================================="
      echo "PR:${pr} Reset to ${MACHINE_ID^}-Ready by user and is now restarting CI tests" || true
    } >> "${output_ci_single}"
    if [[ "${driver_PID}" -ne 0 ]]; then
      echo "Driver PID: ${driver_PID} no longer running this build having it killed"
      if [[ "${driver_HOST}" == "${host_name}"  ]]; then
        pstree -A -p "${driver_PID}" | grep -Pow "(?<=\()[0-9]+(?=\))" | xargs kill
        sleep 30
      else
        ssh "${driver_HOST}" 'pstree -A -p "${driver_PID}" | grep -Eow "[0-9]+" | xargs kill'
        sleep 30
      fi
      {
        echo "Driver PID: Requested termination of ${driver_PID} and children on ${driver_HOST}"
        echo "Driver PID: has restarted as $$ on ${host_name}"
      } >> "${output_ci_single}"
    fi

    experiments=$(find "${pr_dir}/RUNTESTS/EXPDIR" -mindepth 1 -maxdepth 1 -type d) || true
    if [[ -z "${experiments}" ]]; then
       echo "No current experiments to cancel in PR: ${pr} on ${MACHINE_ID^}" >> "${output_ci_single}"
    else
      for case in ${experiments}; do
        case_name=$(basename "${case}")
        cancel_slurm_jobs "${case_name}"
        {
          echo "Canceled all jobs for experiment ${case_name} in PR:${pr} on ${MACHINE_ID^}"
        } >> "${output_ci_single}"
      done
    fi
    sed -i "1 i\`\`\`" "${output_ci_single}"
    "${GH}" pr comment "${pr}" --repo "${REPO_URL}" --body-file "${output_ci_single}"
    db_list=$("${ROOT_DIR}/ci/scripts/pr_list_database.py" --remove_pr "${pr}" --dbfile "${pr_list_dbfile}")
    db_list=$("${ROOT_DIR}/ci/scripts/pr_list_database.py" --add_pr "${pr}" --dbfile "${pr_list_dbfile}")
  fi
done

pr_list=""
if [[ -f "${pr_list_dbfile}" ]]; then
  pr_list=$("${ROOT_DIR}/ci/scripts/pr_list_database.py" --display --dbfile "${pr_list_dbfile}" | grep -v Failed | grep Open | grep Ready | awk '{print $1}') || true
fi
if [[ -z "${pr_list+x}" ]]; then
  echo "no PRs open and ready for checkout/build .. exiting"
  exit 0
fi


#############################################################
# Loop throu all open PRs
# Clone, checkout, build, creat set of cases, for each
#############################################################

for pr in ${pr_list}; do
  # Skip pr's that are currently Building for when overlapping driver scripts are being called from within cron
  pr_building=$("${ROOT_DIR}/ci/scripts/pr_list_database.py" --display "${pr}" --dbfile "${pr_list_dbfile}" | grep Building) || true
  if [[ -z "${pr_building+x}" ]]; then
      continue
  fi
  id=$("${GH}" pr view "${pr}" --repo "${REPO_URL}" --json id --jq '.id')
  pr_dir="${GFS_CI_ROOT}/PR/${pr}"
  output_ci="${pr_dir}/output_ci_${id}"
  output_ci_single="${GFS_CI_ROOT}/PR/${pr}/output_single.log"
  driver_build_PID=$$
  driver_build_HOST=$(hostname -s)
  "${GH}" pr edit --repo "${REPO_URL}" "${pr}" --remove-label "CI-${MACHINE_ID^}-Ready" --add-label "CI-${MACHINE_ID^}-Building"
  "${ROOT_DIR}/ci/scripts/pr_list_database.py" --dbfile "${pr_list_dbfile}" --update_pr "${pr}" Open Building "${driver_build_PID}:${driver_build_HOST}"
  rm -Rf "${pr_dir}"
  mkdir -p "${pr_dir}"
  {
    echo "CI Update on ${MACHINE_ID^} at $(date +'%D %r')" || true
    echo "============================================"
    echo "Cloning and Building global-workflow PR: ${pr}"
    echo "with PID: ${driver_build_PID} on host: ${driver_build_HOST}"
    echo ""
  } >> "${output_ci_single}"
  sed -i "1 i\`\`\`" "${output_ci_single}"
  "${GH}" pr comment "${pr}" --repo "${REPO_URL}" --body-file "${output_ci_single}"
  set +e
  "${ROOT_DIR}/ci/scripts/clone-build_ci.sh" -p "${pr}" -d "${pr_dir}" -o "${output_ci}"
  ci_status=$?
  ##################################################################
  # Checking for special case when Ready label was updated
  # but a race condtion caused the clone-build_ci.sh to start
  # and this instance fails before it was killed.  In th case we
  # we need to exit this instance of the driver script
  #################################################################
  if [[ ${ci_status} -ne 0 ]]; then
     build_PID_check=$("${ROOT_DIR}/ci/scripts/pr_list_database.py" --display "{pr}" --dbfile "${pr_list_dbfile}" | awk '{print $4}' | cut -d":" -f1) || true
     if [[ "${build_PID_check}" -ne "$$" ]]; then
        echo "Driver build PID: ${build_PID_check} no longer running this build ... exiting"
        exit 0
     fi
  fi
  set -e
  if [[ ${ci_status} -eq 0 ]]; then
    "${ROOT_DIR}/ci/scripts/pr_list_database.py" --dbfile "${pr_list_dbfile}" --update_pr "${pr}" Open Built "0:0"
    #setup space to put an experiment
    # export RUNTESTS for yaml case files to pickup
    export RUNTESTS="${pr_dir}/RUNTESTS"
    rm -Rf "${pr_dir:?}/RUNTESTS/"*

    #############################################################
    # loop over every yaml file in the PR's ci/cases
    # and create an run directory for each one for this PR loop
    #############################################################
    HOMEgfs="${pr_dir}/global-workflow"
    cd "${HOMEgfs}"
    pr_sha=$(git rev-parse --short HEAD)

    for yaml_config in "${HOMEgfs}/ci/cases/pr/"*.yaml; do
      case=$(basename "${yaml_config}" .yaml) || true
      # export pslot for yaml case files to pickup
      export pslot="${case}_${pr_sha}"
      rm -Rf "${STMP}/RUNDIRS/${pslot}"
      set +e
      export LOGFILE_PATH="${HOMEgfs}/ci/scripts/create_experiment.log"
      rm -f "${LOGFILE_PATH}"
      "${HOMEgfs}/workflow/create_experiment.py" --yaml "${HOMEgfs}/ci/cases/pr/${case}.yaml"  > "${LOGFILE_PATH}" 2>&1
      ci_status=$?
      set -e
      if [[ ${ci_status} -eq 0 ]]; then
        last_line=$(tail -1 "${LOGFILE_PATH}")
        if [[ "${last_line}" == *"Skipping creation"* ]]; then
          action="Skipped"
        else
          action="Completed"
        fi
        {
          echo "Case setup: ${action} for experiment ${pslot}" || true
        } >> "${output_ci}"
      else
        {
          echo "*** Failed *** to create experiment: ${pslot} on ${MACINE_ID^}"
          echo ""
          cat "${LOGFILE_PATH}"
        } >> "${output_ci}"
        "${GH}" pr edit "${pr}" --repo "${REPO_URL}" --remove-label "CI-${MACHINE_ID^}-Building" --add-label "CI-${MACHINE_ID^}-Failed"
        "${ROOT_DIR}/ci/scripts/pr_list_database.py" --remove_pr "${pr}" --dbfile "${pr_list_dbfile}"
        "${GH}" pr comment "${pr}" --repo "${REPO_URL}" --body-file "${output_ci}"
        exit 1
      fi
    done

    "${GH}" pr edit --repo "${REPO_URL}" "${pr}" --remove-label "CI-${MACHINE_ID^}-Building" --add-label "CI-${MACHINE_ID^}-Running"
    "${ROOT_DIR}/ci/scripts/pr_list_database.py" --dbfile "${pr_list_dbfile}" --update_pr "${pr}" Open Running "0:0"
    "${GH}" pr comment "${pr}" --repo "${REPO_URL}" --body-file "${output_ci}"

  else
    {
      echo "Failed on cloning and building global-workflowi PR: ${pr}"
      echo "CI on ${MACHINE_ID^} failed to build on $(date) for repo ${REPO_URL}" || true
    } >> "${output_ci}"
    "${GH}" pr edit "${pr}" --repo "${REPO_URL}" --remove-label "CI-${MACHINE_ID^}-Building" --add-label "CI-${MACHINE_ID^}-Failed"
    "${ROOT_DIR}/ci/scripts/pr_list_database.py" --remove_pr "${pr}" --dbfile "${pr_list_dbfile}"
    "${GH}" pr comment "${pr}" --repo "${REPO_URL}" --body-file "${output_ci}"
  fi

done # looping over each open and labeled PR

##########################################
# scrub working directory for older files
##########################################
#
#find "${GFS_CI_ROOT}/PR/*" -maxdepth 1 -mtime +3 -exec rm -rf {} \;
