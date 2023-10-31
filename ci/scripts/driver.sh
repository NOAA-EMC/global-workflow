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
echo "Begin ${scriptname} at $(date -u)" || true
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
  pr_id=0
  #############################################################
  # Check if a Ready labeled PR has changed back from once set
  # and in that case remove all previous jobs in scheduler and
  # and remove PR from filesystem to start clean
  #############################################################
  if [[ "${db_list}" == *"already is in list"* ]]; then
    pr_id=$("${ROOT_DIR}/ci/scripts/pr_list_database.py" --dbfile "${pr_list_dbfile}" --display "${pr}" | awk '{print $4}') || true
    pr_id=$((pr_id+1))
    "${ROOT_DIR}/ci/scripts/pr_list_database.py" --dbfile "${pr_list_dbfile}" --update_pr "${pr}" Open Ready "${pr_id}"
    for cases in "${pr_dir}/RUNTESTS/"*; do
      if [[ -z "${cases+x}" ]]; then
         break
      fi
      pslot=$(basename "${cases}")
      sacct --format=jobid,jobname%35,WorkDir%100,stat | grep "${pslot}" | grep "PR\/${pr}\/RUNTESTS" |  awk '{print $1}' | xargs scancel || true
    done
    rm -Rf "${pr_dir}"
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
  "${GH}" pr edit --repo "${REPO_URL}" "${pr}" --remove-label "CI-${MACHINE_ID^}-Ready" --add-label "CI-${MACHINE_ID^}-Building"
  "${ROOT_DIR}/ci/scripts/pr_list_database.py" --dbfile "${pr_list_dbfile}" --update_pr "${pr}" Open Building
  echo "Processing Pull Request #${pr}"
  pr_dir="${GFS_CI_ROOT}/PR/${pr}"
  rm -Rf "${pr_dir}"
  mkdir -p "${pr_dir}"
  # call clone-build_ci to clone and build PR
  id=$("${GH}" pr view "${pr}" --repo "${REPO_URL}" --json id --jq '.id')
  set +e
  "${ROOT_DIR}/ci/scripts/clone-build_ci.sh" -p "${pr}" -d "${pr_dir}" -o "${pr_dir}/output_${id}"
  ci_status=$?
  ##################################################################
  # Checking for special case when Ready label was updated
  # that cause a running driver exit fail because was currently
  # building so we force and exit 0 instead to does not get relabled
  #################################################################
  if [[ ${ci_status} -ne 0 ]]; then
     pr_id_check=$("${ROOT_DIR}/ci/scripts/pr_list_database.py" --display "{pr}" --dbfile "${pr_list_dbfile}" | awk '{print $4}') || true
     if [[ "${pr_id}" -ne "${pr_id_check}" ]]; then
        exit 0
     fi
  fi
  set -e
  if [[ ${ci_status} -eq 0 ]]; then
    "${ROOT_DIR}/ci/scripts/pr_list_database.py" --dbfile "${pr_list_dbfile}" --update_pr "${pr}" Open Built
    #setup space to put an experiment
    # export RUNTESTS for yaml case files to pickup
    export RUNTESTS="${pr_dir}/RUNTESTS"
    #rm -Rf "${pr_dir:?}/RUNTESTS/"*

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
      "${HOMEgfs}/workflow/create_experiment.py" --yaml "${HOMEgfs}/ci/cases/pr/${case}.yaml"
      ci_status=$?
      set -e
      if [[ ${ci_status} -eq 0 ]]; then
        {
          echo "Created experiment:            *SUCCESS*"
          echo "Case setup: Completed at $(date) for experiment ${pslot}" || true
        } >> "${GFS_CI_ROOT}/PR/${pr}/output_${id}"
        "${GH}" pr edit --repo "${REPO_URL}" "${pr}" --remove-label "CI-${MACHINE_ID^}-Building" --add-label "CI-${MACHINE_ID^}-Running"
        "${ROOT_DIR}/ci/scripts/pr_list_database.py" --dbfile "${pr_list_dbfile}" --update_pr "${pr}" Open Running
      else
        {
          echo "Failed to create experiment:  *FAIL* ${pslot}"
          echo "Experiment setup: failed at $(date) for experiment ${pslot}" || true
          echo ""
          cat "${LOGFILE_PATH}"
        } >> "${GFS_CI_ROOT}/PR/${pr}/output_${id}"
        "${GH}" pr edit "${pr}" --repo "${REPO_URL}" --remove-label "CI-${MACHINE_ID^}-Building" --add-label "CI-${MACHINE_ID^}-Failed"
        "${ROOT_DIR}/ci/scripts/pr_list_database.py" --remove_pr "${pr}" --dbfile "${pr_list_dbfile}"
      fi
    done

  else
    {
      echo '```'
      echo "Failed on cloning and building global-workflowi PR: ${pr}"
      echo "CI on ${MACHINE_ID^} failed to build on $(date) for repo ${REPO_URL}" || true
    } >> "${GFS_CI_ROOT}/PR/${pr}/output_${id}"
    "${GH}" pr edit "${pr}" --repo "${REPO_URL}" --remove-label "CI-${MACHINE_ID^}-Building" --add-label "CI-${MACHINE_ID^}-Failed"
    "${ROOT_DIR}/ci/scripts/pr_list_database.py" --remove_pr "${pr}" --dbfile "${pr_list_dbfile}"
  fi
  sed -i "s/\`\`\`//2g" "${GFS_CI_ROOT}/PR/${pr}/output_${id}"
  "${GH}" pr comment "${pr}" --repo "${REPO_URL}" --body-file "${GFS_CI_ROOT}/PR/${pr}/output_${id}"

done # looping over each open and labeled PR

##########################################
# scrub working directory for older files
##########################################
#
#find "${GFS_CI_ROOT}/PR/*" -maxdepth 1 -mtime +3 -exec rm -rf {} \;
