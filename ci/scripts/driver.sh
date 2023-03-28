#!/bin/bash --login
set -eux

#####################################################################################
#
# Script description: Top level driver script for checking PR
#                     ready for CI regression testing
#
# Abstract:
#
# This script uses GitHub CLI to check for Pull Requests with {machine}-CI tags on the
# development branch for the global-workflow repo.  It then stages tests directories per
# PR number and calls clone-build_ci.sh to perform a clone and full build from $(HOMEgfs)/sorc
# of the PR. It then is ready to run a suite of regression tests with various
# configurations with run_tests.py.
#######################################################################################

#################################################################
# TODO using static build for GitHub CLI until fixed in HPC-Stack
#################################################################
export GH=/home/Terry.McGuinness/bin/gh

################################################################
# Setup the reletive paths to scripts and PS4 for better logging 
################################################################
HOMEGFS_DIR="$(cd "$(dirname  "${BASH_SOURCE[0]}")/../.." >/dev/null 2>&1 && pwd )"
scriptname=$(basename "${BASH_SOURCE[0]}")
echo "Begin ${scriptname} at $(date -u)" || true
export PS4='+ $(basename ${BASH_SOURCE})[${LINENO}]'

#########################################################################
#  Set up runtime environment varibles for accounts on supproted machines
#########################################################################

source "${HOMEGFS_DIR}/ush/detect_machine.sh"
case ${MACHINE_ID} in
  hera | orion)
    echo "Running Automated Testing on ${MACHINE_ID}"
    source "${HOMEGFS_DIR}/ci/environments/${MACHINE_ID}.sh"
    ;;
  *)
    echo "Unsupported platform. Exiting with error."
    exit 1
    ;;
esac
export MACHINE_ID
REPO_URL=${REPO_URL:-"https://github.com/NOAA-EMC/global-workflow.git"}
export REPO_URL

############################################################
# query repo and get list of open PRs with tags {machine}-CI
############################################################
set -eux
pr_list_file="open_pr_list"
rm -f "${pr_list_file}"
list=$(${GH} pr list --repo "${REPO_URL}" --label "${MACHINE_ID^}-CI" --state "open")
list=$(echo "${list}" | awk '{print $1;}' > "${GFS_CI_ROOT}/${pr_list_file}")

if [[ -s "${GFS_CI_ROOT}/${pr_list_file}" ]]; then
 pr_list=$(cat "${GFS_CI_ROOT}/${pr_list_file}")
else
 echo "no PRs to process .. exit"
 exit 0
fi 

#############################################################
# Loop throu all open PRs
# Clone, checkout, build, creat set of experiments, for each
#############################################################

for pr in ${pr_list}; do
  "${GH}" pr edit --repo "${REPO_URL}" "${pr}" --remove-label "${MACHINE_ID^}-CI" --add-label "${MACHINE_ID^}-Running"
  echo "Processing Pull Request #${pr}"
  pr_dir="${GFS_CI_ROOT}/PR/${pr}"
  mkdir -p "${pr_dir}"
  # call clone-build_ci to clone and build PR
  id=$("${GH}" pr view "${pr}" --repo "${REPO_URL}" --json id --jq '.id')
  #"${HOMEGFS_DIR}/ci/scripts/clone-build_ci.sh" -p "${pr}" -d "${pr_dir}" -o "${pr_dir}/output_${id}"
  echo "SKIPPING clone-build script"
  ci_status=$?
  if [[ ${ci_status} -eq 0 ]]; then
    #setup runtime env for correct python install
    module use "${pr_dir}/global-workflow/modulefiles"
    module load "module_setup.${MACHINE_ID}"
    module list
    #setup space to put an experiment
    export RUNTEST="${pr_dir}/RUNTEST"
    rm -Rf "${RUNTEST:?}"/*
    mkdir -p "${RUNTEST}"
    #############################################################
    # loop over every yaml file in ${HOMEGFS_DIR}/ci/experiments
    # and create an run directory for each one for this PR loop
    #############################################################
    for yaml_config in "${HOMEGFS_DIR}/ci/experiments/"*.yaml; do
      pslot=$(basename "${yaml_config}" .yaml) || true
      "${HOMEGFS_DIR}/ci/scripts/create_experiment.py" --yaml "${HOMEGFS_DIR}/ci/experiments/${pslot}.yaml" --dir "${pr_dir}/global-workflow"
      ci_status=$?
      if [[ ${ci_status} -eq 0 ]]; then
        {
          echo "Created experiment"
          echo "Experiment setup: Completed at $(date) for expirment ${pslot}" || true
        } >> "${GFS_CI_ROOT}/PR/${pr}/output_${id}"
      else 
        {
          echo "Failed on createing experiment ${pslot}"
          echo "Experiment setup: failed at $(date) for experiment ${pslot}" || true
        } >> "${GFS_CI_ROOT}/PR/${pr}/output_${id}"
        "${GH}" pr edit "${pr}" --repo "${REPO_URL}" --remove-label "${MACHINE_ID^}-Running" --add-label "${MACHINE_ID^}-Failed"
      fi
    done
    "${GH}" pr comment "${pr}" --repo "${REPO_URL}" --body-file "${GFS_CI_ROOT}/PR/${pr}/output_${id}"
    "${GH}" pr edit --repo "${REPO_URL}" "${pr}" --remove-label "${MACHINE_ID^}-Running" --add-label "${MACHINE_ID^}-Passed"
  else 
    {
      echo "Failed on cloning and building global-workflowi PR: ${pr}"
      echo "CI on ${MACHINE_ID^} failed to build on $(date) for repo ${REPO_URL}}" || true
    } >> "${GFS_CI_ROOT}/PR/${pr}/output_${id}"
    "${GH}" pr edit "${pr}" --repo "${REPO_URL}" --remove-label "${MACHINE_ID^}-Running" --add-label "${MACHINE_ID^}-Failed"
  fi

done # looping over each open and labeled PR

##########################################
# scrub working directory for older files
##########################################
#
#find "${GFS_CI_ROOT}/PR/*" -maxdepth 1 -mtime +3 -exec rm -rf {} \;
