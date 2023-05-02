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
# PR number and calls clone-build_ci.sh to perform a clone and full build from $(HOMEgfs)/sorc
# of the PR. It then is ready to run a suite of regression tests with various
# configurations with run_tests.py.
#######################################################################################

#################################################################
# TODO using static build for GitHub CLI until fixed in HPC-Stack
#################################################################
export GH=${HOME}/bin/gh
export REPO_URL=${REPO_URL:-"https://github.com/NOAA-EMC/global-workflow.git"}

################################################################
# Setup the reletive paths to scripts and PS4 for better logging 
################################################################
HOMEgfs="$(cd "$(dirname  "${BASH_SOURCE[0]}")/../.." >/dev/null 2>&1 && pwd )"
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

######################################################
# setup runtime env for correct python install and git
######################################################
set +x
source "${HOMEgfs}/ush/module-setup.sh"
module use "${HOMEgfs}/modulefiles"
module load "module_gwsetup.${MACHINE_ID}"
set -x

############################################################
# query repo and get list of open PRs with tags {machine}-CI
############################################################
pr_list_dbfile="${GFS_CI_ROOT}/open_pr_list.db"
if [[ ! -f "${pr_list_dbfile}" ]]; then
  "${HOMEgfs}/ci/scripts/pr_list_database.py" --create "${pr_list_dbfile}"
fi

pr_list=$(${GH} pr list --repo "${REPO_URL}" --label "CI-${MACHINE_ID^}-Ready" --state "open" | awk '{print $1}')

for pr in ${pr_list}; do
  "${HOMEgfs}/ci/scripts/pr_list_database.py" --add_pr "${pr}" "${pr_list_dbfile}"
done

pr_list=""
if [[ -f "${pr_list_dbfile}" ]]; then
  pr_list=$(${HOMEgfs}/ci/scripts/pr_list_database.py --display "${pr_list_dbfile}" | grep -v Failed | grep Open | grep Ready | awk '{print $1}')
fi
if [[ -z "${pr_list}" ]]; then
  echo "no PRs open and ready for checkout/build .. exiting"
  exit 0
fi

 
#############################################################
# Loop throu all open PRs
# Clone, checkout, build, creat set of cases, for each
#############################################################

for pr in ${pr_list}; do

  "${GH}" pr edit --repo "${REPO_URL}" "${pr}" --remove-label "CI-${MACHINE_ID^}-Ready" --add-label "CI-${MACHINE_ID^}-Building"
  echo "Processing Pull Request #${pr}"
  pr_dir="${GFS_CI_ROOT}/PR/${pr}"
  mkdir -p "${pr_dir}"
  # call clone-build_ci to clone and build PR
  id=$("${GH}" pr view "${pr}" --repo "${REPO_URL}" --json id --jq '.id')
  set +e
  "${HOMEgfs}/ci/scripts/clone-build_ci.sh" -p "${pr}" -d "${pr_dir}" -o "${pr_dir}/output_${id}"
  ci_status=$?
  set -e
  if [[ ${ci_status} -eq 0 ]]; then
    "${HOMEgfs}/ci/scripts/pr_list_database.py" --update_pr "${pr}" Open Built
    #setup space to put an experiment
    # export RUNTESTS for yaml case files to pickup
    export RUNTESTS="${pr_dir}/RUNTESTS"
    rm -Rf "${pr_dir:?}/RUNTESTS/"*

    #############################################################
    # loop over every yaml file in ${HOMEgfs}/ci/cases
    # and create an run directory for each one for this PR loop
    #############################################################
    for yaml_config in "${HOMEgfs}/ci/cases/"*.yaml; do
      pslot=$(basename "${yaml_config}" .yaml) || true
      export pslot
      set +e
      "${HOMEgfs}/ci/scripts/create_experiment.py" --yaml "${HOMEgfs}/ci/cases/${pslot}.yaml" --dir "${pr_dir}/global-workflow"
      ci_status=$?
      set -e
      if [[ ${ci_status} -eq 0 ]]; then
        {
          echo "Created experiment:            *SUCCESS*"
          echo "Case setup: Completed at $(date) for experiment ${pslot}" || true
        } >> "${GFS_CI_ROOT}/PR/${pr}/output_${id}"
        "${GH}" pr edit --repo "${REPO_URL}" "${pr}" --remove-label "CI-${MACHINE_ID^}-Building" --add-label "CI-${MACHINE_ID^}-Running"
      else 
        {
          echo "Failed to create experiment}:  *FAIL* ${pslot}"
          echo "Experiment setup: failed at $(date) for experiment ${pslot}" || true
        } >> "${GFS_CI_ROOT}/PR/${pr}/output_${id}"
        "${GH}" pr edit "${pr}" --repo "${REPO_URL}" --remove-label "CI-${MACHINE_ID^}-Building" --add-label "CI-${MACHINE_ID^}-Failed"
      fi
    done

  else 
    {
      echo "Failed on cloning and building global-workflowi PR: ${pr}"
      echo "CI on ${MACHINE_ID^} failed to build on $(date) for repo ${REPO_URL}}" || true
    } >> "${GFS_CI_ROOT}/PR/${pr}/output_${id}"
    "${GH}" pr edit "${pr}" --repo "${REPO_URL}" --remove-label "CI-${MACHINE_ID^}-Building" --add-label "CI-${MACHINE_ID^}-Failed"
  fi
  "${GH}" pr comment "${pr}" --repo "${REPO_URL}" --body-file "${GFS_CI_ROOT}/PR/${pr}/output_${id}"

done # looping over each open and labeled PR

##########################################
# scrub working directory for older files
##########################################
#
#find "${GFS_CI_ROOT}/PR/*" -maxdepth 1 -mtime +3 -exec rm -rf {} \;
