#!/bin/bash
set -eux

##############################################################################################
#
# Script description: Top level driver script for checking PR
#                     ready for CI regression testing
#
# Abstract:
#
# This script uses GitHub CLI to check for Pull Requests with CI-Ready-${machine} tags on the
# development branch for the global-workflow repo.  It then stages tests directories per
# PR number and calls clone-build_ci.sh to perform a clone and full build from $(HOMEgfs)/sorc
# of the PR. It then creates an expiment directory in ${GFS_CI_ROOT}/PR/$[pr}/RUNTESTS
# for each yaml case file in ${HOMEgfs}/ci/cases/pr.  At his point the run-ci.sh script
# will advance the experiment using Rocoto while check_ci.sh script will tests for complition
#######################################################################################i######

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

 
#############################################################
# Loop throu all open PRs
# Clone, checkout, build, creat set of cases, for each
#############################################################

  echo "Building development branch on ${MACHINE_ID}"
  develop_dir="${GFS_CI_ROOT}/develop"
  rm -Rf "${develop_dir:?}/global-workflow"
  mkdir -p "${develop_dir}"
  id="develop"
  # call clone-build_ci to clone and build PR
  set +e
  "${HOMEgfs}/ci/scripts/clone-build_ci.sh" -p develop -d "${develop_dir}" -o "${develop_dir}/output_${id}.log"
  ci_status=$?
  ##################################################################
  # Checking for special case when Ready label was updated
  # that cause a running driver exit fail because was currently
  # building so we force and exit 0 instead to does not get relabled
  #################################################################
  set -e
  if [[ ${ci_status} -eq 0 ]]; then
  
    #############################################################
    # loop over every yaml file in the PR's ci/cases/weekly dir
    # and create an run directory for each one for this PR loop
    #############################################################
    HOMEgfs_PR="${develop_dir}/global-workflow"
    export HOMEgfs_PR
    cd "${HOMEgfs_PR}"
    pr_sha=$(git rev-parse --short HEAD)

    for yaml_config in "${HOMEgfs_PR}/ci/cases/weekly/"*.yaml; do
      case=$(basename "${yaml_config}" .yaml) || true
      pslot="${case}_${pr_sha}"
      export pslot
      set +e
      "${HOMEgfs_PR}/ci/scripts/create_experiment.py" --yaml "${HOMEgfs_PR}/ci/cases/pr/${case}.yaml" --dir "${HOMEgfs_PR}"
      ci_status=$?
      set -e
      if [[ ${ci_status} -eq 0 ]]; then
        {
          echo "Created experiment:            *SUCCESS*"
          echo "Case setup: Completed at $(date) for experiment ${pslot}" || true
        } >> "${GFS_CI_ROOT}/PR/${pr}/output_${id}"
        # TODO Message "${GH}" pr edit --repo "${REPO_URL}" "${pr}" --remove-label "CI-${MACHINE_ID^}-Building" --add-label "CI-${MACHINE_ID^}-Running"
      else 
        {
          echo "Failed to create experiment:  *FAIL* ${pslot}"
          echo "Experiment setup: failed at $(date) for experiment ${pslot}" || true
          echo ""
          cat "${HOMEgfs_PR}/ci/scripts/"setup_*.std*
        } >> "${GFS_CI_ROOT}/PR/${pr}/output_${id}"
        # TODO Message "${GH}" pr edit "${pr}" --repo "${REPO_URL}" --remove-label "CI-${MACHINE_ID^}-Building" --add-label "CI-${MACHINE_ID^}-Failed"
      fi
    done

  else 
    {
      echo '```'
      echo "Failed on cloning and building global-workflowi PR: ${pr}"
      echo "CI on ${MACHINE_ID^} failed to build on $(date) for repo ${REPO_URL}" || true
    } >> "${GFS_CI_ROOT}/PR/${pr}/output_${id}"
    # TODO Message "${GH}" pr edit "${pr}" --repo "${REPO_URL}" --remove-label "CI-${MACHINE_ID^}-Building" --add-label "CI-${MACHINE_ID^}-Failed"
  fi
  sed -i "s/\`\`\`//2g" "${GFS_CI_ROOT}/PR/${pr}/output_${id}"
  # TODO Message "${GH}" pr comment "${pr}" --repo "${REPO_URL}" --body-file "${GFS_CI_ROOT}/PR/${pr}/output_${id}"

done # looping over each open and labeled PR

##########################################
# scrub working directory for older files
##########################################
#
#find "${GFS_CI_ROOT}/PR/*" -maxdepth 1 -mtime +3 -exec rm -rf {} \;
