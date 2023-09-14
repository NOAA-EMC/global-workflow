#!/bin/bash
set -eux

##############################################################################################
#
# Script description: Top level driver script for checking PR
#                     ready for CI regression testing
#
# Abstract:
#
# This script runs the CI Expirments foreach case files found in $HOMEgfs/ci/cases/weekly
# from the develop branch for the global-workflow repo that is intended to run on a weekly basis
# from a cron job. When ran it will clone and build from the EMC's global-workflow and
# create each expiment directory in ${GFS_CI_ROOT}/develop/RUNTESTS
# for each yaml case file in ${HOMEgfs}/ci/cases/weekly.  As each EXPDIR is created it will
# run run-check_ci.sh agaist it to advance the experiment using Rocoto.  After each
# weekly case file is finished it will email from the EMAIL_LIST the results.
#######################################################################################i######

#################################################################
# TODO using static build for GitHub CLI until fixed in HPC-Stack
#################################################################
export GH=${HOME}/bin/gh
export REPO_URL=${REPO_URL:-"https://github.com/NOAA-EMC/global-workflow.git"}

#EMAIL_LIST="walter.kolczynski@noaa.gov, rahul.mahajan@noaa.gov, terry.mcguinness@noaa.gov"
EMAIL_LIST="terry.mcguinness@noaa.gov"

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
# Clone, checkout, build, develop branch and creat set of
# cases from $HOMEgfs/ci/cases/weekly 
#############################################################

  echo "Building development branch on ${MACHINE_ID}"
  develop_dir="${GFS_CI_ROOT}/develop"
  RUNTESTS="${develop_dir}/RUNTESTS"
  export RUNTESTS
  mkdir -p "${RUNTESTS}"
  date_stamp=$(date | awk '{print $1"-"$2"-"$3"-"$6}')
  export CI_LOG="${develop_dir}/ci_${date_stamp}.log"
  rm -f "${CI_LOG}"
  # call clone-build_ci to clone and build the develop branch
  set +e
   rm -Rf "${develop_dir:?}/HOMEgfs"
  "${HOMEgfs}/ci/scripts/clone-build_ci.sh" -p develop -d "${develop_dir}" -o "${CI_LOG}"
  #echo "SKIPPING: ${HOMEgfs}/ci/scripts/clone-build_ci.sh -p develop -d ${develop_dir} -o ${CI_LOG}"
  ci_status=$?
  set -e
  if [[ ${ci_status} -eq 0 ]]; then
  
    #############################################################
    # loop over every yaml file in ci/cases/weekly dir
    # and create an EXPDIR directory for each one
    #############################################################
    HOMEgfs_PR="${develop_dir}/HOMEgfs"
    export HOMEgfs_PR
    cd "${HOMEgfs_PR}"
    pr_sha=$(git rev-parse --short HEAD)

    for yaml_config in "${HOMEgfs_PR}/ci/cases/weekly/"*.yaml; do
      case=$(basename "${yaml_config}" .yaml) || true
      pslot="${case}_${pr_sha}"
      export pslot
      set +e
      "${HOMEgfs_PR}/ci/scripts/create_experiment.py" --yaml "${yaml_config}" --dir "${HOMEgfs_PR}"
      ci_status=$?
      set -e
      if [[ ${ci_status} -eq 0 ]]; then
        {
          echo "Created experiment:            *SUCCESS*"
          echo "Case setup: Completed at $(date) for experiment ${pslot}" || true
        } >> "${CI_LOG}"
        rm -f "${develop_dir}/RUNTESTS/ci.log"
        "${HOMEgfs}/ci/scripts/run-check_ci.sh" "${develop_dir}" "${pslot}" 2>> "${develop_dir}/output_${case}.stderr" > "${develop_dir}/output_${case}.stdout"
        ci_status=$?
        if [[ ${ci_status} -eq 0 ]]; then
          {
            echo -e "\n**** CASE ${case} SUCCEDED at $(date) ****" || true
            cat "${develop_dir}/RUNTESTS/ci.log"
          } >> "${CI_LOG}"
        else
         {
          echo -e "\n**** CASE ${case} FAILED ****"
          cat "${develop_dir}/output_${case}.stderr"
          cat "${develop_dir}/RUNTESTS/ci.log"
         } >> "${CI_LOG}"
        fi
      else 
        {
          echo "Failed to create experiment:  *FAIL* ${pslot}"
          echo "Experiment setup: failed at $(date) for experiment ${pslot}" || true
          echo ""
          cat "${HOMEgfs_PR}/ci/scripts/"setup_*.std*
        } >> "${CI_LOG}"
      fi
    done

   else 
   {
      echo "Failed on cloning and building global-workflow"
      echo "CI on ${MACHINE_ID^} failed to build on $(date) for repo ${REPO_URL}" || true
   } >> "${CI_LOG}"
  fi
  mail -s "Weekly CI Tests Results for ${date_stamp}" "${EMAIL_LIST}" < "${CI_LOG}"

