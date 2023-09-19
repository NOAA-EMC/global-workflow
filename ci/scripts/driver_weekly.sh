#!/bin/bash
set -eux

##############################################################################################
#
# Script description: Top level driver script for checking PR
#                     ready for CI regression testing
#
# Abstract:
#
# This script runs the high resolution cases found in $HOMEgfs/ci/cases/weekly
# from the develop branch for the global-workflow repo that is intended to run on a weekly basis
# from a cron job. When ran it will clone and build a new branch from the EMC's global-workflow and
# and create a pr using GitHub CLI by moving and replacing the yaml case files in
# ${HOMEgfs}/ci/cases/weekly to {HOMEgfs}/ci/cases/pr.  Then at point it can simply also add the
# the requisite labels so that current BASH CI framework can run these cases.
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

#########################################################
# Create a new branch from develop and move yaml files
#########################################################
branch="weekly_ci_$(date +%A_%b_%Y)"
develop_dir="${GFS_CI_ROOT}/develop_temp"
HOMEgfs_PR="${develop_dir}/global-workflow"
echo "Creating new branch ${branch} from develop on ${MACHINE_ID} in ${develop_dir}"
export HOMEgfs_PR
rm -Rf "${develop_dir}"
mkdir -p "${develop_dir}"
cd "${develop_dir}" || exit 1
git clone "${REPO_URL}"
cd "${HOMEgfs_PR}" || exit 1
git checkout -b "${branch}"

######################################################
# move yaml files from ci/cases/weekly to ci/cases/pr 
# and push new branch for PR weekly CI tests to GitHub

rm -Rf "${HOMEgfs_PR}/ci/cases/pr"
mv "${HOMEgfs_PR}/ci/cases/weekly" "${HOMEgfs_PR}/ci/cases/pr"
cd "${HOMEgfs_PR}" || exit 1
git add "${HOMEgfs_PR}/ci/cases/pr"
git commit -m "Moved weekly cases files into pr for high resultuion testing"
git push --set-upstream origin "${branch}"

####################################################################
# Create Pull Request using GitHub CLI and add labels for CI testing
####################################################################

REPO_OWNER="TerrenceMcGuinness-NOAA"
#REPO_OWNER="NOAA-EMC"
REPO_NAME="global-workflow"
BASE_BRANCH="develop"
HEAD_BRANCH="${branch}"
PULL_REQUEST_TITLE="Weekly High Resolution Forecast Tests $(date +'%A %b %Y')"
PULL_REQUEST_BODY="${PULL_REQUEST_TITLE}"
PULL_REQUEST_LABELS=("CI-Orion-Ready" "CI-Hera-Ready")

"${GH}" repo set-default "${REPO_OWNER}/${REPO_NAME}"
"${GH}" pr create --title "${PULL_REQUEST_TITLE}" --body "${PULL_REQUEST_BODY}" --base "${BASE_BRANCH}" --head "${HEAD_BRANCH}"

# Add labels to the pull request
for label in "${PULL_REQUEST_LABELS[@]}"
do
  "${GH}" pr edit --add-label "${label}"
done
