#!/bin/bash
set -eux

##############################################################################################
#
# Script description: Top level driver script for running
#                     weekly CI regression tests
#
# Abstract:
#
# This script runs the high resolution cases found in $HOMEgfs/ci/cases/weekly
# from the develop branch for the global-workflow repo that are intended to run on a weekly basis
# from a cron job. When run it will clone and build a new branch from the EMC's global-workflow and
# and create a pr using GitHub CLI by moving and replacing the yaml case files in
# ${HOMEgfs}/ci/cases/weekly to {HOMEgfs}/ci/cases/pr.  Then the requisite labels are added
# so that the current BASH CI framework can then run these cases.  Since this script
# creates a PR with the CI-Ready labels, the BASH CI framework will automatically run these cases 
# from that point so it is only required to run this script once from a single machine.
##############################################################################################

#################################################################
# TODO using static build for GitHub CLI until fixed in HPC-Stack
#################################################################
export GH=${HOME}/bin/gh
export REPO_URL="ssh://git@ssh.github.com:443/NOAA-EMC/global-workflow.git"

################################################################
# Setup the relative paths to scripts and PS4 for better logging
################################################################
HOMEgfs="$(cd "$(dirname  "${BASH_SOURCE[0]}")/../.." >/dev/null 2>&1 && pwd )"
scriptname=$(basename "${BASH_SOURCE[0]}")
echo "Begin ${scriptname} at $(date -u)" || true
export PS4='+ $(basename ${BASH_SOURCE[0]})[${LINENO}]'

#########################################################################
# Set up runtime environment variables for accounts on supported machines
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
branch="weekly_ci_$(date +%Y%m%d)"
develop_dir="${GFS_CI_ROOT}/develop_weekly"
echo "Creating new branch ${branch} from develop on ${MACHINE_ID} in ${develop_dir}"
rm -Rf "${develop_dir}"
mkdir -p "${develop_dir}"
cd "${develop_dir}" || exit 1
git clone "${REPO_URL}"
cd global-workflow || exit 1
git checkout -b "${branch}"

######################################################
# move yaml files from ci/cases/weekly to ci/cases/pr 
# and push new branch for PR weekly CI tests to GitHub
REPO_OWNER="emcbot"
REPO_NAME="global-workflow"
REMOTE_NAME="${REPO_OWNER}"

rm -Rf ci/cases/pr
mv ci/cases/weekly ci/cases/pr
git add ci/cases
git commit -m "Moved weekly cases files into pr for high resolution testing"

git remote add "${REMOTE_NAME}" "git@github.com:${REPO_OWNER}/${REPO_NAME}.git"

set +e
# Delete the branch if it exists
git ls-remote --exit-code upstream "${branch}"
ci_status=$?
if [[ "${ci_status}" == '0' ]]; then
    git push "${REMMOT_NAME}" --delete "${branch}"
fi
set -e

git push --set-upstream "${REMOTE_NAME}" "${branch}"

####################################################################
# Create Pull Request using GitHub CLI and add labels for CI testing
####################################################################

HEAD_BRANCH="${REPO_OWNER}:${branch}"
BASE_BRANCH="develop"
PULL_REQUEST_TITLE="[DO NOT MERGE] Weekly High Resolution CI Tests $(date +'%A %b %d, %Y')"
PULL_REQUEST_BODY="${PULL_REQUEST_TITLE}"
PULL_REQUEST_LABELS=("CI/CD" "CI-Orion-Ready" "CI-Hera-Ready")

"${GH}" repo set-default "NOAA-EMC/global-workflow"
"${GH}" pr create --title "${PULL_REQUEST_TITLE}" --body "${PULL_REQUEST_BODY}" --base "${BASE_BRANCH}" --head "${HEAD_BRANCH}"
"${GH}" pr ready --undo

# Add labels to the pull request
for label in "${PULL_REQUEST_LABELS[@]}"
do
  "${GH}" pr edit --add-label "${label}"
done
cd "${GFS_CI_ROOT}"
rm -Rf "${develop_dir}"
