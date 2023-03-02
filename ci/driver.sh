#!/bin/bash --login

################################################################################
#                      .                                             .
# Script name:         ci_driver.sh
# Script description:  driver script for checking PR ready for CI regresssion testing
#
# Author:   Cory Martin / Terry McGuinness   Org: NCEP/EMC     Date: 2023-02-27
#
# Abstract: This script uses GitHub CL to check for Pull Requests with {machine}-CI tags
#           on the development branch for the global-workflow repo and stages tests
#           by cloning the PRs and then calling run_ci.sh to building from $(HOMEgfs)/sorc
#           and then run a suite of regression tests with various configurations.
#
# Script history log:
# 2022        Cory Martin        Initial Script
# 2023-02-25  Terry McGuinness   Refactored for global-workflow

#################################################################
# TODO using static build for GitHub CLI until fixed in HPC-Stack
#################################################################
GH_EXEC=/home/Terry.McGuinness/bin/gh

################################################################
# Setup the reletive paths to scripts and PS4 for better logging 
################################################################
set -ex
pwd="$( cd "$( dirname "${BASH_SOURCE[0]}")" >/dev/null 2>&1 && pwd )"
scriptname=$(basename "${BASH_SOURCE[1]}")
start_time=$(date +%s)
start_time_human=$(date -d"@${start_time}" -u)
echo "Begin ${scriptname} at ${start_time_human}"
export PS4='+ $(basename ${BASH_SOURCE})[${LINENO}]'

HOMEgfs=$(realpath "${pwd}/..")
export HOMEgfs
host=$(hostname) || true

usage() {
  set +x
  echo
  echo "Usage: $0 -t <target> -h"
  echo
  echo "  -t  target/machine script is running on    DEFAULT: ${host}"
  echo "  -h  display this message and quit"
  echo
  exit 1
}

#########################################################################
#  Set up runtime environment varibles for accounts on supproted machines
#########################################################################
TARGET=$(hostname) || true
export TARGET
while getopts "t:h" opt; do
  case ${opt} in
    t)
      TARGET=${OPTARG}
      ;;
    h|\?|:)
      usage
      ;;
    *)
      exit
  esac
done

case ${TARGET} in
  hera | orion)
    echo "Running Automated Testing on ${TARGET}"
    source "${HOMEgfs}/ci/${TARGET}.sh"
    ;;
  *)
    echo "Unsupported platform. Exiting with error."
    exit 1
    ;;
esac

#repo_url="https://github.com/NOAA-EMC/global-workflow.git"
# using Terrence.McGuinness-NOAA for development with GitHub interations
repo_url="https://github.com/TerrenceMcGuinness-NOAA/global-workflow.git"

#########################################################################
# pull on the repo and get list of open PRs with tags {machine}-CI
#########################################################################
mkdir -p "${GFS_CI_ROOT}/repo"
cd "${GFS_CI_ROOT}/repo"
if [[ ! -d "${GFS_CI_ROOT}/repo/global-workflow" ]]; then
 git clone "${repo_url}"
fi
cd "${GFS_CI_ROOT}/repo/global-workflow"
git pull
CI_LABEL="${GFS_CI_HOST}"
${GH_EXEC} pr list --label "${CI_LABEL}-CI" --state "open" | awk '{print $1;}' > "${GFS_CI_ROOT}/open_pr_list"
if [[ -s "${GFS_CI_ROOT}/open_pr_list" ]]; then
 open_pr_list=$(cat "${GFS_CI_ROOT}/open_pr_list")
else
 echo "no PRs to process .. exit"
 exit
fi 


# clone, checkout, build, test, each PR
# loop throu all open PRs
#######################################
for pr in ${open_pr_list}; do
  "${GH_EXEC}" pr edit --repo "${repo_url}" "${pr}" --remove-label "${CI_LABEL}-CI" --add-label "${CI_LABEL}-Running"
  echo "Processing Pull Request #${pr}"
  mkdir -p "${GFS_CI_ROOT}/PR/${pr}"
  cd "${GFS_CI_ROOT}/PR/${pr}"

  # clone copy of repo
  if [[ -d global-workflow ]]; then
    rm -Rf global-workflow
  fi
  git clone "${repo_url}"
  cd global-workflow

  # checkout pull request
  "${GH_EXEC}" pr checkout "${pr}" --repo "${repo_url}"

  # get commit hash
  commit=$(git log --pretty=format:'%h' -n 1)
  echo "$commit" > "${GFS_CI_ROOT}/PR/${pr}/commit"

  # run build and testing command
  "${HOMEgfs}/ci/run_ci.sh" -d "$GFS_CI_ROOT/PR/$pr/global-workflow" -o "${GFS_CI_ROOT}/PR/${pr}/output_${commit}"
  ci_status=$?
  "${GH_EXEC}" pr comment "${pr}" --repo "${repo_url}" --body-file "${GFS_CI_ROOT}/PR/${pr}/output_${commit}"
  if [[ ${ci_status} -eq 0 ]]; then
    "${GH_EXEC}" pr edit --repo "${repo_url}" "${pr}" --remove-label "${CI_LABEL}-Running" --add-label "${CI_LABEL}-Passed"
  else
    "${GH_EXEC}" pr edit "${pr}" --repo "${repo_url}" --remove-label "${CI_LABEL}-Running" --add-label "${CI_LABEL}-Failed"
  fi
done

##########################################
# scrub working directory for older files
##########################################
find "${GFS_CI_ROOT}/PR/*" -maxdepth 1 -mtime +3 -exec rm -rf {} \;

exit 0
