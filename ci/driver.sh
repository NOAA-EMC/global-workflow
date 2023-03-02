#!/bin/bash --login
#
#####################################################################################
#
# Script description: Top level driver script for checking PR
#                     ready for CI regresssion testing
#
# Abstract:
#
#  This script uses GitHub CL to check for Pull Requests with {machine}-CI tags
#  on the development branch for the global-workflow repo and stages tests directors
#  per PR number and then calling run_ci.sh to clone and  building from
#  $(HOMEgfs)/sorc and then run a suite of regression tests with various configurations.
#
#######################################################################################

#################################################################
# TODO using static build for GitHub CLI until fixed in HPC-Stack
#################################################################
GH=/home/Terry.McGuinness/bin/gh

################################################################
# Setup the reletive paths to scripts and PS4 for better logging 
################################################################
set -eux
pwd="$(cd "$(dirname  "${BASH_SOURCE[0]}")" >/dev/null 2>&1 && pwd )"
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
# using Terrence.McGuinness-NOAA fork for development with GitHub interations
repo_url="https://github.com/TerrenceMcGuinness-NOAA/global-workflow.git"
export repo_url

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
list=$(${GH} pr list --label "${CI_LABEL}-CI" --state "open")
list=echo "${list}" | awk '{print $1;}' > "${GFS_CI_ROOT}/open_pr_list"

if [[ -s "${GFS_CI_ROOT}/open_pr_list" ]]; then
 open_pr_list=$(cat "${GFS_CI_ROOT}/open_pr_list")
else
 echo "no PRs to process .. exit"
 exit
fi 


#######################################
# clone, checkout, build, test, each PR
# loop throu all open PRs
#######################################
for pr in ${open_pr_list}; do
  "${GH}" pr edit --repo "${repo_url}" "${pr}" --remove-label "${CI_LABEL}-CI" --add-label "${CI_LABEL}-Running"
  echo "Processing Pull Request #${pr}"
  pr_dir="${GFS_CI_ROOT}/PR/${pr}"
  mkdir -p "${pr_dir}"
  # call run_ci to clone and build PR
  "${HOMEgfs}/ci/run_ci.sh" -p "${pr}" -d "${pr_dir}" -o "${pr_dir}/output_${commit}"
  ci_status=$?
  "${GH}" pr comment "${pr}" --repo "${repo_url}" --body-file "${GFS_CI_ROOT}/PR/${pr}/output_${commit}"
  if [[ ${ci_status} -eq 0 ]]; then
    "${GH}" pr edit --repo "${repo_url}" "${pr}" --remove-label "${CI_LABEL}-Running" --add-label "${CI_LABEL}-Passed"
  else
    "${GH}" pr edit "${pr}" --repo "${repo_url}" --remove-label "${CI_LABEL}-Running" --add-label "${CI_LABEL}-Failed"
  fi

  ####################################
  # TODO setup_test.py -t testname.py
  ####################################
done

##########################################
# scrub working directory for older files
##########################################
find "${GFS_CI_ROOT}/PR/*" -maxdepth 1 -mtime +3 -exec rm -rf {} \;

exit 0
