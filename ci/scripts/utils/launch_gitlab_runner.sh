#!/usr/bin/env bash

set -e

#########################################################################
# launch_gitlab_runner.sh - Script to manage GitLab runners for CI/CD
# 
# This script handles three main operations for GitLab runners:
# 1. register - Registers a new GitLab runner with the GitLab server
# 2. run - Starts a GitLab runner in the background
# 3. unregister - Removes a GitLab runner from the GitLab server
#
# Usage: ./launch_gitlab_runner.sh [register|run|unregister] [token]
#########################################################################

# Set the HOMEGFS_ variable to the root directory of the global workflow
HOMEGFS_="$(cd "$(dirname  "${BASH_SOURCE[0]}")/../../.." >/dev/null 2>&1 && pwd )"
# Get the hostname of the current machine
host="$(hostname)"

#########################################################################
#  Set up runtime environment variables for accounts on supported machines
#########################################################################

# Source the detect_machine.sh script to determine the MACHINE_ID
source "${HOMEGFS_}/ush/detect_machine.sh"
# Check the MACHINE_ID and set up the environment accordingly
case "${MACHINE_ID}" in
  hera | orion | hercules | wcoss2 | gaeac5 | gaeac6 )
    echo "Running GitLab Runner script on ${MACHINE_ID}";;
  noaacloud )
    echo "Running GitLab Runner script on ${PW_CSP}";;
  *)
    echo "Unsupported platform. Exiting with error."
    exit 1;;
esac

# Source the platform-specific configuration file
# This file contains platform-specific variables such as GITLAB_URL, GITLAB_CI_BUILDS_DIR, 
# and GITLAB_RUNNER_DIR which are required for runner registration and execution
source "${HOMEGFS_}/ci/platforms/config.${MACHINE_ID}"

# Change to the GitLab runner directory defined in the platform config
cd "${GITLAB_RUNNER_DIR}" || exit 1

# Set the log file name with the current date and time
DATE=$(date +%Y%m%d%M) || true
GITLAB_LOG="${PWD}/launched_gitlab_runner-${DATE}.log"
rm -f "${GITLAB_LOG}"

#########################################################################
# GitLab Token Handling
# The token is used to authenticate the runner with the GitLab server
#########################################################################

# Get the GitLab runner token from:
# 1. The second command-line argument
# 2. The GITLAB_RUNNER_TOKEN environment variable
# 3. A gitlab_token file in the current directory
GITLAB_RUNNER_TOKEN="${2:-${GITLAB_RUNNER_TOKEN}}"
if [[ -z "${GITLAB_RUNNER_TOKEN}" ]]; then
  if [[ -f gitlab_token ]]; then	
   source gitlab_token
  fi 
fi
if [[ -z "${GITLAB_RUNNER_TOKEN}" ]]; then
  echo "ERROR: GITLAB_RUNNER_TOKEN not set"
  exit 1
fi  

# Download the GitLab runner binary if it does not exist
if [[ ! -f gitlab-runner ]]; then
  curl -L --output "${PWD}/gitlab-runner" https://gitlab-runner-downloads.s3.amazonaws.com/latest/binaries/gitlab-runner-linux-amd64
  chmod +x ./gitlab-runner
fi

#########################################################################
# REGISTER argument handling
# Registers a new GitLab runner with the GitLab server
#########################################################################

if [[ "${1}" == "register" ]]; then

  echo "Registering GitLab Runner ${MACHINE_ID} on host ${host} at ${DATE}" >> "${GITLAB_LOG}"
  echo "with runner name: ${GITLAB_RUNNER_NAME}" >> "${GITLAB_LOG}"
  # Register the GitLab runner with the following parameters:
  # -n: Run in non-interactive mode
  # -t: Registration token from GitLab
  # --url: URL of the GitLab server (from config.MACHINE_ID)
  # --executor: Type of executor (shell in this case)
  # --builds-dir: Directory where builds will be stored (from config.MACHINE_ID)
  # --custom_build_dir-enabled: Enable custom build directories
  # --request-concurrency: Number of concurrent requests that can be handled
  ./gitlab-runner register -n -t "${GITLAB_RUNNER_TOKEN}" --url "${GITLAB_URL}" --executor shell --shell bash --builds-dir "${GITLAB_CI_BUILDS_DIR}" --custom_build_dir-enabled true --request-concurrency 24
  
  # Set the concurrent job limit in the GitLab runner config file
  sed -i 's/concurrent.*/concurrent = 24/' ~/.gitlab-runner/config.toml
  exit 0
fi

#########################################################################
# RUN: Starts a GitLab runner in the background
#########################################################################

if [[ "${1}" == "run" ]]; then
  # --working-directory: Directory where the runner is launched and keeps its working files (from config.$MACHINE_ID)
  # do not confuse this with GitLabs CI_BUILDS_DIR which is designate by GFS_CI_BUILDS_DIR and is where the builds are stored
  COMMAND="nohup ./gitlab-runner run --working-directory ${GITLAB_RUNNER_DIR}"
  echo -e "Running gitlab-runner with the command:\n${COMMAND}\nsee log ${GITLAB_LOG}"
  echo -e "Running gitlab-runner with the command:${COMMAND}" >& "${GITLAB_LOG}"
  ${COMMAND} >> "${GITLAB_LOG}" 2>&1 &
  cat "${GITLAB_LOG}"
  exit 0
fi

#########################################################################
# UNREGISTER: Removes a GitLab runner from the GitLab server
#########################################################################

if [[ "${1}" == "unregister" ]]; then
  # Unregister the GitLab runner by name
  ./gitlab-runner unregister --name "${GITLAB_RUNNER_NAME}"
fi
