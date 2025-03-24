#!/usr/bin/env bash

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
    echo "Launching GitLab Runner on ${MACHINE_ID}";;
  noaacloud )
    echo "Launching GitLab Runner on ${PW_CSP}";;
  *)
    echo "Unsupported platform. Exiting with error."
    exit 1;;
esac

# Source the platform-specific configuration file
# This file contains platform-specific variables such as GITLAB_URL, GITLAB_BUILDS_DIR, 
# and GITLAB_RUNNER_DIR which are required for runner registration and execution
# See config.gaeac6 for details on these variables
source "${HOMEGFS_}/ci/platforms/config.${MACHINE_ID}"

# Change to the GitLab runner directory defined in the platform config
cd "${GITLAB_RUNNER_DIR}"

# Set the log file name with the current date and time
GITLAB_LOG="launched_gitlab_runner-$(date +%Y%m%d%M).log"
# Set the GitLab runner name - this name will appear in the GitLab UI
GITLAB_RUNNER_NAME="RDHPCS Gaea C6"
# Remove any existing log file
rm -f "${LOG}"
# Log the registration details
echo "Registering GitLab Runner ${MACHINE_ID} on host ${host} at $(date)" >> "${GITLAB_LOG}" || true
echo "with runner name: ${GITLAB_RUNNER_NAME}" >> "${GITLAB_LOG}"

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
  # Register the GitLab runner with the following parameters:
  # -n: Run in non-interactive mode
  # -t: Registration token from GitLab
  # --url: URL of the GitLab server (from config.gaeac6)
  # --executor: Type of executor (shell in this case)
  # --shell: Shell to use for job execution
  # --builds-dir: Directory where builds will be stored (from config.gaeac6)
  # --custom_build_dir-enabled: Enable custom build directories
  # --request-concurrency: Number of concurrent requests that can be handled
  ./gitlab-runner register -n -t "${GITLAB_RUNNER_TOKEN}" --url "${GITLAB_URL}" --executor shell --shell bash --builds-dir "${GITLAB_BUILDS_DIR}" --custom_build_dir-enabled true --request-concurrency 24
  
  # Set the concurrent job limit in the GitLab runner config file
  sed -i 's/concurrent.*/concurrent = 24/' ~/.gitlab-runner/config.toml
fi

#########################################################################
# RUN argument handling
# Starts a GitLab runner in the background
#########################################################################

if [[ "${1}" == "run" ]]; then
  # Construct the command to run the GitLab runner
  # nohup: Run the command immune to hangups
  # --working-directory: Directory where the runner will store its working files (from config.gaeac6)
  COMMAND="nohup ./gitlab-runner run --working-directory ${GITLAB_BUILDS_DIR}"
  # --user ${USER}"  # This line is commented out in the original script
  
  # Print the command and log file location
  echo -e "Running gitlab-runner with the command:\n${COMMAND}\nsee log ${PWD}/${GITLAB_LOG}"
  
  # Run the command in the background and redirect output to the log file
  nohup $COMMAND >> "${GITLAB_LOG}" 2>&1 &
  
  # Display the current contents of the log file
  cat "${GITLAB_LOG}"
fi

#########################################################################
# UNREGISTER argument handling
# Removes a GitLab runner from the GitLab server
#########################################################################

if [[ "${1}" == "unregister" ]]; then
  # Unregister the GitLab runner by name
  ./gitlab-runner unregister --name "${GITLAB_RUNNER_NAME}"
fi
