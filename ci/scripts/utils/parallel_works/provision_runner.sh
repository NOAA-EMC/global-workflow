#!/usr/bin/env bash

# This script provisions a GitHub Actions runner on a Rocky or CentOS system.
# It performs the following steps:
# 1. Checks the operating system from /etc/os-release.
# 2. Verifies if the operating system is either Rocky or CentOS.
# 3. Checks if an actions-runner process is already running for the current user.
# 4. Copies the actions-runner tar file from a specified directory to the home directory.
# 5. Extracts the tar file and starts the actions-runner in the background.
#
# The actions-runner tar file contains the necessary binaries and scripts to run
# a GitHub Actions runner. It is specific to the operating system and is expected
# to be located in the /contrib/${CI_USER}/SETUP/ directory.

CI_USER="Terry.McGuinness"

# Get the Operating System name from /etc/os-release
OS_NAME=$(grep -E '^ID=' /etc/os-release | sed -E 's/ID="?([^"]*)"?/\1/') || true

# Check if the OS is Rocky or CentOS
if [[ "${OS_NAME}" == "rocky" || "${OS_NAME}" == "centos" ]]; then
  echo "Operating System is ${OS_NAME}"
else
  echo "Unsupported Operating System: ${OS_NAME}"
  exit 1
fi

running=$(pgrep -u "${USER}" run-helper -c) || true
if [[ "${running}" -gt 0 ]]; then
   echo "actions-runner is already running"
   exit
fi

cp "/contrib/${CI_USER}/SETUP/actions-runner_${OS_NAME}.tar.gz" "${HOME}"
cd "${HOME}" || exit
tar -xf "actions-runner_${OS_NAME}.tar.gz"
cd actions-runner || exit
d=$(date +%Y-%m-%d-%H:%M)
nohup ./run.sh >& "run_nohup${d}.log" &
