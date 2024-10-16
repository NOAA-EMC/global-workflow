#!/usr/bin/env bash

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

running=$(pgrep -u ${USER} run-helper -c) || true
if [[ "${running}" -gt 0 ]]; then
   echo "actions-runner is already running"
   exit
fi

cp /contrib/${CI_USER}/SETUP/actions-runner_${OS_NAME}.tar.gz ${HOME}
cd $HOME || exit
tar -xf "actions-runner_${OS_NAME}.tar.gz"
cd actions-runner || exit
d=$(date +%Y-%m-%d-%H:%M)
nohup ./run.sh >& "run_noup${d}.log" &
