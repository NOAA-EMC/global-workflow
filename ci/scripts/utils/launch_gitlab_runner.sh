#!/usr/bin/env bash

HOMEGFS_="$(cd "$(dirname  "${BASH_SOURCE[0]}")/../../.." >/dev/null 2>&1 && pwd )"
host=$(hostname)

#########################################################################
#  Set up runtime environment varibles for accounts on supproted machines
#########################################################################

source "${HOMEGFS_}/ush/detect_machine.sh"
case ${MACHINE_ID} in
  hera | orion | hercules | wcoss2 | gaeac5 | gaeac6 )
    echo "Launch GitLab Runner on ${MACHINE_ID}";;
  noaacloud )
    echo "Launch GitLab Runner on ${PW_CSP}";;
  *)
    echo "Unsupported platform. Exiting with error."
    exit 1;;
esac
source ${HOMEGFS_}/ci/platforms/config.${MACHINE_ID}

cd ${GITLAB_RUNNER_DIR}

GITLAB_LOG=launched_gitlab_runner-$(date +%Y%m%d%M).log
rm -f "${LOG}"
echo "Registering Gitlab Runner ${MACHINE_ID} on host ${host} at $(date)" >> "${GITLAB_LOG}"

# Get token from 2nd arg, env, or file
GITLAB_RUNNER_TOKEN=${2:-${GITLAB_RUNNER_TOKEN}}
if [[ -z ${GITLAB_RUNNER_TOKEN} ]]; then
  if [[ -f gitlab_token ]]; then	
   source gitlab_token
  fi 
fi
if [[ -z ${GITLAB_RUNNER_TOKEN} ]]; then
  echo "ERROR: GITLAB_RUNNER_TOKEN not set"
  exit 1
fi  

if [[ ! -f gitlab-runner ]]; then
  curl -L --output $PWD/gitlab-runner https://gitlab-runner-downloads.s3.amazonaws.com/latest/binaries/gitlab-runner-linux-amd64
  chmod +x ./gitlab-runner
fi

if [[ $1 == "register" ]]; then
  ./gitlab-runner register -n -t ${GITLAB_RUNNER_TOKEN} --url ${GITLAB_URL} --executor shell --shell bash --builds-dir ${GITLAB_BUILDS_DIR} --custom_build_dir-enabled true
fi
if [[ $1 == "run" ]]; then
  nohup ./gitlab-runner run --working-directory ${GITLAB_BUILDS_DIR} --user ${USER} --group ${USER} --log-level debug >> ${GITLAB_LOG} 2>&1 &
fi
if [[ $1 == "unregister" ]]; then
  ./gitlab-runner unregister --name ${GITLAB_RUNNER_NAME}
fi
