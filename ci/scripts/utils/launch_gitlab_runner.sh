#!/usr/bin/env bash

HOMEGFS_="$(cd "$(dirname  "${BASH_SOURCE[0]}")/../../.." >/dev/null 2>&1 && pwd )"
host=$(hostname)

#########################################################################
#  Set up runtime environment varibles for accounts on supproted machines
#########################################################################

source "${HOMEGFS_}/ush/detect_machine.sh"
case ${MACHINE_ID} in
  hera | orion | hercules | wcoss2 | gaeac5 | gaeac6 )
    echo "Launching GitLab Runner on ${MACHINE_ID}";;
  noaacloud )
    echo "Launching GitLab Runner on ${PW_CSP}";;
  *)
    echo "Unsupported platform. Exiting with error."
    exit 1;;
esac
source ${HOMEGFS_}/ci/platforms/config.${MACHINE_ID}

cd ${GITLAB_RUNNER_DIR}

GITLAB_LOG=launched_gitlab_runner-$(date +%Y%m%d%M).log
GITLAB_RUNNER_NAME="RDHPCS Gaea C6"
rm -f "${LOG}"
echo "Registering Gitlab Runner ${MACHINE_ID} on host ${host} at $(date)" >> "${GITLAB_LOG}"
echo "with runner name: ${GITLAB_RUNNER_NAME}" >> "${GITLAB_LOG}"

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
  ./gitlab-runner register -n -t ${GITLAB_RUNNER_TOKEN} --url ${GITLAB_URL} --executor shell --shell bash --builds-dir ${GITLAB_BUILDS_DIR} --custom_build_dir-enabled true --request-concurrency 24
  sed -i 's/concurrent.*/concurrent = 24/' ~/.gitlab-runner/config.toml
fi
if [[ $1 == "run" ]]; then
  COMMAND="nohup ./gitlab-runner run --working-directory ${GITLAB_BUILDS_DIR}"
  # --user ${USER}"
  echo -e "Running gitlab-runner with the command:\n${COMMAND}\nsee log ${PWD}/${GITLAB_LOG}"
  nohup $COMMAND >> ${GITLAB_LOG} 2>&1 &
  cat ${GITLAB_LOG}
fi
if [[ $1 == "unregister" ]]; then
  ./gitlab-runner unregister --name ${GITLAB_RUNNER_NAME}
fi
