#!/usr/bin/env bash

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

URL=https://vlab.noaa.gov/gitlab-licensed
BUILDS_DIR=/gpfs/f6/drsa-precip3/scratch/role.glopara/GFS_CI_ROOT/GITLAB/CI

if [[ ! -f gitlab-runner ]]; then
  curl -L --output $PWD/gitlab-runner https://gitlab-runner-downloads.s3.amazonaws.com/latest/binaries/gitlab-runner-linux-amd64
  chmod +x ./gitlab-runner
fi

if [[ $1 == "register" ]]; then
  ./gitlab-runner register -n -t ${GITLAB_RUNNER_TOKEN} --url ${URL} --executor shell --builds-dir ${BUILDS_DIR} --shell bash --custom_build_dir-enabled true
fi
