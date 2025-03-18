#!/usr/bin/env bash

TOKEN=glrt-t3_ZTdxY19NwELUjMTg8Kdn

if [[ ! -f gitlab-runner ]]; then
  curl -L --output $PWD/gitlab-runner https://gitlab-runner-downloads.s3.amazonaws.com/latest/binaries/gitlab-runner-linux-amd64
  sudo chmod +x ./gitlab-runner
fi

# Check if the GitLab Runner user already exists
if id "gitlab-runner" &>/dev/null; then
  echo "User gitlab-runner already exists."
else
  sudo useradd --comment 'GitLab Runner' --create-home gitlab-runner --shell /bin/bash
fi

if [[ $1 == "install_service" ]]; then
  sudo gitlab-runner install --user=gitlab-runner --working-directory=/home/gitlab-runner
  gitlab-runner start
fi

if [[ $1 == "register" ]]; then
  gitlab-runner register  --url http://localhost:8929  --token ${TOKEN} \
    --executor shell \
    --description "gitlab-runner" \
    --name "orion" \
    --tag-list "orion" \
    --run-untagged="true" \
    --locked="false" \
    --access-level="not_protected"
fi
