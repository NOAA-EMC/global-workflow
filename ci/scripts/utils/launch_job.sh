#!/usr/bin/env bash

set -x

controller_url="https://jenkins.epic.oarcloud.noaa.gov"
api_token=112991ee61d29bdf912525a4815a7cc97e
project_name=global-workflow
job_name=EMC-Pipelines
username=terry.mcguinness
pr_number=PR-292
node=Hera-EMC
machine_name=hera

build_url="${controller_url}/job/${project_name}/job/${job_name}/job/${pr_number}/buildWithParameters?delay=0sec"

echo ""
echo $build_url
echo ""

curl -sSL "${contoller_url}/pluginManager/api/json?depth=1" -u $username:$api_token ${build_url}
#| jq '.plugins[] | {shortName, version, enabled}'

exit

curl -X POST ${build_url} --user $username:$api_token ${build_url} --data-urlencode "Node=${node}" --data-urlencode "machine=${machine_name}"
