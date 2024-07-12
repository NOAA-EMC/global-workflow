#!/bin/env bash
set -e

controller_url="https://jenkins.epic.oarcloud.noaa.gov"
controller_user="terry.mcguinness"
controller_user_auth_token="jenkins_token"

HOMEgfs="$(cd "$(dirname  "${BASH_SOURCE[0]}")/../../.." >/dev/null 2>&1 && pwd )"
host=$(hostname)

#########################################################################
#  Set up runtime environment varibles for accounts on supproted machines
#########################################################################

source "${HOMEgfs}/ush/detect_machine.sh"
case ${MACHINE_ID} in
  hera | orion | hercules | wcoss2)
    echo "Launch Jenkins Java Controler on ${MACHINE_ID}"
    ;;
  *)
    echo "Unsupported platform. Exiting with error."
    exit 1
    ;;
esac

LOG=lanuched_agent-$(date +%Y%m%d%M).log
rm -f "${LOG}"

source "${HOMEgfs}/ush/module-setup.sh"
module use "${HOMEgfs}/modulefiles"
module load "module_gwsetup.${MACHINE_ID}"
source "${HOMEgfs}/ci/platforms/config.${MACHINE_ID}"

JAVA_HOME="${JENKINS_AGENT_LANUCH_DIR}/JAVA/jdk-17.0.10"
if [[ ! -d "${JAVA_HOME}" ]]; then
  JAVA_HOME=/usr/lib/jvm/jre-17
  if [[ ! -d "${JAVA_HOME}" ]]; then
    echo "ERROR: JAVA_HOME not found. Exiting with error."
    exit 1
  fi
fi

JAVA="${JAVA_HOME}/bin/java"
echo "JAVA VERSION: "
${JAVA} -version

export GH="${HOME}/bin/gh"
command -v "${GH}"
${GH} --version

check_mark=$(gh auth status -t 2>&1 | grep "Token:" | awk '{print $1}')
if [[ "${check_mark}" != "✓" ]]; then
  echo "gh not authenticating with emcbot token"
  exit 1
fi
echo "gh authenticating with emcbot TOKEN ok"


if [[ -d "${JENKINS_AGENT_LANUCH_DIR}" ]]; then
  echo "Jenkins Agent Lanuch Directory: ${JENKINS_AGENT_LANUCH_DIR}"
else
  echo "ERROR: Jenkins Agent Lanuch Directory not found. Exiting with error."
  exit 1
fi
cd "${JENKINS_AGENT_LANUCH_DIR}"

if ! [[ -f agent.jar ]]; then
  curl -sO "${controller_url}/jnlpJars/agent.jar"
  echo "Updated agent.jar downloaded"
fi

if [[ ! -f "${controller_user_auth_token}" ]]; then
   echo "User Jenkins authetication TOKEN to the controller for using the Remote API does not exist"
   exit 1
fi

JENKINS_TOKEN=$(cat "${controller_user_auth_token}")
echo -e "#!/usr/bin/env python
import json,sys
with open(sys.argv[1], 'r') as file:
    data = json.load(file)
print(data[\"offline\"])
" > parse.py
chmod u+x parse.py

parse_json_response() {
    curl_response=$(curl --silent -u "${controller_user}:${JENKINS_TOKEN}" "${controller_url}/computer/${MACHINE_ID^}-EMC/api/json?pretty=true") || true
    echo -n "${curl_response}" > curl_response
    $(./parse.py curl_response)
}

offline=parse_json_response
echo "Jenkins Agent is offline: ${offline}"
exit

if [[ "${offline}" != "False" ]]; then
  echo "Jenkins Agent is offline. Waiting 5 more minutes to try again"
  echo "Jenkins Agent is offline. Lanuching Jenkins Agent on ${host}"
  command="nohup ${JAVA} -jar agent.jar -jnlpUrl ${controller_url}/computer/${MACHINE_ID^}-EMC/jenkins-agent.jnlp  -secret @jenkins-secret-file -workDir ${JENKINS_WORK_DIR}"
  echo -e "Lanuching Jenkins Agent on ${host} with the command:\n${command}" >& "${LOG}"
  ${command} >> "${LOG}" 2>&1 &
  nohup_PID=$!
  echo "Java agent running on PID: ${nohup_PID}" >> "${LOG}" 2>&1
  echo "Java agent running on PID: ${nohup_PID}"
else
  echo "Jenkins Agent is online (nothing done)"
fi
