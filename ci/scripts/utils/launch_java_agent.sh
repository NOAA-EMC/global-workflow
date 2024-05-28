#!/bin/env bash
set -e

controller_url="https://jenkins.epic.oarcloud.noaa.gov"
controller_user="terry.mcguinness"
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

JAVA_HOME="${JENKINS_AGENT_JAVA_HOME}/JAVA/jdk-17.0.10/bin"
JAVA="${JAVA_HOME}/java"
echo "JAVA VERSION: "
${JAVA} -version

export GH="${HOME}/bin/gh"
command -v $GH
$GH --version

echo "JENKINS_AGENT_LANUCH_DIR: ${JENKINS_AGENT_LANUCH_DIR}"
cd "${JENKINS_AGENT_LANUCH_DIR}"

if ! [ -f agent.jar ]; then
  curl -sO "${controller_url}/jnlpJars/agent.jar"
fi

JENKINS_TOKEN=$(cat jenkins_token)

offline=$(curl --silent -u "${controller_user}:$JENKINS_TOKEN" "${controller_url}/computer/${MACHINE_ID^}-EMC/api/json?pretty=true" | grep '\"offline\"' | awk '{gsub(/,/,"");print $3}')

echo "Jenkins Agent offline setting: ${offline}"

exit 0

command="nohup ${JAVA} -jar agent.jar -jnlpUrl https://jenkins.epic.oarcloud.noaa.gov/computer/Hera%2DEMC/jenkins-agent.jnlp  -secret @jenkins-secret-file -workDir /scratch1/NCEPDEV/global/Terry.McGuinness"
echo -e "Lanuching Jenkins Agent on $host with the command:\n${command}" >& "${LOG}"
${command} >> "${LOG}" 2>&1 &
ps -efx | grep agent.jar >> "${LOG}" 2>&1
