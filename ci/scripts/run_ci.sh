#!/bin/bash
#set -eux

#####################################################################################
#
# Script description: BASH script for checking for experiments in a given PR and 
#                     simply running rocotorun on each.  This script is intended
#                     to run from within a cron job in the CI Mangers account
# Abstract TODO
#####################################################################################

HOMEGFS_DIR="$(cd "$(dirname  "${BASH_SOURCE[0]}")/../.." >/dev/null 2>&1 && pwd )"
scriptname=$(basename "${BASH_SOURCE[0]}")
echo "Begin ${scriptname} at $(date -u)" || true
export PS4='+ $(basename ${BASH_SOURCE})[${LINENO}]'

#########################################################################
#  Set up runtime environment varibles for accounts on supproted machines
#########################################################################

source "${HOMEGFS_DIR}/ush/detect_machine.sh"
case ${MACHINE_ID} in
  hera | orion)
   echo "Running Automated Testing on ${MACHINE_ID}"
   source "${HOMEGFS_DIR}/ci/environments/${MACHINE_ID}.sh"
   ;;
 *)
   echo "Unsupported platform. Exiting with error."
   exit 1
   ;;
esac

module load rocoto
rocotorun=$(which rocotorun)
if [[ -z ${var+x} ]]; then
  echo "rocotorun being used from ${rocotorun}"
else
  echo "rocotorun not found on system"
  exit 1
fi

pr_list_file="open_pr_list"

if [[ -s "${GFS_CI_ROOT}/${pr_list_file}" ]]; then
  pr_list=$(cat "${GFS_CI_ROOT}/${pr_list_file}")
else
  echo "no PRs to process .. exit"
  exit 0
fi

#############################################################
# Loop throu all PRs in PR List and look for expirments in
# the RUNTESTS dir and for each one run runcotorun on them
#############################################################

for pr in ${pr_list}; do
  echo "Processing Pull Request #${pr} and looking for experiments"
  pr_dir="${GFS_CI_ROOT}/PR/${pr}"
  for experiments in "${pr_dir}/RUNTESTS/"*; do
    pslot=$(basename "${experiments}")
    xml="${pr_dir}/RUNTESTS/${pslot}/EXPERMENT/${pslot}/${pslot}.xml"
    db="${pr_dir}/RUNTESTS/${pslot}/EXPERMENT/${pslot}/${pslot}.db"
    echo "Running: ${rocotorun} -v 6 -w ${xml} -d ${db}"
    ${rocotorun} -v 6 -w ${xml} -d ${db}
  done
done

