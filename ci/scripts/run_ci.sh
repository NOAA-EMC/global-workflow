#!/bin/bash
set -eux

#####################################################################################
#
# Script description: BASH script for checking for cases in a given PR and 
#                     simply running rocotorun on each.  This script is intended
#                     to run from within a cron job in the CI Managers account
# Abstract TODO
#####################################################################################

HOMEgfs="$(cd "$(dirname  "${BASH_SOURCE[0]}")/../.." >/dev/null 2>&1 && pwd )"
scriptname=$(basename "${BASH_SOURCE[0]}")
echo "Begin ${scriptname} at $(date -u)" || true
export PS4='+ $(basename ${BASH_SOURCE})[${LINENO}]'

#########################################################################
#  Set up runtime environment varibles for accounts on supproted machines
#########################################################################

source "${HOMEgfs}/ush/detect_machine.sh"
case ${MACHINE_ID} in
  hera | orion)
   echo "Running Automated Testing on ${MACHINE_ID}"
   source "${HOMEgfs}/ci/platforms/${MACHINE_ID}.sh"
   ;;
 *)
   echo "Unsupported platform. Exiting with error."
   exit 1
   ;;
esac
set +x
source "${HOMEgfs}/ush/module-setup.sh"
module use "${HOMEgfs}/modulefiles"
module load "module_gwsetup.${MACHINE_ID}"
module list
set -eux
rocotorun=$(which rocotorun)
if [[ -z ${var+x} ]]; then
  echo "rocotorun being used from ${rocotorun}"
else
  echo "rocotorun not found on system"
  exit 1
fi

pr_list_dbfile="${GFS_CI_ROOT}\open_pr_list.db"

#NOTE: last pipe with head -2 limits no more tha two PRs at a time
pr_list=""
if [[ -f "${pr_list_dbfile}" ]]; then
  pr_list=$(${HOMEgfs}/ci/scripts/pr_list_database.py --display "${pr_list_dbfile}" | grep -v Failed | grep Open | grep Built | awk '{print $1}' | head -2)
fi
if [[ -z "${pr_list}" ]]; then
  echo "no PRs open and ready for checkout/build .. exiting"
  exit 0
fi

#############################################################
# Loop throu all PRs in PR List and look for expirments in
# the RUNTESTS dir and for each one run runcotorun on them
# only upto two cases will advance at a time
#############################################################

for pr in ${pr_list}; do
  echo "Processing Pull Request #${pr} and looking for cases"
  pr_dir="${GFS_CI_ROOT}/PR/${pr}"
  # If the directory RUNTESTS is not present then
  # setupexpt.py has no been run yet for this PR
  if [[ ! -d "${pr_dir}/RUNTESTS" ]]; then
     continue
  fi
  num_cases=0
  for cases in "${pr_dir}/RUNTESTS/"*; do
    if [[ ! -d "${cases}" ]]; then
       continue
    fi
    ((num_cases=num_cases+1))
    # No more than two cases are going forward at a time for each PR
    if [[ "${num_cases}" -gt 2 ]]; then
       continue
    fi
    pslot=$(basename "${cases}")
    xml="${pr_dir}/RUNTESTS/${pslot}/EXPDIR/${pslot}/${pslot}.xml"
    db="${pr_dir}/RUNTESTS/${pslot}/EXPDIR/${pslot}/${pslot}.db"
    echo "Running: ${rocotorun} -v 6 -w ${xml} -d ${db}"
    "${rocotorun}" -v 10 -w "${xml}" -d "${db}"
  done
done
