#!/usr/bin/env bash
set -eux

#########################################################################
#
# Script description: BASH script for creating an experiment
#                     by only taking a single YAML file
########################################################################

usage() {
  set +x
  echo
  echo "Usage: $0 -y case/yaml_conf_file.yaml"
  echo
  echo "  -c  global workflow configuration yaml defining an experiment (\$HOMEgfs/ci/cases)"
  echo "  -d  full path to directory to place COMROT with EXPDIR for createing experiments"
  echo "  -h  display this message and quit"
  echo
  exit 1
}

while getopts "c:d:h" opt; do
  case ${opt} in
    c)
      YAML_CASE=${OPTARG}
      ;;
    d)
      RUNTESTS=${OPTARG}
      ;;
    h|\?|:)
      usage
      ;;
    *)
      echo "Unrecognized option"
      usage
     ;;
  esac
done

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
   source "${HOMEgfs}/ci/platforms/${MACHINE_ID}.sh"
   ;;
 *)
   echo "Unsupported platform. Exiting with error."
   exit 1
   ;;
esac

filename=$(basename -- ${YAML_CASE})
export pslot="${filename%.*}"
export RUNTESTS=${RUNTESTS:-"${PWD}"}
# TODO env HOMEgfs being set will cause runtime error
# with HOMEgfs being blank in Rocoto XLM file
export HOMEgfs_PR="${HOMEgfs}"
unset HOMEgfs;

${HOMEgfs_PR}/ci/scripts/create_experiment.py --yaml $YAML_CASE
