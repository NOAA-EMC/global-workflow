#! /usr/bin/env bash
set -eux

cwd=$(pwd)

OPTIND=1
while getopts ":dov" option; do
  case "${option}" in
    d) export BUILD_TYPE="DEBUG";;
    o) _ops="YES";;
    v) export BUILD_VERBOSE="YES";;
    \?)
      echo "[$BASH_SOURCE]: Unrecognized option: ${option}"
      usage
      ;;
    :)
      echo "[$BASH_SOURCE]: ${option} requires an argument"
      usage
      ;;
  esac
done
shift $((OPTIND-1))

GSI_INSTALL_PREFIX="${cwd}/gsi_enkf.fd/install"
if [[ -d "${GSI_INSTALL_PREFIX}" ]]; then
  CMAKE_OPTS="-Dncdiag_ROOT=${GSI_INSTALL_PREFIX}"
else
  echo << EOF
FATAL: ${GSI_INSTALL_PREFIX} does not exist
       Have you built GSI yet?
EOF
  exit 2
fi

BUILD_TYPE=${BUILD_TYPE:-"Release"} \
BUILD_VERBOSE=${BUILD_VERBOSE:-"YES"} \
CMAKE_OPTS=${CMAKE_OPTS} \
${cwd}/gsi_monitor.fd/ush/build.sh

exit
