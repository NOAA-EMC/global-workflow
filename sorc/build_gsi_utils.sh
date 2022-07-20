#! /usr/bin/env bash
set -eux

cwd=$(pwd)

OPTIND=1
while getopts ":dov" option; do
  case "${option}" in
    d) export BUILD_TYPE="DEBUG";;
    o) _ops="YES";;  # TODO - unused; remove?
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

UTIL_OPTS="-DBUILD_UTIL_ENKF_GFS=ON -DBUILD_UTIL_NCIO=ON"
GSIENKF_INSTALL_PREFIX="${cwd}/gsi_enkf.fd/install"
if [[ -d "${GSIENKF_INSTALL_PREFIX}" ]]; then
  CMAKE_OPTS="-Dgsi_ROOT=${GSIENKF_INSTALL_PREFIX} -Denkf_ROOT=${GSIENKF_INSTALL_PREFIX}"
  UTIL_OPTS+=" -DBUILD_UTIL_EFSOI=ON"
fi

BUILD_TYPE=${BUILD_TYPE:-"Release"} \
BUILD_VERBOSE=${BUILD_VERBOSE:-"NO"} \
CMAKE_OPTS="${CMAKE_OPTS:-}" \
UTIL_OPTS="${UTIL_OPTS:-}" \
${cwd}/gsi_utils.fd/ush/build.sh

exit
