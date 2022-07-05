#! /usr/bin/env bash
set -eux

cwd=$(pwd)

BUILD_TYPE="Release"
DIR_ROOT="${cwd}/gsi_utils.fd"
INSTALL_PREFIX="${DIR_ROOT}/install"

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

BUILD_TYPE=${BUILD_TYPE} \
BUILD_VERBOSE=${BUILD_VERBOSE:-"YES"} \
UTIL_OPTS="-DBUILD_UTIL_ENKF_GFS=ON -DBUILD_UTIL_NCIO=ON" \
${cwd}/gsi_utils.fd/ush/build.sh

exit
