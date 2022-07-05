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

#TODO - remove UTIL_OPTS line after GSI removes all utilities from its repo.
BUILD_TYPE=${BUILD_TYPE:-"Release"} \
BUILD_VERBOSE=${BUILD_VERBOSE:-"YES"} \
GSI_MODE=GFS \
ENKF_MODE=GFS \
REGRESSION_TESTS=NO \
UTILS_OPTS="-DBUILD_UTIL_ALL=OFF" \
./gsi.fd/ush/build.sh

exit

