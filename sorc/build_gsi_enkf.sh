#! /usr/bin/env bash
set -eux

OPTIND=1
while getopts ":dov" option; do
  case "${option}" in
    d) export BUILD_TYPE="DEBUG";;
    o) _ops="YES";;
    v) export BUILD_VERBOSE="YES";;
    :)
      echo "[${BASH_SOURCE[0]}]: ${option} requires an argument"
      usage
      ;;
    *)
      echo "[${BASH_SOURCE[0]}]: Unrecognized option: ${option}"
      usage
      ;;
  esac
done
shift $((OPTIND-1))

# Temporary crtm version setting
export crtm_ver="2.4.0"

BUILD_TYPE=${BUILD_TYPE:-"Release"} \
BUILD_VERBOSE=${BUILD_VERBOSE:-"NO"} \
GSI_MODE=GFS \
ENKF_MODE=GFS \
REGRESSION_TESTS=NO \
./gsi_enkf.fd/ush/build.sh

exit

