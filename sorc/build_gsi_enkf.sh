#! /usr/bin/env bash
set -eux

OPTIND=1
while getopts ":j:dv" option; do
  case "${option}" in
    d) export BUILD_TYPE="DEBUG";;
    j) export BUILD_JOBS="${OPTARG}";;
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

BUILD_TYPE=${BUILD_TYPE:-"Release"} \
BUILD_VERBOSE=${BUILD_VERBOSE:-"NO"} \
BUILD_JOBS=${BUILD_JOBS:-8} \
GSI_MODE=GFS \
ENKF_MODE=GFS \
REGRESSION_TESTS=NO \
./gsi_enkf.fd/ush/build.sh

exit

