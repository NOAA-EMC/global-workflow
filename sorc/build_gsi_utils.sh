#! /usr/bin/env bash
set -eux

OPTIND=1
while getopts ":j:dv" option; do
  case "${option}" in
    d) BUILD_TYPE="Debug";;
    j) BUILD_JOBS="${OPTARG}";;
    v) BUILD_VERBOSE="YES";;
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
UTIL_OPTS="-DBUILD_UTIL_ENKF_GFS=ON -DBUILD_UTIL_NCIO=ON" \
"./gsi_utils.fd/ush/build.sh"

exit
