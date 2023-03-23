#! /usr/bin/env bash
set -eux

OPTIND=1
while getopts ":dov" option; do
  case "${option}" in
    d) export BUILD_TYPE="DEBUG";;
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

# TODO: GDASApp does not presently handle BUILD_TYPE

BUILD_TYPE=${BUILD_TYPE:-"Release"} \
BUILD_VERBOSE=${BUILD_VERBOSE:-"NO"} \
BUILD_JOBS="${BUILD_JOBS:-8}" \
WORKFLOW_BUILD="ON" \
./gdas.cd/build.sh

exit
