#! /usr/bin/env bash
set -eux

cwd=$(pwd)

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
"${cwd}/gsi_monitor.fd/ush/build.sh"

exit
