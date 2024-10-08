#! /usr/bin/env bash
set -eux

function usage() {
  cat << EOF
Builds the GFS utility programs.

Usage: ${BASH_SOURCE[0]} [-d][-h][-j n][-v]
  -d:
    Build with debug options
  -h:
    Print this help message and exit
  -j:
    Build with n build jobs
  -v:
    Turn on verbose output
EOF
  exit 1
}

OPTIND=1
while getopts ":j:dvh" option; do
  case "${option}" in
    d) BUILD_TYPE="Debug";;
    v) BUILD_VERBOSE="YES";;
    j) BUILD_JOBS="${OPTARG}";;
    h)
      usage
      ;;
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
"./gfs_utils.fd/ush/build.sh"

exit
