#! /usr/bin/env bash
set -eux

function usage() {
  cat << EOF
Builds the GFS utility programs.

Usage: ${BASH_SOURCE[0]} [-d][-h][-v]
  -d:
    Build with debug options
  -h:
    Print this help message and exit
  -v:
    Turn on verbose output
EOF
  exit 1
}

cwd=$(pwd)

OPTIND=1
while getopts ":dvh" option; do
  case "${option}" in
    d) export BUILD_TYPE="DEBUG";;
    v) export BUILD_VERBOSE="YES";;
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
"${cwd}/gfs_utils.fd/ush/build.sh"

exit
