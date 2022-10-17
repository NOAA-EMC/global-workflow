#! /usr/bin/env bash
set -eux

function _usage() {
  cat << EOF
Builds all of the global-workflow components by calling the individual build
  scripts in sequence.

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
while getopts ":dv" option; do
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

"${cwd}/gfs_utils.fd/sorc/build_gfs_utils.sh"

exit