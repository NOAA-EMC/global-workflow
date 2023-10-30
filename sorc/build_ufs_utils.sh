#! /usr/bin/env bash
set -eux

OPTIND=1
while getopts ":j:dv" option; do
  case "${option}" in
    j) export BUILD_JOBS="${OPTARG}";;
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

script_dir=$(dirname "${BASH_SOURCE[0]}")
cd "${script_dir}/ufs_utils.fd" || exit 1

CMAKE_OPTS="-DGFS=ON" \
BUILD_JOBS=${BUILD_JOBS:-8} \
BUILD_VERBOSE=${BUILD_VERBOSE:-} \
./build_all.sh

exit

