#! /usr/bin/env bash
set -eux

# shellcheck disable=SC2155
readonly HOMEglobal_=$(cd "$(dirname "$(readlink -f -n "${BASH_SOURCE[0]}")")" && git rev-parse --show-toplevel)

OPTIND=1
while getopts ":j:dv" option; do
    case "${option}" in
        d) BUILD_TYPE="Debug" ;;
        j) BUILD_JOBS="${OPTARG}" ;;
        v) BUILD_VERBOSE="YES" ;;
        :)
            echo "[${BASH_SOURCE[0]}]: ${option} requires an argument"
            ;;
        *)
            echo "[${BASH_SOURCE[0]}]: Unrecognized option: ${option}"
            ;;
    esac
done
shift $((OPTIND - 1))

source "${HOMEglobal_}/ush/detect_machine.sh"

CMAKE_OPTS="-DGFS=ON" \
    BUILD_TYPE=${BUILD_TYPE:-"Release"} \
    BUILD_JOBS=${BUILD_JOBS:-8} \
    BUILD_VERBOSE=${BUILD_VERBOSE:-} \
    "${HOMEglobal_}/sorc/ufs_utils.fd/build_all.sh"

exit
