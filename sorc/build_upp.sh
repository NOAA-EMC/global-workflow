#! /usr/bin/env bash
set -eux

# shellcheck disable=SC2155
readonly HOMEglobal_=$(cd "$(dirname "$(readlink -f -n "${BASH_SOURCE[0]}")")" && git rev-parse --show-toplevel)

script_dir=$(dirname "${BASH_SOURCE[0]}")
cd "${script_dir}" || exit 1

OPTIND=1
_opts=""
while getopts ":dj:v" option; do
    case "${option}" in
        d) _opts+="-d " ;;
        j) BUILD_JOBS="${OPTARG}" ;;
        v) _opts+="-v " ;;
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

cd "${HOMEglobal_}/sorc/ufs_model.fd/UFSATM/upp/tests"
# shellcheck disable=SC2086
BUILD_JOBS=${BUILD_JOBS:-8} bash -x "${HOMEglobal_}/sorc/ufs_model.fd/UFSATM/upp/tests/compile_upp.sh" ${_opts}

exit 0
