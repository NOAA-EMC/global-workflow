#! /usr/bin/env bash
set -eux

# shellcheck disable=SC2155
readonly HOMEgfs_=$(cd "$(dirname "$(readlink -f -n "${BASH_SOURCE[0]}")")/.." && pwd -P)

script_dir=$(dirname "${BASH_SOURCE[0]}")
cd "${script_dir}" || exit 1

OPTIND=1
_opts=""
while getopts ":dj:v" option; do
  case "${option}" in
    d) _opts+="-d " ;;
    j) BUILD_JOBS="${OPTARG}" ;;
    v) _opts+="-v ";;
    :)
      echo "[${BASH_SOURCE[0]}]: ${option} requires an argument"
      ;;
    *)
      echo "[${BASH_SOURCE[0]}]: Unrecognized option: ${option}"
      ;;
  esac
done
shift $((OPTIND-1))

cd "${HOMEgfs_}/sorc/ufs_model.fd/FV3/upp/tests"
# shellcheck disable=SC2086
BUILD_JOBS=${BUILD_JOBS:-8} "${HOMEgfs_}/sorc/ufs_model.fd/FV3/upp/tests/compile_upp.sh" ${_opts}

exit 0
