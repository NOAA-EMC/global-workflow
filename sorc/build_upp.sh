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

source "${HOMEgfs_}/ush/detect_machine.sh"

if [[ "${MACHINE_ID}" == "gaeac6" ]]; then
  # The UPP does not recognise gaeac6 as a valid machine, so we need to set it to "gaea"
  #    TODO: remove this stop-gap fix when UPP#1308 is addressed and that hash is brought into the GW.
  export MACHINE_ID="gaea"
fi

cd "${HOMEgfs_}/sorc/ufs_model.fd/UFSATM/upp/tests"
# shellcheck disable=SC2086
BUILD_JOBS=${BUILD_JOBS:-8} bash -x "${HOMEgfs_}/sorc/ufs_model.fd/UFSATM/upp/tests/compile_upp.sh" ${_opts}

exit 0
