#! /usr/bin/env bash
set -eux

usage() {
  echo "Usage: $0 [-d] [-j <jobs>] [-v]"
  echo "  -d        Build in debug mode"
  echo "  -j <jobs> Number of parallel build jobs"
  echo "  -v        Verbose build output"
  exit 1
}

# shellcheck disable=SC2155
readonly HOMEgfs_=$(cd "$(dirname "$(readlink -f -n "${BASH_SOURCE[0]}" )" )/.." && pwd -P)

OPTIND=1
_opts="-f "  # forces a clean build
while getopts ":j:dv" option; do
  case "${option}" in
    d) _opts+="-c -DCMAKE_BUILD_TYPE=Debug " ;;
    j) BUILD_JOBS=${OPTARG};;
    v) _opts+="-v ";;
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

# double quoting opts will not work since it is a string of options
# shellcheck disable=SC2086
BUILD_JOBS="${BUILD_JOBS:-1}" \
./nexus.fd/build.sh ${_opts} -f -w ${HOMEgfs_}

exit
