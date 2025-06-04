#! /usr/bin/env bash
set -eux

OPTIND=1
_opts="-f "  # forces a clean build
while getopts ":j:dvw:" option; do
  case "${option}" in
    d) _opts+="-c -DCMAKE_BUILD_TYPE=Debug " ;;
    j) BUILD_JOBS=${OPTARG};;
    v) _opts+="-v ";;
    w) HOMEgfs=${OPTARG};;
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

if [[ -z "${HOMEgfs:-}" ]]; then
  echo "Error: HOMEgfs is a required argument" >&2
  exit 1
fi

# double quoting opts will not work since it is a string of options
# shellcheck disable=SC2086
BUILD_JOBS="${BUILD_JOBS:-8}" \
WORKFLOW_BUILD="${WORKFLOW_BUILD:-"ON"}" \
WORKFLOW_TESTS="${WORKFLOW_TESTS:-"OFF"}" \
HOMEgfs="${HOMEgfs}" \
./gdas.cd/build.sh ${_opts} -f

exit
