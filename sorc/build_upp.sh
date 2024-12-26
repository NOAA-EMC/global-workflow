#! /usr/bin/env bash
set -eux

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

# Check final exec folder exists
if [[ ! -d "../exec" ]]; then
  mkdir -p ../exec
fi

# The UPP does not load a cmake module and the WCOSS2 compute nodes do not have cmake in PATH by default
# Add cmake to the default modules if the command isn't found
# TODO remove this workaround when issue NOAA-EMC/UPP#1106 is addressed.
if ! command -v cmake >& /dev/null; then
   export COMPILER="intel"
   if [[ -z ${HOMEgfs+x} ]]; then
      # shellcheck disable=SC2155
      readonly HOMEgfs=$(cd "$(dirname "$(readlink -f -n "${BASH_SOURCE[0]}" )" )/.." && pwd -P)
   fi
   source "${HOMEgfs}/ush/detect_machine.sh"
   if [[ "${MACHINE_ID}" == "wcoss2" ]]; then
      set +x
      module try-load cmake

      if module is-loaded cmake; then
         LMOD_SYSTEM_DEFAULT_MODULES="${LMOD_SYSTEM_DEFAULT_MODULES} cmake"
         echo "Added cmake to the default modules"
      else
         echo "FATAL ERROR Could not find cmake or a cmake module!"
         exit 2
      fi
      set -x
   fi
fi

cd ufs_model.fd/FV3/upp/tests
# shellcheck disable=SC2086
BUILD_JOBS=${BUILD_JOBS:-8} ./compile_upp.sh ${_opts}
