#! /usr/bin/env bash
set -x

# shellcheck disable=SC2312
_HOMEgfs=$(cd "$(dirname "$(readlink -f -n "${BASH_SOURCE[0]}" )" )/.." && pwd -P)
cd "${_HOMEgfs}/sorc" || exit 1

# Default settings
PDLIB="ON"

while getopts ":j:a:dvw" option; do
  case "${option}" in
  d) BUILD_TYPE="Debug" ;;
  j) BUILD_JOBS="${OPTARG}" ;;
  v) export BUILD_VERBOSE="YES" ;;
  w) PDLIB="OFF" ;;
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

# Determine machine and load modules
set +x
source "${_HOMEgfs}/sorc/ufs_model.fd/tests/detect_machine.sh"
source "${_HOMEgfs}/sorc/ufs_model.fd/tests/module-setup.sh"
module use "${_HOMEgfs}/sorc/ufs_model.fd/modulefiles"
module load "ufs_${MACHINE_ID}.intel"
set -x

#Set WW3 directory
cd "${_HOMEgfs}/sorc/ufs_model.fd/WW3" || exit 1
WW3_DIR=$(pwd -P)
export WW3_DIR

# Determine which switch to use
if [[ "${PDLIB}" == "ON" ]]; then
  ww3switch="model/bin/switch_meshcap_pdlib"
  path_build="${WW3_DIR}/build/pdlib_ON"
  path_install="${WW3_DIR}/install/pdlib_ON"
else
  ww3switch="model/bin/switch_meshcap"
  path_build="${WW3_DIR}/build/pdlib_OFF"
  path_install="${WW3_DIR}/install/pdlib_OFF"
fi
export SWITCHFILE="${WW3_DIR}/${ww3switch}"

#create build directory:
[[ -d "${path_build}" ]] && rm -rf "${path_build}"
mkdir -p "${path_build}" || exit 1
cd "${path_build}" || exit 1
echo "Forcing a SHRD build"

buildswitch="${path_build}/switch"

cat "${SWITCHFILE}" >"${path_build}/tempswitch"

sed -e "s/DIST/SHRD/g" \
  -e "s/OMPG / /g" \
  -e "s/OMPH / /g" \
  -e "s/MPIT / /g" \
  -e "s/MPI / /g" \
  -e "s/PIO / /g" \
  -e "s/B4B / /g" \
  -e "s/PDLIB / /g" \
  -e "s/SCOTCH / /g" \
  -e "s/METIS / /g" \
  -e "s/NOGRB/NCEP2/g" \
  "${path_build}/tempswitch" >"${path_build}/switch"
rm "${path_build}/tempswitch"

echo "Switch file is ${buildswitch} with switches:"
cat "${buildswitch}"

#define cmake build options
MAKE_OPT="-DCMAKE_INSTALL_PREFIX=${path_install}"
[[ ${BUILD_TYPE:-"Release"} = "Debug" ]] && MAKE_OPT+=" -DCMAKE_BUILD_TYPE=Debug"

#Build executables:
# shellcheck disable=SC2086
cmake "${WW3_DIR}" -DSWITCH="${buildswitch}" ${MAKE_OPT}
rc=$?
if ((rc != 0)); then
  echo "Fatal error in cmake."
  exit "${rc}"
fi

make -j "${BUILD_JOBS:-8}"
rc=$?
if ((rc != 0)); then
  echo "Fatal error in make."
  exit "${rc}"
fi

make install
rc=$?
if ((rc != 0)); then
  echo "Fatal error in make install."
  exit "${rc}"
fi

exit 0
