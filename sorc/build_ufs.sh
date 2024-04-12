#! /usr/bin/env bash
set -eu

cwd=$(pwd)

# Default settings
APP="S2SWA"
CCPP_SUITES="FV3_GFS_v17_p8_ugwpv1,FV3_GFS_v17_coupled_p8_ugwpv1"  # TODO: does the g-w need to build with all these CCPP_SUITES?

while getopts ":da:j:vw" option; do
  case "${option}" in
    d) BUILD_TYPE="Debug";;
    a) APP="${OPTARG}";;
    j) BUILD_JOBS="${OPTARG}";;
    v) export BUILD_VERBOSE="YES";;
    w) PDLIB="ON";; 
    :)
      echo "[${BASH_SOURCE[0]}]: ${option} requires an argument"
      ;;
    *)
      echo "[${BASH_SOURCE[0]}]: Unrecognized option: ${option}"
      ;;
  esac
done

cd "${cwd}/ufs_model.fd"

source "./tests/detect_machine.sh"
source "./tests/module-setup.sh"

MAKE_OPT="-DAPP=${APP} -D32BIT=ON -DCCPP_SUITES=${CCPP_SUITES}"
[[ ${PDLIB:-"OFF"} = "ON" ]] && MAKE_OPT+=" -DPDLIB=ON"
[[ ${BUILD_TYPE:-"Release"} = "Debug" ]] && MAKE_OPT+=" -DDEBUG=ON"
COMPILE_NR=0
CLEAN_BEFORE=YES
CLEAN_AFTER=NO

echo "MACHINE_ID: ${MACHINE_ID}"
echo "PW_CSP: ${PW_CSP}"

if [[ "${MACHINE_ID}" != "noaacloud" ]]; then
  BUILD_JOBS=${BUILD_JOBS:-8} ./tests/compile.sh "${MACHINE_ID}" "${MAKE_OPT}" "${COMPILE_NR}" "intel" "${CLEAN_BEFORE}" "${CLEAN_AFTER}"
  mv "./tests/fv3_${COMPILE_NR}.exe" ./tests/ufs_model.x
  mv "./tests/modules.fv3_${COMPILE_NR}.lua" ./tests/modules.ufs_model.lua
  cp "./modulefiles/ufs_common.lua" ./tests/ufs_common.lua
else

 #if [[ "${PW_CSP:-}" == "aws" ]]; then
 #  set +x
 #  # TODO: This will need to be addressed further when the EPIC stacks are available/supported.
 #  module purge
 #  module use /contrib/spack-stack/spack-stack-1.6.0/envs/unified-env/install/modulefiles/Core
 #  module load stack-intel/2021.3.0
 #  module load cmake/3.23.1
 # #module list
 # #export SPACK_ROOT=/contrib/spack-stack/spack-stack-1.6.0/spack
 # #export PATH=$PATH:$SPACK_ROOT/bin
 # #. $SPACK_ROOT/share/spack/setup-env.sh
 # #module load stack-intel/2021.3.0
 # #module load cmake/3.23.1
 #  set -x
 #fi

 #export CMAKE_FLAGS="${MAKE_OPT}"
 #BUILD_JOBS=${BUILD_JOBS:-8} ./build.sh
 #mv "${cwd}/ufs_model.fd/build/ufs_model" "${cwd}/ufs_model.fd/tests/ufs_model.x"

  echo "MACHINE_ID: ${MACHINE_ID}"
  echo "COMPILE_NR: ${COMPILE_NR}"

  BUILD_JOBS=${BUILD_JOBS:-8} ./tests/compile.sh "${MACHINE_ID}" "${MAKE_OPT}" "${COMPILE_NR}" "intel" "${CLEAN_BEFORE}" "${CLEAN_AFTER}"
  mv "./tests/fv3_${COMPILE_NR}.exe" ./tests/ufs_model.x
  mv "./tests/modules.fv3_${COMPILE_NR}.lua" ./tests/modules.ufs_model.lua
  cp "./modulefiles/ufs_common.lua" ./tests/ufs_common.lua
fi

exit 0
