#! /usr/bin/env bash
set -eux

cwd=$(pwd)

# Default settings
APP="S2SWA"
CCPP_SUITES="FV3_GFS_v17_p8_ugwpv1,FV3_GFS_v17_coupled_p8_ugwpv1,FV3_global_nest_v1"  # TODO: does the g-w need to build with all these CCPP_SUITES?
PDLIB="ON"
HYDRO="OFF"
EXEC_NAME="gfs_model.x"
# Valid only for WCOSS2; enable parallel restart I/O
# TODO: Remove this option when ufs-weather-model#2716 is fixed
PARALLEL_RESTART="NO"

while getopts ":da:fj:e:pvwy" option; do
  case "${option}" in
    d) BUILD_TYPE="DEBUG";;
    a) APP="${OPTARG}";;
    f) FASTER="ON";;
    j) BUILD_JOBS="${OPTARG}";;
    v) export BUILD_VERBOSE="YES";;
    w) PDLIB="OFF";;
    y) HYDRO="ON";;
    p) PARALLEL_RESTART="YES";;
    e) EXEC_NAME="${OPTARG}";;
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
if [[ ${PDLIB:-"OFF"} = "ON" ]]; then
    MAKE_OPT+=" -DPDLIB=ON"
fi
if [[ ${HYDRO:-"OFF"} = "ON" ]]; then
    MAKE_OPT+=" -DHYDRO=ON"
fi
if [[ ${BUILD_TYPE:-"Release"} = "DEBUG" ]] ; then
    MAKE_OPT+=" -DDEBUG=ON"
elif [[ "${FASTER:-OFF}" == ON ]] ; then
    MAKE_OPT+=" -DFASTER=ON"
fi

case "${EXEC_NAME}" in
  "ufs_model.x") COMPILE_ID=0 ;;
  "gfs_model.x") COMPILE_ID=1 ;;
  "gefs_model.x") COMPILE_ID=2 ;;
  "sfs_model.x") COMPILE_ID=3 ;;
  *) echo "Unsupported executable name: ${EXEC_NAME}"; exit 1 ;;
esac
CLEAN_BEFORE=YES
CLEAN_AFTER=NO

# The test/compile.sh script adds " -DENABLE_PARALLELRESTART=ON" when compiling on WCOSS2, which is causing issues
# TODO: when ufs-weather-model#2716 is fixed, return to using tests/compile.sh
if [[ "${MACHINE_ID}" == "wcoss2" && "${PARALLEL_RESTART}" == "NO" ]]; then
   set +x
   module use modulefiles
   module load "ufs_wcoss2.intel"
   module list
   set -x

   if [[ ${MAKE_OPT} == *-DDEBUG=ON* ]]; then
      MAKE_OPT+=" -DCMAKE_BUILD_TYPE=Debug"
   else
      MAKE_OPT+=" -DCMAKE_BUILD_TYPE=Release"
   fi

   MAKE_OPT+=" -DMPI=ON"

   BUILD_NAME="fv3_${COMPILE_ID}"
   BUILD_DIR="$(pwd)/build_${BUILD_NAME}"
   if [[ "${CLEAN_BEFORE}" == "YES" ]]; then
      rm -rf "${BUILD_DIR}"
   fi

   BUILD_DIR=${BUILD_DIR} BUILD_VERBOSE=1 BUILD_JOBS=${BUILD_JOBS:-8} CMAKE_FLAGS="${MAKE_OPT}" ./build.sh

   mv "${BUILD_DIR}/ufs_model" "tests/${BUILD_NAME}.exe"
   cp modulefiles/ufs_wcoss2.intel.lua "tests/modules.${BUILD_NAME}.lua"
   if [[ "${CLEAN_AFTER}" == "YES" ]]; then
      rm -rf "${BUILD_DIR}"
   fi
else
   BUILD_JOBS=${BUILD_JOBS:-8} ./tests/compile.sh "${MACHINE_ID}" "${MAKE_OPT}" "${COMPILE_ID}" "intel" "${CLEAN_BEFORE}" "${CLEAN_AFTER}"
fi
mv "./tests/fv3_${COMPILE_ID}.exe" "./tests/${EXEC_NAME}"
if [[ ! -f "./tests/modules.ufs_model.lua" ]]; then mv "./tests/modules.fv3_${COMPILE_ID}.lua" "./tests/modules.ufs_model.lua"; fi
if [[ ! -f "./tests/ufs_common.lua" ]]; then cp "./modulefiles/ufs_common.lua" ./tests/ufs_common.lua; fi

exit 0
