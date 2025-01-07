#! /usr/bin/env bash
set -eux

cwd=$(pwd)

# Default settings
APP="S2SWA"
CCPP_SUITES="FV3_GFS_v17_p8_ugwpv1,FV3_GFS_v17_coupled_p8_ugwpv1,FV3_global_nest_v1"  # TODO: does the g-w need to build with all these CCPP_SUITES?
PDLIB="ON"
HYDRO="OFF"
EXEC_NAME="gfs_model.x"

while getopts ":da:fj:e:vwy" option; do
  case "${option}" in
    d) BUILD_TYPE="DEBUG";;
    a) APP="${OPTARG}";;
    f) FASTER="ON";;
    j) BUILD_JOBS="${OPTARG}";;
    v) export BUILD_VERBOSE="YES";;
    w) PDLIB="OFF";;
    y) HYDRO="ON";;
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
[[ ${PDLIB:-"OFF"} = "ON" ]] && MAKE_OPT+=" -DPDLIB=ON"
[[ ${HYDRO:-"OFF"} = "ON" ]] && MAKE_OPT+=" -DHYDRO=ON"
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

BUILD_JOBS=${BUILD_JOBS:-8} ./tests/compile.sh "${MACHINE_ID}" "${MAKE_OPT}" "${COMPILE_ID}" "intel" "${CLEAN_BEFORE}" "${CLEAN_AFTER}"
mv "./tests/fv3_${COMPILE_ID}.exe" "./tests/${EXEC_NAME}"
if [[ ! -f "./tests/modules.ufs_model.lua" ]]; then mv "./tests/modules.fv3_${COMPILE_ID}.lua" "./tests/modules.ufs_model.lua"; fi
if [[ ! -f "./tests/ufs_common.lua" ]]; then cp "./modulefiles/ufs_common.lua" ./tests/ufs_common.lua; fi

exit 0
