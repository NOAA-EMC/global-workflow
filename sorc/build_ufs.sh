#! /usr/bin/env bash
set -eux

cwd=$(pwd)

# Default settings
APP="S2SWA"
CCPP_SUITES="FV3_GFS_v17_p8_ugwpv1,FV3_GFS_v17_coupled_p8_ugwpv1,FV3_global_nest_v1"  # TODO: does the g-w need to build with all these CCPP_SUITES?
PDLIB="ON"

while getopts ":da:fj:vw" option; do
  case "${option}" in
    d) BUILD_TYPE="Debug";;
    a) APP="${OPTARG}";;
    f) FASTER="ON";;
    j) BUILD_JOBS="${OPTARG}";;
    v) export BUILD_VERBOSE="YES";;
    w) PDLIB="OFF";;
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
if [[ ${BUILD_TYPE:-"Release"} = "DEBUG" ]] ; then
    MAKE_OPT+=" -DDEBUG=ON"
elif [[ "${FASTER:-OFF}" == ON ]] ; then
    MAKE_OPT+=" -DFASTER=ON"
fi
COMPILE_NR=0
CLEAN_BEFORE=YES
CLEAN_AFTER=NO

BUILD_JOBS=${BUILD_JOBS:-8} ./tests/compile.sh "${MACHINE_ID}" "${MAKE_OPT}" "${COMPILE_NR}" "intel" "${CLEAN_BEFORE}" "${CLEAN_AFTER}"
mv "./tests/fv3_${COMPILE_NR}.exe" ./tests/ufs_model.x
mv "./tests/modules.fv3_${COMPILE_NR}.lua" ./tests/modules.ufs_model.lua
cp "./modulefiles/ufs_common.lua" ./tests/ufs_common.lua

exit 0
