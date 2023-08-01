#! /usr/bin/env bash
set -eux

cwd=$(pwd)

# Default settings
APP="S2SWA"
CCPP_SUITES="FV3_GFS_v16,FV3_GFS_v16_no_nsst,FV3_GFS_v16_ugwpv1,FV3_GFS_v17_p8,FV3_GFS_v16_coupled_nsstNoahmpUGWPv1,FV3_GFS_v17_coupled_p8"
export RT_COMPILER="intel"
source "${cwd}/ufs_model.fd/tests/detect_machine.sh"
source "${cwd}/ufs_model.fd/tests/module-setup.sh"

while getopts ":da:vw" option; do
  case "${option}" in
    d) BUILD_TYPE="DEBUG";;
    a) APP="${OPTARG}" ;;
    v) export BUILD_VERBOSE="YES";;
    w) export PDLIB="ON";; 
    :)
      echo "[${BASH_SOURCE[0]}]: ${option} requires an argument"
      ;;
    *)
      echo "[${BASH_SOURCE[0]}]: Unrecognized option: ${option}"
      ;;
  esac
done

cd "${cwd}/ufs_model.fd"

MAKE_OPT="-DAPP=${APP} -D32BIT=ON -DCCPP_SUITES=${CCPP_SUITES}"
[[ ${PDLIB:-"OFF"} = "ON" ]] && MAKE_OPT+=" -DPDLIB=ON"
[[ ${BUILD_TYPE:-"Release"} = "DEBUG" ]] && MAKE_OPT+=" -DDEBUG=ON"
COMPILE_NR=0
RT_COMPILER=intel
CLEAN_BEFORE=YES
CLEAN_AFTER=NO

./tests/compile.sh "${MACHINE_ID}" "${MAKE_OPT}" "${COMPILE_NR}" "${RT_COMPILER}" "${CLEAN_BEFORE}" "${CLEAN_AFTER}"
mv "./tests/fv3_${COMPILE_NR}.exe" ./tests/ufs_model.x
mv "./tests/modules.fv3_${COMPILE_NR}.lua" ./tests/modules.ufs_model.lua
cp "./modulefiles/ufs_common.lua" ./tests/ufs_common.lua

exit 0
