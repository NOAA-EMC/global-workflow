#! /usr/bin/env bash
set -eux

cwd=$(pwd)

# Default settings
APP="S2SWA"
CCPP_SUITES="FV3_GFS_v16,FV3_GFS_v16_no_nsst,FV3_GFS_v16_ugwpv1,FV3_GFS_v17_p8,FV3_GFS_v16_coupled_nsstNoahmpUGWPv1,FV3_GFS_v17_coupled_p8"  # TODO: does the g-w need to build with all these CCPP_SUITES?

while getopts ":da:v" option; do
  case "${option}" in
    d) BUILD_TYPE="DEBUG";;
    a) APP="${OPTARG}" ;;
    v) export BUILD_VERBOSE="YES";;
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
[[ ${BUILD_TYPE:-"Release"} = "DEBUG" ]] && MAKE_OPT+=" -DDEBUG=ON"
COMPILE_NR=0
CLEAN_BEFORE=YES
CLEAN_AFTER=NO

./tests/compile.sh "${MACHINE_ID}" "${MAKE_OPT}" "intel" "${COMPILE_NR}" "${CLEAN_BEFORE}" "${CLEAN_AFTER}"
mv "./tests/fv3_${COMPILE_NR}.exe" ./tests/ufs_model.x
mv "./tests/modules.fv3_${COMPILE_NR}.lua" ./tests/modules.ufs_model.lua
cp "./modulefiles/ufs_common.lua" ./tests/ufs_common.lua
cp "./modulefiles/ufs_common_spack.lua" ./tests/ufs_common_spack.lua

exit 0
