#! /usr/bin/env bash
set -eux

cwd=$(pwd)

# Default settings
APP="S2SWA"
CCPP_SUITES="FV3_GFS_v16,FV3_GFS_v16_ugwpv1,FV3_GFS_v17_p8,FV3_GFS_v16_coupled_nsstNoahmpUGWPv1,FV3_GFS_v17_coupled_p8"

while getopts ":da:v" option; do
  case "${option}" in
    d) BUILD_TYPE="Debug";;
    a) APP="${OPTARG}" ;;
    v) BUILD_VERBOSE="YES";;
    \?)
      echo "[$BASH_SOURCE]: Unrecognized option: ${option}"
      ;;
    :)
      echo "[$BASH_SOURCE]: ${option} requires an argument"
      ;;
  esac
done

cd $cwd/ufs_model.fd

export RT_COMPILER="intel"
source $cwd/ufs_model.fd/tests/detect_machine.sh
MAKE_OPT="-DAPP=${APP} -DCCPP_SUITES=${CCPP_SUITES}"
[[ ${BUILD_TYPE:-"Release"} = "DEBUG" ]] && MAKE_OPT+=" -DDEBUG=ON"
COMPILE_NR=0
CLEAN_BEFORE=YES
CLEAN_AFTER=NO

./tests/compile.sh $MACHINE_ID "$MAKE_OPT" $COMPILE_NR $CLEAN_BEFORE $CLEAN_AFTER
mv ./tests/fv3_${COMPILE_NR}.exe ./tests/ufs_model.x
mv ./tests/modules.fv3_${COMPILE_NR} ./tests/modules.ufs_model

exit 0
