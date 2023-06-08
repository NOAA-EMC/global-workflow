#! /usr/bin/env bash
set -eux

cwd=$(pwd)

# Default settings
APP="S2SWA"
CCPP_SUITES="FV3_GFS_v16,FV3_GFS_v16_no_nsst,FV3_GFS_v16_ugwpv1,FV3_GFS_v17_p8,FV3_GFS_v16_coupled_nsstNoahmpUGWPv1,FV3_GFS_v17_coupled_p8"

export RT_COMPILER="intel"
source "${cwd}/ufs_model.fd/tests/detect_machine.sh"
source "${cwd}/ufs_model.fd/tests/module-setup.sh"

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

cd "${cwd}/ufs_model.fd" # TODO: This is redundant; see below.

MAKE_OPT="-DAPP=${APP} -D32BIT=ON -DCCPP_SUITES=${CCPP_SUITES}"
[[ ${BUILD_TYPE:-"Release"} = "DEBUG" ]] && MAKE_OPT+=" -DDEBUG=ON"
COMPILE_NR=0
CLEAN_BEFORE=YES
CLEAN_AFTER=NO

if [[ "${MACHINE_ID}" != "noaacloud" ]]; then
  ./tests/compile.sh "${MACHINE_ID}" "${MAKE_OPT}" "${COMPILE_NR}" "${CLEAN_BEFORE}" "${CLEAN_AFTER}"
  mv "./tests/fv3_${COMPILE_NR}.exe" ./tests/ufs_model.x
  mv "./tests/modules.fv3_${COMPILE_NR}.lua" ./tests/modules.ufs_model.lua
  cp "./modulefiles/ufs_common.lua" ./tests/ufs_common.lua
fi

if [[ "${MACHINE_ID}" == "noaacloud" ]]; then
  case $(dnsdomainname -f) in   
     # TODO: Add Google and Azure platforms.
      *pw-noaa*pw.local) CLOUD_MACHINE_ID="aws.${RT_COMPILER}" ;;
      *) ;; 
  esac

  if [[ "${CLOUD_MACHINE_ID}" == "aws.intel" ]]; then
    module use /contrib/spack-stack/envs/ufswm/install/modulefiles/Core
    module load stack-intel
    module load stack-intel-oneapi-mpi
    module load ufs-weather-model-env/1.0.0
    # TODO: It is still uncertain why this is the only module that is 
    # missing; check the spack build as this needed to be added manually.
    module load w3emc/2.9.2 # TODO: This has similar issues for the EPIC stack.
    module list
  fi

  cd "${cwd}/ufs_model.fd" # TODO: This is redundant; see line #28.
  export CMAKE_FLAGS="${MAKE_OPT}"
  ./build.sh 
  mv "${cwd}/ufs_model.fd/build/ufs_model" "${cwd}/ufs_model.fd/tests/ufs_model.x"
 
  # TODO: This is hack? Where is this step performed in the build system?
  mkdir -p "${HOMEgfs}/exec"
  cp "${cwd}/ufs_model.fd/tests/ufs_model.x" "${HOMEgfs}/exec/ufs_model.x" 
fi

exit 0
