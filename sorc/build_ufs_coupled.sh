#! /usr/bin/env bash
set -eux

# Build S2S by default
APP="S2SW"
CCPP_SUITES="FV3_GFS_v16,FV3_GFS_v16_coupled,FV3_GFS_v16_couplednsst,FV3_GFS_v16_coupled_nsstNoahmpUGWPv1"

while getopts "a" option; do
  case $option in
    a)
      APP="ATMAERO"
      CCPP_SUITES="FV3_GFS_v16,FV3_GFS_v16_ugwpv1"
      shift
      ;;
    *)
      echo "Unrecognized option: ${1}"
      exit 1
      ;;
  esac
done

source ./machine-setup.sh > /dev/null 2>&1
cwd=`pwd`

# Set target platform
case "${target}" in
  hera|orion|stampede)
    target=${target}.intel
    ;;
esac

MOD_PATH=$cwd/ufs_coupled.fd/modulefiles

module purge

cd ufs_coupled.fd/

if [ -d build ]; then
  rm -Rf build
fi

module use ${MOD_PATH}
module load ufs_${target}
CMAKE_FLAGS="-DAPP=${APP} -DCCPP_SUITES=${CCPP_SUITES}" ./build.sh
