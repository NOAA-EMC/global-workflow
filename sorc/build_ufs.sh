#! /usr/bin/env bash
set -eux

# Default settings
APP="S2SWA"
CCPP_SUITES="FV3_GFS_v16,FV3_GFS_v16_ugwpv1,FV3_GFS_v17_p8,FV3_GFS_v16_coupled_nsstNoahmpUGWPv1,FV3_GFS_v17_coupled_p8"

while getopts "a:v" option; do
  case "${option}" in
    a) APP="${OPTARG}" ;;
    v) BUILD_VERBOSE="YES";;
    *)
      echo "Unrecognized option: ${1}"
      exit 1
      ;;
  esac
done

source ./machine-setup.sh > /dev/null 2>&1
cwd=$(pwd)

# Set target platform
case "${target}" in
  hera|orion|stampede|jet|cheyenne)
    target=${target}.intel
    ;;
esac

MOD_PATH=$cwd/ufs_model.fd/modulefiles

cd ufs_model.fd/
set +x
module purge
module use ${MOD_PATH}
module load ufs_${target}
set -x

# Remove previous build directory if it exists
if [ -d build ]; then
  rm -R build
fi
mkdir -p build && cd build
cmake -DAPP=${APP} -DCCPP_SUITES=${CCPP_SUITES} ..
OMP_NUM_THREADS=1 make -j ${BUILD_JOBS:-8} VERBOSE=${BUILD_VERBOSE:-}
