#! /usr/bin/env bash
set -eux

# Build S2S by default
CMAKE_FLAGS="-DAPP=S2SW"
CCPP_SUITES="FV3_GFS_v16_coupled,FV3_GFS_v16_couplednsst,FV3_GFS_v16"

while [ ${#} -gt 0 ]
do
  case "${1}" in
    -a|--aerosols)
      CMAKE_FLAGS="-DAPP=ATMAERO"
      CCPP_SUITES="FV3_GFS_v16"
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

# Check final exec folder exists
if [ ! -d "../exec" ]; then
  mkdir ../exec
fi

# Set target platform
case "${target}" in
  hera|orion|stampede)
    target=${target}.intel
    ;;
esac

MOD_PATH=$cwd/ufs_coupled.fd/modulefiles

module purge

cd ufs_coupled.fd/
module use ${MOD_PATH}
module load ufs_${target}
CMAKE_FLAGS="${CMAKE_FLAGS}" CCPP_SUITES="${CCPP_SUITES}" ./build.sh
