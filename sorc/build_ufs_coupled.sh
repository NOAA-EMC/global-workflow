#! /usr/bin/env bash
set -eux

source ./machine-setup.sh > /dev/null 2>&1
cwd=`pwd`

# Check final exec folder exists
if [ ! -d "../exec" ]; then
  mkdir ../exec
fi

if [ $target = hera ]; then target=hera.intel ; fi
if [ $target = orion ]; then target=orion.intel ; fi
if [ $target = stampede ]; then target=stampede.intel ; fi

MOD_PATH=$cwd/ufs_coupled.fd/modulefiles

module purge 
module use $MOD_PATH 
module load ufs_${target}
cd ufs_coupled.fd/
if [[ -d build ]]; then rm -Rf build; fi
if [[ -d GOCART ]]; then
  module load ufs_aerosols_${target}
  CMAKE_FLAGS="-DAPP=ATMAERO" CCPP_SUITES="FV3_GFS_v16" ./build.sh
else
  CMAKE_FLAGS="-DS2S=ON -DWW3=ON" CCPP_SUITES="FV3_GFS_v15p2_coupled,FV3_GFS_v16_coupled,FV3_GFS_v16" ./build.sh
fi
