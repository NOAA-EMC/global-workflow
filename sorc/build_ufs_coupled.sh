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

MOD_PATH=$cwd/ufs_coupled.fd/modulefiles/$target

module purge 
module use $MOD_PATH 
module load fv3 
cd ufs_coupled.fd/
CMAKE_FLAGS="-DS2S=ON" CCPP_SUITES="FV3_GFS_2017_coupled,FV3_GFS_2017_satmedmf_coupled,FV3_GFS_v15p2_coupled" ./build.sh 

