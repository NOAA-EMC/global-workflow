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
if [ $target = orion.intel ]; then
  #temporary orion build fix
  module load python/3.7.5
fi
module use $MOD_PATH 
module load fv3 
cd ufs_coupled.fd/
CMAKE_FLAGS="-DS2S=ON -DWW3=ON" CCPP_SUITES="FV3_GFS_v15p2_coupled,FV3_GFS_v16_coupled,FV3_GFS_v16" ./build.sh 

