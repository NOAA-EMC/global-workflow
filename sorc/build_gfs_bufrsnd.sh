#!/bin/sh
set -eux

source ./machine-setup.sh > /dev/null 2>&1
cwd=`pwd`

module use ${cwd}/../modulefiles
module load gfs_bufr.$target             > /dev/null 2>&1

# Check final exec folder exists
if [ ! -d "../exec" ]; then
  mkdir ../exec
fi

if [ $target = "wcoss2" ]; then
  export SIGIO_LIB4=$SIGIO_LIB
  export SIGIO_INC4=$SIGIO_INC
  export NETCDF_INC=$NETCDF_INCLUDES
  export NETCDF_LIB=$NETCDF_LIBRARIES
  export HDF5_LIB=$HDF5_LIBRARIES
fi

if [ -f gfs_bufr.fd/getncdimlen ]; then
	   cp gfs_bufr.fd/getncdimlen ../exec
   fi
echo "compiling ...." 
# Compile codes under /sorc
compile1='gfs_bufr tocsbufr'

for comp in $compile1
do
  echo "Compiling ${comp}"
  cd $cwd/${comp}.fd
  make -f makefile_module clean
  make -f makefile_module 
done

echo "Build complete"
