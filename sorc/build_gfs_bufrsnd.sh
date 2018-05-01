#! /usr/bin/env bash
set -eux

source ./machine-setup.sh > /dev/null 2>&1
cwd=`pwd`

USE_PREINST_LIBS=${USE_PREINST_LIBS:-"true"}
if [ $USE_PREINST_LIBS = true ]; then
  export MOD_PATH=/scratch3/NCEPDEV/nwprod/lib/modulefiles
  source ../modulefiles/gfs_bufr.$target             > /dev/null 2>&1
else
  export MOD_PATH=${cwd}/lib/modulefiles
  if [ $target = wcoss_cray ]; then
    source ../modulefiles/gfs_bufr.${target}_userlib > /dev/null 2>&1
  else
    source ../modulefiles/gfs_bufr.$target           > /dev/null 2>&1
  fi
fi
module list

# Check final exec folder exists
if [ ! -d "../exec" ]; then
  mkdir ../exec
fi

# Compile codes under /sorc
compile1='gfs_bufr tocsbufr'

for comp in $compile1
do
  echo "Compiling ${comp}"
  cd $cwd/${comp}.fd
  make -f makefile_module clean
  make -f makefile_module 
done
