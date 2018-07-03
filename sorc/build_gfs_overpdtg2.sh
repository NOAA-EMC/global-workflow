#! /usr/bin/env bash
set -eux

source ./machine-setup.sh > /dev/null 2>&1
cwd=`pwd`

USE_PREINST_LIBS=${USE_PREINST_LIBS:-"true"}
if [ $USE_PREINST_LIBS = true ]; then
  export MOD_PATH=/scratch3/NCEPDEV/nwprod/lib/modulefiles
  source ../modulefiles/gfs_overpdtg2.$target             > /dev/null 2>&1
else
  export MOD_PATH=${cwd}/lib/modulefiles
  if [ $target = wcoss_cray ]; then
    source ../modulefiles/gfs_overpdtg2.${target}_userlib > /dev/null 2>&1
  else
    source ../modulefiles/gfs_overpdtg2.$target           > /dev/null 2>&1
  fi
fi
module list

# Check final exec folder exists
if [ ! -d "../exec" ]; then
  mkdir ../exec
fi

cd overpdtg2.fd
make -f makefile.$target
make -f makefile.$target install
make -f makefile.$target clean
mv overpdtg2 ../../exec
