#! /usr/bin/env bash
set -eux

source ./machine-setup.sh > /dev/null 2>&1
cwd=`pwd`

USE_PREINST_LIBS=${USE_PREINST_LIBS:-"true"}
if [ $USE_PREINST_LIBS = true ]; then
  export MOD_PATH=/scratch3/NCEPDEV/nwprod/lib/modulefiles
else
  if [ $target = jet ]; then
    export MOD_PATH=/lfs4/HFIP/hfv3gfs/nwprod/hpc-stack/libs/modulefiles/stack
  else
    export MOD_PATH=${cwd}/lib/modulefiles
  fi
fi

# Check final exec folder exists
if [ ! -d "../exec" ]; then
  mkdir ../exec
fi

cd gldas.fd/sorc
./build_all_gldas.sh

exit

