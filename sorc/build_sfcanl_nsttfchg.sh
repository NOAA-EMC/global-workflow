#! /usr/bin/env bash
set -eux

source ./machine-setup.sh > /dev/null 2>&1
cwd=`pwd`

module purge
USE_PREINST_LIBS=${USE_PREINST_LIBS:-"true"}
if [ $USE_PREINST_LIBS = true ]; then
  export MOD_PATH=/scratch3/NCEPDEV/nwprod/lib/modulefiles
  source ../modulefiles/fv3gfs/gaussian_sfcanl.$target             > /dev/null 2>&1
else
  export MOD_PATH=${cwd}/lib/modulefiles
  if [ $target = wcoss_cray ]; then
    source ../modulefiles/fv3gfs/gaussian_sfcanl.${target}_userlib > /dev/null 2>&1
  else
    source ../modulefiles/fv3gfs/gaussian_sfcanl.$target           > /dev/null 2>&1
  fi
fi
module list

cd ${cwd}/gaussian_sfcanl.fd
./makefile.sh

cd ${cwd}/nst_tf_chg.fd
./makefile.sh
