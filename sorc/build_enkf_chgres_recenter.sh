#!/bin/sh
set -eux

source ./machine-setup.sh > /dev/null 2>&1
cwd=`pwd`

USE_PREINST_LIBS=${USE_PREINST_LIBS:-"true"}
if [ $USE_PREINST_LIBS = true ]; then
  export MOD_PATH=/scratch3/NCEPDEV/nwprod/lib/modulefiles
  source ../modulefiles/fv3gfs/enkf_chgres_recenter.$target             > /dev/null 2>&1
else
  export MOD_PATH=${cwd}/lib/modulefiles
  if [ $target = wcoss_cray ]; then
    source ../modulefiles/fv3gfs/enkf_chgres_recenter.${target}_userlib > /dev/null 2>&1
  else
    source ../modulefiles/fv3gfs/enkf_chgres_recenter.$target           > /dev/null 2>&1
  fi
fi
module list

# Check final exec folder exists
if [ ! -d "../exec" ]; then
  mkdir ../exec
fi

cd ${cwd}/enkf_chgres_recenter.fd

export FFLAGS="-O3 -r8 -i4 -qopenmp -traceback -fp-model precise"

make clean
make
make install
make clean
