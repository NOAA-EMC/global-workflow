#!/bin/sh
set -eux

source ./machine-setup.sh > /dev/null 2>&1
cwd=`pwd`

USE_PREINST_LIBS=${USE_PREINST_LIBS:-"true"}
if [ $USE_PREINST_LIBS = true ]; then
  export MOD_PATH=/scratch3/NCEPDEV/nwprod/lib/modulefiles
  source ../modulefiles/fv3gfs/enkf_chgres_recenter_nc.$target             > /dev/null 2>&1
else
  export MOD_PATH=${cwd}/lib/modulefiles
  if [ $target = wcoss_cray ]; then
    source ../modulefiles/fv3gfs/enkf_chgres_recenter_nc.${target}_userlib > /dev/null 2>&1
  else
    source ../modulefiles/fv3gfs/enkf_chgres_recenter_nc.$target           > /dev/null 2>&1
  fi
fi
module list

# Check final exec folder exists
if [ ! -d "../exec" ]; then
  mkdir ../exec
fi

cd ${cwd}/enkf_chgres_recenter_nc.fd

export FFLAGS="-O3 -qopenmp -traceback -fp-model precise"
export FV3GFS_NCIO_LIB="${cwd}/gsi.fd/build/lib/libfv3gfs_ncio.a"
export FV3GFS_NCIO_INC="${cwd}/gsi.fd/build/include"

if [ ! -f $FV3GFS_NCIO_LIB ]; then
  echo "BUILD ERROR: missing GSI library file"
  echo "Missing file: $FV3GFS_NCIO_LIB"
  echo "Please build the GSI first (build_gsi.sh)"
  echo "EXITING..."
  exit 1
fi

make clean
make
make install
make clean
