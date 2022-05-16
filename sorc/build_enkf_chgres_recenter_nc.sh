#!/bin/sh
set -eux

source ./machine-setup.sh > /dev/null 2>&1
cwd=`pwd`

module use ${cwd}/../modulefiles/fv3gfs
module load enkf_chgres_recenter_nc.$target
module list

# Check final exec folder exists
if [ ! -d "../exec" ]; then
  mkdir ../exec
fi

cd ${cwd}/enkf_chgres_recenter_nc.fd

export FFLAGS="-O3 -qopenmp -g -traceback -fp-model precise"
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
