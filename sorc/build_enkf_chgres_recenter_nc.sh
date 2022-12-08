#!/bin/sh
set -eux

source ./machine-setup.sh > /dev/null 2>&1
cwd=`pwd`

set +x
module use ${cwd}/../modulefiles/fv3gfs
module load enkf_chgres_recenter_nc.$target
module list
set -x

# Check final exec folder exists
if [ ! -d "../exec" ]; then
  mkdir ../exec
fi

cd ${cwd}/enkf_chgres_recenter_nc.fd

export FFLAGS="-O3 -qopenmp -g -traceback -fp-model precise"

make clean
make
make install
make clean
