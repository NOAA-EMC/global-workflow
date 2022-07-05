#!/bin/sh
set -eux

source ./machine-setup.sh > /dev/null 2>&1
cwd=`pwd`

module use ${cwd}/../modulefiles/fv3gfs
module load enkf_chgres_recenter.$target             > /dev/null 2>&1
module list

# Check final exec folder exists
if [ ! -d "../exec" ]; then
  mkdir ../exec
fi

cd ${cwd}/enkf_chgres_recenter.fd

export FFLAGS="-O3 -r8 -i4 -qopenmp -g -traceback -fp-model precise"

make clean
make
make install
make clean
