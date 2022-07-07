#! /usr/bin/env bash
set -eux

source ./machine-setup.sh > /dev/null 2>&1
cwd=`pwd`

module use ${cwd}/../modulefiles/fv3gfs
module load gaussian_sfcanl.$target             > /dev/null 2>&1
module list

cd ${cwd}/gaussian_sfcanl.fd
./makefile.sh
