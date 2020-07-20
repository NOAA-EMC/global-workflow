#! /usr/bin/env bash
set -eux

source ./machine-setup.sh > /dev/null 2>&1
cwd=`pwd`

source ../modulefiles/gfs_wintemv.$target             > /dev/null 2>&1
module list

# Check final exec folder exists
if [ ! -d "../exec" ]; then
  mkdir ../exec
fi

cd wintemv.fd
make -f makefile.$target
make -f makefile.$target install
make -f makefile.$target clean
mv wintemv ../../exec
