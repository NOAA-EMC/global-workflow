#! /usr/bin/env bash
set -eux

source ./machine-setup.sh > /dev/null 2>&1
cwd=`pwd`

source ../modulefiles/gfs_fbwndgfs.$target             > /dev/null 2>&1
module list

# Check final exec folder exists
if [ ! -d "../exec" ]; then
  mkdir ../exec
fi

cd fbwndgfs.fd
make -f makefile.GENERIC
make -f makefile.GENERIC clean
mv fbwndgfs   ../../exec/
