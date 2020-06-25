#! /usr/bin/env bash
set -eux

source ./machine-setup.sh > /dev/null 2>&1
cwd=`pwd`

source ../modulefiles/fv3gfs/gaussian_sfcanl.$target             > /dev/null 2>&1
module list

cd ${cwd}/gaussian_sfcanl.fd
./makefile.sh

#cd ${cwd}/nst_tf_chg.fd
#./makefile.sh
