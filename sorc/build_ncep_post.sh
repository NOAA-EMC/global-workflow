#! /usr/bin/env bash
set -eux

source ./machine-setup.sh > /dev/null 2>&1
cwd=`pwd`

USE_PREINST_LIBS=${USE_PREINST_LIBS:-"true"}
if [ $USE_PREINST_LIBS = true ]; then
  export MOD_PATH=/scratch3/NCEPDEV/nwprod/lib/modulefiles
else
  export MOD_PATH=${cwd}/lib/modulefiles
fi

# Check final exec folder exists
if [ ! -d "../exec" ]; then
  mkdir ../exec
fi

# Cheats until moving through commit process with post
cp build_ncep_post_native.sh gfs_post.fd/sorc/build_ncep_post.sh
cp ../modulefiles/fv3gfs/post/v7.0.0-gaea gfs_post.fd/modulefiles/post

cd gfs_post.fd/sorc
sh build_ncep_post.sh
