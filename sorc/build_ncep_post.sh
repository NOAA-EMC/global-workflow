#! /usr/bin/env bash
set -eux

set +e
source ./machine-setup.sh > /dev/null 2>&1
set -e
cwd=`pwd`

USE_PREINST_LIBS=${USE_PREINST_LIBS:-"false"}
if [ $USE_PREINST_LIBS = true ]; then
  export MOD_PATH=/scratch3/NCEPDEV/nwprod/lib/modulefiles
else
  export MOD_PATH=${cwd}/../modulefiles
fi

# Check final exec folder exists
if [ ! -d "../exec" ]; then
  mkdir ../exec
fi

# Cheats until moving through commit process with post
if [[ $target == 'jet' || $target == 'gaea' ]]; then
    cp -f build_ncep_post_native.sh gfs_post.fd/sorc
    cp -f ../modulefiles/fv3gfs/post/v8.0.0-${target} gfs_post.fd/modulefiles/post
    cd gfs_post.fd/sorc
    sh build_ncep_post_native.sh
    exit 0
fi

cd gfs_post.fd/sorc
sh build_ncep_post.sh
