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

export BUILD_TARGET=$target
[[ "$target" == wcoss_cray ]] && BUILD_TARGET=cray

# Check final exec folder exists
if [ ! -d "../exec" ]; then
  mkdir ../exec
fi

cd ufs_da.fd/UFS-DA/ush
./build_UFSDA.sh

exit

