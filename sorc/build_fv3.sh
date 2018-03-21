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

cd fv3gfs.fd/tests
FV3=$( cd ../FV3 ; pwd -P )

./compile.sh "$FV3" "$target" "NCEP64LEV=Y HYDRO=N 32BIT=Y" 1
mv -f fv3_1.exe ../NEMS/exe/fv3_gfs_nh.prod.32bit.x
