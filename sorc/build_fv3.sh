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

if [ $target = hera ]; then target=hera.intel ; fi
if [ $target = orion ]; then target=orion.intel ; fi

cd fv3gfs.fd/
cd tests/
#### Modify to turn on CCPP with four suite 
#### As of 11/12/2020 The FV3_GFS_v16beta has been tested with C768 cycled run on DELL system
#./compile.sh "$target" "WW3=Y 32BIT=Y" 1
# mv -f fv3_1.exe ../NEMS/exe/global_fv3gfs.x
# mv -f fv3_2.exe ../NEMS/exe/global_fv3gfs_ccpp.x
./compile.sh "$target" "CCPP=Y 32BIT=Y SUITES=FV3_GFS_v15,FV3_GSD_v0,FV3_GSD_noah,FV3_GFS_v16beta" 2 NO NO
mv -f fv3_2.exe ../NEMS/exe/global_fv3gfs.x
