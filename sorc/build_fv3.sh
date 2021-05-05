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
FV3=$( pwd -P )/FV3
cd tests/

# Standalone FV3 (no WW3)
#./compile.sh "$target" "APP=ATM 32BIT=Y SUITES=FV3_GFS_v16" 2 NO NO

# FV3 + WW3
./compile.sh "$target" "APP=ATMW 32BIT=Y SUITES=FV3_GFS_v16" 2 NO NO

if [ ! -d ../NEMS/exe ]; then mkdir ../NEMS/exe ; fi
mv -f fv3_2.exe ../NEMS/exe/global_fv3gfs.x
