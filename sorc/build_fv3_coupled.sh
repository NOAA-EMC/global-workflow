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


cd fv3_coupled.fd/NEMS 
make -j 8 app=coupledFV3_CCPP_MOM6_CICE_WW3 build
mv ./exe/NEMS.x ./exe/nems_fv3_ccpp_mom6_cice5_ww3.x
