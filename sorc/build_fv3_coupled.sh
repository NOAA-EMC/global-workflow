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

if [ $target = theia ]; then target=theia.intel ; fi

cd fv3_coupled.fd/
./NEMS/NEMSAppBuilder rebuild app=coupledFV3_MOM6_CICE
mv ./NEMS/exe/NEMS.x ./NEMS/exe/nems_fv3_mom6_cice5.x
