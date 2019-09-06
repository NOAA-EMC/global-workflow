#!/bin/sh
set -eux

source ./machine-setup.sh > /dev/null 2>&1
cwd=`pwd`

USE_PREINST_LIBS=${USE_PREINST_LIBS:-"true"}
if [ $USE_PREINST_LIBS = true ]; then
  export MOD_PATH=/scratch3/NCEPDEV/nwprod/lib/modulefiles
#  source ../modulefiles/gdas_gridbull.$target             > /dev/null 2>&1
  source ../modulefiles/gdas_gridbull.GENERIC             > /dev/null 2>&1
else
  export MOD_PATH=${cwd}/lib/modulefiles
  if [ $target = wcoss_cray ]; then
    source ../modulefiles/gdas_gridbull.${target}_userlib > /dev/null 2>&1
  else
    source ../modulefiles/gdas_gridbull.$target           > /dev/null 2>&1
  fi
fi

# Check final exec folder exists
if [ ! -d "../exec" ]; then
  mkdir ../exec
fi
export target=GENERIC

### gridbull
 cd $cwd/gridbull.fd
 make -f makefile.$target
 make -f makefile.$target clean
 mv gridbull ../../exec/

### navybull
 cd $cwd
 source $cwd/machine-setup.sh            > /dev/null 2>&1
export target=GENERIC
 if [ $USE_PREINST_LIBS = true ]; then
   export MOD_PATH=/scratch3/NCEPDEV/nwprod/lib/modulefiles
   source ../modulefiles/gdas_navybull.GENERIC             > /dev/null 2>&1
 else
   export MOD_PATH=${cwd}/lib/modulefiles
   if [ $target = wcoss_cray ]; then
     source ../modulefiles/gdas_navybull.${target}_userlib > /dev/null 2>&1
   else
     source ../modulefiles/gdas_navybull.$target           > /dev/null 2>&1
   fi
 fi
 cd $cwd/navybull.fd
 make -f makefile.$target
 make -f makefile.$target clean
 mv navybull ../../exec/

### gdas_trpsfcmv
 cd $cwd
 source $cwd/machine-setup.sh            > /dev/null 2>&1
 source $cwd/ncl.setup
module list
export NCARG_LIB=$NCARG_ROOT/lib

export target=GENERIC
 if [ $USE_PREINST_LIBS = true ]; then
   export MOD_PATH=/scratch3/NCEPDEV/nwprod/lib/modulefiles
   source ../modulefiles/gdas_trpsfcmv.GENERIC             > /dev/null 2>&1
 else
   export MOD_PATH=${cwd}/lib/modulefiles
   if [ $target = wcoss_cray ]; then
     source ../modulefiles/gdas_trpsfcmv.${target}_userlib > /dev/null 2>&1
   else
     source ../modulefiles/gdas_trpsfcmv.$target           > /dev/null 2>&1
   fi
 fi
 cd $cwd/gdas_trpsfcmv.fd
 make -f makefile.$target
 make -f makefile.$target clean
 mv gdas_trpsfcmv ../../exec/

exit
