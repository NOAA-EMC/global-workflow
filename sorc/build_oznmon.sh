#! /usr/bin/env bash
set -eux

source ./machine-setup.sh > /dev/null 2>&1
cwd=`pwd`

USE_PREINST_LIBS=${USE_PREINST_LIBS:-"true"}
if [ $USE_PREINST_LIBS = true ]; then
  export MOD_PATH=/scratch3/NCEPDEV/nwprod/lib/modulefiles
  source ../modulefiles/OznMonBuild.$target             > /dev/null 2>&1
else
  export MOD_PATH=${cwd}/lib/modulefiles
  if [ $target = wcoss_cray ]; then
    source ../modulefiles/OznMonBuild.${target}_userlib > /dev/null 2>&1
  else
    source ../modulefiles/OznMonBuild.$target           > /dev/null 2>&1
  fi
fi
module list

# Check final exec folder exists
if [ ! -d "../exec" ]; then
  mkdir ../exec
fi

dlist="oznmon_horiz.fd oznmon_time.fd"
for dir in $dlist; do
   cd $dir
   make -f makefile clean
   make -f makefile
   make -f makefile install
   make -f makefile clean
   cd ../
done
