#! /usr/bin/env bash
set -eux

source ./machine-setup.sh > /dev/null 2>&1
cwd=`pwd`

# Check final exec folder exists
if [ ! -d "../exec" ]; then
  mkdir ../exec
fi

for prog in emcsfc_ice_blend emcsfc_snow2mdl
do
  module purge
  USE_PREINST_LIBS=${USE_PREINST_LIBS:-"true"}
  if [ $USE_PREINST_LIBS = true ]; then
    source ../modulefiles/modulefile.global_${prog}.${target}             > /dev/null 2>&1
  else
    export MOD_PATH=${cwd}/lib/modulefiles
    if [ $target = wcoss_cray ]; then
      source ../modulefiles/modulefile.global_${prog}.${target}_userlib   > /dev/null 2>&1
    else
      source ../modulefiles/modulefile.global_${prog}.${target}           > /dev/null 2>&1
    fi
  fi
  module list
  cd ${cwd}/${prog}.fd
  ./make.sh
  cd $cwd
done

echo; echo DONE BUILDING EMCSFC PROGRAMS
