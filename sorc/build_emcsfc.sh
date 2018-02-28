#!/bin/sh

#------------------------------------------------------------
# Build all "emcsfc" programs.
# 
# For more details, see the documentation in each
# program sub-directory:
#
#  ./sorc/emcsfc_ice_blend.fd
#  ./sorc/emcsfc_snow2mdl.fd
#
# To run, type "build_emcsfc.sh" from the command line.
#------------------------------------------------------------

set -x

source ./machine-setup.sh > /dev/null 2>&1
cwd=`pwd`

# Check final exec folder exists
if [ ! -d "../exec" ]; then
  mkdir ../exec
fi

for prog in emcsfc_ice_blend emcsfc_snow2mdl
do
  USE_PREINST_LIBS=${USE_PREINST_LIBS:-"true"}
  if [ $USE_PREINST_LIBS = true ]; then
    export MOD_PATH=/scratch3/NCEPDEV/nwprod/lib/modulefiles
  else
    export MOD_PATH=${cwd}/lib/modulefiles
  fi

  cd ${prog}.fd
  make clean
  sh make.sh
  module list
  cd $cwd
done

echo; echo DONE BUILDING EMCSFC PROGRAMS
