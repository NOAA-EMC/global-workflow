#!/bin/sh
set -xeu
#------------------------------------
# USER DEFINED STUFF:
#
# USE_PREINST_LIBS: set to "true" to use preinstalled libraries.
#                   Anything other than "true"  will use libraries locally.
#------------------------------------

if [ $# -eq 1 ]; then
model=$1
fi
model=${model:-"uncoupled"}

export USE_PREINST_LIBS="true"

#------------------------------------
# END USER DEFINED STUFF
#------------------------------------

build_dir=`pwd`
logs_dir=$build_dir/logs
if [ ! -d $logs_dir  ]; then
  echo "Creating logs folder"
  mkdir $logs_dir
fi

# Check final exec folder exists
if [ ! -d "../exec" ]; then
  echo "Creating ../exec folder"
  mkdir ../exec
fi

#------------------------------------
# GET MACHINE
#------------------------------------
target=""
source ./machine-setup.sh > /dev/null 2>&1

#------------------------------------
# INCLUDE PARTIAL BUILD 
#------------------------------------

. ./partial_build.sh

#------------------------------------
# build reg2grb2
#------------------------------------
if [ $target = hera -o $target = orion ]; then
  $Build_reg2grb2 && {
  echo " .... Building reg2grb2 .... "
  ./build_reg2grb2.sh > $logs_dir/build_reg2grb2.log 2>&1
}
fi

echo;echo " .... Build system finished .... "

exit 0
