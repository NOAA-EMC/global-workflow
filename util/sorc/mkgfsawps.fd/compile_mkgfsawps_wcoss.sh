#!/bin/sh
LMOD_EXACT_MATCH=no
source ../../../sorc/machine-setup.sh > /dev/null 2>&1
cwd=$(pwd)

if [ "$target" = "hera" ]; then
   echo " "
   echo " You are on $target "
   echo " "
else
   echo " "
   echo " Your machine $target is not supported"
   echo " The script $0 can not continue.  Aborting!"
   echo " "
   exit
fi
echo " "

# Load required modules
source ../../modulefiles/gfs_util.${target}
module list

set -x

mkdir -p ../../exec
make -f makefile.$target
make -f makefile.$target clean
mv mkgfsawps ../../exec
