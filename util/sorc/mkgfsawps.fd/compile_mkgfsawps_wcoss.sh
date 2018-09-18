#!/bin/sh
LMOD_EXACT_MATCH=no
source ../../../sorc/machine-setup.sh > /dev/null 2>&1
cwd=`pwd`

if [ "$target" = "wcoss_dell_p3" ] || [ "$target" = "wcoss_cray" ] || [ "$target" = "theia" ] ; then
   echo " "
   echo " You are on WCOSS:  $target "
   echo " "
elif [ "$target" = "wcoss" ] ; then
   echo " "
   echo " "
   echo " You are on WCOSS:  $target "
   echo " You do not need to build GFS utilities for GFS V15.0.0 "
   echo " "
   echo " "
   exit
else
   echo " "
   echo " Your machine is $target is not recognized as a WCOSS machine."
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
