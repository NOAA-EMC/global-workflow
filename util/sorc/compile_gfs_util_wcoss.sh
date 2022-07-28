#!/bin/sh

######################################################################
#
# Build executable GFS utility for GFS V16.0.0
#
######################################################################

LMOD_EXACT_MATCH=no
source ../../sorc/machine-setup.sh > /dev/null 2>&1
cwd=$(pwd)

if [ "$target" = "hera" ] ; then
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
source ../modulefiles/gfs_util.${target}
module list

dirlist="overgridid rdbfmsua webtitle mkgfsawps"
set -x

for dir in $dirlist
do
  cd ${dir}.fd
  echo "PWD: $PWD"
  set +x
  echo " "
  echo " ### ${dir} ### "
  echo " "
  set -x
  ./compile_${dir}_wcoss.sh
  set +x
  echo " "
  echo " ######################################### "
  echo " "
  cd ..
  echo "BACK TO: $PWD"
done
