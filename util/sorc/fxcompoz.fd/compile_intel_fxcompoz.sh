#!/bin/sh

######################################################################
#
# Build executable utility: util_shared using module compile standard
#
######################################################################
######################################################################

target=$1
if [ $# -ne 1 ]; then
 set +x
 echo " "
 echo " "
 echo "  ###################################################"
 echo "  #                                                 #"
 echo "  #   Usage:                                        #"
 echo "  #                                                 #"
 echo "  #         $0   cray      #"
 echo "  #                                                 #"
 echo "  ###################################################"
 echo " "
 echo " "
 exit
fi

if [ $target = cray ]; then
. $MODULESHOME/init/sh
module load ../../modulefiles/gfs_util_share.$target
else
 echo "  "
 echo "  "
 echo "  $1   is invalid argument. Usage: "
 echo "  "
 echo "  "
 echo "           $0   cray       "
 echo "  "
 exit
fi

module list

set -x

mkdir -p ../../exec

make
mv fxcompoz ../../exec
make clean
