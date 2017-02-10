#!/bin/sh
set -x -e

######################################################################
#
# Build executable utility: navybull using module compile standard
#
######################################################################
######################################################################

target=$1
if [ $# -ne 1 ]; then
 set +x
 echo " "
 echo "  #################################################"
 echo "  #                                               #"
 echo "  #   Usage:                                      #"
 echo "  #                                               #"
 echo "  #         $0   wcoss      #"
 echo "  #      or                                       #"
 echo "  #                                               #"
 echo "  #         $0   cray       #"
 echo "  #      or                                       #"
 echo "  #                                               #"
 echo "  #         $0   theia      #"
 echo "  #                                               #"
 echo "  #################################################"
 echo " "
 echo " "
 exit
fi

if [ $target = wcoss ]; then
/usrx/local/Modules/3.2.10/init/sh
elif [ $target = cray ]; then
. $MODULESHOME/init/sh
elif [ $target = theia ]; then
. /apps/lmod/lmod/init/sh
else
 exit
fi

if [ $target = wcoss -o $target = cray ]; then
 module load ../modulefiles/gdas_navybull.$target
else
 source ../modulefiles/gdas_navybull.$target
fi

module list

cd navybull.fd

make -f makefile.$target
make  -f makefile.$target clean
mv navybull   ../../exec/
