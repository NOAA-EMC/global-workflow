#!/bin/sh

######################################################################
#
# Build executable utility: cnvgrib21_gfs using module compile standard
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
 echo "  #         $0   wcoss  #"
 echo "  #      or                                       #"
 echo "  #                                               #"
 echo "  #         $0   cray   #"
 echo "  #      or                                       #"
 echo "  #                                               #"
 echo "  #         $0   theia  #"
 echo "  #                                               #"
 echo "  #################################################"
 echo " "
 echo " "
 exit
fi

if [ $target = wcoss ]; then
. /usrx/local/Modules/3.2.10/init/sh
elif [ $target = cray ]; then
. $MODULESHOME/init/sh
elif [ $target = theia ]; then
. /apps/lmod/lmod/init/sh
else
 set +x
 echo " "
 echo "  #################################################"
 echo "  #                                               #"
 echo "  #   Usage:                                      #"
 echo "  #                                               #"
 echo "  #         $0   wcoss  #"
 echo "  #      or                                       #"
 echo "  #                                               #"
 echo "  #         $0   cray   #"
 echo "  #      or                                       #"
 echo "  #                                               #"
 echo "  #         $0   theia  #"
 echo "  #                                               #"
 echo "  #################################################"
 echo " "
 echo " "
 exit
fi

if [ $target = wcoss -o $target = cray ]; then
 module load ../modulefiles/gfs_cnvgrib21_gfs.$target
else
 source ../modulefiles/gfs_cnvgrib21_gfs.$target
fi

module list

cd cnvgrib21_gfs.fd
make -f makefile.$target
make -f makefile.$target install
make -f makefile.$target clean
