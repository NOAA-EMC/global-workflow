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
 :
elif [ $target = cray ]; then
 :
elif [ $target = theia ]; then
 :
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

mod=$( cd ../modulefiles/ ; pwd -P )
source "$mod/module-setup.sh.inc"
module use ../modulefiles
module load gfs_cnvgrib21_gfs.$target

module list

cd cnvgrib21_gfs.fd
make -f makefile.$target
make -f makefile.$target install
make -f makefile.$target clean
