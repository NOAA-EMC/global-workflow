#!/bin/sh
set -x -e

######################################################################
#
# Build executable utility: gridbull using module compile standard
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

mod=$( cd ../modulefiles/ ; pwd -P )
source "$mod/module-setup.sh.inc"
module use ../modulefiles
module load gdas_gridbull.$target

module list

cd gridbull.fd

make -f makefile.$target
make -f makefile.$target clean
mv gridbull   ../../exec/
