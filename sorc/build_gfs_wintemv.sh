#!/bin/sh

######################################################################
#
# Build executable utility: wintemv using module compile standard
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
 echo "  #         $0   wcoss        #"
 echo "  #      or                                       #"
 echo "  #                                               #"
 echo "  #         $0   cray         #"
 echo "  #      or                                       #"
 echo "  #                                               #"
 echo "  #         $0   theia        #"
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
 echo "  "
 echo "           $0   wcoss      "
 echo "  "
 echo "        or                 "
 echo "  "
 echo "           $0   cray       "
 echo "  "
 exit
fi
set -x
mod=$( cd ../modulefiles/ ; pwd -P )
source "$mod/module-setup.sh.inc"
module use ../modulefiles
module load gfs_wintemv.$target

module list

cd wintemv.fd
make -f makefile.$target
make -f makefile.$target install
make -f makefile.$target clean
