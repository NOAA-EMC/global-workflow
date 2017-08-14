#!/bin/sh
set -eux

#####################################################################################
# 08/06/2016 Fanglin.Yang@noaa.gov:   to build nemsio_util on wcoss, theia and cray
#####################################################################################

if [ $# -ne 1 ]; then
 echo "Usage: $0 wcoss or cray or theia"
 exit
fi
target=$1

EXECdir=../exec
[ -d $EXECdir ] || mkdir $EXECdir

if [ $target = wcoss ]; then
. /usrx/local/Modules/3.2.10/init/sh
elif [ $target = cray ]; then
. $MODULESHOME/init/sh
export FCMP=ftn
elif [ $target = theia ]; then
. /apps/lmod/lmod/init/sh
export FCMP=ifort
else
 exit
fi

if [ $target = wcoss -o $target = cray ]; then
 module load ../modulefiles/module_nemsutil.$target
else
 source ../modulefiles/module_nemsutil.$target
fi
module list

curdir=`pwd`

for prog in nemsio_get.fd  mkgfsnemsioctl.fd  nemsio_cvt.fd  nemsio_read.fd ;do

cd ${curdir}/$prog
make -f makefile

done

