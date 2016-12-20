#!/bin/sh
set -x -e

target=$1
if [ $# -ne 1 ]; then
 echo "Usage: $0 wcoss or cray or theia"
 exit
fi

EXECdir=../exec
[ -d $EXECdir ] || mkdir $EXECdir

if [ $target = wcoss ]; then
. /usrx/local/Modules/3.2.10/init/sh
elif [ $target = cray ]; then
. $MODULESHOME/init/sh
elif [ $target = theia ]; then
. /apps/lmod/lmod/init/sh
else
 exit
fi
module purge
if [ $target = wcoss -o $target = cray ]; then
 module load ../modulefiles/Module_gdas_trpsfcmv.$target
else
 source ../modulefiles/Module_gdas_trpsfcmv.$target
fi
module list

cd gdas_trpsfcmv.fd
   make -f makefile
   cp gdas_trpsfcmv ../$EXECdir/.
   cd ../
