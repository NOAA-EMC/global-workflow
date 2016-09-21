#!/bin/sh

target=$1
if [ $# -ne 1 ]; then
 echo "Usage: $0 wcoss or cray or theia"
 exit
fi

set -x -e
EXECdir=../exec
[ -d $EXECdir ] || mkdir $EXECdir

if [ $target = wcoss ]; then
. /usrx/local/Modules/3.2.10/init/sh
conf_target=nco
elif [ $target = cray ]; then
. $MODULESHOME/init/sh
conf_target=nco
elif [ $target = theia ]; then
. /apps/lmod/lmod/init/sh
conf_target=theia
else
 exit
fi

module purge
if [ $target = wcoss -o $target = cray ]; then
 module load ../modulefiles/modulefile.global_gsi.$target
else
 source ../modulefiles/modulefile.global_gsi.$target
fi
module list

dlist="gsi.fd"
for dir in $dlist; do
 cd $dir
 ./configure clean
 ./configure $conf_target
 make -f Makefile clean
 make -f Makefile -j 8
 cp -p global_gsi ../$EXECdir
 make -f Makefile clean
 ./configure clean
 cd ..
done
