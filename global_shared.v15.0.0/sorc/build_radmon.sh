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
module use -a ../modulefiles
module load RadMonBuild.$target
module list

dlist="verf_radang.fd verf_radbcoef.fd verf_radbcor.fd verf_radtime.fd"
for dir in $dlist; do
   cd $dir
   make -f makefile clean
   make -f makefile
   make -f makefile install
   make -f makefile clean
   cd ../
done
