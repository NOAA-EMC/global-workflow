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
 module load ../modulefiles/modulefile.gdas_enkf.$target
else
 source ../modulefiles/modulefile.gdas_enkf.$target
fi
module list

dlist="adderrspec_nmcmeth_spec.fd enkf_update.fd getsfcensmeanp.fd getsigensstatp.fd getnstensmeanp.fd getsfcnstensupdp.fd getsigensmeanp_smooth_ncep.fd recentersigp.fd"
for dir in $dlist; do
 if [ $dir = enkf_update.fd ]; then
   cd $dir
   ./configure clean
   ./configure $conf_target
   make -f Makefile clean
   make -f Makefile -j 8
   cd enkf
   ./configure clean
   ./configure $conf_target
   make -f Makefile clean
   make -f Makefile
   cp -p global_enkf ../../$EXECdir/
   make -f Makefile clean
   ./configure clean
   cd ../
   make -f Makefile clean
   ./configure clean
   cd ../
 else
   cd $dir
   ./configure clean
   ./configure $conf_target
   make -f Makefile clean
   make -f Makefile
   cp -p *.x ../$EXECdir/
   rm -f ../$EXECdir/log*.x 
   make -f Makefile clean
   ./configure clean
   cd ../
 fi
done
