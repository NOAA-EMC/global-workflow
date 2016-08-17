#!/bin/sh
target=${1:-wcoss}
set -x -e
EXECdir=../exec
[ -d $EXECdir ] || mkdir $EXECdir

. /usrx/local/Modules/3.2.10/init/sh
module purge
module load ../modulefiles/modulefile.gdas_enkf.$target
module list

dlist="adderrspec_nmcmeth_spec.fd enkf_update.fd getsfcensmeanp.fd getsigensmeanp_smooth_ncep.fd recentersigp.fd"
for dir in $dlist; do
 if [ $dir = enkf_update.fd ]; then
   cd $dir
   ./configure clean
   ./configure $target
   make -f Makefile clean
   make -f Makefile -j 8
   cd enkf
   ./configure clean
   ./configure $target
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
   ./configure $target
   make -f Makefile clean
   make -f Makefile
   cp -p *.x ../$EXECdir/
   rm -f ../$EXECdir/log*.x 
   make -f Makefile clean
   ./configure clean
   cd ../
 fi
done
