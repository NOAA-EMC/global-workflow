#!/bin/sh

target=$1
if [ $# -ne 1 ]; then
    echo "Usage: $0 wcoss or wcoss_c/cray or theia"
    exit
fi

set -x -e

cd ..
dir_root=$(pwd)
dir_util=$dir_root/util
dir_scripts=$dir_root/scripts
[ -d $dir_root/exec ] || mkdir -p $dir_root/exec

if [ $target = wcoss ]; then
    . /usrx/local/Modules/3.2.10/init/sh
    conf_target=nco
elif [ $target = cray -o $target = wcoss_c ]; then
    . $MODULESHOME/init/sh
    conf_target=nco
elif [ $target = theia ]; then
    . /apps/lmod/lmod/init/sh
    conf_target=theia
else
    echo "unknown target = $target"
    exit 9
fi

# First build GSI
module purge
if [ $target = wcoss -o $target = cray ]; then
    module load $dir_root/scripts/modulefile.global_gsi.$target
else
    source $dir_root/scripts/modulefile.global_gsi.$target
fi
module list

cd $dir_root/src
./configure clean
./configure $conf_target
make -f Makefile clean
make -f Makefile -j 8
cp -p global_gsi $dir_root/exec
# Do not clean yet, EnKF requires GSI modules and objects
#make -f Makefile clean
#./configure clean

# Next build EnKF
module purge
if [ $target = wcoss -o $target = cray ]; then
    module load $dir_root/scripts/modulefile.gdas_enkf.$target
else
    source $dir_root/scripts/modulefile.gdas_enkf.$target
fi
module list

cd $dir_root/src/enkf
./configure clean
./configure $conf_target
make -f Makefile clean
make -f Makefile -j 8
cp -p global_enkf $dir_root/exec
make -f Makefile clean
./configure clean
# Now clean the GSI directory
cd ..
make -f Makefile clean
./configure clean

# Then build EnKF utilities
module purge
if [ $target = wcoss -o $target = cray ]; then
    module load $dir_root/scripts/modulefile.gdas_enkf.$target
else
    source $dir_root/scripts/modulefile.gdas_enkf.$target
fi
module list

dlist="adderrspec_nmcmeth_spec.fd getsfcensmeanp.fd getsigensstatp.fd getnstensmeanp.fd getsfcnstensupdp.fd getsigensmeanp_smooth_ncep.fd recentersigp.fd calc_increment_ens.fd gribmean.fd"

for dir in $dlist; do

    cd $dir_root/util/EnKF/gfs/src/$dir
    ./configure clean
    ./configure $conf_target
    make -f Makefile clean
    make -f Makefile
    cp -p *.x $dir_root/exec
    rm -f $dir_root/exec/log*.x
    make -f Makefile clean
    ./configure clean

done

exit
