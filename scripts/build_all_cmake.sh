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
dir_modules=$dir_root/modulefiles
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

rm -rf $dir_root/build
mkdir -p $dir_root/build
cd $dir_root/build

module purge
if [ $target = wcoss -o $target = cray ]; then
    module load $dir_modules/modulefile.ProdGSI.$target
else
    source $dir_modules/modulefile.ProdGSI.$target
fi
module list

cmake -DBUILD_UTIL=ON ..

make -j 6

exit
