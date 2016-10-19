SHELL=/bin/sh
set -x

#####################################################################################
# orog using module compile standard
# 10/10/2016 Fanglin.Yang@noaa.gov:    Create module load version
#####################################################################################
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
elif [ $target = cray ]; then
. $MODULESHOME/init/sh
elif [ $target = theia ]; then
. /apps/lmod/lmod/init/sh
else
 exit
fi

module purge
if [ $target = wcoss -o $target = cray ]; then
 module load ./modulefiles/orog.$target
else
 source ./modulefiles/orog.$target          
fi
module list
curdir=`pwd`


cd ${curdir}/orog.fd                       
makefile.sh



