SHELL=/bin/sh
set -x

#####################################################################################
# gsm using module compile standard
# 01/26/2016 Fanglin.Yang@noaa.gov:    Create module load version
# 07/10/2016 Fanglin.Yang@noaa.gov:    update for building on wcoss, theia and cray
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
version=v15.0.0

if [ $target = wcoss -o $target = cray ]; then
 module load ../modulefiles/gfs/gsm_${version}.$target
else
 source ../modulefiles/gfs/gsm_${version}.$target          
fi
module list


curdir=`pwd`
#cd ${curdir}/global_fcst.fd 
#makefile_${target}.sh                    

cd ${curdir}/global_cycle.fd
makefile.sh   

cd ${curdir}/global_sfchdr.fd
makefile.sh

cd ${curdir}/global_sighdr.fd
makefile.sh


