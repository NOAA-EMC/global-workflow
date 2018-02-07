SHELL=/bin/sh

#-------------------------------------------------------
# Builds bufrsnd codes found under the /sorc directory
# 11/2016 H Chuang: Generalized build script to meet NCO standard
#-------------------------------------------------------

set -x -e
pwd=`pwd`

mac=`hostname |cut -c1`
if [ $mac = g -o $mac = t ] ; then
export  machine=wcoss
elif [ $mac = l -o $mac = s ] ; then
export  machine=cray
fi

mod=$( cd ../modulefiles/ ; pwd -P )
source "$mod/module-setup.sh.inc"

#moduledir=`dirname $(readlink -f ../modulefiles)`
moduledir=$pwd/../modulefiles
module use ${moduledir}
module load gfs_bufr.${machine}
module list

# Compile codes under /sorc
compile1='gfs_bufr tocsbufr'

for comp in $compile1
do
  echo "Compiling ${comp}"
  cd $pwd/${comp}.fd
  make -f makefile_module clean
  make -f makefile_module 
done
