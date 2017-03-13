SHELL=/bin/sh

####################################################################################################
#
# tocsbufr and gfs_flux using module compile standard
#
#####################################################################################################

export  curdir=`pwd`

. /apps/lmod/lmod/init/sh
module purge
source ../modulefiles/gfs_bufr.theia

cd gfs_bufr.fd
make -f makefile_theia_module clean
make -f makefile_theia_module

for dir in tocsbufr gfs_flux
do
   cd $curdir/$dir.fd
   makefile.sh
done
