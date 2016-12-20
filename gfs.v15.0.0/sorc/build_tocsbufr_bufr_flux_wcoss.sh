SHELL=/bin/sh

####################################################################################################
#
# tocsbufr and gfs_flux using module compile standard
#
#####################################################################################################

export  curdir=`pwd`

module purge

module load ../modulefiles/gfs_bufr.wcoss

cd gfs_bufr.fd
make -f makefile_wcoss_module clean
make -f makefile_wcoss_module

for dir in tocsbufr gfs_flux
do
   cd $curdir/$dir.fd
   makefile.sh
done
