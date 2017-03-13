SHELL=/bin/sh
set -x

##################################################################
# wafs using module compile standard
# 02/01/2016 yali.ma@noaa.gov:    Create module load version
##################################################################

module purge
module load ../modulefiles/modulefile.gfs_smartinit.cray

 curdir=`pwd`

for dir in smartinit.fd smartprecip.fd; do
 cd ${curdir}/$dir
 make clean
 make
 make clean
done


