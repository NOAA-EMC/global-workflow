#! /usr/bin/env bash
set -eux

source ./machine-setup.sh > /dev/null 2>&1
cwd=`pwd`

USE_PREINST_LIBS=${USE_PREINST_LIBS:-"true"}
if [ $USE_PREINST_LIBS = true ]; then
  export MOD_PATH=/scratch3/NCEPDEV/nwprod/lib/modulefiles
  source ../modulefiles/modulefile.grib_util.$target             > /dev/null 2>&1
else
  export MOD_PATH=${cwd}/lib/modulefiles
  if [ $target = wcoss_cray ]; then
    source ../modulefiles/modulefile.grib_util.${target}_userlib > /dev/null 2>&1
  else
    source ../modulefiles/modulefile.grib_util.$target           > /dev/null 2>&1
  fi
fi

# Move to util/sorc folder
cd ../util/sorc

# Check final exec folder exists
if [ ! -d "../exec" ]; then
  mkdir ../exec
fi

for grib_util in cnvgrib copygb2 degrib2 grbindex tocgrib2 tocgrib \
                 copygb grb2index grib2grib tocgrib2super
do
 cd $grib_util.fd
 make -f makefile_$target clean
 make -f makefile_$target
 make -f makefile_$target install
 make -f makefile_$target clean
 cd ..
done

#
# compile wgrib
#
cd wgrib.cd
 make -f makefile_$target clean
 make -f makefile_$target
 make -f makefile_$target install
 make -f makefile_$target clean
cd ..

#
# compile wgrib2
#
cd $cwd
source ./machine-setup.sh > /dev/null 2>&1

if [ $target = wcoss_cray -a $USE_PREINST_LIBS != true ]; then
  source ../modulefiles/modulefile.wgrib2.${target}_userlib > /dev/null 2>&1
else
  source ../modulefiles/modulefile.wgrib2.$target           > /dev/null 2>&1
fi

# Move to util/sorc folder
cd ../util/sorc
cwd=`pwd`

#----------------------------------------------------------------
export CPPFLAGS="-ffast-math -O3 -DGFORTRAN"
cd $cwd/wgrib2.cd/gctpc/
make -f makefile.gctpc clean
make -f makefile.gctpc
rm -f *.o
#----------------------------------------------------------------
if [ $target = wcoss_cray ]; then
  export FFLAGS=-O2
  cd $cwd/wgrib2.cd/iplib/
  make clean
  make
  rm -f *.o
fi
#----------------------------------------------------------------
cd $cwd/wgrib2.cd
module list
make -f makefile_$target clean
make -f makefile_$target
make -f makefile_$target install
make -f makefile_$target clean
#----------------------------------------------------------------

exit
