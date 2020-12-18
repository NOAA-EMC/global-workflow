#!/bin/sh

set -x

mac=$(hostname -f)

case $mac in

#---------------------------------------------------------------------------------
# BUILD PROGRAM ON WCOSS Phase 1/2.
#---------------------------------------------------------------------------------

g????.ncep.noaa.gov | t????.ncep.noaa.gov)

  echo "WCOSS PHASE 1/2 BUILD NOT ADDED YET"
  exit 1 ;;

#---------------------------------------------------------------------------------
# BUILD PROGRAM ON WCOSS CRAY.
#---------------------------------------------------------------------------------

llogin? | slogin?)

  echo "WCOSS CRAY BUILD NOT ADDED YET"
  exit 1 ;;

#---------------------------------------------------------------------------------
# BUILD PROGRAM ON HERA.
#---------------------------------------------------------------------------------

hfe??)

  source /apps/lmod/lmod/init/sh
  module purge

  module load intel/18.0.5.274

  export FCOMP=ifort
  export FFLAGS="-O0 -g -traceback -r8 -i4 -convert big_endian -check bounds"

  module load netcdf/4.7.0
  module load hdf5/1.10.5
  export NETCDF_INCLUDE="-I${NETCDF}/include"
  export NETCDF_LDFLAGS_F="-L${NETCDF}/lib -lnetcdf -lnetcdff -L${HDF5}/lib -lhdf5 -lhdf5_fortran"
  
  module use -a /scratch2/NCEPDEV/nwprod/NCEPLIBS/modulefiles
  module load sp/2.0.2

  make clean
  make
  rc=$?  ;;

*)

  echo "DOES NOT BUILD ON THIS MACHINE."
  exit 1 ;;

esac

exit
