#!/bin/tcsh


if (`hostname | cut -c1-4` == "slog" || `hostname | cut -c1-4` == "llog" ) then
   echo " WCOSS Cray environment "
   if (${1} != 'fast') sleep 4
   sh $MODULESHOME/init/sh
   echo " module purge then load modules "
   module  purge
   module load PrgEnv-intel
   module rm intel
   module load intel/16.3.210   
   module load craype-haswell
   module load cray-netcdf

   # Load NCEPLIBS modules
   #module load sigio-intel/2.0.1
   #module load w3nco-intel/2.0.6
   #module load w3emc-intel/2.2.0
   #module load ip-intel/3.0.0
   #module load sfcio-intel/1.0.0
   #module load gfsio-intel/1.1.0
   #module load landsfcutil-intel/2.1.0
   #module use /usrx/local/nceplibs/modulefiles
   #module load bacio-intel/2.0.2

   setenv NETCDF_DIR /opt/cray/netcdf/4.3.2/INTEL/140
   setenv TEMPLATE ../../site/intel.mk
   exit
endif

if (`hostname | cut -c1-1` == "g" || `hostname | cut -c1-1` == "t" ) then
   echo " WCOSS Tide-Gyre environment "
   if (${1} != 'fast') sleep 4
   module purge
   module load ics/14.0.1
   module use /usrx/local/modulefiles
   module load NetCDF/4.2/serial  

   # Load NCEPLIBS modules
   #module use /nwprod2/lib/modulefiles           
   #module load sigio/2.0.1
   #module load w3nco/2.0.6
   #module load w3emc/2.2.0
   #module load ip/3.0.0
   #module load sfcio/1.0.0
   #module load gfsio/1.1.0
   #module load landsfcutil/2.1.0
   #module load bacio/v2.0.1

   setenv NETCDF_DIR /usrx/local/NetCDF/4.2/serial   
   setenv TEMPLATE ../../site/intel.mk_wcoss
   exit
endif

if (`hostname | cut -c1-4` == "gaea" ) then
   echo " gaea environment "
   if (${1} != 'fast') sleep 4
   module unload PrgEnv-pgi
   module load   PrgEnv-intel
   module rm intel
#   module load intel/16.0.0.109
   module load intel/16.0.3.210
   module load cray-netcdf
   module load craype-hugepages4M
   setenv TEMPLATE ../../site/intel.mk
   exit
endif

if (`hostname | cut -c1-2` == "fe" || `hostname | cut -c1` == "x") then
   echo " jet environment "
   if (${1} != 'fast') sleep 4
   source ${MODULESHOME}/init/csh
   module purge
   module load newdefaults
   module load intel/2016.2.181 # Jet's default is 15.0.3.187, but this one is 16.0.2.181
   module load szip/2.1
   module load hdf5/1.8.9
   module load netcdf4/4.2.1.1
   module load mvapich2/2.1
   setenv LIBRARY_PATH ${LIBRARY_PATH}:${NETCDF4}/lib:${HDF5}/lib
   setenv NETCDF_DIR ${NETCDF4}
   setenv TEMPLATE ../../site/intel.mk.mv2.Jet
   exit
endif

if (`hostname | cut -c1-4` == "cori") then
   echo " cori nvironment "
   if (${1} != 'fast') sleep 4
   module unload PrgEnv-pgi
   module load   PrgEnv-intel
   module rm intel
   module load intel/16.0.3.210
   module load cray-netcdf
   module load craype-hugepages4M
   setenv TEMPLATE ../../site/intel.mk
   exit
endif

if (`hostname | cut -c1` == "t" ) then
   echo " theia environment "
   if (${1} != 'fast') sleep 4
   source $MODULESHOME/init/csh
   module load intel/15.1.133
   module load netcdf/4.3.0
   module load hdf5/1.8.14
   setenv LIBRARY_PATH ${LIBRARY_PATH}:${NETCDF}/lib:${HDF5}/lib
   setenv NETCDF_DIR $NETCDF
   setenv TEMPLATE ../../site/intel.mk.mv2
   exit
endif

echo " no environment available based on the hostname "
