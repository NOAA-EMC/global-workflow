#!/bin/sh --login

#---------------------------------------------------------------------------------
#  The driver script for compiling the emcsfc_ice_blend program.  Loads
#  module files and exports environment variables required by the makefile
#  Then, invokes the makefile.  
#
#  Only tested on Theia, NCEP WCOSS Phase 1/2 and WCOSS-Cray machines.
#
#  To invoke: type 'make.sh' from the command line.  If successfully built, 
#  the executable will be installed the ../../exec subdirectory.
#
#  See the README.build file for more details.
#---------------------------------------------------------------------------------

#set -x

mac=$(hostname -f)

case $mac in
 
#---------------------------------------------------------------------------------
# COMPILE ON WCOSS PHASE 1 AND PHASE 2.
#---------------------------------------------------------------------------------

g????.ncep.noaa.gov | t????.ncep.noaa.gov) 

  echo
  echo "BUILD EMCSFC_ICE_BLEND PROGRAM ON WCOSS PHASE 1 AND PHASE2."
  echo

  module purge
  module load ../../modulefiles/modulefile.global_emcsfc_ice_blend.wcoss

  make clean
  make all
  rc=$? ;;

#---------------------------------------------------------------------------------
# BUILD PROGRAM ON WCOSS CRAY.
#---------------------------------------------------------------------------------

llogin? | slogin?)

  echo
  echo "BUILD EMCSFC_ICE_BLEND PROGRAM ON WCOSS-CRAY."
  echo

  module purge
  module load modules/3.2.6.7
  module load ../../modulefiles/modulefile.global_emcsfc_ice_blend.cray

  make clean
  make
  rc=$?  ;;

#---------------------------------------------------------------------------------
# COMPILE ON THEIA MACHINE
#---------------------------------------------------------------------------------

tfe??) 

  echo
  echo "BUILD EMCSFC_ICE_BLEND PROGRAM ON THEIA."
  echo

  module purge

# load intel compiler

  module load intel/14.0.2
  export FCOMP=ifort
  export FFLAGS="-O0 -i4"

# load ncep library modules

  module use -a /scratch3/NCEPDEV/nwprod/lib/modulefiles
  module load w3nco/v2.0.6
  module load bacio/v2.0.1
  module load jasper
  module load z
  module load png
  module load g2/v2.5.0

  make clean
  make
  rc=$? ;;

#---------------------------------------------------------------------------------
# UNKNOWN MACHINE
#---------------------------------------------------------------------------------

*) 

  echo "MACHINE OPTION NOT FOUND. EXIT."
  exit ;;

esac

#---------------------------------------------------------------------------------
# INSTALL EXECUTABLE
#---------------------------------------------------------------------------------

if ((rc != 0));then
  echo "BUILD FAILED. EXIT."
  exit
else
  make install
fi

exit
