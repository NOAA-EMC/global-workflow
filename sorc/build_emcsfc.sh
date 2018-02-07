#!/bin/sh

#------------------------------------------------------------
# Build all "emcsfc" programs.
# 
# For more details, see the documentation in each
# program sub-directory:
#
#  ./sorc/emcsfc_ice_blend.fd
#  ./sorc/emcsfc_snow2mdl.fd
#
# To run, type "build_emcsfc.sh" from the command line.
#------------------------------------------------------------

#set -x

for directory in emcsfc_ice_blend.fd emcsfc_snow2mdl.fd
do
  cd $directory
  make clean
  sh make.sh
  module list
  cd ..
done

echo; echo DONE BUILDING EMCSFC PROGRAMS
