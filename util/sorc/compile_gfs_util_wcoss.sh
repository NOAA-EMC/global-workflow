#!/bin/sh

######################################################################
#
# Build executable GFS utility for GFS V15.0.0 
#
######################################################################

LMOD_EXACT_MATCH=no
module load ips/18.0.1.163
module load prod_util/1.1.0
machine=$(getsystem.pl -t)
ver=1.0.8

if [ "$machine" = "IBM" ] || [ "$machine" = "Cray" ] || [ "$machine" = "Dell" ]; then
   echo " "
   echo " You are on WCOSS:  $(getsystem.pl -p)"
else
   echo " "
   echo " Your machine is $machine is not recognized as a WCOSS machine."
   echo " The script $0 can not continue.  Aborting!"
   echo " "
   exit
fi
echo " "

machine_lc=${machine,,} # Get lower case

# Load required modules
module use ../modulefiles
module load build_util_shared/$machine_lc/$ver
module list 
 sleep 5

dirlist="faxmakr faxmakrx fxcompoz gendata mkgfsawps overgridid plotvpap
         ras2bit ras2bity rdbfmsua redsat rsonde rsondplt sixbitb sixbitb2 
         trpanl trpsfcmv upaprep webtitle wndanftf"
set -x

for dir in $dirlist
do
  if [ $dir = ascii2shp ] ; then
     cd $dir.cd
  else
     cd $dir.fd
  fi
  set +x
  echo " "
  echo " ### ${dir} ### "
  echo " "
  set -x
  mkdir -p ../../exec
  make
  mv ${dir} ../../exec
  make clean
  set +x
  echo " "
  echo " ######################################### "
  echo " "
  cd ..
done
