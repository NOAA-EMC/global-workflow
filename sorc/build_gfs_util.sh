#! /usr/bin/env bash
set -eux

source ./machine-setup.sh > /dev/null 2>&1
export dir=` pwd `

cd ../util/sorc

# Check for gfs_util folders exist
if [ ! -d "./mkgfsawps.fd" ]; then 
   echo " "
   echo "  GFS_UTIL folders DO NOT exist "
   echo "  "
   exit
fi 

if [ $target = jet ]; then
   echo "  GFS_UTIL does not support JET "
   echo "  "
   exit
fi

echo ""
echo " Building ... Executables for GFS_UTILITIES "
echo ""

source ./compile_gfs_util_wcoss.sh
