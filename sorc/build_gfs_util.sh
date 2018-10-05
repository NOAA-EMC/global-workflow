#! /usr/bin/env bash
set -eux

source ./machine-setup.sh > /dev/null 2>&1
export dir=` pwd `

cd ../util/sorc

# Check for gfs_util folders exist
if [ ! -d "./faxmakrx.fd" ]; then 
   echo " "
   echo "  GFS_UTIL folders DO NOT exist "
   echo "  "
   exit
fi 

echo ""
echo " Building ... Executables for GFS_UTILITIES "
echo ""

source ./compile_gfs_util_wcoss.sh
