#! /usr/bin/env bash
set -eux

source ./machine-setup.sh > /dev/null 2>&1
cwd=`pwd`

# Check final exec folder exists
if [ ! -d "../exec" ]; then
  mkdir ../exec
fi

if [ $target = wcoss_dell_p3 ]; then 
 cd gfs_wafs.fd/sorc
 sh build_wafs_dell.sh
elif [ $target = hera ]; then
 cd gfs_wafs.fd/sorc
 sh build_wafs_hera.sh
else
 echo "This version of wafs only works on Dell and Hera. exit"
fi

exit
