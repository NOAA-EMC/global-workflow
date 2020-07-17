#! /usr/bin/env bash
set -eux

source ./machine-setup.sh > /dev/null 2>&1
cwd=`pwd`

# Check final exec folder exists
if [ ! -d "../exec" ]; then
  mkdir ../exec
fi

cd gsd_prep_chem.fd/prep-chem/fv3-prep-chem/bin/build
make clean

#Set GSD-prep-chem target (gpctarget)
if [ $target = wcoss_cray ]; then
  gpctarget=wcoss-cray
elif [ $target = wcoss_dell_p3 ]; then
  gpctarget=wcoss-dell-p3
elif [ $target = wcoss ]; then
  gpctarget=wcoss12
else
  gpctarget=$target
fi

./mk-fv3-${gpctarget}

exit
