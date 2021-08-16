#! /usr/bin/env bash
set -eux

source ./machine-setup.sh
cwd=`pwd`

if [ $target = wcoss_dell_p3 ]; then target=dell; fi
if [ $target = wcoss_cray ]; then target=cray; fi

cd ufs_utils.fd

./build_all.sh

exit

