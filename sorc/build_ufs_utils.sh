#! /usr/bin/env bash
set -eux

source ./machine-setup.sh > /dev/null 2>&1
cwd=`pwd`

if [ $target=wcoss_dell_p3 ]; then target=dell; fi
if [ $target=wcoss_cray ]; then target=cray; fi

cd ufs_utils.fd/sorc

./build_all_ufs_utils.sh

./link_fixdirs.sh emc $target

exit

