#! /usr/bin/env bash
set -eux

source ./machine-setup.sh > /dev/null 2>&1
cwd=`pwd`

cd ufs_utils.fd/sorc

./build_all_ufs_utils.sh

./link_fixdirs.sh emc $target

exit

