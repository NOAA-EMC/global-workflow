#! /usr/bin/env bash
set -eux

source ./machine-setup.sh > /dev/null 2>&1
cwd=$(pwd)

cd ufs_utils.fd

./build_all.sh

exit

