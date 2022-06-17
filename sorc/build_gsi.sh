#! /usr/bin/env bash
set -eux

source ./machine-setup.sh > /dev/null 2>&1
cwd=$(pwd)

gsitarget=$target
[[ "$target" == wcoss_cray ]] && gsitarget=cray

# Check final exec folder exists
if [ ! -d "../exec" ]; then
  mkdir ../exec
fi

cd gsi.fd/ush/
export BUILD_CLEAN=YES
./build.sh

exit

