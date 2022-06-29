#! /usr/bin/env bash
set -eux

source ./machine-setup.sh > /dev/null 2>&1
cwd=$(pwd)

export BUILD_TARGET=$target

# use more build jobs if on NOAA HPC
build_jobs=4
case "${target}" in
  hera|orion)
    build_jobs=10
    ;;
esac

# Check final exec folder exists
if [ ! -d "../exec" ]; then
  mkdir ../exec
fi

cd gdas.cd
BUILD_JOBS=$build_jobs ./build.sh -t $BUILD_TARGET

exit

