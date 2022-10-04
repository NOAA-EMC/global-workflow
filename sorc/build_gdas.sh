#! /usr/bin/env bash
set -eux

script_dir=$(dirname "${BASH_SOURCE[0]}")
cd "${script_dir}" || exit 1

# shellcheck disable=SC1091
source gfs_utils.fd/ush/machine-setup.sh > /dev/null 2>&1
# shellcheck disable=

export BUILD_TARGET="${target}"

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
BUILD_JOBS="${build_jobs}" ./build.sh -t "${BUILD_TARGET}"

exit

