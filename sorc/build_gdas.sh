#! /usr/bin/env bash
set -eux

script_dir=$(dirname "${BASH_SOURCE[0]}")
cd "${script_dir}" || exit 1

# shellcheck disable=SC1091
source gfs_utils.fd/ush/detect_machine.sh
# shellcheck disable=

# detect_machine now includes the compiler on some machines
#   but the GDAS build script does not want it
MACHINE_ID=$(echo ${MACHINE_ID} | cut -d "." -f 1)

# use more build jobs if on NOAA HPC
build_jobs=4
case "${MACHINE_ID}" in
  hera|orion)
    build_jobs=10
    ;;
esac

# Check final exec folder exists
if [ ! -d "../exec" ]; then
  mkdir ../exec
fi

cd gdas.cd
BUILD_JOBS="${build_jobs}" ./build.sh -t "${MACHINE_ID}"

exit

