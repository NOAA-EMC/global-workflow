#! /usr/bin/env bash
set -eux

script_dir=$(dirname "${BASH_SOURCE[0]}")
cd "${script_dir}" || exit 1

source gfs_utils.fd/ush/detect_machine.sh
source gfs_utils.fd/ush/module-setup.sh

# detect_machine now includes the compiler on some machines
#   but the GDAS build script does not want it
MACHINE_ID=$(echo "${MACHINE_ID}" | cut -d "." -f 1)

case "${MACHINE_ID}" in
    hera|orion) build_jobs=10 ;;
    *)          build_jobs=4 ;;
esac

# Check final exec folder exists
if [[ ! -d "../exec" ]]; then
  mkdir ../exec
fi

cd gdas.cd

WORKFLOW_BUILD="ON" BUILD_JOBS="${build_jobs}" ./build.sh -t "${MACHINE_ID}"

exit $?

