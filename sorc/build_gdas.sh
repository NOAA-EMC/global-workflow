#! /usr/bin/env bash
set -eux

script_dir=$(dirname "${BASH_SOURCE[0]}")
cd "${script_dir}" || exit 1

export COMPILER="intel"
source gfs_utils.fd/ush/detect_machine.sh  # TODO: this script should not source something from another repo
set +x
source gfs_utils.fd/ush/module-setup.sh
set -x

# detect_machine now includes the compiler on some machines
#   but the GDAS build script does not want it
TARGET=$(echo "${MACHINE_ID}" | cut -d "." -f 1)

case "${TARGET}" in
    hera|orion) build_jobs=10 ;;
    *)          build_jobs=4 ;;
esac

# Check final exec folder exists
if [[ ! -d "../exec" ]]; then
  mkdir ../exec
fi

cd gdas.cd

WORKFLOW_BUILD="ON" BUILD_JOBS="${build_jobs}" ./build.sh -t "${TARGET}"

exit $?

