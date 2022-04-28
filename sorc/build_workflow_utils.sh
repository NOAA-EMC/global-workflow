#!/bin/bash
set -eux

[[ $(uname -s) == Darwin ]] && cmd=$(which greadlink) || cmd=$(which readlink)
readonly UTILS_DIR=$(cd "$(dirname "$($cmd -f -n "${BASH_SOURCE[0]}" )" )" && pwd -P)

# Adapt for global-workflow structure.
source ${UTILS_DIR}/machine-setup.sh > /dev/null 2>&1
module use ${UTILS_DIR}/../modulefiles
modulefile=${UTILS_DIR}/../modulefiles/workflow_utils.${target}
if [[ -f ${modulefile}.lua ]]; then
  set +x
  module load workflow_utils.$target
  module list
  set -x
else
  echo "FATAL: modulefile ${modulefile}.lua not found!"
  exit 1
fi
# End adaptation

BUILD_DIR=${BUILD_DIR:-${UTILS_DIR}/build}
[[ -d $BUILD_DIR ]] && rm -rf $BUILD_DIR
mkdir -p ${BUILD_DIR}
cd $BUILD_DIR

INSTALL_DIR=${INSTALL_DIR:-${UTILS_DIR}/install}

CMAKE_FLAGS+=" -DCMAKE_INSTALL_PREFIX=$INSTALL_DIR"

cmake ${UTILS_DIR} ${CMAKE_FLAGS}
make -j ${BUILD_JOBS:-4} VERBOSE=${BUILD_VERBOSE:-}
make install
