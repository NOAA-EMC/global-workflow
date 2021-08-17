#!/bin/bash
set -eux

[[ $(uname -s) == Darwin ]] && cmd=$(which greadlink) || cmd=$(which readlink)
readonly UTILS_DIR=$(cd "$(dirname "$($cmd -f -n "${BASH_SOURCE[0]}" )" )" && pwd -P)

# Adapt for global-workflow structure.
target=${target:-"NULL"}
modulefile=${UTILS_DIR}/../modulefiles/workflow_utils.$target
if [[ -f $modulefile ]]; then
  set +x
  source ${UTILS_DIR}/machine-setup.sh > /dev/null 2>&1
  source $modulefile
  module list
  set -x
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
