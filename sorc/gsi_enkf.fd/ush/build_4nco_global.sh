#!/bin/bash
#
# This is a wrapper script for building the GFS version of GSI for NCO
# It sets the explicit values that are passed to the general build script

set -eu

# Get the root of the cloned GSI directory
readonly DIR_ROOT=$(cd "$(dirname "$(readlink -f -n "${BASH_SOURCE[0]}" )" )/.." && pwd -P)

# Set NCO explicit variables
export BUILD_TYPE="Release"
export BUILD_VERBOSE="YES"
export BUILD_DIR="$DIR_ROOT/build_4nco"
export INSTALL_PREFIX="$DIR_ROOT/install_4nco"

export GSI_MODE="GFS"
export ENKF_MODE="GFS"
export REGRESSION_TESTS="NO"

# Optionally set compiler flags
##export FFLAGS="-check all,noarg_temp_created"

# Prune the directory structure per NCO liking
if [[ "${PRUNE_4NCO:-}" =~ [yYtT] ]]; then
  $DIR_ROOT/ush/prune_4nco_global.sh prune
  rc=$?
  [[ $rc -ne 0 ]] && (echo "Error in prune_4nco_global.sh; ABORT!"; exit $rc)
fi

# Call the general build script from ush/build.sh
$DIR_ROOT/ush/build.sh
rc=$?
[[ $rc -ne 0 ]] && (echo "Error in build.sh; ABORT!"; exit $rc)

# NCO "installs" binaries in $DIR_ROOT/exec
[[ -d $DIR_ROOT/exec ]] && rm -rf $DIR_ROOT/exec
mv $INSTALL_PREFIX/bin $DIR_ROOT/exec

# Clean up build and install directories
rm -rf $BUILD_DIR $INSTALL_PREFIX

exit
