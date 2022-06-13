#! /usr/bin/env bash
set -eux

source ./machine-setup.sh > /dev/null 2>&1
cwd=$(pwd)

BUILD_TYPE="Release"
DIR_ROOT="${cwd}/gsi_monitor.fd"
BUILD_DIR="${DIR_ROOT}/build"
INSTALL_PREFIX="${DIR_ROOT}/install"
GSI_INSTALL_PREFIX="${cwd}/gsi.fd/install"

OPTIND=1
while getopts ":dov" option; do
	case "${option}" in
		d) export BUILD_TYPE="DEBUG";;
		o) _ops="YES";;
		v) export BUILD_VERBOSE="YES";;
		\?)
			echo "[$BASH_SOURCE]: Unrecognized option: ${option}"
			usage
			;;
		:)
			echo "[$BASH_SOURCE]: ${option} requires an argument"
			usage
			;;
	esac
done
shift $((OPTIND-1))

# Load necessary modules
source machine-setup.sh > /dev/null 2>&1
module use ${cwd}/../modulefiles
modulefile=${cwd}/../modulefiles/gsi_monitor.${target}
if [[ -f ${modulefile}.lua ]]; then
  set +x
  module load gsi_monitor.$target
  module list
  set -x
else
  echo "FATAL: modulefile ${modulefile}.lua not found!"
  exit 1
fi

if [[ -d "${BUILD_DIR}" ]]; then
	rm -Rf "${BUILD_DIR}"
fi
mkdir -p "${BUILD_DIR}"
cd "${BUILD_DIR}"

CMAKE_OPTS=""

CMAKE_OPTS+="-DBUILD_UTIL_ALLMON=YES"

if [[ -d "${GSI_INSTALL_PREFIX}" ]]; then
	CMAKE_OPTS+=" -Dncdiag_ROOT=${GSI_INSTALL_PREFIX}"
else
	echo <<- EOF
		FATAL: ${GSI_INSTALL_PREFIX} does not exist
		  Have you built GSI yet?
	EOF
	exit 2
fi

# Collect BUILD Options
CMAKE_OPTS+=" -DCMAKE_BUILD_TYPE=${BUILD_TYPE}"

# Install destination for built executables, libraries, CMake Package config
CMAKE_OPTS+=" -DCMAKE_INSTALL_PREFIX=${INSTALL_PREFIX}"

set -x
cmake $CMAKE_OPTS $DIR_ROOT
make -j ${BUILD_JOBS:-8} VERBOSE=${BUILD_VERBOSE:-}
make install
set +x

# Clean up build directory
rm -Rf "${BUILD_DIR}"

exit
