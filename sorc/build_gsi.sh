#! /usr/bin/env bash
set -eux

source ./machine-setup.sh > /dev/null 2>&1
cwd=$(pwd)

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

# Check final exec folder exists
if [ ! -d "../exec" ]; then
  mkdir ../exec
fi

cd gsi.fd/ush/
# EFSOI block - see if this is necessary
##./build_all_cmake.sh "PRODUCTION" "$cwd/gsi.fd"
#./build.sh "Release" "$cwd/gsi.fd"
##./build_all_cmake.sh "PRODUCTION" "$cwd/gsi.fd" "NCO"  # use this line for pruned NCO install
#=======
export GSI_MODE="GFS"
export REGRESSION_TESTS="NO"
# Temporary setting until GSI default is updated or workflow adds versions file
export ncio_ver="1.1.2"
export BUILD_CLEAN="YES"
./build.sh "$cwd/gsi.fd"

exit

