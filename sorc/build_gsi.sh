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
export GSI_MODE="GFS"
export REGRESSION_TESTS="NO"
./build.sh "$cwd/gsi.fd"

exit

