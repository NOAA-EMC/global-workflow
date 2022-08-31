#! /usr/bin/env bash
set -eux

script_dir=$(dirname ${BASH_SOURCE[0]})
cd ${script_dir}

OPTIND=1
_opts=""
while getopts ":dov" option; do
	case "${option}" in
		d) export BUILD_TYPE="DEBUG";;
		o) _opts+="-g ";;
		v) _opts+="-v ";;
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

cd ufs_model.fd/FV3/upp/tests
./compile_upp.sh ${_opts}
