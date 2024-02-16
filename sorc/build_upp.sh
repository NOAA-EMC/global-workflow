#! /usr/bin/env bash
set -eux

script_dir=$(dirname "${BASH_SOURCE[0]}")
cd "${script_dir}" || exit 1

OPTIND=1
_opts=""
while getopts ":dvj:" option; do
	case "${option}" in
		d) _opts+="-d ";;
		v) _opts+="-v ";;
    j) export BUILD_JOBS="${OPTARG}"; echo success ;;
		:)
			echo "[${BASH_SOURCE[0]}]: ${option} requires an argument"
			;;
		*)
			echo "[${BASH_SOURCE[0]}]: Unrecognized option: ${option}"
			;;
	esac
done
shift $((OPTIND-1))

# Check final exec folder exists
if [[ ! -d "../exec" ]]; then
  mkdir ../exec
fi

cd ufs_model.fd/FV3/upp/tests
# shellcheck disable=SC2086
BUILD_JOBS=${BUILD_JOBS:-8} ./compile_upp.sh ${_opts}
