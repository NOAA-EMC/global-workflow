#! /usr/bin/env bash

set -eu

# shellcheck disable=SC2155,SC2312
HOMEgfs=$(cd "$(dirname "$(readlink -f -n "${BASH_SOURCE[0]}" )" )/.." && pwd -P)
declare -rx HOMEgfs

source "${HOMEgfs}/ush/load_fv3gfs_modules.sh" 1>/dev/null 2>&1
module load "nccmp/${nccmp_ver:-"1.9.0.1"}"

file1=${1:?}
file2=${2:?}

nccmp -d -S -f -B --warn=format "${file1}" "${file2}"
