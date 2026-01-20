#! /usr/bin/env bash

HOMEgfs=$(cd "$(dirname "$(readlink -f -n "${BASH_SOURCE[0]}")")" && git rev-parse --show-toplevel)
declare -rx HOMEgfs

source "${HOMEgfs}/dev/ush/load_modules.sh" run
set +eu
module load "nccmp/${nccmp_ver:-"1.9.0.1"}"

file1=${1:?}
file2=${2:?}

nccmp -d -S -f -B --warn=format "${file1}" "${file2}"
echo $?
