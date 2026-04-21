#! /usr/bin/env bash

# shellcheck disable=SC2155,SC2312
HOMEglobal=$(cd "$(dirname "$(readlink -f -n "${BASH_SOURCE[0]}")")/../.." && pwd -P)
declare -rx HOMEglobal

source "${HOMEglobal}/dev/ush/load_modules.sh" run
module load "nccmp/${nccmp_ver:-"1.9.0.1"}"

file1=${1:?}
file2=${2:?}

nccmp -d -S -f -B --warn=format "${file1}" "${file2}"
