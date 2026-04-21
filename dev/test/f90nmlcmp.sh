#! /usr/bin/env bash

# Compare two F90 namelists (forward and backward)

# shellcheck disable=SC2155,SC2312
HOMEglobal=$(cd "$(dirname "$(readlink -f -n "${BASH_SOURCE[0]}")")/../.." && pwd -P)
declare -rx HOMEglobal

source "${HOMEglobal}/dev/ush/load_modules.sh" run

file1=${1:?}
file2=${2:?}

"${HOMEglobal}/dev/ush/compare_f90nml.py" "${file1}" "${file2}"
echo " "
"${HOMEglobal}/dev/ush/compare_f90nml.py" "${file2}" "${file1}"
echo " "
