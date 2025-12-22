#! /usr/bin/env bash

# Compare two F90 namelists (forward and backward)

HOMEgfs=$(cd "$(dirname "${BASH_SOURCE[0]}")" && git rev-parse --show-toplevel)
declare -rx HOMEgfs

source "${HOMEgfs}/dev/ush/load_modules.sh" run

file1=${1:?}
file2=${2:?}

"${HOMEgfs}/dev/ush/compare_f90nml.py" "${file1}" "${file2}"
echo " "
"${HOMEgfs}/dev/ush/compare_f90nml.py" "${file2}" "${file1}"
echo " "
