#! /usr/bin/env bash

# Compare two F90 namelists (forward and backward)

set -eu

# shellcheck disable=SC2155,SC2312
HOMEgfs=$(cd "$(dirname "$(readlink -f -n "${BASH_SOURCE[0]}" )" )/.." && pwd -P)
declare -rx HOMEgfs

source "${HOMEgfs}/ush/load_fv3gfs_modules.sh" 1>/dev/null 2>&1

file1=${1:?}
file2=${2:?}

"${HOMEgfs}/ush/compare_f90nml.py" "${file1}" "${file2}"
echo " "
"${HOMEgfs}/ush/compare_f90nml.py" "${file2}" "${file1}"
echo " "
