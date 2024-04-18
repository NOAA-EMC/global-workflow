#! /usr/bin/env bash

# Compare two grib2 files with wgrib2
# The files must have the same fields in the same order

set -eu

# shellcheck disable=SC2155,SC2312
HOMEgfs=$(cd "$(dirname "$(readlink -f -n "${BASH_SOURCE[0]}" )" )/.." && pwd -P)
declare -rx HOMEgfs

source "${HOMEgfs}/ush/load_fv3gfs_modules.sh" 1>/dev/null 2>&1

file1=${1:?}
file2=${2:?}

wgrib2 ${file2} -var -lev -rpn "sto_1" -import_grib ${file1} -rpn "rcl_1:print_corr:print_rms" | egrep -v "rpn_corr=1"

