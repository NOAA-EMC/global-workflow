#! /usr/bin/env bash

# Compare two grib2 files with wgrib2
# The files must have the same fields in the same order

HOMEglobal=$(cd "$(dirname "$(readlink -f -n "${BASH_SOURCE[0]}")")" && git rev-parse --show-toplevel)
declare -rx HOMEglobal

file1=${1:?}
file2=${2:?}

source "${HOMEglobal}/dev/ush/load_modules.sh" run

# Use wgrib2 to compute correlations and print any record that does not have corr=1 for mismatch
wgrib2 "${file2}" -var -lev -rpn "sto_1" -import_grib "${file1}" -rpn "rcl_1:print_corr:print_rms" | grep -v "rpn_corr=1"
