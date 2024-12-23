#! /usr/bin/env bash

# This script takes in a master grib file and creates products at various interpolated resolutions
# Generate 0.25 / 0.5 / 1 degree interpolated grib2 files for each input grib2 file
# trim's RH and tweaks sea-ice cover

source "${USHgfs}/preamble.sh"

input_file=${1:-"pgb2file_in"}  # Input pressure grib2 file
output_file_prefix=${2:-"pgb2file_out"}  # Prefix for output grib2 file; the prefix is appended by resolution e.g. _0p25
grid_string=${3:-"0p25"}  # Target grids; e.g. "0p25" or "0p25:0p50"; If multiple, they need to be ":" seperated

WGRIB2=${WGRIB2:-${wgrib2_ROOT}/bin/wgrib2}

# wgrib2 options for regridding
defaults="-set_grib_type same -set_bitmap 1 -set_grib_max_bits 16"
interp_winds="-new_grid_winds earth"
interp_bilinear="-new_grid_interpolation bilinear"
interp_neighbor="-if :(CSNOW|CRAIN|CFRZR|CICEP|ICSEV): -new_grid_interpolation neighbor -fi"
interp_budget="-if :(APCP|ACPCP|PRATE|CPRAT|DZDT): -new_grid_interpolation budget -fi"
increased_bits="-if :(APCP|ACPCP|PRATE|CPRAT): -set_grib_max_bits 25 -fi"

# interpolated target grids
# shellcheck disable=SC2034
grid0p25="latlon 0:1440:0.25 90:721:-0.25"
# shellcheck disable=SC2034
grid0p50="latlon 0:720:0.5 90:361:-0.5"
# shellcheck disable=SC2034
grid1p00="latlon 0:360:1.0 90:181:-1.0"

# "Import" functions used in this script
source "${USHgfs}/product_functions.sh"

# Transform the input ${grid_string} into an array for processing
IFS=':' read -ra grids <<< "${grid_string}"

output_grids=""
for grid in "${grids[@]}"; do
  gridopt="grid${grid}"
  output_grids="${output_grids} -new_grid ${!gridopt} ${output_file_prefix}_${grid}"
done

#shellcheck disable=SC2086
${WGRIB2} "${input_file}" ${defaults} \
                          ${interp_winds} \
                          ${interp_bilinear} \
                          ${interp_neighbor} \
                          ${interp_budget} \
                          ${increased_bits} \
                          ${output_grids}
export err=$?; err_chk

# trim and mask for all grids
for grid in "${grids[@]}"; do
  trim_rh "${output_file_prefix}_${grid}"; export err=$?; err_chk
  # shellcheck disable=SC2312
  var_count=$(${WGRIB2} "${output_file_prefix}_${grid}" -match "LAND|ICEC" |wc -l)
  if [[ "${var_count}" -eq 2 ]]; then
    mod_icec "${output_file_prefix}_${grid}"; export err=$?; err_chk
  fi
done

exit 0
