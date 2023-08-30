#! /usr/bin/env bash

# This script takes in a master grib file and creates products at various interpolated resolutions
# Generate 0.25 / 0.5 / 1 degree interpolated grib2 files for each input grib2 file
# Also generate 1 degree grib1 file fox XYZ TODO: who?
# trim's RH and tweaks sea-ice cover


source "${HOMEgfs}/ush/preamble.sh"

input_file=${1:-"pgb2file_in"}  # Input pressure grib2 file
output_file_prefix=${2:-"pgb2file_out"}  # Prefix for output grib2 file; the prefix is appended by resolution e.g. _0p25

WGRIB2=${WGRIB2:-${wgrib2_ROOT}/bin/wgrib2}
PGBS=${PGBS:-"NO"}  # Supplemental 1/2-degree and 1-degree products

# wgrib2 options for regridding
defaults="-set_grib_type same -set_bitmap 1 -set_grib_max_bits 16"
interp_winds="-new_grid_winds earth"
interp_bilinear="-new_grid_interpolation bilinear"
interp_neighbor="-if :(CSNOW|CRAIN|CFRZR|CICEP|ICSEV): -new_grid_interpolation neighbor -fi"
interp_budget="-if :(APCP|ACPCP|PRATE|CPRAT|DZDT): -new_grid_interpolation budget -fi"
max_bits="-if :(APCP|ACPCP|PRATE|CPRAT): -set_grib_max_bits 25 -fi"

# interpolated target grids
# shellcheck disable=SC2034
grid0p25="latlon 0:1440:0.25 90:721:-0.25"
# shellcheck disable=SC2034
grid0p50="latlon 0:720:0.5 90:361:-0.5"
# shellcheck disable=SC2034
grid1p00="latlon 0:360:1.0 90:181:-1.0"

# Functions used in this script
function trim_rh() {
  # trim RH values larger than 100.
  local filename=$1
  ${WGRIB2} "${filename}" \
        -not_if ':RH:' -grib "${filename}.new" \
        -if ':RH:' -rpn "10:*:0.5:+:floor:1000:min:10:/" -set_grib_type same \
        -set_scaling -1 0 -grib_out "${filename}.new"
  rc=$?
  if (( rc == 0 )); then mv "${filename}.new" "${filename}"; fi
  return "${rc}"
}

function mod_icec() {
  # modify icec based on land-sea mask
  local filename=$1
  ${WGRIB2} "${filename}" \
          -if 'LAND' -rpn 'sto_1' -fi \
          -if 'ICEC' -rpn 'rcl_1:0:==:*' -fi \
          -set_grib_type same \
          -set_scaling same same \
          -grib_out "${filename}.new"
  rc=$?
  if (( rc == 0 )); then mv "${filename}.new" "${filename}"; fi
  return "${rc}"
}

# Determine grids once
grids=("0p25")
if [[ "${PGBS}" = "YES" ]]; then
  grids+=("0p50")
  grids+=("1p00")
fi

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
                          ${max_bits} \
                          ${output_grids}
export err=$?; err_chk

# trim and mask for all grids
for grid in "${grids[@]}"; do
  trim_rh "${output_file_prefix}_${grid}"; export err=$?; err_chk
  mod_icec "${output_file_prefix}_${grid}"; export err=$?; err_chk
done

exit 0
