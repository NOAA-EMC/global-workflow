#! /usr/bin/env bash

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

function scale_dec() {
	local filename=$1
	${WGRIB2} "${filename}" -not_if ':(TMP|PWAT|WEASD):' -grib "${filename}.new" \
        -if ':(TMP|PWAT):' -set_grib_type same \
        -set_scaling -1 0 -grib_out "${filename}.new" \
        -if ':(WEASD):' -set_grib_type same \
        -set_scaling 0 0 -grib_out "${filename}.new"
  rc=$?
  if (( rc == 0 )); then mv "${filename}.new" "${filename}"; fi
  return "${rc}"
}
