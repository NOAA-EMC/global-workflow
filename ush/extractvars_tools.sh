#! /usr/bin/env bash

gen_parmlist() {
  infileg=$1
  requestedvar_in_file1=$2
  varlist=$3
  rm -vf "${requestedvar_in_file1}"
  while read -r vari; do
    varinfile=$(${WGRIB2} "${infileg}" | grep "${vari}") || true
    if [[ -n "${varinfile}" ]];then # if varinfile is not empty
      echo "${vari}" >> "${requestedvar_in_file1}"
    fi
  done <"${varlist}"
}

check_atmos() {
  requestedvar_in_file1=$1
  requestedvar_in_file2=$2
  requestedvar_in_allgrb2file=requestedvar_in_allgrb2file.txt
  rm -rvf "${requestedvar_in_allgrb2file}"
  cat "${requestedvar_in_file1}" "${requestedvar_in_file2}" >> "${requestedvar_in_allgrb2file}"
  mapfile -t requestedvar_in_allgrb2file_arr < "${requestedvar_in_allgrb2file}"
  while read -r vari; do
    if [[ ! ${requestedvar_in_allgrb2file_arr[*]} =~ ${vari} ]] ;then
      echo "WARNING: PARM VARIABLE (${vari}) is not available in pgrb and pgrb2b."
    fi
  done <"${varlist}"
}

daily_avg_atmos() {
  fnd=$(printf "%2.2d" "${dcnt}")
  davg_file=${outdirpre}/gefs.${cycle}.pgrb2.${outres}.24hr_avg.ldy${fnd}
  vcnt=1
  while read -r vari; do
    davgtmp=${subdata}/gefs.${cycle}.tmp.pgrb2.${outres}.ldy${fnd}.${vcnt}
    # shellcheck disable=SC2312
    ${WGRIB2} "${outfile}" | grep "${vari}" | ${WGRIB2} -i "${outfile}" -fcst_ave 6hr "${davgtmp}"
    # shellcheck disable=SC2312
    ${WGRIB2} "${davgtmp}" | ${WGRIB2} -i "${davgtmp}" -append -grib "${davg_file}"
    rm -f "${davgtmp}"
    vcnt=$(( vcnt + 1 ))
  done <"${varlist_d}" #variable
}
