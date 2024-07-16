#! /usr/bin/env bash

gen_parmlist() {
  infileg=$1
  requestedvar_in_file1=$2
  varlist=$3
  file_vars=$(${WGRIB2} "${infileg}")
  while read -r vari; do
    if [[ "${file_vars}" =~ ${vari} && -n "${vari}" ]]; then
      echo "${vari}" >> "${requestedvar_in_file1}"
    fi
  done <"${varlist}"
}

check_atmos() {
  requestedvar_in_file1=$1
  requestedvar_in_file2=$2
  varlistl=$3
  fnhl=$4
  requestedvar_in_allgrb2file="${subdata}/requestedvar_in_allgrb2file.txt"
  rm -rvf "${requestedvar_in_allgrb2file}"
  cat "${requestedvar_in_file1}" "${requestedvar_in_file2}" >> "${requestedvar_in_allgrb2file}"
  mapfile -t requestedvar_in_allgrb2file_arr < "${requestedvar_in_allgrb2file}"
  while read -r vari; do
    if [[ ! ${requestedvar_in_allgrb2file_arr[*]} =~ ${vari} ]] ;then
      echo "WARNING: PARM VARIABLE (${vari}) is not available in pgrb and pgrb2b for f${fnhl}."
    fi
  done <"${varlistl}"
}

daily_avg_atmos() {
  outfile_p=$1
  fnd=$(printf "%2.2d" "${dcnt}")
  davg_file=${outdirpre}/${RUN}.${cycle}.pgrb2.${outres}.24hr_avg.ldy${fnd}
  vcnt=1
  while read -r vari; do
    davgtmp=${subdata}/${RUN}.${cycle}.tmp.pgrb2.${outres}.ldy${fnd}.${vcnt}
    # shellcheck disable=SC2312
    ${WGRIB2} "${outfile_p}" | grep "${vari}" | ${WGRIB2} -i "${outfile_p}" -fcst_ave 6hr "${davgtmp}"
    # shellcheck disable=SC2312
    ${WGRIB2} "${davgtmp}" | ${WGRIB2} -i "${davgtmp}" -append -grib "${davg_file}"
    rm -f "${davgtmp}"
    vcnt=$(( vcnt + 1 ))
  done <"${varlist_d}" #variable
}

copy_to_comout() {
  rundir_outfile=$1 #output data file generated in RUNDIR
  comout_dir=$2 #destination directory to which to copy the data file
  if [[ -f "${rundir_outfile}" ]];then
    ${NCP} "${rundir_outfile}" "${comout_dir}"
  else
    echo "WARNING: Output variable (${rundir_outfile}) does not exist."
  fi
}

declare -xf gen_parmlist
declare -xf check_atmos
declare -xf daily_avg_atmos
declare -xf copy_to_comout
