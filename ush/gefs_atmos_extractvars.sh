#! /usr/bin/env bash
################################################################################
#   Script:    
#
source "${USHgfs}/preamble.sh"
source "${USHgfs}/extractvars_tools.sh"
fcnt=1
dcnt=1 
subdata=${1}

cd "${subdata}" || true

for outtype in "f2d" "f3d"; do

  if [[ "${outtype}" == "f2d" ]];then
    varlist=${varlist_2d}
  elif [[ "${outtype}" == "f3d" ]];then 
    varlist=${varlist_3d}
    varlist_d=${varlist_3d_d}
  fi

  outdirpre="${subdata}/${outtype}"
  if [[ ! -d "${outdirpre}" ]]; then mkdir -p "${outdirpre}"; fi 

  nh=${FHMIN}
  while [[ ${nh} -le ${FHMAX} ]];do
    fnh=$(printf "%3.3d" "${nh}")

    if [[ "${outtype}" == "f2d" ]];then
      if [[ ${nh} -le ${FHMAXHF} ]];then
        outres="0p25"
      else
        outres="0p50"
      fi
    elif [[ "${outtype}" == "f3d" ]];then
      outres="1p00"
    fi

    if [[ ${nh} -lt ${FHMAXHF} ]];then
      outfreq=${FHOUTHF}
    else
      outfreq=${FHOUTLF}
    fi                                      

    com_var="COMIN_ATMOS_GRIB_${outres}"
    infile1="${!com_var}/gefs.${cycle}.pgrb2.${outres}.f${fnh}"
    infile2="${!com_var}/gefs.${cycle}.pgrb2b.${outres}.f${fnh}"
    oufile=${outdirpre}/gefs.${cycle}.pgrb2.${outres}.f${fnh}
    rm -f "${oufile}" #remove outfile if it already exists before extraction
    requestedvars1="partial_parm1.txt"
    requestedvars2="partial_parm2.txt"
    rm -f "${requestedvars1}"
    rm -f "${requestedvars2}"
    if [[ -f "${infile1}" ]]; then #check if input file exists before extraction
      gen_parmlist "${infile1}" "${requestedvars1}" "${varlist}"
      # shellcheck disable=SC2312
      ${WGRIB2} "${infile1}" | grep -F -f "${requestedvars1}" | ${WGRIB2} -i "${infile1}" -append -grib "${oufile}">/dev/null
    else
      echo "WARNING: ${infile1} does not exist."
    fi 

    if [[ -f "${infile2}" ]]; then #check if input file exists before extraction
      gen_parmlist "${infile2}" "${requestedvars2}" "${varlist}"
      # shellcheck disable=SC2312
      ${WGRIB2} "${infile2}" | grep -F -f "${requestedvars2}" | ${WGRIB2} -i "${infile2}" -append -grib "${oufile}">/dev/null
    else
      echo "WARNING: ${infile2} does not exist."
    fi

    if [[ ! -f "${requestedvars1}" ]]; then touch "${requestedvars1}"; fi
    if [[ ! -f "${requestedvars2}" ]]; then touch "${requestedvars2}"; fi
    check_atmos "${requestedvars1}" "${requestedvars2}"

    #Compute daily average for a subset of variables
    if (( nh % 6 == 0 )) && (( nh != 0 )) && [[ "${outtype}" == "f3d" ]];then
      outfile=${subdata}/vartmp_raw_vari_ldy${dcnt}.grib2
      # shellcheck disable=SC2312
      ${WGRIB2} "${infile1}" | grep -F -f "${varlist_d}" | ${WGRIB2} -i "${infile1}" -append -grib "${outfile}"
      # shellcheck disable=SC2312
      ${WGRIB2} "${infile2}" | grep -F -f "${varlist_d}" | ${WGRIB2} -i "${infile2}" -append -grib "${outfile}"
      if [[ ${fcnt} -eq 4 ]];then
        daily_avg_atmos
        fcnt=1
        dcnt=$(( dcnt + 1 ))
      else
        fcnt=$(( fcnt + 1 ))
      fi #If at final lead hour of a given day
    fi #if lead hour is divisible by 6 and outtype is f3d

    nh=$(( nh + outfreq ))
  done #fhr

done #f2d,f3d

exit 0
