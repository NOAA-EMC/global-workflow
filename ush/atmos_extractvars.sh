#! /usr/bin/env bash

################################################################################
## UNIX Script Documentation Block
## Script name:         atmos_extractvars.sh
## Script description:  Extracts and calculates 24-hr averages of variables
##                      from atmosphere products and saves these variables in arcdir
#######################
# Main body starts here
#######################

source "${USHgfs}/preamble.sh"

fcnt=1 # 1 is 1st quarter, 2 is 2nd quarter and 3 is 3rd quarter of the day
dcnt=1 # lead day
subdata=${1}

[[ -d "${subdata}" ]] || mkdir -p "${subdata}"

for outtype in "f2d" "f3d"; do

  if [[ "${outtype}" == "f2d" ]]; then
    varlist=${varlist_2d}
    ARC_RFCST_PROD_ATMOS="${ARC_RFCST_PROD_ATMOS_F2D}"
  elif [[ "${outtype}" == "f3d" ]]; then
    varlist=${varlist_3d}
    varlist_d=${varlist_3d_d}
    ARC_RFCST_PROD_ATMOS="${ARC_RFCST_PROD_ATMOS_F3D}"
  fi

  outdirpre="${subdata}/${outtype}"
  [[ -d "${outdirpre}" ]] || mkdir -p "${outdirpre}"

  nh=${FHMIN}
  while (( nh <= FHMAX_GFS )); do
    fnh=$(printf "%3.3d" "${nh}")

    if [[ "${outtype}" == "f2d" ]]; then
      if (( nh < FHMAX_HF_GFS )); then
        outres="0p25"
      else
        outres="0p50"
      fi
    elif [[ "${outtype}" == "f3d" ]]; then
      outres="1p00"
    fi

    if (( nh <= FHMAX_HF_GFS )); then
      outfreq=${FHOUT_HF_GFS}
    else
      outfreq=${FHOUT_GFS}
    fi                                      

    com_var="COMIN_ATMOS_GRIB_${outres}"
    infile1="${!com_var}/${RUN}.t${cyc}z.pgrb2.${outres}.f${fnh}"
    infile2="${!com_var}/${RUN}.t${cyc}z.pgrb2b.${outres}.f${fnh}"
    outfile="${outdirpre}/${RUN}.t${cyc}z.pgrb2.${outres}.f${fnh}"
    rm -f "${outfile}" #remove outfile if it already exists before extraction

    for infile in "${infile1}" "${infile2}"; do
      if [[ -f "${infile}" ]]; then # check if input file exists before extraction
        # shellcheck disable=SC2312
        ${WGRIB2} "${infile}" | grep -F -f "${varlist}" | ${WGRIB2} -i "${infile}" -append -grib "${outfile}"
      else
        echo "WARNING: ${infile} does not exist."
      fi
    done

    check_atmos "${infile1}" "${infile2}" "${varlist}" "${fnh}"
    copy_to_comout "${outfile}" "${ARC_RFCST_PROD_ATMOS}"

    # Compute daily average for a subset of variables
    if (( nh % 6 == 0 )) && (( nh != 0 )) && [[ "${outtype}" == "f3d" ]]; then
      outfile=${subdata}/vartmp_raw_vari_ldy${dcnt}.grib2
      for infile in "${infile1}" "${infile2}"; do
        if [[ -f "${infile}" ]]; then # check if input file exists before extraction
          # shellcheck disable=SC2312
          ${WGRIB2} "${infile}" | grep -F -f "${varlist_d}" | ${WGRIB2} -i "${infile}" -append -grib "${outfile}"
        else
          echo "WARNING: ${infile} does not exist."
        fi
      done
      if [[ ${fcnt} -eq 4 ]]; then
        daily_avg_atmos "${outfile}" "${dcnt}" "${outres}"
        copy_to_comout "${davg_file}" "${ARC_RFCST_PROD_ATMOS}"
        fcnt=1
        dcnt=$(( dcnt + 1 ))
      else
        fcnt=$(( fcnt + 1 ))
      fi # If at final lead hour of a given day
    fi # if lead hour is divisible by 6 and outtype is f3d

    nh=$(( nh + outfreq ))
  done # nh

done # f2d,f3d

exit 0
