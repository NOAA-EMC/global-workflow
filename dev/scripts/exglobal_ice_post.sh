#! /usr/bin/env bash

##############################################
# Begin JOB SPECIFIC work
##############################################

#LINK NEW FILE NAMES TO THE OUTPUT DAILY ICE HISTORY FILES FOR MONTHLY AVERAGING
CICE_OUTPUT_FH=($(seq -s ' ' "${FHMIN_GFS}" "${FHOUT_ICE}" "${FHMAX_GFS}"))
if [[ "${RUN}" == sfs ]]; then
     for fhr in "${CICE_OUTPUT_FH[@]}"; do
       if [[ -z ${last_fhr:-} ]]; then
       last_fhr=${fhr}
       continue
       fi
       fhr3=$(printf %03i "${fhr}")
       (( interval = fhr - last_fhr ))
       (( midpoint = last_fhr + interval/2 ))
       vdate_mid=$(date --utc -d "${current_cycle:0:8} ${current_cycle:8:2} + ${midpoint} hours" +%Y%m%d%H)
       vdate_mid_str="${vdate_mid:0:4}_${vdate_mid:4:2}_${vdate_mid:6:2}_${vdate_mid:8:2}"
       new_file="iceh_24h_${vdate_mid_str}.nc"
       ori_file="${RUN}.t${cyc}z.native.f${fhr3}.nc"
       ${NLN} "${COMOUT_ICE_NETCDF}/native/${ori_file}" "${DATAoutput}/CICE_OUTPUT/${new_file}"
       last_fhr=${fhr}
     done
fi

#GENERATE MONTHLY MEAN FILES FROM SFS DAILY ICE PRODUCT FILES.
if [[ "${RUN}" == sfs ]]; then
    if [[ ${FHMAX_GFS} -lt 744 ]]; then
       echo "Forecast length is ${FHMAX_GFS} hours, shorter than one month, please run at least 744 hours"
       exit 0
    else
       # Obain the information of the last fcst file
       last_fh_output="${COMOUT_ICE_NETCDF}/native/${RUN}.t${cyc}z.native.f${FHMAX_GFS}.nc"
       (( interval = 24 ))
       (( midpoint = FHMAX_GFS - interval/2 ))
       last_fhr_vdate_mid=$(date --utc -d "${current_cycle:0:8} ${current_cycle:8:2} + ${midpoint} hours" +%Y%m%d%H)
    fi
    # Extract the Year/Month/Day from YYYYMMDD format:
    yyyy=${last_fhr_vdate_mid:0:4}
    mm=${last_fhr_vdate_mid:4:2}
    dd=${last_fhr_vdate_mid:6:2}
    # Check leap or non-year for the last month of the year:
    if (( (${yyyy} % 4 == 0 && ${yyyy} % 100 != 0) || (${yyyy} % 400 == 0) )); then
      leap_yr="true"
    else
      leap_yr="false"
    fi
    # Full Month CHeck: if the last month is a partial month or a full month?
    # Check days for leap or non-leap February
    if [[ "${mm}" == "02" && "${leap_yr}" == "true" && "${dd}" == "29" ]]; then
       full_month="true"
    elif [[ "${mm}" == "02" && "${leap_yr}" == "false" && "${dd}" == "28" ]]; then
       full_month="true"
    # Check for 31-day months
    elif [[ "${mm}" =~ ^(01|03|05|07|08|10|12)$ && "${dd}" == "31" ]]; then
       full_month="true"
    # Check for 30-day months
    elif [[ "${mm}" =~ ^(04|06|09|11)$ && "${dd}" == "30" ]]; then
       full_month="true"
    else
       full_month="false"
    fi

   # Expand the wildcard directly into an array (Avoids 'ls' issues)
   file_list_mon=( "${DATAoutput}"/CICE_OUTPUT/iceh_24h_????_??_01_12.nc )
   if [[ -f "${last_fh_output}" ]] && [[ "${full_month}" == "true" ]]; then
     # Keep the full list if it's a complete month
     file_list_mon=( "${file_list_mon[@]}" )
   else
    # Check if array has elements before slicing to avoid errors
     if (( ${#file_list_mon[@]} > 0 )); then
    # Skip the last element using array slicing
      file_list_mon=( "${file_list_mon[@]::${#file_list_mon[@]}-1}" )
     fi
   fi

   for f in "${file_list_mon[@]}"; do
     f_name=$( basename "${f}" )
     YR="${f_name:9:4}"
     MN="${f_name:14:2}"
     cdo -O mergetime "${DATAoutput}/CICE_OUTPUT/iceh_24h_${YR}_${MN}_??_12.nc" "${DATAoutput}/CICE_OUTPUT/iceh_24h_${YR}_${MN}_merge.nc"
     ncra -O "${DATAoutput}/CICE_OUTPUT/iceh_24h_${YR}_${MN}_merge.nc" "${COMOUT_ICE_NETCDF}/native/${RUN}.ice.t${current_cycle}.monthly_avg.${YR}-${MN}.nc"
     rm -f "${DATAoutput}/CICE_OUTPUT/iceh_24h_${YR}_${MN}_merge.nc"
     # Compress the monthly data
     output_month_file="${COMOUT_ICE_NETCDF}/native/${RUN}.ice.t${current_cycle}.monthly_avg.${YR}-${MN}.nc"
     nccopy -k 4 -d 5 "${output_month_file}" "${output_month_file}.tmp" && mv "${output_month_file}.tmp" "${output_month_file}"
   done

   export err=$?
   if [[ ${err} -ne 0 ]]; then
     echo "FATAL ERROR: Failed to generate monthly mean ice products files"
     exit "${err}"
   fi
fi
##############################################
# End JOB SPECIFIC work
##############################################

exit 0
