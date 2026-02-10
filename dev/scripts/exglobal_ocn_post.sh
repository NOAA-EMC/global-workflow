#! /usr/bin/env bash

#path to python script to calculate depth of 20C isotherm,TCHP and OHC
CALC_D20="${USHgfs}/python/ocn_diag/calc_d20.py"
CALC_TCHP="${USHgfs}/python/ocn_diag/calc_tchp.py"
CALC_OHC="${USHgfs}/python/ocn_diag/calc_ohc.py"

##############################################
# Begin JOB SPECIFIC work
##############################################

#GENERATE 6-HOURLY FILES FOR POTENTIAL TEMP at 5M AND REMAP TO 1P00 GRID.
if [[ "${RUN}" == sfs ]]; then
   grid_out="${HOMEgfs}/ush/python/ocn_diag/r360x181"
   FHOUT_ocn6hr=6
   MOM6_OUTPUT6hr_FH=($(seq -s ' ' "${FHMIN_GFS}" "${FHOUT_ocn6hr}" "${FHMAX_GFS}"))
   for fhr in "${MOM6_OUTPUT6hr_FH[@]}"; do
      if [[ -z ${last_fhr:-} ]]; then
      last_fhr=${fhr}
      continue
      fi
      fhr3=$(printf %03i "${fhr}")
      (( interval = fhr - last_fhr ))
      (( midpoint = last_fhr + interval/2 ))
      vdate_mid=$(date --utc -d "${current_cycle:0:8} ${current_cycle:8:2} + ${midpoint} hours" +%Y%m%d%H)
      vdate_mid_str="${vdate_mid:0:4}_${vdate_mid:4:2}_${vdate_mid:6:2}_${vdate_mid:8:2}"

      input_file="${DATAoutput}/MOM6_OUTPUT/temp_${vdate_mid_str}.nc"
      tmp_file="${DATAoutput}/MOM6_OUTPUT/tmp_${vdate_mid_str}.nc"
      output_native_file="${COMOUT_OCEAN_NETCDF}/${grid}/${RUN}.t${cyc}z.native.${interval}hr_avg.f${fhr3}.nc"
      output_1p00_file="${COMOUT_OCEAN_NETCDF}/${grid}/${RUN}.temp5m.t${cyc}z.${grid}.${interval}hr_avg.f${fhr3}.nc"

      if [[ -f ${tmp_file} ]]; then
      rm -f "${tmp_file}"
      fi

      if [[ -f ${output_native_file} ]]; then
      rm -f "${output_native_file}"
      fi

      ncks -d z_l,2,2 -v temp "${input_file}" "${tmp_file}"
      ncwa -a z_l -O "${tmp_file}" "${output_native_file}"  #remove z_l as dimension
      ncks -A -v geolon,geolat "${input_file}" "${output_native_file}"

      #cdo remapping using bilinear methods:
      ncatted -a coordinates,temp,c,c,"geolon geolat" "${output_native_file}"
      cdo remapbil,r360x181 -setgridtype,curvilinear "${output_native_file}" "${output_1p00_file}"
      #edit long_name of potential temperature:
      ncatted -a long_name,temp,o,c,"Potential Temperature at 5m below sea level" "${output_1p00_file}"

      rm -f "${tmp_file}" "${output_native_file}"

      last_fhr=${fhr}
   done
   # merge all fcst hours to one single file
   merge_file="${COMOUT_OCEAN_NETCDF}/${grid}/${RUN}.temp5m.t${cyc}z.${grid}.${FHOUT_ocn6hr}hr_avg.nc"
   cdo mergetime "${COMOUT_OCEAN_NETCDF}/${grid}/${RUN}.temp5m.t${cyc}z.${grid}.${FHOUT_ocn6hr}hr_avg.f*.nc" "${merge_file}"
   # compress the final file
   nccopy -k 4 -d 5 "${merge_file}" "${merge_file}.tmp" && mv "${merge_file}.tmp" "${merge_file}"

   if [[ -f "${merge_file}.tmp" ]]; then
   rm -f "${merge_file}.tmp"
   fi

   rm -f "${COMOUT_OCEAN_NETCDF}/${grid}/${RUN}.temp5m.t${cyc}z.${grid}.${FHOUT_ocn6hr}hr_avg.f"*".nc"
fi

#LINK FILE NAMES TO THE OUTPUT DAILY OCEAN REGULAR GRID (1p00,0p25) PRODUCT FILES FOR MONTHLY AVERAGING
OCEAN_OUTPUT_FH=($(seq -s ' ' "${FHMIN_GFS}" "${FHOUT_OCN}" "${FHMAX_GFS}"))
if [[ "${RUN}" == sfs ]]; then
   for fhr in "${OCEAN_OUTPUT_FH[@]}"; do
      if [[ -z ${last_fhr:-} ]]; then
         last_fhr=${fhr}
         continue
      fi
      fhr3=$(printf %03i "${fhr}")
      (( interval = fhr - last_fhr ))
      (( midpoint = last_fhr + interval/2 ))
      vdate_mid=$(date --utc -d "${current_cycle:0:8} ${current_cycle:8:2} + ${midpoint} hours" +%Y%m%d%H)
      vdate_mid_str="${vdate_mid:0:4}_${vdate_mid:4:2}_${vdate_mid:6:2}_${vdate_mid:8:2}"
      new_file="ocn_24h_${vdate_mid_str}.nc"
      ori_file="${RUN}.t${cyc}z.${grid}.f${fhr3}.nc"
      ${NLN} "${COMOUT_OCEAN_NETCDF}/${grid}/${ori_file}" "${DATAoutput}/MOM6_OUTPUT/${new_file}"
      last_fhr=${fhr}
   done
fi

#GENERATE MONTHLY MEAN FILES FROM SFS DAILY HISTORY FILES AND ADD MONTHLY D20/OHC/TCHP
if [[ "${RUN}" == sfs ]]; then
   # Obain the information of the last fcst file
   last_fh_output="${COMOUT_OCEAN_NETCDF}/${grid}/${RUN}.t${cyc}z.1p00.f${FHMAX_GFS}.nc"
   (( interval = 24 ))
   (( midpoint = FHMAX_GFS - interval/2 ))
   last_fhr_vdate_mid=$(date --utc -d "${current_cycle:0:8} ${current_cycle:8:2} + ${midpoint} hours" +%Y%m%d%H)

   # Extract the last fcst date (Year/Month/Day from YYYYMMDD format):
   yyyy=${last_fhr_vdate_mid:0:4}
   mm=${last_fhr_vdate_mid:4:2}
   dd=${last_fhr_vdate_mid:6:2}
   # Check leap or non-year for the last month of the year:
   if (( (${yyyy} % 4 == 0 && ${yyyy} % 100 != 0) || (${yyyy} % 400 == 0) )); then
      leap_yr="true"
   else
      leap_yr="false"
   fi
   # Full Month Check: if the last month is a partial month or a full month?
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
   file_list_mon=( "${DATAoutput}"/MOM6_OUTPUT/ocn_24h_????_??_01_12.nc )
   if [[ -f "${last_fh_output}" ]] && [[ "${full_month}" == "true" ]]; then
      # Keep the full list if it's a complete month
      file_list_mon=( "${file_list_mon[@]}" )
   else
      # Check if array has elements before slicing to avoid errors
      if (( ${#file_list_mon[@]} > 0 )); then
         # Skip the last element using array slicing
         file_list_mon=( "${file_list_mon[@]::${#file_list_mon[@]}-1}" )
      else
         file_list_mon=()
      fi
   fi

   # Start to process monthly averaging based on the above file_list_month if not empty 
   if (( ${#file_list_mon[@]} > 0 )); then
      for f in "${file_list_mon[@]}"; do
         f_name=$( basename "${f}" )
         YR="${f_name:8:4}"
         MN="${f_name:13:2}"
         cdo mergetime "${DATAoutput}/MOM6_OUTPUT/ocn_24h_${YR}_${MN}_??_12.nc" "${DATAoutput}/MOM6_OUTPUT/ocn_24h_${YR}_${MN}_merge.nc"
         cdo monavg "${DATAoutput}/MOM6_OUTPUT/ocn_24h_${YR}_${MN}_merge.nc" "${COMOUT_OCEAN_NETCDF}/${grid}/${RUN}.ocean.t${current_cycle}.${grid}.monthly_avg.${YR}-${MN}.nc"

         levels="1,3,5,7,9,15,25,35,45,55,65,75,85,95,105,115,125,135,145,155,165,175,185,195,205,215,225.8694,241.0626,266.5239,300"
         in_file="${COMOUT_OCEAN_NETCDF}/${grid}/${RUN}.ocean.t${current_cycle}.${grid}.monthly_avg.${YR}-${MN}.nc"
         temp3d_file="${COMOUT_OCEAN_NETCDF}/${grid}/${RUN}.temp3d.t${current_cycle}.${grid}.monthly_avg.${YR}-${MN}.nc"
         temp3d_file_300m="${COMOUT_OCEAN_NETCDF}/${grid}/${RUN}.temp3d.300m.t${current_cycle}.${grid}.monthly_avg.${YR}-${MN}.nc"
         out_file_dt20c="${COMOUT_OCEAN_NETCDF}/${grid}/${RUN}.dt20c.t${current_cycle}.${grid}.monthly_avg.${YR}-${MN}.nc"
         out_file_tchp="${COMOUT_OCEAN_NETCDF}/${grid}/${RUN}.TCHP.t${current_cycle}.${grid}.monthly_avg.${YR}-${MN}.nc"
         out_file_ocnheat="${COMOUT_OCEAN_NETCDF}/${grid}/${RUN}.ocnheat.t${current_cycle}.${grid}.monthly_avg.${YR}-${MN}.nc"

         ncks -v temp "${in_file}" "${temp3d_file}"
         cdo intlevel,"$levels" "${temp3d_file}" "${temp3d_file_300m}"

         python3 "${CALC_D20}" "${in_file}" "${out_file_dt20c}"
         python3 "${CALC_TCHP}" "${in_file}" "${out_file_tchp}"
         python3 "${CALC_OHC}" "${temp3d_file_300m}" "${out_file_ocnheat}"

         rm -f "${DATAoutput}/MOM6_OUTPUT/ocn_24h_${YR}_${MN}_merge.nc"
         rm -f "${temp3d_file}" "${temp3d_file_300m}"
         # Compress the monthly mean data
         nccopy -k 4 -d 5 "${in_file}" "${in_file}.tmp" && mv "${in_file}.tmp" "${in_file}"
         nccopy -k 4 -d 5 "${out_file_dt20c}" "${out_file_dt20c}.tmp" && mv "${out_file_dt20c}.tmp" "${out_file_dt20c}"
         nccopy -k 4 -d 5 "${out_file_tchp}" "${out_file_tchp}.tmp" && mv "${out_file_tchp}.tmp" "${out_file_tchp}"
         nccopy -k 4 -d 5 "${out_file_ocnheat}" "${out_file_ocnheat}.tmp" && mv "${out_file_ocnheat}.tmp" "${out_file_ocnheat}"

        if [[ -f "${in_file}.tmp" ]]; then
           rm -f "${in_file}.tmp"
        fi
        if [[ -f "${out_file_dt20c}.tmp" ]]; then
           rm -f  "${out_file_dt20c}.tmp"
        fi
        if [[ -f "${out_file_tchp}.tmp" ]]; then
           rm -f "${out_file_tchp}.tmp"
        fi
        if [[ -f "${out_file_ocnheat}.tmp" ]]; then
           rm -f "${out_file_ocnheat}.tmp"
        fi

      done
   else
      echo "No monthly mean ocean product files are generated because the fcst period is not within one full month"
   fi

   export err=$?
   if [[ ${err} -ne 0 ]]; then
      echo "FATAL ERROR: Failed to generate monthly mean ocean product files"
      rm -f "${COMOUT_OCEAN_NETCDF}/${grid}/${RUN}."*".t${current_cycle}.${grid}.monthly_avg."*".nc"
      exit "${err}"
   fi
fi

#CALCULATE D20/TCHP/OHC FROM DAILY OCEAN REGULAR GRID PRODUCT FILES

if [[ "${RUN}" == sfs ]]; then
   MOM6_OUTPUT_FH=($(seq -s ' ' "${FHOUT_OCN}" "${FHOUT_OCN}" "${FHMAX_GFS}"))
   varslist=("SSH" "SST" "SSU" "SSV" "MLD_003" "MLD_0125" "ePBL" "latent" "sensible" "SW" "LW" "taux" "tauy" "temp" "uo" "vo" "so")
   for fhr in "${MOM6_OUTPUT_FH[@]}"; do
      fhr3=$(printf %03i "${fhr}")
      input_file="${COMOUT_OCEAN_NETCDF}/${grid}/sfs.t00z.${grid}.f${fhr3}.nc"
      output_file_dt20c="${COMOUT_OCEAN_NETCDF}/${grid}/sfs.dt20c.t00z.${grid}.f${fhr3}.nc"
      output_file_tchp="${COMOUT_OCEAN_NETCDF}/${grid}/sfs.TCHP.t00z.${grid}.f${fhr3}.nc"
      output_file_ocnheat="${COMOUT_OCEAN_NETCDF}/${grid}/sfs.ocnheat.t00z.${grid}.f${fhr3}.nc"

      levels="1,3,5,7,9,15,25,35,45,55,65,75,85,95,105,115,125,135,145,155,165,175,185,195,205,215,225.8694,241.0626,266.5239,300"
      temp3d_file="${COMOUT_OCEAN_NETCDF}/${grid}/sfs.temp3d.t00z.${grid}.f${fhr3}.nc"
      temp3d_file_300m="${COMOUT_OCEAN_NETCDF}/${grid}/sfs.temp3d.300m.t00z.${grid}.f${fhr3}.nc"

      ncks -v temp "${input_file}" "${temp3d_file}"
      cdo intlevel,"${levels}" "${temp3d_file}" "${temp3d_file_300m}"

      python3 "${CALC_D20}" "${input_file}" "${output_file_dt20c}"
      python3 "${CALC_TCHP}" "${input_file}" "${output_file_tchp}"
      python3 "${CALC_OHC}" "${temp3d_file_300m}" "${output_file_ocnheat}"

      rm -f "${temp3d_file}" "${temp3d_file_300m}"

      for var in "${varslist[@]}"; do
         output_file="${COMOUT_OCEAN_NETCDF}/${grid}/sfs.${var}.t00z.${grid}.f${fhr3}.nc"

         if [[ -f ${output_file} ]]; then
            rm -f "${output_file}"
         fi

         ncks -O -v "${var}" "${input_file}" "${output_file}"
      done
   done
   # merge each variable to one single file
   newvarslist=("dt20c" "TCHP" "ocnheat" "SSH" "SST" "SSU" "SSV" "MLD_003" "MLD_0125" "ePBL" "latent" "sensible" "SW" "LW" "taux" "tauy" "temp" "uo" "vo" "so") 
   for var in "${newvarslist[@]}"; do
      merge_file="${COMOUT_OCEAN_NETCDF}/${grid}/sfs.${var}.t00z.${grid}.daily.nc"
      cdo mergetime "${COMOUT_OCEAN_NETCDF}/${grid}/sfs.${var}.t00z.${grid}.f*.nc" "${merge_file}"
      nccopy -k 4 -d 5 "${merge_file}" "${merge_file}.tmp" && mv "${merge_file}.tmp" "${merge_file}"
      if [[ -f "${merge_file}.tmp" ]]; then
      rm -f "${merge_file}.tmp"
      fi

      rm -f "${COMOUT_OCEAN_NETCDF}/${grid}/sfs.${var}.t00z.${grid}.f"*".nc"
   done
fi

# remove the original product files if all derived products are successfully generated
status=$?
if [[ ${status} -ne 0 ]]; then
   echo "Error detected (status ${status}). Keep the original history and remapped ocean files."
   exit "${status}"
else
   echo "Ocean post success! Remove the original remapped ocean files:"
   rm -f "${COMOUT_OCEAN_NETCDF}/${grid}/sfs.t00z.${grid}.f"*".nc"
   rm -f "${COMOUT_OCEAN_HISTORY}/sfs."*".nc"
fi

##############################################
# End JOB SPECIFIC work
##############################################

exit 0
