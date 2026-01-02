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
      output_1p00_file="${COMOUT_OCEAN_NETCDF}/${grid}/${RUN}.t${cyc}z.1p00.${interval}hr_avg.f${fhr3}.nc"

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

      #compress the data
      xz "${output_1p00_file}"

      rm -f "${tmp_file}" "${output_native_file}"

      last_fhr=${fhr}
    done
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
    if [[ ${FHMAX_GFS} -lt 744 ]]; then
       echo "Forecast length is ${FHMAX_GFS} hours, shorter than one month, please run at least 744 hours"
       exit 0
    else
       last_fh_output="${COMOUT_OCEAN_NETCDF}/${grid}/${RUN}.t${cyc}z.1p00.f${FHMAX_GFS}.nc"
    fi
    if [[ -f ${last_fh_output} ]]; then
       file_list="${DATAoutput}/MOM6_OUTPUT/ocn_24h_????_??_28_12.nc"
       file_list_mon="$( ls ${file_list} )"
       for f in ${file_list_mon}; do
         f_name=$( basename "${f}" )
         cdo mergetime "${DATAoutput}/MOM6_OUTPUT/ocn_24h_${f_name:8:4}_${f_name:13:2}_??_12.nc" "${DATAoutput}/MOM6_OUTPUT/ocn_24h_${f_name:8:4}_${f_name:13:2}_merge.nc"
         cdo monavg "${DATAoutput}/MOM6_OUTPUT/ocn_24h_${f_name:8:4}_${f_name:13:2}_merge.nc" "${COMOUT_OCEAN_NETCDF}/${grid}/${RUN}.ocean.t${current_cycle}.${grid}.monthly_avg.${f_name:8:4}-${f_name:13:2}.nc"

         levels="1,3,5,7,9,15,25,35,45,55,65,75,85,95,105,115,125,135,145,155,165,175,185,195,205,215,225.8694,241.0626,266.5239,300"
         in_file="${COMOUT_OCEAN_NETCDF}/${grid}/${RUN}.ocean.t${current_cycle}.${grid}.monthly_avg.${f_name:8:4}-${f_name:13:2}.nc"
         temp3d_file="${COMOUT_OCEAN_NETCDF}/${grid}/${RUN}.temp3d.t${current_cycle}.${grid}.monthly_avg.${f_name:8:4}-${f_name:13:2}.nc"
         temp3d_file_300m="${COMOUT_OCEAN_NETCDF}/${grid}/${RUN}.temp3d.300m.t${current_cycle}.${grid}.monthly_avg.${f_name:8:4}-${f_name:13:2}.nc"
         out_file_dt20c="${COMOUT_OCEAN_NETCDF}/${grid}/${RUN}.dt20c.t${current_cycle}.${grid}.monthly_avg.${f_name:8:4}-${f_name:13:2}.nc"
         out_file_tchp="${COMOUT_OCEAN_NETCDF}/${grid}/${RUN}.TCHP.t${current_cycle}.${grid}.monthly_avg.${f_name:8:4}-${f_name:13:2}.nc"
         out_file_ocnheat="${COMOUT_OCEAN_NETCDF}/${grid}/${RUN}.ocnheat.t${current_cycle}.${grid}.monthly_avg.${f_name:8:4}-${f_name:13:2}.nc"

         ncks -v temp "${in_file}" "${temp3d_file}"
         cdo intlevel,"$levels" "${temp3d_file}" "${temp3d_file_300m}"

         python3 "${CALC_D20}" "${in_file}" "${out_file_dt20c}"
         python3 "${CALC_TCHP}" "${in_file}" "${out_file_tchp}"
         python3 "${CALC_OHC}" "${temp3d_file_300m}" "${out_file_ocnheat}"

         rm -f "${DATAoutput}/MOM6_OUTPUT/ocn_24h_${f_name:8:4}_${f_name:13:2}_merge.nc"
         rm -f "${temp3d_file}" "${temp3d_file_300m}"
          # Compress the monthly mean data
         xz "${in_file}"
         xz "${out_file_dt20c}"
         xz "${out_file_tchp}"
         xz "${out_file_ocnheat}"
       done
    fi

    export err=$?
    if [[ ${err} -ne 0 ]]; then
        echo "FATAL ERROR: Failed to generate monthly mean ocean product files"
        exit "${err}"
    fi
fi

#CALCULATE D20/TCHP/OHC FROM DAILY OCEAN REGULAR GRID PRODUCT FILES

if [[ "${RUN}" == sfs ]]; then
MOM6_OUTPUT_FH=($(seq -s ' ' "${FHOUT_OCN}" "${FHOUT_OCN}" "${FHMAX_GFS}"))
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

    rm -rf "${temp3d_file}" "${temp3d_file_300m}"

    # compress the daily data
    xz "${input_file}"
    xz "${output_file_dt20c}"
    xz "${output_file_tchp}"
    xz "${output_file_ocnheat}"
    done
fi

##############################################
# End JOB SPECIFIC work
##############################################

exit 0
