#! /usr/bin/env bash

#path to python script to calculate depth of 20C isotherm,TCHP and OHC
CALC_D20="${USHgfs}/python/ocn_diag/calc_d20.py"
CALC_TCHP="${USHgfs}/python/ocn_diag/calc_tchp.py"
CALC_OHC="${USHgfs}/python/ocn_diag/calc_ohc.py"

##############################################
# Begin JOB SPECIFIC work
##############################################

#GENERATE 6-HOURLY FILES FOR POTENTIAL TEMP at 5m.
if [[ "${RUN}" == sfs ]]; then
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
      output_file="${COMOUT_OCEAN_HISTORY}/${RUN}.ocean.t${cyc}z.${interval}hr_avg.f${fhr3}.nc"
      ncks -d z_l,2,2 -v temp "${input_file}" "${tmp_file}"
      ncwa -a z_l "${tmp_file}" "${output_file}"
      ncks -A -v geolon,geolat "${input_file}" "${output_file}"

      rm -f "${tmp_file}"

      last_fhr=${fhr}
    done
fi

#GENERATE MONTHLY MEAN FILES FROM SFS DAILY HISTORY FILES AND ADD MONTHLY D20/OHC/TCHP
if [[ "${RUN}" == sfs ]]; then
    last_fh_output="${COMOUT_OCEAN_HISTORY}/${RUN}.ocean.t${cyc}z.${FHOUT_OCN}hr_avg.f${FHMAX_GFS}.nc"
    if [[ -f ${last_fh_output} ]]; then
       file_list="${DATAoutput}/MOM6_OUTPUT/ocn_????_??_28_12.nc"
       file_list_mon="$( ls ${file_list} )"
       for f in ${file_list_mon}; do
         f_name=$( basename "${f}" )
         cdo mergetime "${DATAoutput}/MOM6_OUTPUT/ocn_${f_name:4:4}_${f_name:9:2}_??_12.nc" "${DATAoutput}/MOM6_OUTPUT/ocn_${f_name:4:4}_${f_name:9:2}_merge.nc"
         cdo monavg "${DATAoutput}/MOM6_OUTPUT/ocn_${f_name:4:4}_${f_name:9:2}_merge.nc" "${COMOUT_OCEAN_HISTORY}/${RUN}.ocn.t${current_cycle}.monthly_avg.${f_name:4:4}-${f_name:9:2}.nc"

         in_file="${COMOUT_OCEAN_HISTORY}/${RUN}.ocn.t${current_cycle}.monthly_avg.${f_name:4:4}-${f_name:9:2}.nc"
         out_file_d20="${COMOUT_OCEAN_HISTORY}/${RUN}.ocn.d20.t${current_cycle}.monthly_avg.${f_name:4:4}-${f_name:9:2}.nc"
         out_file_tchp="${COMOUT_OCEAN_HISTORY}/${RUN}.ocn.tchp.t${current_cycle}.monthly_avg.${f_name:4:4}-${f_name:9:2}.nc"
         out_file_ohc="${COMOUT_OCEAN_HISTORY}/${RUN}.ocn.ohc.t${current_cycle}.monthly_avg.${f_name:4:4}-${f_name:9:2}.nc"
         python3 "${CALC_D20}" "${in_file}" "${out_file_d20}"
         python3 "${CALC_TCHP}" "${in_file}" "${out_file_tchp}"
         python3 "${CALC_OHC}" "${in_file}" "${out_file_ohc}"
         ncks -A -v D20 "${out_file_d20}" "${in_file}"
         ncks -A -v TCHP "${out_file_tchp}" "${in_file}"
         ncks -A -v OHC "${out_file_ohc}" "${in_file}"
         rm -f "${DATAoutput}/MOM6_OUTPUT/ocn_${f_name:4:4}_${f_name:9:2}_merge.nc"
         rm -f "${out_file_d20}" "${out_file_tchp}" "${out_file_ohc}"
       done
    fi

    export err=$?
    if [[ ${err} -ne 0 ]]; then
        echo "FATAL ERROR: Failed to generate monthly mean ocean history files"
        exit "${err}"
    fi
fi

#CALCULATE D20/TCHP/OHC FROM DAILY OCEAN HISTORY FILES AND ADDED THEM TO DAILY OCEAN HISTORY FILES

if [[ "${RUN}" == sfs ]]; then

MOM6_OUTPUT_FH=($(seq -s ' ' "${FHOUT_OCN}" "${FHOUT_OCN}" "${FHMAX_GFS}"))
for fhr in "${MOM6_OUTPUT_FH[@]}"; do
    fhr3=$(printf %03i "${fhr}")
    input_file="${COMOUT_OCEAN_HISTORY}/sfs.ocean.t00z.24hr_avg.f${fhr3}.nc"
    output_file_d20="${COMOUT_OCEAN_HISTORY}/sfs.ocean.d20.t00z.24hr_avg.f${fhr3}.nc"
    output_file_tchp="${COMOUT_OCEAN_HISTORY}/sfs.ocean.tchp.t00z.24hr_avg.f${fhr3}.nc"
    output_file_ohc="${COMOUT_OCEAN_HISTORY}/sfs.ocean.ohc300.t00z.24hr_avg.f${fhr3}.nc"
    python3 "${CALC_D20}" "${input_file}" "${output_file_d20}"
    python3 "${CALC_TCHP}" "${input_file}" "${output_file_tchp}"
    python3 "${CALC_OHC}" "${input_file}" "${output_file_ohc}"
    ncks -A -v D20 "${output_file_d20}" "${input_file}"
    ncks -A -v TCHP "${output_file_tchp}" "${input_file}"
    ncks -A -v OHC "${output_file_ohc}" "${input_file}"
    rm -f "${output_file_d20}" "${output_file_tchp}" "${output_file_ohc}"
    done
fi

##############################################
# End JOB SPECIFIC work
##############################################

exit 0
