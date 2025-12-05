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
       ori_file="${RUN}.ice.t${cyc}z.${interval}hr_avg.f${fhr3}.nc"
       ${NLN} "${COMOUT_ICE_HISTORY}/${ori_file}" "${DATAoutput}/CICE_OUTPUT/${new_file}"
       last_fhr=${fhr}
     done
fi

#GENERATE MONTHLY MEAN FILES FROM SFS DAILY ICE HISTORY FILES.
if [[ "${RUN}" == sfs ]]; then
    last_fh_output="${COMOUT_ICE_HISTORY}/${RUN}.ice.t${cyc}z.${FHOUT_ICE}hr_avg.f${FHMAX_GFS}.nc"
    if [[ -f ${last_fh_output} ]]; then
       file_list="${DATAoutput}/CICE_OUTPUT/iceh_24h_????_??_28_12.nc"
       file_list_mon="$( ls ${file_list} )"
       for f in ${file_list_mon}; do
        f_name=$( basename "${f}" )
        cdo mergetime "${DATAoutput}/CICE_OUTPUT/iceh_24h_${f_name:9:4}_${f_name:14:2}_??_12.nc" "${DATAoutput}/CICE_OUTPUT/iceh_24h_${f_name:9:4}_${f_name:14:2}_merge.nc"
        ncra "${DATAoutput}/CICE_OUTPUT/iceh_24h_${f_name:9:4}_${f_name:14:2}_merge.nc" "${COMOUT_ICE_NETCDF}/native/${RUN}.ice.t${current_cycle}.monthly_avg.${f_name:9:4}-${f_name:14:2}.nc"
       rm -f "${DATAoutput}/CICE_OUTPUT/iceh_24h_${f_name:9:4}_${f_name:14:2}_merge.nc"
       done
    fi

    export err=$?
    if [[ ${err} -ne 0 ]]; then
       echo "FATAL ERROR: Failed to generate monthly mean ice history files"
       exit "${err}"
    fi
fi
##############################################
# End JOB SPECIFIC work
##############################################

exit 0
