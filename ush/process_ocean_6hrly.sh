#!/usr/bin/env bash
# ocean_6hr_task.sh: Extract 6-hrly 5m temp from 6-hrly ocean history (temp) files

set -e

start_fhr=$1
interval=6  # Fixed 6-hour interval
fhr_inc=120   # Number of 6-hour jobs per task

for (( i=0; i<${fhr_inc}; i++ )); do
    # Calculate current fhr: start + (0, 6, 12, 18)and the first start_fhr=6
    current_fhr=$(( start_fhr + i * interval ))
    
    # Safety check: don't exceed the total forecast length (8784)
    if [ "${current_fhr}" -gt "${FHMAX_GFS}" ]; then break; fi
    
    fhr3=$(printf %03i "${current_fhr}")

    # Calculate midpoint for date string
    (( midpoint = current_fhr - interval / 2 ))
    vdate_mid=$(date --utc -d "${current_cycle:0:8} ${current_cycle:8:2} + ${midpoint} hours" +%Y%m%d%H)
    vdate_mid_str="${vdate_mid:0:4}_${vdate_mid:4:2}_${vdate_mid:6:2}_${vdate_mid:8:2}"

    # Paths
    input_file="${DATAoutput}/MOM6_OUTPUT/temp_${vdate_mid_str}.nc"
    tmp_file="${DATAoutput}/MOM6_OUTPUT/tmp_${fhr3}.nc"
    output_native_file="${COMOUT_OCEAN_NETCDF}/${grid}/${RUN}.t${cyc}z.native.${interval}hr_avg.f${fhr3}.nc"
    output_1p00_file="${COMOUT_OCEAN_NETCDF}/${grid}/${RUN}.temp5m.t${cyc}z.${grid}.${interval}hr_avg.f${fhr3}.nc"

    # Processing (ncks/ncwa/cdo)
    if [ -f "${input_file}" ]; then
        ncks -O -d z_l,2,2 -v temp "${input_file}" "${tmp_file}"
        ncwa -a z_l -O "${tmp_file}" "${output_native_file}"
        ncks -A -v geolon,geolat "${input_file}" "${output_native_file}"

        ncatted -a coordinates,temp,c,c,"geolon geolat" "${output_native_file}"
        cdo remapbil,r360x181 -setgridtype,curvilinear "${output_native_file}" "${output_1p00_file}"
        ncatted -a long_name,temp,o,c,"Potential Temperature at 5m below sea level" "${output_1p00_file}"

        rm -f "${tmp_file}" "${output_native_file}"
    else
        echo "WARNING: Input file ${input_file} missing for fhr ${current_fhr}"
    fi
done
