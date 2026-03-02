#!/usr/bin/env bash
# ocean_daily_task.sh: compute daily d20/TCHP/ocnheat and merge all daily files into one single file for each variable

set -e

#paths to python script to calculate depth of 20C isotherm,TCHP and OHC
CALC_D20="${USHgfs}/python/ocn_diag/calc_d20.py"
CALC_TCHP="${USHgfs}/python/ocn_diag/calc_tchp.py"
CALC_OHC="${USHgfs}/python/ocn_diag/calc_ohc.py"

#path to SSH output resolution (0p25: r1440x721)
r1440x721="${USHgfs}/python/ocn_diag/r1440x721"

# Arguments:
start_fhr=$1
fhout_ocn=$2

last_fhr="${FHMAX_GFS}"
fhr_inc=30  # Process 30 files per task
varslist=("SST" "SSU" "SSV" "MLD_003" "MLD_0125" "ePBL" "latent" "sensible" "SW" "LW" "taux" "tauy" "temp" "uo" "vo" "so")
levels="1,3,5,7,9,15,25,35,45,55,65,75,85,95,105,115,125,135,145,155,165,175,185,195,205,215,225.8694,241.0626,266.5239,300"

for (( j=0; j<${fhr_inc}; j++ )); do
    current_fhr=$(( start_fhr + j * fhout_ocn ))
    if [ "${current_fhr}" -gt "${FHMAX_GFS}" ]; then break; fi
    fhr3=$(printf %03i "${current_fhr}")

    input_file="${COMOUT_OCEAN_NETCDF}/${grid}/sfs.t00z.${grid}.f${fhr3}.nc"
    in_hist_file="${COMOUT_OCEAN_HISTORY}/sfs.t00z.24hr_avg.f${fhr3}.nc"
    in_ssh_file="${COMOUT_OCEAN_HISTORY}/sfs.SSH.t00z.24hr_avg.f${fhr3}.nc"
    [ ! -f "${input_file}" ] && continue
    [ ! -f "${in_hist_file}" ] && continue

    # Outputs
    output_ssh="${COMOUT_OCEAN_NETCDF}/${grid}/sfs.SSH.t00z.0p25.f${fhr3}.nc"
    output_dt20c="${COMOUT_OCEAN_NETCDF}/${grid}/sfs.dt20c.t00z.${grid}.f${fhr3}.nc"
    output_tchp="${COMOUT_OCEAN_NETCDF}/${grid}/sfs.TCHP.t00z.${grid}.f${fhr3}.nc"
    output_ocnheat="${COMOUT_OCEAN_NETCDF}/${grid}/sfs.ocnheat.t00z.${grid}.f${fhr3}.nc"
    temp3d_file="${DATA}/sfs.temp3d.f${fhr3}.nc"
    temp3d_300m="${DATA}/sfs.temp3d.300m.f${fhr3}.nc"

    # 1. Calc OHC/D20/TCHP
    ncks -O -v temp "${input_file}" "${temp3d_file}"
    cdo -s intlevel,"${levels}" "${temp3d_file}" "${temp3d_300m}"
    
    python3 "${CALC_D20}" "${input_file}" "${output_dt20c}"
    python3 "${CALC_TCHP}" "${input_file}" "${output_tchp}"
    python3 "${CALC_OHC}" "${temp3d_300m}" "${output_ocnheat}"
    
    rm -f "${temp3d_file}" "${temp3d_300m}"

    #2. Extract SSH from ocean history files and then conduct CDO remapping to get 0p25 SSH files
    ncks -O -v geolon,geolat,SSH "${in_hist_file}" "${in_ssh_file}" # extract SSH, geolon, geolat variables into one new file
    ncatted -a coordinates,SSH,c,c,"geolon geolat" "${in_ssh_file}" # reedit SSH cooridinates information "geolon geolat"
    cdo remapbil,${r1440x721} -setgridtype,curvilinear "${in_ssh_file}" "${output_ssh}" # remap from mx025 to 0p25

    rm -f "${in_ssh_file}"

    # 3. Extract specific variables into individual files for later merging
    for var in "${varslist[@]}"; do
        output_var="${COMOUT_OCEAN_NETCDF}/${grid}/sfs.${var}.t00z.${grid}.f${fhr3}.nc"
        ncks -O -v "${var}" "${input_file}" "${output_var}"
    done
done
