#!/usr/bin/env bash
# ocean_monthly_task.sh: Calculate monthly mean d20/TCHP/ohc from monthly mean products
# Path to python script to calculate depth of 20C isotherm,TCHP and OHC

set -e

# Scripts for computing d20/TCHP/ohc
CALC_D20="${USHgfs}/python/ocn_diag/calc_d20.py"
CALC_TCHP="${USHgfs}/python/ocn_diag/calc_tchp.py"
CALC_OHC="${USHgfs}/python/ocn_diag/calc_ohc.py"

# Arguments: 
YR=$1
MN=$2

# Input files
in_file="${COMOUT_OCEAN_NETCDF}/${grid}/${RUN}.ocean.t${current_cycle}.${grid}.monthly_avg.${YR}-${MN}.nc"
temp3d_file="${DATAoutput}/MOM6_OUTPUT/temp3d_${YR}${MN}.nc"
temp3d_300m="${DATAoutput}/MOM6_OUTPUT/temp3d_300m_${YR}${MN}.nc"

# Diagnostics outputs
out_dt20c="${COMOUT_OCEAN_NETCDF}/${grid}/${RUN}.dt20c.t${current_cycle}.${grid}.monthly_avg.${YR}-${MN}.nc"
out_tchp="${COMOUT_OCEAN_NETCDF}/${grid}/${RUN}.TCHP.t${current_cycle}.${grid}.monthly_avg.${YR}-${MN}.nc"
out_ocnheat="${COMOUT_OCEAN_NETCDF}/${grid}/${RUN}.ocnheat.t${current_cycle}.${grid}.monthly_avg.${YR}-${MN}.nc"

if [[ "${RUN}" == sfs ]]; then
   # 1. CDO mergetime and averaging
   cdo mergetime "${DATAoutput}/MOM6_OUTPUT/ocn_${YR}_${MN}_??_12.nc" "${DATAoutput}/MOM6_OUTPUT/ocn_${YR}_${MN}_merge.nc"
   cdo monavg "${DATAoutput}/MOM6_OUTPUT/ocn_${YR}_${MN}_merge.nc" "${in_file}"
   rm -f "${DATAoutput}/MOM6_OUTPUT/ocn_${YR}_${MN}_merge.nc"

   # 2. Extract Temp and interpolate for OHC
   levels="1,3,5,7,9,15,25,35,45,55,65,75,85,95,105,115,125,135,145,155,165,175,185,195,205,215,225.8694,241.0626,266.5239,300"
   ncks -v temp "${in_file}" "${temp3d_file}"
   cdo intlevel,"$levels" "${temp3d_file}" "${temp3d_300m}"

   # 3. Python Diagnostics
   python3 "${CALC_D20}" "${in_file}" "${out_dt20c}"
   python3 "${CALC_TCHP}" "${in_file}" "${out_tchp}"
   python3 "${CALC_OHC}" "${temp3d_300m}" "${out_ocnheat}"

   # 4. Compression
   for f in "${in_file}" "${out_dt20c}" "${out_tchp}" "${out_ocnheat}"; do
       nccopy -k 4 -d 5 "$f" "${f}.tmp" && mv "${f}.tmp" "$f"
   done

   # 5. Cleanup local task files
   rm -f "${temp3d_file}" "${temp3d_300m}"

fi
