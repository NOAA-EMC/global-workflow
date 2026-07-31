#!/usr/bin/env bash
# ocean_monthly_task.sh: Calculate monthly mean d20/TCHP/ohc from monthly mean products
# Path to python script to calculate depth of 20C isotherm,TCHP and OHC

set -e

# Scripts for computing d20/TCHP/ohc
CALC_D20="${USHglobal}/python/ocn_diag/calc_d20.py"
CALC_TCHP="${USHglobal}/python/ocn_diag/calc_tchp.py"
CALC_OHC="${USHglobal}/python/ocn_diag/calc_ohc.py"

# Arguments:
YR=$1
MN=$2

# temporary ocean temp files for TCHP/ocnheat/dt20c calculation:
temp3d_file="${DATAoutput}/MOM6_OUTPUT/temp3d_${YR}${MN}.nc"
temp3d_300m="${DATAoutput}/MOM6_OUTPUT/temp3d_300m_${YR}${MN}.nc"

if [[ "${RUN}" == sfs ]]; then
    # 1. CDO mergetime and averaging
    # 1.0 Remove the temporary files (if exist) generated from the previous failed run
    rm -f "${DATAoutput}/MOM6_OUTPUT/ocn_${YR}_${MN}_merge.nc"
    rm -f "${DATAoutput}/MOM6_OUTPUT/ssh_${YR}_${MN}_merge.nc"
    rm -f "${temp3d_file}" "${temp3d_300m}"
    # 1.1 Do monthly averaging for all variables except SSH
    cdo mergetime "${DATAoutput}/MOM6_OUTPUT/ocn_${YR}_${MN}_??_12.nc" "${DATAoutput}/MOM6_OUTPUT/ocn_${YR}_${MN}_merge.nc"
    cdo monavg "${DATAoutput}/MOM6_OUTPUT/ocn_${YR}_${MN}_merge.nc" "${DATAoutput}/MOM6_OUTPUT/${RUN}.ocean.t${current_cycle}.${grid}.monthly_avg.${YR}-${MN}.nc"
    # 1.2 Do monthly averaging for SSH
    cdo mergetime "${DATAoutput}/MOM6_OUTPUT/ssh_${YR}_${MN}_??_12.nc" "${DATAoutput}/MOM6_OUTPUT/ssh_${YR}_${MN}_merge.nc"
    cdo monavg "${DATAoutput}/MOM6_OUTPUT/ssh_${YR}_${MN}_merge.nc" "${DATAoutput}/MOM6_OUTPUT/${RUN}.SSH.t${current_cycle}.0p25.monthly_avg.${YR}-${MN}.nc"

    rm -f "${DATAoutput}/MOM6_OUTPUT/ocn_${YR}_${MN}_merge.nc"
    rm -f "${DATAoutput}/MOM6_OUTPUT/ssh_${YR}_${MN}_merge.nc"

    # 2. Extract Temp and interpolate to 300m depth exactly for 0-300m OHC calculation
    levels="1,3,5,7,9,15,25,35,45,55,65,75,85,95,105,115,125,135,145,155,165,175,185,195,205,215,225.8694,241.0626,266.5239,300"
    ncks -v temp "${DATAoutput}/MOM6_OUTPUT/${RUN}.ocean.t${current_cycle}.${grid}.monthly_avg.${YR}-${MN}.nc" "${temp3d_file}"
    cdo intlevel,"${levels}" "${temp3d_file}" "${temp3d_300m}"

    # 3. Python Diagnostics
    python3 "${CALC_D20}" "${DATAoutput}/MOM6_OUTPUT/${RUN}.ocean.t${current_cycle}.${grid}.monthly_avg.${YR}-${MN}.nc" "${DATAoutput}/MOM6_OUTPUT/${RUN}.dt20c.t${current_cycle}.${grid}.monthly_avg.${YR}-${MN}.nc"
    python3 "${CALC_TCHP}" "${DATAoutput}/MOM6_OUTPUT/${RUN}.ocean.t${current_cycle}.${grid}.monthly_avg.${YR}-${MN}.nc" "${DATAoutput}/MOM6_OUTPUT/${RUN}.TCHP.t${current_cycle}.${grid}.monthly_avg.${YR}-${MN}.nc"
    python3 "${CALC_OHC}" "${temp3d_300m}" "${DATAoutput}/MOM6_OUTPUT/${RUN}.ocnheat.t${current_cycle}.${grid}.monthly_avg.${YR}-${MN}.nc"

    # 4. Compression and save the final ocean monthly mean products
    # For SSH
    nccopy -k 4 -d 5 "${DATAoutput}/MOM6_OUTPUT/${RUN}.SSH.t${current_cycle}.0p25.monthly_avg.${YR}-${MN}.nc" "${DATAoutput}/MOM6_OUTPUT/${RUN}.SSH.t${current_cycle}.0p25.monthly_avg.${YR}-${MN}.tmp.nc"
    cpfs "${DATAoutput}/MOM6_OUTPUT/${RUN}.SSH.t${current_cycle}.0p25.monthly_avg.${YR}-${MN}.tmp.nc" "${COMOUT_OCEAN_NETCDF}/${grid}/${RUN}.SSH.t${current_cycle}.0p25.monthly_avg.${YR}-${MN}.nc"
    rm -f "${DATAoutput}/MOM6_OUTPUT/${RUN}.SSH.t${current_cycle}.0p25.monthly_avg.${YR}-${MN}.nc" "${DATAoutput}/MOM6_OUTPUT/${RUN}.SSH.t${current_cycle}.0p25.monthly_avg.${YR}-${MN}.tmp.nc"
    # For other fields
    for var in "ocean" "dt20c" "TCHP" "ocnheat"; do
        nccopy -k 4 -d 5 "${DATAoutput}/MOM6_OUTPUT/${RUN}.${var}.t${current_cycle}.${grid}.monthly_avg.${YR}-${MN}.nc" "${DATAoutput}/MOM6_OUTPUT/${RUN}.${var}.t${current_cycle}.${grid}.monthly_avg.${YR}-${MN}.tmp.nc"
        cpfs "${DATAoutput}/MOM6_OUTPUT/${RUN}.${var}.t${current_cycle}.${grid}.monthly_avg.${YR}-${MN}.tmp.nc" "${COMOUT_OCEAN_NETCDF}/${grid}/${RUN}.${var}.t${current_cycle}.${grid}.monthly_avg.${YR}-${MN}.nc"
        rm -f "${DATAoutput}/MOM6_OUTPUT/${RUN}.${var}.t${current_cycle}.${grid}.monthly_avg.${YR}-${MN}.tmp.nc" "${DATAoutput}/MOM6_OUTPUT/${RUN}.${var}.t${current_cycle}.${grid}.monthly_avg.${YR}-${MN}.nc"
    done

    # 5. Cleanup temporary ocean temp files
    rm -f "${temp3d_file}" "${temp3d_300m}"

fi
