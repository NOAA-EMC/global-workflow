#! /usr/bin/env bash
set -e
source "${USHgfs}/preamble.sh"

#------------------------------------------------------------------------------
# Ocean post tasks: 
# (A) Generate 6hrly 5m potential temperature from 6hrly temperature files and
#     merge into one single file for entire forecast length
# (B) Generate monthly products from daily files and d20/TCHP/ocnheat monthly 
#     files
# (C) Compute TCHP/d20c/oceanheat from daily and merge all daily files into one
#     single file for each variable
#------------------------------------------------------------------------------

#------------------------------------------------------------------------------
# 0. VALIDATE SCRIPTS FOR OCEAN POST-PROCESSES
#------------------------------------------------------------------------------

echo "INFO: Validating script line endings..."

# Scripts used
PROCESS_OCEAN_6HRLYSH=${PROCESS_OCEAN_6HRLYSH:-"${USHgfs}/process_ocean_6hrly.sh"}
PROCESS_OCEAN_DAILYSH=${PROCESS_OCEAN_DAILYSH:-"${USHgfs}/process_ocean_daily.sh"}
PROCESS_OCEAN_MONTHLYSH=${PROCESS_OCEAN_MONTHLYSH:-"${USHgfs}/process_ocean_monthly.sh"}
RUN_MPMDSH=${RUN_MPMDSH:-"${USHgfs}/run_mpmd.sh"}

# List all scripts involved in the workflow
workflow_scripts=(
    "${0}"
    "${PROCESS_OCEAN_6HRLYSH}"
    "${PROCESS_OCEAN_DAILYSH}"
    "${PROCESS_OCEAN_MONTHLYSH}"
    "${RUN_MPMDSH}"
)

for script in "${workflow_scripts[@]}"; do
    if [[ -f "${script}" ]]; then
        # Use sed to remove carriage returns (\r) safely
        sed -i 's/\r//g' "${script}"
    fi
done

# Ensure any existing task files are cleaned
if [[ -s "${DATA}"/mpmd_s*.txt ]]; then
    sed -i 's/\r//g' "${DATA}"/mpmd_s*.txt
fi

echo "INFO: Line ending validation complete."

#path to python scripts to calculate depth of 20C isotherm,TCHP and OHC
export CALC_D20="${USHgfs}/python/ocn_diag/calc_d20.py"
export CALC_TCHP="${USHgfs}/python/ocn_diag/calc_tchp.py"
export CALC_OHC="${USHgfs}/python/ocn_diag/calc_ohc.py"

# Path for ocean post output
mkdir -m 755 -p "${COMOUT_OCEAN_NETCDF}/${grid}"

#Run MPMD mode
export USE_CFP=YES

###############################################################################
# Begin JOB SPECIFIC work
###############################################################################

echo "Begin OCEAN POST work"

if [[ "${RUN}" == sfs ]]; then

# Task 0. Clean all intermediate and unfinished output files from the previous failed run
   rm -f "${COMOUT_OCEAN_NETCDF}/${grid}/${RUN}.temp5m.t${cyc}z.${grid}.6hr_avg.f"*".nc"
   rm -f "${COMOUT_OCEAN_NETCDF}/${grid}/${RUN}.temp5m.t${cyc}z.${grid}.6hr_avg.nc"
   rm -f "${COMOUT_OCEAN_NETCDF}/${grid}/sfs"*"monthly_avg"*".nc"
   rm -f "${COMOUT_OCEAN_NETCDF}/${grid}/sfs."*".t00z.${grid}.f"*".nc"
   rm -f "${COMOUT_OCEAN_NETCDF}/${grid}/sfs."*".t00z.${grid}.daily.nc"
   rm -f "${COMOUT_OCEAN_NETCDF}/${grid}/sfs.SSH.t00z.0p25.daily.nc"
   echo "Finish Cleaning All Generated Files From the Previous Failed Run!"
# -----------------------------------------------------------------------------
# Task A: Generated 6hrly 5m potential temperature from 6hrly temperature files
# and merge into one single file for entire forecast length
# -----------------------------------------------------------------------------
   # 1. Generate the command file
   ocean_6hrly_cmdfile="${DATA}/ocean_6hrly_cmds.txt"
   > "${ocean_6hrly_cmdfile}"

   # Step by 720 hours. Each task starts at 6, 726, etc and the first start_fhr=6 for 6 hour_avg files.
   # Each task will process 120 files if FHMAX_GFS>=720 (e.g., Task start_fhr=6 handles 6, 12, 18, 24, ... 720 hrs)
   for start_fhr in $(seq  6 720 "${FHMAX_GFS}"); do
       echo "${PROCESS_OCEAN_6HRLYSH} ${start_fhr}" >> "${ocean_6hrly_cmdfile}"
   done

   # 2. Dynamically count tasks and execute (e.g., 13 tasks for a full year)
   if [[ -s "${ocean_6hrly_cmdfile}" ]]; then
       n_tasks=$(wc -l < "${ocean_6hrly_cmdfile}")
       echo "INFO: Launching MPMD for extracting 6hrly variables with ${n_tasks} tasks."
       "${RUN_MPMDSH}" "${ocean_6hrly_cmdfile}"
       err=$?
   fi

   # 3. Final Consolidation (Post-MPMD)
   FHOUT_ocn6hr=6
   merge_file="${COMOUT_OCEAN_NETCDF}/${grid}/${RUN}.temp5m.t${cyc}z.${grid}.${FHOUT_ocn6hr}hr_avg.nc"
   cdo mergetime "${COMOUT_OCEAN_NETCDF}/${grid}/${RUN}.temp5m.t${cyc}z.${grid}.${FHOUT_ocn6hr}hr_avg.f*.nc" "${merge_file}"

   # 4. Compress
   nccopy -k 4 -d 5 "${merge_file}" "${merge_file}.tmp" && mv "${merge_file}.tmp" "${merge_file}"

   # 5. Cleanup fragments
   rm -f "${COMOUT_OCEAN_NETCDF}/${grid}/${RUN}.temp5m.t${cyc}z.${grid}.${FHOUT_ocn6hr}hr_avg.f"*".nc"

# -----------------------------------------------------------------------------
# Task B: Compute TCHP/d20c/oceanheat from daily and merge all daily files into
# one single file for each variable
# -----------------------------------------------------------------------------
    # 1. Prepare Command File
    ocean_daily_cmdfile="${DATA}/ocean_daily_cmds.txt"
    > "${ocean_daily_cmdfile}"

    # Step by 720 hours (30 days * 24 hours) and the first start_fhr=24 for daily files (720~=8784/12)
    for start_fhr in $(seq "${FHOUT_OCN}" 720 "${FHMAX_GFS}"); do
        echo "${PROCESS_OCEAN_DAILYSH} ${start_fhr} ${FHOUT_OCN}" >> "${ocean_daily_cmdfile}"
    done

    # 2. Run_MPMD
    "${RUN_MPMDSH}" "${ocean_daily_cmdfile}"
    diag_status=$?

    # 3. Final Consolidation (Merge each variable into one long time-series file)
    if [[ ${diag_status} -eq 0 ]]; then
        newvarslist=("dt20c" "TCHP" "ocnheat" "SST" "SSU" "SSV" "MLD_003" "MLD_0125" "ePBL" "latent" "sensible" "SW" "LW" "taux" "tauy" "temp" "uo" "vo" "so")

        for var in "${newvarslist[@]}"; do
            merge_file="${COMOUT_OCEAN_NETCDF}/${grid}/sfs.${var}.t00z.${grid}.daily.nc"
            # Merge and compress 1p00 files
            cdo mergetime "${COMOUT_OCEAN_NETCDF}/${grid}/sfs.${var}.t00z.${grid}.f*.nc" "${merge_file}"
            nccopy -k 4 -d 5 "${merge_file}" "${merge_file}.tmp" && mv "${merge_file}.tmp" "${merge_file}"
            if [[ -f "${merge_file}.tmp" ]]; then
               rm -f "${merge_file}.tmp"
            fi

            # Remove daily fragments
            rm -f "${COMOUT_OCEAN_NETCDF}/${grid}/sfs.${var}.t00z.${grid}.f"*".nc"
        done
        # Merge and Compress 0p25 SSH files
        merge_ssh_file="${COMOUT_OCEAN_NETCDF}/${grid}/sfs.SSH.t00z.0p25.daily.nc"
        cdo mergetime "${COMOUT_OCEAN_NETCDF}/${grid}/sfs.SSH.t00z.0p25.f*.nc" "${merge_ssh_file}"
        nccopy -k 4 -d 5 "${merge_ssh_file}" "${merge_ssh_file}.tmp" && mv "${merge_ssh_file}.tmp" "${merge_ssh_file}"
        if [[ -f "${merge_ssh_file}.tmp" ]]; then
           rm -f "${merge_ssh_file}.tmp"
        fi
    else
        echo "FATAL: MPMD diagnostics failed. Keeping input files for debugging."
        exit 1
    fi

# -----------------------------------------------------------------------------
# Task C: Generate monthly products from daily files and d20/TCHP/ocnheat monthly files
# -----------------------------------------------------------------------------
   # 0.  Link file names TO the output daily ocean regular grid (1p00,0p25) product files for monthly averaging
   # Array of dalily mean ocean output fhr (24, 48, ..., etc)
   OCEAN_OUTPUT_FH=($(seq -s ' ' "${FHMIN_GFS}" "${FHOUT_OCN}" "${FHMAX_GFS}"))
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
      new_file="ocn_${vdate_mid_str}.nc"
      new_ssh_file="ssh_${vdate_mid_str}.nc"
      ori_file="${RUN}.t${cyc}z.${grid}.f${fhr3}.nc"
      ori_ssh_file="${RUN}.SSH.t${cyc}z.0p25.f${fhr3}.nc"
      ${NLN} "${COMOUT_OCEAN_NETCDF}/${grid}/${ori_file}" "${DATAoutput}/MOM6_OUTPUT/${new_file}"
      ${NLN} "${COMOUT_OCEAN_NETCDF}/${grid}/${ori_ssh_file}" "${DATAoutput}/MOM6_OUTPUT/${new_ssh_file}"
      last_fhr=${fhr}
   done
   # 1. Check the final month is full or partial
   last_fh_output="${COMOUT_OCEAN_NETCDF}/${grid}/${RUN}.t${cyc}z.1p00.f${FHMAX_GFS}.nc"
   (( interval = 24 ))
   (( midpoint = FHMAX_GFS - interval/2 ))
   last_fhr_vdate_mid=$(date --utc -d "${current_cycle:0:8} ${current_cycle:8:2} + ${midpoint} hours" +%Y%m%d%H)

   # 1.1 Extract the last fcst date (Year/Month/Day from YYYYMMDD format):
   yyyy=${last_fhr_vdate_mid:0:4}
   mm=${last_fhr_vdate_mid:4:2}
   dd=${last_fhr_vdate_mid:6:2}
   # 1.2 Check leap or non-year for the last month of the year:
   if (( (${yyyy} % 4 == 0 && ${yyyy} % 100 != 0) || (${yyyy} % 400 == 0) )); then
      leap_yr="true"
   else
      leap_yr="false"
   fi
   # 1.3 Full Month Check: if the last month is a partial month or a full month?
   # 1.3.1 Check days for leap or non-leap February
   if [[ "${mm}" == "02" && "${leap_yr}" == "true" && "${dd}" == "29" ]]; then
      full_month="true"
   elif [[ "${mm}" == "02" && "${leap_yr}" == "false" && "${dd}" == "28" ]]; then
      full_month="true"
   # 1.3.2 Check for 31-day months
   elif [[ "${mm}" =~ ^(01|03|05|07|08|10|12)$ && "${dd}" == "31" ]]; then
      full_month="true"
   # 1.3.3 Check for 30-day months
   elif [[ "${mm}" =~ ^(04|06|09|11)$ && "${dd}" == "30" ]]; then
      full_month="true"
   else
      full_month="false"
   fi

   # 2. Expand the wildcard directly into an array (Avoids 'ls' issues)
   file_list_mon=( "${DATAoutput}"/MOM6_OUTPUT/ocn_????_??_01_12.nc )
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

   if (( ${#file_list_mon[@]} > 0 )); then
      ocean_monthly_cmdfile="${DATA}/ocean_monthly_cmds.txt"
      > "${ocean_monthly_cmdfile}"

      # Generate commands for each month
      for f in "${file_list_mon[@]}"; do
         f_name=$( basename "${f}" )
         YR="${f_name:4:4}"
         MN="${f_name:9:2}"
         echo "${PROCESS_OCEAN_MONTHLYSH} ${YR} ${MN}" >> "${ocean_monthly_cmdfile}"
      done

      # Run MPMD
   if [[ -s "${ocean_monthly_cmdfile}" ]]; then
       n_tasks=$(wc -l < "${ocean_monthly_cmdfile}")
       echo "INFO: Launching MPMD for monthly averaging with ${n_tasks} tasks."
       "${RUN_MPMDSH}" "${ocean_monthly_cmdfile}"
       err=$?
   fi

      if [[ ${err} -ne 0 ]]; then
         echo "FATAL ERROR: Failed MPMD monthly mean generation"
         exit "${err}"
      fi
   else
      echo "No full months found to process."
   fi

fi

#------------------------------------------------------------------------------
# CLEANUP
#------------------------------------------------------------------------------
rm -f "${DATA}"/*.txt
rm -f "${DATA}"/mpmd.*.out
# Remove daily SSH fragments
rm -f "${COMOUT_OCEAN_NETCDF}/${grid}/sfs.SSH.t00z.0p25.f"*".nc"
echo "Ocean post success! Removing remapped input files and history files."
rm -f "${COMOUT_OCEAN_NETCDF}/${grid}/sfs.t00z.${grid}.f"*".nc"
rm -f "${COMOUT_OCEAN_HISTORY}/sfs."*".nc"
echo "INFO: Cleanup Complete. Workflow status: SUCCESS"
echo "End OCEAN POST work"
###############################################################################
# End JOB SPECIFIC work
###############################################################################

exit 0
