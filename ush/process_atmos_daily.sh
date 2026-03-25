#!/usr/bin/env bash
# process_atmos_daily.sh: Handles Stage 2 Daily Processing (supports 13+ months)
# Optimized for MPMD mode with physical temp files for wgrib2 random access.

set -e

lastfile=$(ls "${COMIN_ATMOS_MASTER}"/sfs.t"${cyc}"z.master.f*.grib2 | sort -V | tail -1)
lastftimemsg=$(${WGRIB2} "${lastfile}" -d 1 -ftime2)
lastftime="${lastftimemsg% hour fcst}"
export lastfhr=${lastftime:4:4}
echo "INFO: Total forecast length is ${lastfhr} hours."

# 1. Accept arguments for the ONE month this specific task is responsible for
i=$1               # Month Index (0, 1, 2... 12+)
daysf=$2           # Continuous cumulative days at end of this month
month_days=$3      # Total days in this specific month
filename_start=$4  # Prefix (e.g., MEM008.1992030100.1992)
lastfhr=$5         # Last forecast hours for the cycle

# 2. Dynamic Date Calculation for the Valid Year/Month
idate_yyyy="${current_cycle:0:4}"
idate_mm="${current_cycle:4:2}"
idate_dd="${current_cycle:6:2}"

valid_date=$(date -d "${idate_yyyy}-${idate_mm}-${idate_dd} +${i} months" +%Y%m)
vyr="${valid_date:0:4}"
vmm="${valid_date:4:2}"
filemm="${vmm}"

# Calculate the starting hour offset for this month
day_offset=$(( daysf - month_days ))

# Create UNIQUE working directories to prevent MPMD race conditions
tmp_acc_work_dir="${OUTDIR}/tmp_acc_${vyr}${vmm}_${MEMDIR}"
tmp_inst_work_dir="${OUTDIR}/tmp_inst_${vyr}${vmm}_${MEMDIR}"
mkdir -p "${tmp_acc_work_dir}" "${tmp_inst_work_dir}"

#---------------------------------------------------------
# LOOP THROUGH EACH DAY OF THE MONTH (1 to month_days)
#---------------------------------------------------------
for (( d=1; d<=$month_days; d++ )); do
    
    end_of_day_fhr=$(( (day_offset + d) * 24 ))
    
    # Skip if we exceed the total available forecast length
    if [ "$end_of_day_fhr" -gt "$lastfhr" ]; then 
       echo "DEBUG: Skipping Day $d because $end_of_day_fhr > $lastfhr"
       continue 
    fi

    # --- PART A: ACCUMULATED VARIABLES (Hours: E-18, E-12, E-6, E) ---
    all_acc_exist=true
    list_acc=""
    for hr in $((end_of_day_fhr - 18)) $((end_of_day_fhr - 12)) $((end_of_day_fhr - 6)) ${end_of_day_fhr}; do
        fpath="${COMIN_ATMOS_MASTER}/sfs.t${cyc}z.master.f$(printf "%03d" $hr).grib2"
        if [[ ! -f "$fpath" ]]; then all_acc_exist=false; break; fi
        list_acc+="$fpath "
    done

    if [ "$all_acc_exist" = true ]; then
        raw_tmp_acc="${tmp_acc_work_dir}/day_${d}_raw.grb"
        # Process from physical file (fixes "cannot random access stdin" error)
        ${GMERGE} "${raw_tmp_acc}" ${list_acc}
        ${WGRIB2} "${raw_tmp_acc}" -match "${dailyaccvars}" -merge_fcst 4 "${tmp_acc_work_dir}/daily_acc${d}.grb"
        rm -f "${raw_tmp_acc}"
    fi

    # --- PART B: INSTANTANEOUS VARIABLES (Hours: E-24, E-18, E-12, E-6, E) ---
    all_inst_exist=true
    list_inst=""
    for hr in $((end_of_day_fhr - 24)) $((end_of_day_fhr - 18)) $((end_of_day_fhr - 12)) $((end_of_day_fhr - 6)) $((end_of_day_fhr)); do
        fpath="${COMIN_ATMOS_MASTER}/sfs.t${cyc}z.master.f$(printf "%03d" $hr).grib2"
        if [[ ! -f "$fpath" ]]; then all_inst_exist=false; break; fi
        list_inst+="$fpath "
    done

    if [ "$all_inst_exist" = true ]; then
        raw_tmp_inst="${tmp_inst_work_dir}/day_${d}_raw.grb"
        ${GMERGE} "${raw_tmp_inst}" ${list_inst}
        ${WGRIB2} "${raw_tmp_inst}" -match "${dailyinstvars}" -fcst_ave 6hr "${tmp_inst_work_dir}/daily_inst${d}.grb"
        rm -f "${raw_tmp_inst}"
    fi
done

#---------------------------------------------------------
# CONSOLIDATE FRAGMENTS INTO MONTHLY FILES (Array Method)
#---------------------------------------------------------
# Paths
OUTDIR="${COMOUT_ATMOS_GRIB}"
mkdir -m 755 -p "${OUTDIR}"
mkdir -p "${OUTDIR}/acc.daily.${MEMDIR}" "${OUTDIR}/inst.daily.${MEMDIR}"
mkdir -p "${OUTDIR}/acc.monthly.${MEMDIR}" "${OUTDIR}/inst.monthly.${MEMDIR}"
dest_acc="${OUTDIR}/acc.daily.${MEMDIR}/acc.daily.${filename_start}${filemm}${filename_end}"
dest_inst="${OUTDIR}/inst.daily.${MEMDIR}/inst.daily.${filename_start}${filemm}${filename_end}"

# 1. Consolidate Accumulated (ACC)
if [ -d "${tmp_acc_work_dir}" ]; then
    # Clear array and fill it using mapfile
    unset acc_files
    mapfile -t acc_files < <(ls -v "${tmp_acc_work_dir}"/daily_acc*.grb 2>/dev/null)
    
    if [ ${#acc_files[@]} -gt 0 ]; then
        echo "INFO: Task $i merging ${#acc_files[@]} days for ACC using array expansion."
        # Use "${acc_files[@]}" to pass each file as a unique argument
        ${GMERGE} "${dest_acc}" "${acc_files[@]}"
    else
        echo "WARNING: Task $i found NO daily_acc files to merge."
    fi
fi

# 2. Consolidate Instantaneous (INST)
if [ -d "${tmp_inst_work_dir}" ]; then
    unset inst_files
    mapfile -t inst_files < <(ls -v "${tmp_inst_work_dir}"/daily_inst*.grb 2>/dev/null)
    
    if [ ${#inst_files[@]} -gt 0 ]; then
        echo "INFO: Task $i merging ${#inst_files[@]} days for INST using array expansion."
        ${GMERGE} "${dest_inst}" "${inst_files[@]}"
    else
        echo "WARNING: Task $i found NO daily_inst files to merge."
    fi
fi

# 3 Final clean up
rm -rf "${tmp_acc_work_dir}" "${tmp_inst_work_dir}"
exit 0
