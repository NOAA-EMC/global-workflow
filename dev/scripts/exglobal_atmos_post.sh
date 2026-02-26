#!/usr/bin/env bash
set -e
source "${USHgfs}/preamble.sh"
#------------------------------------------------------------------------------
# AUTO-CLEANUP: CONVERT WINDOWS (CRLF) TO UNIX (LF) FORMAT
# This prevents the "/usr/bin/cat: invalid option -- 'm'" error.
#------------------------------------------------------------------------------
#------------------------------------------------------------------------------
# 0. VALIDATE SCRIPTS FOR ATMOS POST-PROCESSES
#------------------------------------------------------------------------------

echo "INFO: Validating script line endings..."

# Scripts used
PROCESS_ATMOS_6HRLYSH=${PROCESS_ATMOS_6HRLYSH:-"${USHgfs}/process_atmos_6hrly.sh"}
PROCESS_ATMOS_DAILYSH=${PROCESS_ATMOS_DAILYSH:-"${USHgfs}/process_atmos_daily.sh"}
RUN_MPMDSH=${RUN_MPMDSH:-"${USHgfs}/run_mpmd.sh"}

# List all scripts involved in the workflow
workflow_scripts=(
    "${0}" 
    "${PROCESS_ATMOS_6HRLYSH}"
    "${PROCESS_ATMOS_DAILYSH}" 
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

# Check if GMERGE is set; if not, print error and exit with status 1
if [[ -z "${GMERGE:-}" ]]; then
    echo "Error: GMERGE is not defined. Exiting script." >&2
    exit 1
fi

# Check if WGRIB2 is set; if not, print error and exit with status 1
if [[ -z "${WGRIB2:-}" ]]; then
    echo "Error: WGRIB2 is not defined. Exiting script." >&2
    exit 1
fi

#------------------------------------------------------------------------------
# 1. ENVIRONMENT SETUP & COMMON VARIABLES
#------------------------------------------------------------------------------
export USE_CFP=YES

# Variables for Stage 1 (Variable Extraction)
declare -a vars=( "DLWRF:surface" "DSWRF:surface" "ULWRF:surface" "USWRF:surface" "ULWRF:top of atmosphere" "LHTFL" "SHTFL" "PRMSL" "PRATE" ":TMP:2 m above" "TMAX:2 m above" "TMIN:2 m above" "DPT:2 m above" "HGT:200 mb" "HGT:500 mb" "HGT:700 mb" "HGT:850 mb" "SPFH:500 mb" "SPFH:700 mb" "SPFH:850 mb" "SPFH:925 mb" ":TMP:50 mb" ":TMP:200 mb" ":TMP:500 mb" ":TMP:700 mb" ":TMP:850 mb" "TCDC" "ICEC" "TSOIL:0-0.1" "SOILM" "WATR" "WEASD" "LAND" "HGT:surface" "(UGRD|VGRD):10 m above" "(UGRD|VGRD):200 mb" "(UGRD|VGRD):500 mb" "(UGRD|VGRD):700 mb" "(UGRD|VGRD):850 mb" "(UGRD|VGRD):925 mb" "(UFLX|VFLX)" ":LFTX:surface" ":CAPE:surface" ":RH:2 m above" ":HLCY:3000-0 m above" "(MAXUW|MAXVW)" "WIND:10 m above ground" )

declare -a filevars=( "dlwrfsfc" "dswrfsfc" "ulwrfsfc" "uswrfsfc" "ulwrftoa" "lhtflsfc" "shtflsfc" "prmsl" "prate" "tmp2m" "tmax2m" "tmin2m" "dpt2m" "hgt200mb" "hgt500mb" "hgt700mb" "hgt850mb" "spfh500mb" "spfh700mb" "spfh850mb" "spfh925mb" "tmp50mb" "tmp200mb" "tmp500mb" "tmp700mb" "tmp850mb" "tcdc" "icec" "tsoil0_10cm" "soilm" "watr" "weasd" "land" "hgtsfc" "wind10m" "wind200mb" "wind500mb" "wind700mb" "wind850mb" "wind925mb" "flux" "lftxsfc" "capesfc" "rh2m" "hlcy3000_0m" "maxwind10m" "maxwindspeed")

# Variables for Stage 2 (Daily Means)
dailyinstvars="(:TMP|UGRD|VGRD):(2|5|10|30|50|100|200|250|300|500|600|700|850|925|1000) mb|HGT:(2|5|10|30|50|100|200|500|700|850|1000) mb|SPFH:(5|30|100|200|300|500|600|700|850|925|1000) mb|VVEL:500 mb|(STRM|VPOT):(200|850) mb|(PRES|HGT|:TMP|CNWAT|WEASD|PEVPR|ICETK|WILT|FLDCP|SUNSD|:LFTX|CAPE|LAND|ICEC|FDNSSTMP|CPOFP):surface|TMP:1 hybrid|(PVORT|:TMP):(450|550|650) K|(TSOIL|SOILW|SOILL):(0-0.1|0.1-0.4|0.4-1|1-2)|SOILM|(:TMP|SPFH|DPT|RH):2 m above|(UGRD|VGRD):10 m above|PRMSL|MSLET|PWAT|TOZNE"
dailyaccvars="(ACPCP|APCP|NCPCP|CPRAT|PRATE|LHTFL|SHTFL|GFLUX|SNOHF|UFLX|VFLX|WATR|DLWRF|DSWRF|ULWRF|USWRF|CDUVB|NDDSF|VDDSF|CSDLF|CSDSF|CSUSF):surface|TSNOWP:surface|TMAX|TMIN|MAXUW|MAXVW|(USWRF|ULWRF|DSWRF):top of atmosphere|TCDC:entire atmosphere|WIND:10 m above ground"

# Variables for Stage 3 (Monthly Means)
monthlyinstvars="(:TMP|UGRD|VGRD|STRM|VPOT):(200|850) mb|HGT:(200|500|700|850) mb|(:TMP|WEASD|CPOFP|LAND):surface|SOILW:(0-0.1|0.1-0.4|0.4-1|1-2)|SOILM|(:TMP|SPFH|DPT|RH):2 m above|(UGRD|VGRD):10 m above|PRMSL"
monthlyaccvars="(ACPCP|APCP|NCPCP|PRATE|LHTFL|SHTFL|UFLX|VFLX|CDUVB|DLWRF|USWRF|WATR):surface|TSNOWP:surface|TMAX|TMIN|ULWRF:top of atmosphere"

export filename_end=".grib.t${cyc}z.grb2"

# Paths
OUTDIR="${COMOUT_ATMOS_GRIB}"
mkdir -m 755 -p "${OUTDIR}"
mkdir -p "${OUTDIR}/acc.daily.${MEMDIR}" "${OUTDIR}/inst.daily.${MEMDIR}"
mkdir -p "${OUTDIR}/acc.monthly.${MEMDIR}" "${OUTDIR}/inst.monthly.${MEMDIR}"

# Explicitly export variables:
# 1.1. Processing Tools
export WGRIB2="${WGRIB2}"
export GMERGE="${GMERGE}"

# 1.2. Variable Lists (the regex strings)
export dailyinstvars="${dailyinstvars}"
export dailyaccvars="${dailyaccvars}"
export monthlyinstvars="${monthlyinstvars}"
export monthlyaccvars="${monthlyaccvars}"

# 1.3. Path & Cycle Info

export COMIN_ATMOS_MASTER="${COMIN_ATMOS_MASTER}"
export OUTDIR="${OUTDIR}"
export MEMDIR="${MEMDIR}"
export current_cycle="${current_cycle}"
export cyc="${cyc}"
export filename_end="${filename_end}"
# -----------------------------------------------------------------------------
# 2. START TO PROCESS ATMOSPHERIC VARIABLES
# -----------------------------------------------------------------------------
echo "Start to Process Atmospheric variables"

# Remove all generated products if the previous jobs failed
rm -rf "${OUTDIR}"/*

# Determine vt_date and lastfhr (Required for all stages)
firstfile="${COMIN_ATMOS_MASTER}/sfs.t${cyc}z.master.f000.grib2"
vt_init=$(${WGRIB2} "${firstfile}" -d 1 -vt)
vt_date=${vt_init:7:10}
yy_init=${vt_init:7:4}
mm_init=$((10#${vt_init:11:2})) # Force base-10 to avoid '08' octal errors

lastfile=$(ls "${COMIN_ATMOS_MASTER}"/sfs.t"${cyc}"z.master.f*.grib2 | sort -V | tail -1)
echo "${lastfile}"

lastftimemsg=$(${WGRIB2} "${lastfile}" -d 1 -ftime2)
lastftime="${lastftimemsg% hour fcst}"
export lastfhr=${lastftime:4:4}
echo "INFO: Total forecast length is ${lastfhr} hours."

vt_final=$(${WGRIB2} "${lastfile}" -d 1 -vt)
yy_final=${vt_final:7:4}
mm_final=$((10#${vt_final:11:2}))
dd_final=$((10#${vt_final:13:2}))

months_in_year=("01" "02" "03" "04" "05" "06" "07" "08" "09" "10" "11" "12")
month_days_in_year=("31" "28" "31" "30" "31" "30" "31" "31" "30" "31" "30" "31")
# Leap Year Check

if [ $((yy_init % 4)) -eq 0 ] && ([ $((yy_init % 100)) -ne 0 ] || [ $((yy_init % 400)) -eq 0 ]); then
    month_days_in_year[1]=29
fi

#------------------------------------------------------------------------------
# STAGE 1: VARIABLE EXTRACTION (6-HOURLY)
#------------------------------------------------------------------------------
echo "INFO: Starting Stage 1 - Variable Extraction"

if [[ -f "${DATA}/mpmd_s1_extract.txt" ]]; then
   rm -f "${DATA}/mpmd_s1_extract.txt"
fi

cmdfile_s1="${DATA}/mpmd_s1_extract.txt"
> "${cmdfile_s1}"

for (( i=0; i<${#vars[@]}; i++)); do
  filename="${filevars[$i]}.${MEMDIR}.${vt_date}.6hourly.grb2"
  output_path="${OUTDIR}/${filename}"
  # CALL THE WRAPPER SCRIPT
  echo "${PROCESS_ATMOS_6HRLYSH} '${vars[$i]}' '${output_path}'" >> "${cmdfile_s1}"
done

if [[ -s "${cmdfile_s1}" ]]; then
  "${RUN_MPMDSH}" "${cmdfile_s1}"
   err=$?
fi

if [[ ${err} -ne 0 ]]; then
   echo "FATAL ERROR: Failed to generate 6-hourly grib2 files"
   exit "${err}"
else
   echo "INFO: Stage 1 Complete."
fi

#--------------------------------------------------------------------------------------
# STAGE 2: DAILY MEANS (ACCUMULATED & INSTANTANEOUS) 
# Handles 13+ months, leap years, and prevents MPMD collisions
#--------------------------------------------------------------------------------------
echo "INFO: Starting Stage 2 - Daily Averaging"

# Ensure all necessary environment variables are exported for the child tasks
export dailyaccvars dailyinstvars COMIN_ATMOS_MASTER cyc OUTDIR MEMDIR lastfhr current_cycle GMERGE WGRIB2

if [[ -f "${DATA}/mpmd_s2_daily.txt" ]]; then
   rm -f "${DATA}/mpmd_s2_daily.txt"
fi

cmdfile_s2="${DATA}/mpmd_s2_daily.txt"
> "${cmdfile_s2}"

# 1. Calculate total months across year boundaries
exp_months=$(( (yy_final - yy_init) * 12 + (mm_final - mm_init) + 1 ))

# 2. Generate the MPMD command file
total_fcst_days=$(( lastfhr / 24 ))
current_daysf=0
for (( i=0; i<exp_months; i++ )); do
    
    # Calculate the Valid Year and Month for this forecast segment
    v_date=$(date -d "${current_cycle:0:8} +${i} months" +%Y%m%d)
    v_year=${v_date:0:4}
    v_month=${v_date:4:2}

    # Get exact days in THIS specific month (Handles Leap Years in Year 2+)
    cal_m_days=$(date -d "${v_year}-${v_month}-01 +1 month -1 day" +%d)
    
    # Start day of this month
    m_start_day=$current_daysf
    
    # Calculate how many days of this month actually exist in the forecast
    days_left=$(( total_fcst_days - m_start_day ))
    
    if [ "$days_left" -le 0 ]; then break; fi # No more data
    
    if [ "$days_left" -lt "$cal_m_days" ]; then
        actual_m_days=$days_left
    else
        actual_m_days=$cal_m_days
    fi
    # Update cumulative days for the NEXT month's offset
    current_daysf=$((current_daysf + actual_m_days))

    # Define filename prefix (e.g., MEM000.1992030100.1993)
    filename_start="${MEMDIR}.${current_cycle}.${v_year}"

    # Construct the command line for the MPMD file
    # Args: Index (i), TotalDays (daysf), MonthDays (actual m_days),Prefix (Filename_start), lastfhr 
    echo "${PROCESS_ATMOS_DAILYSH} ${i} ${current_daysf} ${actual_m_days} ${filename_start} ${lastfhr}" >> "${cmdfile_s2}"
done

# 3. Dynamically count tasks and execute
if [[ -s "${cmdfile_s2}" ]]; then
    n_tasks=$(wc -l < "${cmdfile_s2}")
    echo "INFO: Launching Stage 2 MPMD with ${n_tasks} months."
    
    # Update the RUN_MPMDSH call
    # Note: Ensure n_tasks matches your Slurm allocation
    "${RUN_MPMDSH}" "${cmdfile_s2}"
    err=$?
fi

if [[ ${err} -ne 0 ]]; then
    echo "FATAL ERROR: Stage 2 Daily Mean generation failed."
    exit "${err}"
else
    echo "INFO: Stage 2 Complete."
fi

#------------------------------------------------------------------------------
# STAGE 3: MONTHLY MEANS (strictly excludes partial months)
#------------------------------------------------------------------------------
echo "INFO: Starting Stage 3 - Monthly Averaging"

if [[ -f "${DATA}/mpmd_s3_monthly.txt" ]]; then
   rm -f "${DATA}/mpmd_s3_monthly.txt"
fi

cmdfile_s3="${DATA}/mpmd_s3_monthly.txt"
> "${cmdfile_s3}"

# Read the files created in Stage 2
accfilelist=( "${OUTDIR}/acc.daily.${MEMDIR}"/* )
insfilelist=( "${OUTDIR}/inst.daily.${MEMDIR}"/* )

# PARTIAL MONTH LOGIC: If dd_final != 01, the very last file in the list is
# the partial month. We remove it from the processing array.
if (( dd_final != 1 )); then
    echo "INFO: Final month is partial (Day $dd_final). Skipping monthly average for this month."
    [[ ${#accfilelist[@]} -gt 0 ]] && unset 'accfilelist[${#accfilelist[@]}-1]'
    [[ ${#insfilelist[@]} -gt 0 ]] && unset 'insfilelist[${#insfilelist[@]}-1]'
fi

# Generate tasks for remaining full months

for file in "${accfilelist[@]}"; do
    filesuffix=$(echo "${file##*/}" | cut -d '.' -f 4-10)
    echo "${WGRIB2} ${file} -match '${monthlyaccvars}' -fcst_ave 24hr ${OUTDIR}/acc.monthly.${MEMDIR}/acc.monthly.${filesuffix}" >> "${cmdfile_s3}"
done

for file in "${insfilelist[@]}"; do
    filesuffix=$(echo "${file##*/}" | cut -d '.' -f 4-10)
    echo "${WGRIB2} ${file} -match '${monthlyinstvars}' -fcst_ave 24hr ${OUTDIR}/inst.monthly.${MEMDIR}/inst.monthly.${filesuffix}" >> "${cmdfile_s3}"
done

if [[ -s "${cmdfile_s3}" ]]; then
    echo "INFO: Launching $(wc -l < ${cmdfile_s3}) monthly mean tasks in parallel."
    ${RUN_MPMDSH} "${cmdfile_s3}"
    err=$?
else
    echo "WARNING: No full months available for averaging!"
    err=0
fi

if [[ ${err} -ne 0 ]]; then
    echo "FATAL ERROR: Failed to generate monthly mean grib2 files"
    exit "${err}"
else
    echo "INFO: Stage 3 -- Monthly Processing Complete."
fi

#------------------------------------------------------------------------------
# CLEANUP
#------------------------------------------------------------------------------
rm -f "${COMIN_ATMOS_MASTER}/sfs.t${cyc}z.master.f"*".grib2"
rm -f "${DATA}"/mpmd_s*.txt
find "${OUTDIR}" -type d -name "tmp_m[0-9][0-9]_${MEMDIR}" -empty -delete
echo "INFO: Cleanup Complete. Workflow status: SUCCESS"

exit 0
