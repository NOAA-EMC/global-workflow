#!/bin/bash

###########################################################################################

# GENERATE DAILY MEAN GRIB2 FILES FOR SFS MASTER 6-HOURLY DATA FILES. THIS SCRIPT 
# GENERATES 2 DIFFERENT KINDS OF DATASETS: DAILY GRIB2 FILES FOR 
# ACC/AVE/MIN/MAX AND INSTANTANEOUS VALUES. FOR ACC/AVE/MIN/MAX VARIABLES, THE DAILY 
# ACC/AVE/MIN/MAX IS COMPUTED FROM THE 6-HOURLY.

# THIS SCRIPT WILL BE CALLED FROM G-W JOBS, WHERE THE FOLLOWING VARIABLES WILL BE
# PREDETERIMED:

###  COMIN_ATMOS_MASTER: path to directory with the SFS master files for a member
###  cyc: cycle of MEMDIR data 
###  MEMDIR: ensemble member of MEMDIR data
###  GMERGE: path to gmerge executable file
###  OUTDIR: path to directory where both daily and monthly means will be saved

##########################################################################################
# Construct COM variables from templates
YMD=${PDY} HH=${cyc} declare_from_tmpl -rx \
    COMIN_ATMOS_MASTER:COM_ATMOS_MASTER_TMPL  \
    COMOUT_ATMOS_GRIB:COM_ATMOS_GRIB_TMPL

OUTDIR="${COMOUT_ATMOS_GRIB}"
#GMERGE="/ncrc/home1/Yangxing.Zheng/wgrib2/gmerge"
mkdir -m 755 -p "${OUTDIR}"
mkdir -m 755 -p "${OUTDIR}/acc.daily.${ENSMEM}"
mkdir -m 755 -p "${OUTDIR}/acc.monthly.${ENSMEM}"
mkdir -m 755 -p "${OUTDIR}/inst.daily.${ENSMEM}"
mkdir -m 755 -p "${OUTDIR}/inst.monthly.${ENSMEM}"

# Lists of variables
dailyinstvars="(:TMP|UGRD|VGRD):(2|5|10|30|50|100|200|250|300|500|600|700|850|925|1000) mb|HGT:(2|5|10|30|50|100|200|500|700|850|1000) mb|SPFH:(5|30|100|200|300|500|600|700|850|925|1000) mb|VVEL:500 mb|(STRM|VPOT):(200|850) mb|(PRES|HGT|:TMP|CNWAT|WEASD|PEVPR|ICETK|WILT|FLDCP|SUNSD|:LFTX|CAPE|LAND|ICEC|FDNSSTMP|CPOFP):surface|TMP:1 hybrid|(PVORT|:TMP):(450|550|650) K|(TSOIL|SOILW|SOILL):(0-0.1|0.1-0.4|0.4-1|1-2)|SOILM|(:TMP|SPFH|DPT|RH):2 m above|(UGRD|VGRD):10 m above|PRMSL|MSLET|PWAT|TOZNE" 
dailyaccvars="(ACPCP|APCP|NCPCP|CPRAT|PRATE|LHTFL|SHTFL|GFLUX|SNOHF|UFLX|VFLX|WATR|DLWRF|DSWRF|ULWRF|USWRF|CDUVB|NDDSF|VDDSF|CSDLF|CSDSF|CSUSF):surface|TSNOWP:surface|TMAX|TMIN|MAXUW|MAXVW|(USWRF|ULWRF|DSWRF):top of atmosphere|TCDC:entire atmosphere"

firstfile="${COMIN_ATMOS_MASTER}/sfs.t${cyc}z.master.grb2f000"

if [[ -s "${COMIN_ATMOS_MASTER}"/sfs.t"${cyc}"z.master.grb2f1002 ]]; then
  lastfile=$(find "${COMIN_ATMOS_MASTER}"/sfs.t"${cyc}"z.master.grb2f???? | sort -V | tail -1)
else
  lastfile=$(find "${COMIN_ATMOS_MASTER}"/sfs.t"${cyc}"z.master.grb2f??? | sort -V | tail -1)
fi

# get validation date of first file
vt_init=$(wgrib2 "${firstfile}" -d 1 -vt)
vt_date=${vt_init:7:10}  # for filename
yy_init=${vt_init:7:4}
yy_init_next=$((yy_init+1))
mm_init=${vt_init:11:2}

# get dates and times of last file
lastftimemsg=$(wgrib2 "${lastfile}" -d 1 -ftime2)
lastftime="${lastftimemsg% hour fcst}"
lastfhr=${lastftime:4:4}
vt_final=$(wgrib2 "${lastfile}" -d 1 -vt)
mm_final=${vt_final:11:2}
dd_final=${vt_final:13:2}
yy_final=${vt_final:7:4}

# set filenames for valid date year and following year
filename_start="${MEMDIR}.${vt_date}.${yy_init}"
filename_start_next="${MEMDIR}.${vt_date}.${yy_init_next}"
filename_end=".grib.t${cyc}z.grb2"

#### Set indexes for finding months of validation date for loops
months_in_year=("01" "02" "03" "04" "05" "06" "07" "08" "09" "10" "11" "12")
start_idx=$((mm_init-1))

# if the last file vt date ends on day 01, do not loop over it.
if (( dd_final == 01 )); then
  end_idx=$((mm_final-2))
else
  end_idx=$((mm_final-1))
fi

#### check for leap year
itime=$(wgrib2 -t "${firstfile}"|head -1|cut -d= -f2)
for i in {1..12}
do
  yyyy=${itime:0:4}
  mm=${itime:4:2}

  ndays=$(date -d "$yyyy-$mm-01 +$i month -1 day" "+%d")

  if (( "$ndays"==28 )); then
    month_days_in_year=("31" "28" "31" "30" "31" "30" "31" "31" "30" "31" "30" "31")
    break
  fi
  if (( "$ndays"==29 )); then
    month_days_in_year=("31" "29" "31" "30" "31" "30" "31" "31" "30" "31" "30" "31")
    break
  fi 
done

### If the end month is higher than start month, loop once. Otherwise loop twice for
### start month to end of year and beginning of year to start month
if (( start_idx < end_idx )); then
  end_loop_idx=$end_idx  # one loop, start to end month
else
  end_loop_idx=$((${#months_in_year[@]}-1)) # there will be two loops, the first one from start month to end of year
fi

daysf=0   # day no. at end of month

# loop from valid date start month to end of calendar year OR end month
for (( i=start_idx; i<end_loop_idx+1; i++ ))
do
  daysf=$((daysf+month_days_in_year[i]))
  daysi=$((daysf-month_days_in_year[i]))

  fhi=$((daysi*24+6))  # initial fhr for start of month (acc values)
  fhf=$((daysf*24))    # final fhr for end of month

  # make sure the last fhr exists
  if [ "$fhf" -gt "$lastfhr" ]; then
    fhf=$lastfhr
  fi

  ### Make list of files for the whole month
  list=$(seq -f "${COMIN_ATMOS_MASTER}/sfs.t${cyc}z.master.grb2f%03.0f" "${fhi}" 6 "${fhf}")
  
  # month of loop for filename
  filemm="${months_in_year[$i]}"

  #merge the min/max/acc/ave variables into daily periods
  # shellcheck disable=SC2086
  # shellcheck disable=SC2086
  ${GMERGE} - ${list} | wgrib2 - -match "${dailyaccvars}" -merge_fcst 4 "${OUTDIR}/acc.daily.${MEMDIR}/acc.daily.${filename_start}${filemm}${filename_end}"

  # daily averages for instantaneous variables
  for j in $(seq "${fhi}" 24 "${fhf}")
  do
    start_hr=$((j-6))
    end_hr=$((j+24-6))
    list_6hrly=$(seq -f "${COMIN_ATMOS_MASTER}/sfs.t${cyc}z.master.grb2f%03.0f" $start_hr 6 $end_hr)
    # shellcheck disable=SC2086
    ${GMERGE} - ${list_6hrly} | wgrib2 - -match "${dailyinstvars}" -fcst_ave 6hr "${OUTDIR}/inst.daily.${MEMDIR}/daily_${end_hr}.grb"
  done

  list_daily=$(ls -v "${OUTDIR}"/inst.daily."${MEMDIR}"/daily_*.grb)

  #### merge all days into single grib2 file and remove unneeded files
  # shellcheck disable=SC2086
  ${GMERGE} - ${list_daily} | wgrib2 - -grib "${OUTDIR}/inst.daily.${MEMDIR}/inst.daily.${filename_start}${filemm}${filename_end}"
  rm "${OUTDIR}"/inst.daily."${MEMDIR}"/daily*.grb
  
done

### This second loop needs to be done if the end month is earlier
### than the start month or the same (e.g., full year run)
### also making sure it is not the same month and year (just one month data)
if (( start_idx==end_idx )) || (( end_idx < start_idx )) && (( $yy_init != $yy_final )); then

# loop from start of calendar year to valid date month
for (( i=0; i<end_idx+1; i++ ))
do
  daysf=$((daysf+month_days_in_year[i]))
  daysi=$((daysf-month_days_in_year[i]))

  fhi=$((daysi*24+6))  # initial fhr for start of month (acc values)      
  fhf=$((daysf*24))    # final fhr for end of month   

  # make sure the last fhr exists
  if [ "$fhf" -gt "$lastfhr" ]; then
    fhf=$lastfhr
  fi

  ### Make list of files for the whole month
  list=$(seq -f "${COMIN_ATMOS_MASTER}/sfs.t${cyc}z.master.grb2f%03.0f" "${fhi}" 6 "${fhf}")

  # month of loop for filename
  filemm="${months_in_year[$i]}"

  #merge the min/max/acc/ave variables into daily periods
  # shellcheck disable=SC2086
  ${GMERGE} - ${list} | wgrib2 - -match "${dailyaccvars}" -merge_fcst 4 "${OUTDIR}/acc.daily.${MEMDIR}/acc.daily.${filename_start_next}${filemm}${filename_end}"
 
  # daily averages for instantaneous variables
  for j in $(seq "${fhi}" 24 "${fhf}")
  do
    start_hr=$((j-6))
    end_hr=$((j+24-6)) 
    list_6hrly=$(seq -f "${COMIN_ATMOS_MASTER}/sfs.t${cyc}z.master.grb2f%03.0f" $start_hr 6 $end_hr)
    # shellcheck disable=SC2086
    ${GMERGE} - ${list_6hrly} | wgrib2 - -match "${dailyinstvars}" -fcst_ave 6hr "${OUTDIR}/inst.daily.${MEMDIR}/daily_${end_hr}.grb"
  done

  list_daily=$(ls -v "${OUTDIR}"/inst.daily."${MEMDIR}"/daily_*.grb)

  #### merge all days into single grib2 file and remove unneeded files
  # shellcheck disable=SC2086
  ${GMERGE} - ${list_daily} | wgrib2 - -grib "${OUTDIR}/inst.daily.${MEMDIR}/inst.daily.${filename_start_next}${filemm}${filename_end}"
  rm "${OUTDIR}"/inst.daily."${MEMDIR}"/daily*.grb

done

fi  # end of if block for checking end month vs. start month
