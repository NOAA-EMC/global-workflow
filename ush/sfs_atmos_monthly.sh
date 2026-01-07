#!/bin/bash

set -ux
set -e

###########################################################################################

# GENERATE MONTHLY MEAN GRIB2 FILES FOR SFS MASTER 6-HOURLY DATA FILES. THIS SCRIPT 
# GENERATES 2 DIFFERENT KINDS OF DATASETS: MONTHLY GRIB2 FILES FOR ACC/AVE/MIN/MAX
# AND INSTANTANEOUS VALUES. FOR ACC/AVE/MIN/MAX VARIABLES, THE MONTHLY IS COMPUTED
# FROM DAILY ACC/AVE/MIN/MAX RESULTS COMPUTED WITH THE SFS_ATMOS_DAILY.SH UTILITY.

# THIS SCRIPT WILL BE CALLED FROM G-W JOBS, WHERE THE FOLLOWING VARIABLES WILL BE
# PREDETERIMED:

###  MEMDIR: ensemble member of MEMDIR (directory with SFS run) data
###  OUTDIR: path to directory where monthly means will be saved

##########################################################################################
# Construct COM variables from templates
YMD=${PDY} HH=${cyc} declare_from_tmpl -rx \
    COMIN_ATMOS_MASTER:COM_ATMOS_MASTER_TMPL  \
    COMOUT_ATMOS_GRIB:COM_ATMOS_GRIB_TMPL

OUTDIR="${COMOUT_ATMOS_GRIB}"
mkdir -m 755 -p "${OUTDIR}/acc.monthly.${MEMDIR}"
mkdir -m 755 -p "${OUTDIR}/inst.monthly.${MEMDIR}"

# Lists of variables
monthlyinstvars="(:TMP|UGRD|VGRD|STRM|VPOT):(200|850) mb|HGT:(200|500|700|850) mb|(:TMP|WEASD|CPOFP|LAND):surface|SOILW:(0-0.1|0.1-0.4|0.4-1|1-2)|SOILM|(:TMP|SPFH|DPT|RH):2 m above|(UGRD|VGRD):10 m above|PRMSL"
monthlyaccvars="(ACPCP|APCP|NCPCP|PRATE|LHTFL|SHTFL|UFLX|VFLX|CDUVB|DLWRF|USWRF|WATR):surface|TSNOWP:surface|TMAX|TMIN|ULWRF:top of atmosphere"

# loop through the daily files and get the monthly means

for file in "${OUTDIR}/acc.daily.${MEMDIR}"/*; do
  filename=${file##*/}
  filesuffix=$(echo "${filename}" | cut -d '.' -f 4-10)
  ${WGRIB2} "${file}" -match "${monthlyaccvars}" -fcst_ave 24hr "${OUTDIR}/acc.monthly.${MEMDIR}/acc.monthly.${filesuffix}"
  if (( ${?} > 0 )); then
    echo "FATAL ERROR: WGRIB2 is not defined"
    exit 1
  fi
done

for file in "${OUTDIR}/inst.daily.${MEMDIR}"/*; do
  filename=${file##*/}
  filesuffix=$(echo "${filename}" | cut -d '.' -f 4-10)
  ${WGRIB2} "${file}" -match "${monthlyinstvars}" -fcst_ave 24hr "${OUTDIR}/inst.monthly.${MEMDIR}/inst.monthly.${filesuffix}"
done
