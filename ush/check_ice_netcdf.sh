#! /usr/bin/env bash

# shellcheck disable=SC2155,SC2312
HOMEgfs=$(cd "$(dirname "$(readlink -f -n "${BASH_SOURCE[0]}" )" )/.." && pwd -P)
declare -rx HOMEgfs

source "${HOMEgfs}/ush/load_fv3gfs_modules.sh" 1>/dev/null 2>&1
source "${HOMEgfs}/ush/jjob_header.sh" -e "oceanice_products" -c "base oceanice_products" 1>/dev/null 2>&1

yyyy=${1?}
mm=${2?}
dd=${3?}
cyc=${4?}
fhr=${5?}
ROTDIR=${6?}
member=${7?}

fhri=$(echo "${fhr}" | sed 's/^0*//')

if [[ "${cyc}" != "00" ]] && (( FHOUT_ICE_GFS % 24 == 0 ));then
  (( fhri = fhri - cyc ))
  fhr3=$(printf %03i "${fhri}")
  if (( fhri <= 24  )); then
    (( interval = 24 - cyc )) 
    ncfile=${ROTDIR}/gefs.${yyyy}${mm}${dd}/${cyc}/mem${member}/model_data/ice/history/gefs.ice.t${cyc}z.${interval}hr_avg.f${fhr3}.nc
  else
    ncfile=${ROTDIR}/gefs.${yyyy}${mm}${dd}/${cyc}/mem${member}/model_data/ice/history/gefs.ice.t${cyc}z.${FHOUT_ICE_GFS}hr_avg.f${fhr3}.nc
  fi
else
  ncfile=${ROTDIR}/gefs.${yyyy}${mm}${dd}/${cyc}/mem${member}/model_data/ice/history/gefs.ice.t${cyc}z.${FHOUT_ICE_GFS}hr_avg.f${fhr}.nc
fi

ls -l "${ncfile}" 2>&1  # redirect stdout and stderr to /dev/null to suppress output in cron

rc=$?
# If there is no error, rc=0, else rc!=0

exit "${rc}"
