#! /usr/bin/env bash

yyyy=${1?}
mm=${2?}
dd=${3?}
cyc=${4?}
fhr=${5?}
ROTDIR=${6?}
member=${7?}
FHOUT_ICE_GFS=${8?}

fhri=$((10#${fhr}))

#Will need to consider fhmin in the future to calculate the offset if we are to stick with this approach. 
((offset = ( cyc ) % FHOUT_ICE_GFS))

if (( offset != 0  )); then
  (( fhri = fhri - cyc ))
  fhr3=$(printf %03i "${fhri}")
  if (( fhri <= FHOUT_ICE_GFS  )); then
    (( interval = FHOUT_ICE_GFS - cyc )) 
    ncfile=${ROTDIR}/gefs.${yyyy}${mm}${dd}/${cyc}/mem${member}/model/ice/history/gefs.ice.t${cyc}z.${interval}hr_avg.f${fhr3}.nc
  else
    ncfile=${ROTDIR}/gefs.${yyyy}${mm}${dd}/${cyc}/mem${member}/model/ice/history/gefs.ice.t${cyc}z.${FHOUT_ICE_GFS}hr_avg.f${fhr3}.nc
  fi
else
  ncfile=${ROTDIR}/gefs.${yyyy}${mm}${dd}/${cyc}/mem${member}/model/ice/history/gefs.ice.t${cyc}z.${FHOUT_ICE_GFS}hr_avg.f${fhr}.nc
fi

#Check if netcdf file exists.
if [[ ! -f "${ncfile}" ]];then
  rc=1
else
  #Check if netcdf file is older than 2 minutes.
  ncage="$(find "${ncfile}" -mmin -2)"
  if [[ -n "${ncage}" ]]; then
    rc=1
  else
    rc=0
  fi
fi

exit "${rc}"
