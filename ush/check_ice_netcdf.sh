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
