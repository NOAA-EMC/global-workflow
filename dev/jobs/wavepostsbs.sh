#! /usr/bin/env bash

set -x

###############################################################
# Source FV3GFS workflow modules
#source "${HOMEgfs}/dev/ush/load_fv3gfs_modules.sh"
source "${HOMEgfs}/dev/ush/load_ufswm_modules.sh"
err=$?
if [[ "${err}" -ne 0 ]]; then
  exit "${err}"
fi

export job="wavepostsbs"

###############################################################
# shellcheck disable=SC2153
IFS=', ' read -r -a fhr_list <<< "${FHR_LIST}"

export FORECAST_HOUR jobid
for FORECAST_HOUR in "${fhr_list[@]}"; do
  fhr3=$(printf '%03d' "${FORECAST_HOUR}")
  jobid="${job}_f${fhr3}.$$"
  # Execute the JJOB
  "${HOMEgfs}/jobs/JGLOBAL_WAVE_POST_SBS"
  err=$?
  if [[ "${err}" -ne 0 ]]; then
    exit "${err}"
  fi
done

exit 0
