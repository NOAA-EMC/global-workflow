#! /usr/bin/env bash

set -x

###############################################################
source "${HOMEgfs}/dev/ush/load_fv3gfs_modules.sh"
err=$?
if [[ "${err}" -ne 0 ]]; then
  exit "${err}"
fi

export job="wavegempak"
export jobid="${job}.$$"

###############################################################
# shellcheck disable=SC2153
IFS=', ' read -r -a fhr_list <<< "${FHR_LIST}"

export FORECAST_HOUR jobid
for FORECAST_HOUR in "${fhr_list[@]}"; do
  fhr3=$(printf '%03d' "${FORECAST_HOUR}")
  jobid="${job}_f${fhr3}.$$"
  # Execute the JJOB
  "${HOMEgfs}/jobs/JGLOBAL_WAVE_GEMPAK"
  err=$?
  if [[ "${err}" -ne 0 ]]; then
    exit "${err}"
  fi
done

exit 0
