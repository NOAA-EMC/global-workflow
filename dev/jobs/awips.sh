#! /usr/bin/env bash

set -x
source "${HOMEgfs}/dev/ush/load_fv3gfs_modules.sh"
status=$?
if [[ ${status} -ne 0 ]]; then
  exit "${status}"
fi

export job="awips"

# shellcheck disable=SC2153
IFS=', ' read -r -a fhr_list <<< "${FHR_LIST}"

export FHR3 jobid
for fhr in "${fhr_list[@]}"; do
  FHR3=$(printf '%03d' "${fhr}")
  jobid="${job}_f${FHR3}.$$"
  ###############################################################
  # Execute the JJOB # TODO
  ###############################################################
 # "${HOMEgfs}/jobs/J${RUN^^}_ATMOS_AWIPS"
  err=$?
  if [[ ${err} -ne 0 ]]; then
    exit "${err}"
  fi
done

exit 0
