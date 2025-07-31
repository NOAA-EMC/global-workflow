#! /usr/bin/env bash

set -x
source "${HOMEgfs}/dev/ush/load_fv3gfs_modules.sh"
status=$?
if (( status != 0 )); then exit "${status}"; fi

export job="gempak"

# shellcheck disable=SC2153
IFS=', ' read -r -a fhr_list <<< "${FHR_LIST}"

export FHR3 jobid
for fhr in "${fhr_list[@]}"; do
  FHR3=$(printf '%03d' "${fhr}")
  jobid="${job}_f${FHR3}.$$"
  ###############################################################
  # Execute the JJOB
  ###############################################################
  "${HOMEgfs}/jobs/J${RUN^^}_ATMOS_GEMPAK"
  err=$?
  [[ ${err} -ne 0 ]] && exit "${err}"
done

exit 0
