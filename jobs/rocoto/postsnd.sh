#! /usr/bin/env bash

source "${HOMEgfs}/ush/preamble.sh"
source "${HOMEgfs}/ush/load_fv3gfs_modules.sh"
status=$?
if [[ ${status} -ne 0 ]]; then
  exit "${status}"
fi

export job="postsnd"
export jobid="${job}.$$"

# shellcheck disable=SC2153
if [[ "${RUN}" == "gefs" ]]; then
  echo "add J-script here for GEFS."
else
  ################################################################
  # Execute the JJOB
  ${HOMEgfs}/jobs/JGFS_ATMOS_POSTSND
  err=$?
  exit "${err}"
fi
