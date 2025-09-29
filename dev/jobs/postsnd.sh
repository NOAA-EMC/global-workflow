#! /usr/bin/env bash

set -x

source "${HOMEgfs}/dev/ush/load_fv3gfs_modules.sh"
status=$?
if [[ ${status} -ne 0 ]]; then
  exit "${status}"
fi

export job="postsnd"
export jobid="${job}.$$"

################################################################
# Execute the JJOB
"${HOMEgfs}/jobs/JGFS_ATMOS_POSTSND"
err=$?
exit "${err}"
